#####################################
############ reactiveVal ############
#####################################

# Create a reactive object to store data
subclass_names <- reactiveVal()

# ReactiveVal to store sample and subclass data
cal_samples <- reactiveVal()

# ReactiveVal to store calibration data
cal_data <- reactiveVal()

# Store matrices and cell selections for each subclass
matrix_list <- reactiveVal(list())
selected_cells_list <- reactiveVal(list())

# Define total_area_table as a global reactive variable
total_area_table <- reactiveVal()
sums_values <- reactiveValues()

graph_data <- reactiveVal(data.frame())

#########################################
################## SQL ##################
#########################################

#' These queries will need to be transferred to db_get files ...

get_subclass_names <- function(db, project) {
  query <- sprintf("SELECT subclass_name FROM subclass WHERE project = '%s'", project)
  cat("Executing query:", query, "\n")  # Debugging
  dbGetQuery(db, query)$subclass_name
}

record_subclass_quanti <- function(db, project, subclass_name) {
  query <- sprintf("INSERT INTO subclass (project, subclass_name) VALUES ('%s', '%s')", project, subclass_name)
  cat("Executing query:", query, "\n")  # Debugging
  dbExecute(db, query)
}

delete_subclass_quanti <- function(db, project, subclass_name) {
  query <- sprintf("DELETE FROM subclass WHERE project = '%s' AND subclass_name = '%s'", project, subclass_name)
  cat("Executing query:", query, "\n")  # Debugging
  dbExecute(db, query)
}

get_samples_project_quanti <- function(db, project = NULL) {
  if (is.null(project)) {
    return(data.frame())
  }

  query <- sprintf(
    "SELECT sample, sample_type FROM sample WHERE sample IN (
      SELECT sample FROM project_sample WHERE project = '%s'
    )", project
  )
  cat("Executing query:", query, "\n")  # Debugging
  dbGetQuery(db, query)
}

get_cal_samples <- function(db, project) {
  project_samples <- dbGetQuery(db, sprintf("SELECT sample FROM project_sample WHERE project = '%s'", project))

  if (nrow(project_samples) == 0) {
    return(data.frame(sample = character(0)))
  }

  samples_str <- paste(sprintf("'%s'", project_samples$sample), collapse = ", ")
  query <- sprintf("SELECT sample FROM sample WHERE sample IN (%s) AND sample_type = 'CAL'", samples_str)
  cat("Executing query:", query, "\n")  # Debugging
  dbGetQuery(db, query)
}

record_cal_data <- function(db, samples, subclasses) {
  if (length(subclasses) == 0) {
    cat("No subclasses provided\n")
    return()
  } else {
    cat("Subclasses:", paste(subclasses, collapse=", "), "\n")
  }

  if (length(samples) == 0) {
    cat("No samples provided\n")
    return()
  } else {
    cat("Samples:", paste(samples, collapse=", "), "\n")
  }

  # Function to escape special characters in strings
  escape_sql <- function(x) {
    gsub("'", "''", x)
  }

  # Escape special characters
  samples <- sapply(samples, escape_sql)
  subclasses <- sapply(subclasses, escape_sql)

  # Insert samples for each subclass in cal_data table
  for (sample in samples) {
    for (subclass in subclasses) {
      # Check if sample already exists for this subclass
      check_query <- sprintf(
        "SELECT COUNT(*) FROM cal_data WHERE sample = '%s' AND subclass = '%s';",
        sample, subclass
      )
      
      existing_count <- dbGetQuery(db, check_query)$`COUNT(*)`
      
      if (existing_count == 0) {
        # Insert sample if it doesn't exist
        insert_query <- sprintf(
          "INSERT INTO cal_data (sample, subclass, concentration, chlorination) VALUES ('%s', '%s', NULL, NULL);",
          sample, subclass
        )

        cat("Executing query for sample:", insert_query, "\n")  # Debugging
        dbExecute(db, insert_query)
      } else {
        cat("Sample already exists for subclass:", sample, subclass, "\n")  # Debugging
      }
    }
  }
}

get_cal_data <- function(db, samples, subclasses) {
  # Check if the lists of samples and subclasses are not empty
  if (length(samples) == 0 || length(subclasses) == 0) {
    cat("No samples or subclasses provided\n")
    return(data.frame())
  }

  # Function to escape special characters in strings
  escape_sql <- function(x) {
    gsub("'", "''", x)
  }

  # Convert lists to comma-separated strings
  samples <- sapply(samples, escape_sql)
  subclasses <- sapply(subclasses, escape_sql)
  samples_str <- paste(sprintf("'%s'", samples), collapse = ", ")
  subclasses_str <- paste(sprintf("'%s'", subclasses), collapse = ", ")

  # Build the SQL query
  query <- sprintf(
    "SELECT * FROM cal_data WHERE sample IN (%s) AND subclass IN (%s);",
    samples_str, subclasses_str
  )

  cat("Executing query:", query, "\n")  # Debugging
  df <- dbGetQuery(db, query)
  if (nrow(df) == 0) {
    cat("No calibration data found for the given samples and subclasses\n")
  }

  return(df)
}

# Extraire les valeurs de concentration de cal_data
get_concentration_data <- function(db, samples) {
  if (length(samples) == 0) {
    cat("No samples provided for concentration data retrieval.\n")
    return(data.frame())
  }

  # No calibration data found for the given samples and subclasses
  query <- sprintf(
    "SELECT DISTINCT sample, concentration FROM cal_data WHERE sample IN (%s);",
    paste(sprintf("'%s'", samples), collapse = ", ")
  )

  cat("Executing query for concentration data:", query, "\n")  # Debugging
  concentration_data <- dbGetQuery(db, query)
  
  return(concentration_data)
}

get_standard_table_quanti <- function(db, project = NULL, adducts = NULL, standard_formulas = NULL, cal_samples = NULL){
  # Retrieve all samples from the project
  sample <- get_samples(db, project)
  
  # Keep only samples that are of type "CAL"
  sample <- sample[sample$sample %in% cal_samples$sample, ]
  
  table <- NULL
  
  for (adduct in adducts) {
    for (standard_formula in standard_formulas) {
      for (i in 1:length(sample$project_sample)) {
        # Query of normal standard with iso = A
        query <- sprintf(
          'SELECT intensities, intensities_b, score, weighted_deviation 
          FROM feature 
          WHERE iso == "A"
          AND project_sample IN (
            SELECT project_sample 
            FROM project_sample 
            WHERE project == %s 
            AND sample_id == "%s"
          ) 
          AND chemical_ion IN (
            SELECT chemical_ion 
            FROM chemical_ion 
            WHERE adduct == "%s" 
            AND chemical == (
              SELECT chemical 
              FROM chemical 
              WHERE chemical_type == "%s"
            )
          );',
          project, sample$sample_id[i], adduct, standard_formula
        )
        
        data <- db_get_query(db, query)
        if (nrow(data) > 0) {
          data <- cbind(sample_id = sample$sample_id[i], formula = standard_formula, adduct = adduct, data)
        }
        
        # Second try: the standard had no results but has a theoretical pattern (iso = "no ROIs" or something like that)
        if (nrow(data) == 0) {
          query <- sprintf(
            'SELECT intensities, intensities_b, score, weighted_deviation 
            FROM feature 
            WHERE iso == "no ROIs" 
            AND project_sample IN (
              SELECT project_sample 
              FROM project_sample 
              WHERE project == %s 
              AND sample_id == "%s"
            ) 
            AND chemical_ion IN (
              SELECT chemical_ion 
              FROM chemical_ion 
              WHERE adduct == "%s" 
              AND chemical == (
                SELECT chemical 
                FROM chemical 
                WHERE chemical_type == "%s"
              )
            );',
            project, sample$sample_id[i], adduct, standard_formula
          )
          
          dataOut <- db_get_query(db, query)
          if (nrow(dataOut) > 0) {
            data <- cbind(sample_id = sample$sample_id[i], formula = standard_formula, adduct = adduct, dataOut)
          }
        }
        
        # Last try: the standard has been asked but the adduct is not possible with it
        if (nrow(data) == 0) {
          data <- cbind(
            sample_id = sample$sample_id[i], 
            formula = standard_formula, 
            adduct = adduct, 
            intensities = "not possible", 
            intensities_b = "not possible", 
            score = "not possible", 
            weighted_deviation = "not possible"
          )
        }
        
        table <- as.data.frame(rbind(table, data))
      }
    }
  }

  # Eliminate duplicates
  table <- unique(table)

  if (class(table$intensities) != "character") {
    table$intensities[which(!is.na(table$intensities))] <- formatC(
      as.numeric(table$intensities[which(!is.na(table$intensities))]), format = 'f', big.mark = " ", digits = 0
    )
  }
  
  if (class(table$intensities_b) != "character") {
    table$intensities_b[which(!is.na(table$intensities_b))] <- formatC(
      as.numeric(table$intensities_b[which(!is.na(table$intensities_b))]), format = 'f', big.mark = " ", digits = 0
    )
  }
  
  if (class(table$score) != "character") {
    table$score[which(!is.na(table$score))] <- round(table$score[which(!is.na(table$score))], digits = 0)
  }
  
  if (class(table$weighted_deviation) != "character") {
    table$weighted_deviation[which(!is.na(table$weighted_deviation))] <- round(
      table$weighted_deviation[which(!is.na(table$weighted_deviation))] * 10^3, digits = 2
    )
  }
  
  data.table::setnames(table, c("intensities", "intensities_b", "weighted_deviation"), c("total area", "area above baseline", "deviation(mDa)"))
  
  return(table)
}

##########################################
############### Functions ################
##########################################

# Change this reactive function to a function with arguments
# Set mat() with export = TRUE to have the most digits
quanti_final_mat_func <- function(db, project, select_choice = 2, adducts) {
  # Initialize the progress bar
  withProgress(message = 'Matrix recovery...', value = 0, {
    # Retrieve all samples from the project
    samples <- get_samples(db, project)
    print("samples")
    print(samples)
    
    # Check if samples is not empty
    if (nrow(samples) == 0) {
      return(list())
    }

    # Filter calibration samples
    cal_samples <- get_cal_samples(db, project)
    print("cal_samples")
    print(cal_samples)
    
    # Keep only calibration samples
    cal_sample_ids <- cal_samples$sample
    print("cal_sample_ids")
    print(cal_sample_ids)
    
    # Filter samples to keep only calibration samples
    cal_samples_filtered <- samples[samples$sample %in% cal_sample_ids, ]
    print("cal_samples_filtered")
    print(cal_samples_filtered)
    
    # Check if cal_samples_filtered is not empty
    if (nrow(cal_samples_filtered) == 0) {
      return(list())
    }

    print("adducts")
    print(adducts)

    # Check if all_adducts is not empty
    if (length(adducts) == 0) {
      return(list())
    }
    
    # Retrieve all available chemical types, excluding standards
    query <- sprintf("SELECT chemical_type 
                      FROM chemical 
                      WHERE chemical_familly != 'Standard' 
                      AND chemical_type IN (
                          SELECT chemical_type 
                          FROM deconvolution_param 
                          WHERE project = '%s'
                      )", project)
    chemical_table <- unique(db_get_query(db, query))
    chemical_types <- chemical_table$chemical_type
    print("chemical_types")
    print(chemical_types)
    
    # Check if chemical_types is not empty
    if (length(chemical_types) == 0) {
      return(list())
    }
    
    result_tables <- list()

    # Calculate the total number of iterations for the progress bar
    total_iterations <- nrow(cal_samples_filtered) * length(chemical_types) * length(adducts)
    iteration <- 0
    
    values$export <- TRUE
    for (file in cal_samples_filtered$sample_id) {
      for (study in chemical_types) {
        for (adduct in adducts) {
          # Update the progress bar
          iteration <- iteration + 1
          incProgress(1 / total_iterations, detail = sprintf("Processing %s - %s - %s (%d/%d)", file, study, adduct, iteration, total_iterations))
          
          # Check if the adduct exists for the current study
          if (adduct %in% names(mat()[[file]][[study]])) {
            # Reduce the matrix based on user choices
            table <- reduce_matrix(mat()[[file]][[study]][[adduct]], select_choice, greycells = TRUE)
            if (!("Error" %in% colnames(table))) {
              result_tables[[paste(file, study, adduct, sep = "_")]] <- list(
                table = table,
                sample_id = file,
                chemical_type = study,
                adduct = adduct
              )
            } else {
              result_tables[[paste(file, study, adduct, sep = "_")]] <- list(
                table = data.frame(Error = paste("This adduct doesn't exist for this chemical type sorry !", 3, sep = "/")),
                sample_id = file,
                chemical_type = study,
                adduct = adduct
              )
            }
          } else {
            result_tables[[paste(file, study, adduct, sep = "_")]] <- list(
              table = data.frame(Error = paste("This adduct doesn't exist for this chemical type sorry !", 3, sep = "/")),
              sample_id = file,
              chemical_type = study,
              adduct = adduct          
            )
          }
        }
      }
    }
    values$export <- FALSE

    print("##########################################")
    print("########### REDUCE_MATRIX DATA ###########")
    print("##########################################")
    print(cal_samples_filtered$sample_id)
    print("##########################################")
    print(chemical_types)
    print("##########################################")
    print(adducts)
    print("##########################################")
    print(select_choice)
    print("##########################################")
    
    return(result_tables)
  })
}


# Function to retrieve values from data matrices based on selected positions
get_selected_values <- function(positions, matrix) {
  values <- apply(positions, 1, function(pos) {
    row <- pos[1]
    col <- pos[2]
    return(matrix[row, col])
  })
  return(values)
}

# Function to calculate the sum of numeric values
calculate_numeric_sum <- function(subclass, quanti_matrices, selected_cells) {
  # Get initial sums for each file name
  initial_sums <- sapply(names(quanti_matrices), function(name) {
    matrix <- quanti_matrices[[name]]$table
    positions <- selected_cells[[subclass]]
    selected_values <- get_selected_values(positions, matrix)
    
    # Filter and convert numeric values
    numeric_values <- as.numeric(gsub("[^0-9.]", "", selected_values, perl = TRUE))
    numeric_values <- numeric_values[!is.na(numeric_values)]
    
    sum(numeric_values, na.rm = TRUE)
  })
  
  # Extract the common base of the file name by removing the suffix after the last underscore
  base_names <- sub("_M.*", "", names(quanti_matrices))
  
  # Create a DataFrame to group and sum
  sums_df <- data.frame(
    file_name = base_names,
    sum_value = initial_sums,
    stringsAsFactors = FALSE
  )
  
    # Group by file name base and sum the values
  grouped_sums <- sums_df %>%
    group_by(file_name) %>%
    summarize(total_sum = sum(sum_value), .groups = 'drop')
  
  grouped_sums <- grouped_sums %>%
    mutate(file_name = sub("_PCAs.*", "", file_name))

  # Convert to named vector for return
  named_sums <- setNames(grouped_sums$total_sum, grouped_sums$file_name)

  return(named_sums)
}


#####################################
############ Sample Type ############
#####################################

#' The goal here is to allow the user to enter sample types
#' 3 types of samples: CAL, BLANK, SAMPLE
#' If the type is equal to CAL then enter the information.
#' The info: subclassname SCCP, MCCP, LCCP || chlorination_degree || concentration
#' 
#' For now, the user can fill in whatever they want in whichever columns they want.
#' Later, add restrictions. (DROPDOWN MENU ...)

# Create a reactive object to store data
sample_type_data <- reactive({
  params <- list(
    table_selected = input$quantification_choice
  )
  tryCatch({  
    if(input$quantification_choice == "Sample Type"){  # Use '==' for comparison
        # Get data from project samples
        data <- get_samples_project_quanti(db, input$project)
        if (nrow(data) == 0) {
          print("Aucune donnée trouvée pour ce projet.")
          return(data.frame())  # Return an empty data frame if no data is found
        }
        # print(data)  # Debugging
        return(data)
  }})
})

output$quanti_table_type <- renderDT({
  DT::datatable(sample_type_data(), editable = list(target = "cell", disable = list(columns = c(0, 1))),
            options = list(
              scrollX = TRUE,  # Enable horizontal scroller
              scroller = TRUE,  # Enable scroller to improve performance with large tables
              paging = FALSE  # Enable pagination
            )
  )
})

observeEvent(c(input$quanti_table_type_cell_edit, input$project), {

  project_id <- input$project
  print(project_id)

  info <- input$quanti_table_type_cell_edit

  # Get the current data
  data_df <- isolate(sample_type_data())

  # Check if info is NULL
  if (!is.null(info)) {
    # Update the value of the data.frame
    row <- info$row
    col <- info$col
    value <- info$value

    # Check edited columns
    editable_columns <- c("sample_type")
    if (colnames(data_df)[col] %in% editable_columns) {
      data_df[row, col] <- DT::coerceValue(value, data_df[row, col])

      # Query to update the database with the modifications
      sample_id <- data_df[row, "sample"]
      column_name <- colnames(data_df)[col]
      update_query <- sprintf("UPDATE sample SET %s = ? WHERE sample = ?", column_name)

      # Execute the query
      dbExecute(db, update_query, params = list(value, sample_id))

      # Update calibration samples after the modification
      cal_samples_data <- get_cal_samples(db, input$project)
      cal_samples(cal_samples_data)
      print(cal_samples())
    }
  }
})

#########################################
############ Sample Subclass ############
#########################################

# Observe changes in project and update subclasses
observeEvent(input$project, {
  subclasses <- get_subclass_names(db, input$project)
  subclass_names(subclasses)
  print(subclass_names(subclasses))  # Debugging
  updateSelectInput(session, "delete_subclass", choices = subclasses)
  
  cal_samples <- get_cal_samples(db, input$project)
  cal_samples(cal_samples)
  print(cal_samples())  # Debugging
})

output$quanti_table_subclass <- renderDT({
  datatable(data.frame(subclass_name = subclass_names()))  # Display the subclass names in the table
})

observeEvent(input$add_subclass, {
  req(input$new_subclass)  # Make sure a new subclass is entered
  record_subclass_quanti(db, input$project, input$new_subclass)
  updateTextInput(session, "new_subclass", value = "")  # Clear the input field
  
  # Update the subclasses
  subclasses <- get_subclass_names(db, input$project)
  subclass_names(subclasses)
  updateSelectInput(session, "delete_subclass", choices = subclasses)

  print(subclass_names())
})

observeEvent(input$remove_subclass, {
  req(input$delete_subclass)  # Make sure a subclass is selected
  delete_subclass_quanti(db, input$project, input$delete_subclass)
  
  # Update the subclasses
  subclasses <- get_subclass_names(db, input$project)
  subclass_names(subclasses)
  updateSelectInput(session, "delete_subclass", choices = subclasses)

  print(subclass_names())
})


#####################################
############ Calibration ############
#####################################

# Observe changes in quantification choice and update the calibrations accordingly
observeEvent(input$quantification_choice, {
  if (input$quantification_choice == "Calibration") {
    samples <- cal_samples()
    subclasses <- subclass_names()
    record_cal_data(db, samples, subclasses)
    df <- get_cal_data(db, samples, subclasses)
    cal_data(df)
  }
})

# Render the calibration table with editable cells
output$cal_samples_table <- renderDT({
  df <- cal_data()
  
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  
  datatable(df[,-1], 
            editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
            options = list(
              paging = FALSE  # Enable pagination
            )
  )
})

# Observer to update the database when a cell is edited
observeEvent(input$cal_samples_table_cell_edit, {
  info <- input$cal_samples_table_cell_edit
  str(info)  # Debugging
  
  # Convert the column index to the column name
  col_name <- colnames(cal_data()[,-1])[info$col]

  # Retrive the row data corresponding to info$row
  row_data <- cal_data()[info$row, ]

  # Construct and execute the UPDATE query
  query <- sprintf(
    "UPDATE cal_data SET %s = '%s' WHERE sample = '%s' AND subclass = '%s';",
    col_name, info$value, row_data$sample, row_data$subclass
  )

  cat("Executing query:", query, "\n")  # Debugging
  dbExecute(db, query)
})

##########################################
############ Homologue Domain ############
##########################################

#' The goal is to return empty matrices for each subclass
#' The user will select homolog groups in each matrix by clicking
#' The matrices will have C6-C36 as the first column and Cl3-Cl30 as the first row
#' The selected homolog groups will be stored by matrix

output$quanti_subclass_dropdown <- shiny::renderUI({
  bsplus::shinyInput_label_embed(
    shiny::selectInput("quanti_subclass_dropdown", "Subclass Name", choices = subclass_names(), multiple = FALSE),
    bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = 'Select Subclass')
  )
})

# Obeserver to initialize matrices and selected cells
observe({
  subclasses <- subclass_names()
  
  # initialize matrices for each subclass
  matrices <- lapply(subclasses, function(subclass) {
    matrix <- matrix(ncol = 28, nrow = 31)
    colnames(matrix) <- paste0("Cl", 3:30)
    rownames(matrix) <- paste0("C", 6:36)
    as.data.frame(matrix)
  })
  
  names(matrices) <- subclasses
  matrix_list(matrices)
  
  # Initialize the selection of cells for each subclass
  selected_cells <- lapply(subclasses, function(subclass) {
    data.frame(row = integer(0), col = integer(0))
  })
  names(selected_cells) <- subclasses
  selected_cells_list(selected_cells)
})

# Display the datatable to show the matrix of the selected subclass
output$quanti_matrix_homologue <- DT::renderDataTable({
  req(input$quanti_subclass_dropdown)
  matrices <- matrix_list()
  matrix_to_display <- matrices[[input$quanti_subclass_dropdown]]
  
  datatable(matrix_to_display,
            selection = list(mode = 'multiple', target = 'cell'),
            class = 'display cell-border compact nowrap',
            options = list(info = FALSE, paging = FALSE, dom = 'Bfrtip',
                           scrollX = TRUE, scroller = TRUE, bFilter = FALSE,
                           ordering = FALSE,
                           columnDefs = list(list(className = 'dt-body-justify',
                                                  targets = "_all")))
  )
})

# Observer to update the selected cells
observeEvent(input$quanti_matrix_homologue_cells_selected, {
  cells <- input$quanti_matrix_homologue_cells_selected
  if (!is.null(cells)) {
    selected_cells <- selected_cells_list()
    selected_cells[[input$quanti_subclass_dropdown]] <- cells
    selected_cells_list(selected_cells)
    # print("selected_cells")
    # print(selected_cells)
  }
})

###########################################
############ Internal Standard ############
###########################################

# Allows choosing an internal standard by subclass (dynamically)
# Allows choosing one or more adducts by subclass (dynamically)

output$quanti_dynamic_IS <- shiny::renderUI({
  # Generate the div for each subclass
  lapply(subclass_names(), function(subclass) {
    shiny::fluidRow(
      shiny::column(width = 12,
        div(
          shiny::column(width = 2, subclass),
          shiny::column(width = 5, shiny::uiOutput(paste0("quanti_IS_chemical_adduct_", subclass))),
          shiny::column(width = 5, shiny::uiOutput(paste0("quanti_IS_standard_formula_", subclass)))
        )
      )
    )
  })
})

# Generate dynamically the `uiOutput` for each subclass
observe({
  project_id <- input$project

  lapply(subclass_names(), function(subclass) {
    local({
      subclass_local <- subclass

      output[[paste0("quanti_IS_chemical_adduct_", subclass_local)]] <- shiny::renderUI({
        table <- db_get_query(db, sprintf("SELECT DISTINCT adduct FROM deconvolution_param WHERE project = %s", project_id))
        adduct_list <- table$adduct
        
        bsplus::shinyInput_label_embed(
          shiny::selectInput(paste0("process_adduct_", subclass_local), "Adduct(s)", choices = adduct_list, multiple = TRUE),
          bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = 'Adduct(s) to use')
        )
      })
      
      output[[paste0("quanti_IS_standard_formula_", subclass_local)]] <- shiny::renderUI({
        query <- sprintf("SELECT chemical_type 
                          FROM chemical 
                          WHERE chemical_familly = 'Standard' 
                          AND chemical_type IN (
                              SELECT chemical_type 
                              FROM deconvolution_param 
                              WHERE project = %s
                          )", project_id)
        table <- unique(db_get_query(db, query))
        std_list <- table$chemical_type
        
        bsplus::shinyInput_label_embed(
          shinyWidgets::pickerInput(paste0("process_standard_type_", subclass_local), "Standard formula", choices = std_list),
          bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = "Formula of the standard")
        )
      })
    })
  })
})

adducts_values <- reactiveValues()
standards_values <- reactiveValues()

observe({
  # For each subclass, create a local environment to store the subclass name
  lapply(subclass_names(), function(subclass) {
    local({
      subclass_local <- subclass

      # Observer for changes in adducts
      observeEvent(input[[paste0("process_adduct_", subclass_local)]], {
        adduct_input <- input[[paste0("process_adduct_", subclass_local)]]
        adducts_values[[subclass_local]] <- adduct_input

        # Debugging
        shiny::req(adduct_input)  # Make sure the input is available
        print(paste("Adduct Input for", subclass_local, ":"))
        print(adduct_input)
      }, priority = 10)

      # Observer for changes in standards
      observeEvent(input[[paste0("process_standard_type_", subclass_local)]], {
        standard_input <- input[[paste0("process_standard_type_", subclass_local)]]
        standards_values[[subclass_local]] <- standard_input

        # Debugging
        shiny::req(standard_input)  # Make sure the input is available
        print(paste("Standard Input for", subclass_local, ":"))
        print(standard_input)
      }, priority = 10)
    })
  })
})


#######################################
############ Launch Quanti ############
#######################################

observeEvent(input$quanti_launch, {
  shinyjs::disable("quanti_launch")  # Disable the launch button
  
  # Initialize the progress bar
  withProgress(message = 'Quantification in progress...', value = 0, {
    
    subclasses <- subclass_names()
    selected_cells <- selected_cells_list()
    print(selected_cells) # Debugging
    cal_samples <- cal_samples()

    # Retrieve calibration data
    samples <- unique(unlist(cal_samples))
    cal_data_df <- get_cal_data(db, samples, subclasses)
    if (nrow(cal_data_df) == 0) {
      showNotification("Failed to retrieve calibration data.", type = "error")
      shinyjs::enable("quanti_launch") # Enable the launch button
      return()
    }
    
    # Retrieve all unique adducts and standards
    adducts <- unique(unlist(lapply(subclasses, function(subclass) adducts_values[[subclass]])))
    standards <- unique(unlist(lapply(subclasses, function(subclass) standards_values[[subclass]])))
    
    if (length(adducts) == 0 || length(standards) == 0) {
      showNotification("No adducts or standards selected in Internal Standard section. Please select them to proceed.", type = "error")
      shinyjs::enable("quanti_launch") # Enable the launch button
      return()
    }

    # Verify that there are selected cells
    if (all(sapply(selected_cells, function(mat) nrow(mat) == 0))) {
      showNotification("No cells selected in Homologue Domain section. Please select cells to proceed.", type = "error")
      shinyjs::enable("quanti_launch") # Enable the launch button
      return()
    }

    # Updating progress
    incProgress(0.1, detail = "Matrix recovery")
    
    quanti_matrices <- quanti_final_mat_func(db, input$project, 2, adducts)
    if (is.null(quanti_matrices)) {
      showNotification("Failed to retrieve quantification matrices.", type = "error")
      shinyjs::enable("quanti_launch") # Enable the launch button
      return()
    }

    # Calculating numeric sums
    incProgress(0.1, detail = "Calculating numerical sums")
    
    lapply(subclasses, function(subclass) {
      if (!is.null(selected_cells[[subclass]])) {       
        sums <- calculate_numeric_sum(subclass, quanti_matrices, selected_cells)
        sums_values[[subclass]] <- sums
      }
    })

    # Convert sums_values to DataFrame
    create_sums_df <- function(sums_values) {
      sums_list <- lapply(names(sums_values), function(subclass) {
        sums <- sums_values[[subclass]]
        data.frame(
          file_name = names(sums),
          subclass = subclass,
          sum_value = as.numeric(sums),
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, sums_list)
    }

    sums_df <- create_sums_df(sums_values)
    
    if (nrow(sums_df) == 0) {
      showNotification("No sums calculated for the selected subclasses.", type = "error")
      shinyjs::enable("quanti_launch") # Enable the launch button
      return()
    }

    # Join calibration data with sums data with dplyr
    cal_data_df$sample_id <- sub("^(neg|pos) ", "", cal_data_df$sample)
    cal_data_df <- cal_data_df %>%
      left_join(sums_df %>% select(file_name, subclass, sum_value), 
                by = c("sample_id" = "file_name", "subclass"))
    
    # Updating progress
    incProgress(0.4, detail = "Calibration data update")
    
    if (nrow(cal_data_df) == 0) {
      showNotification("Calibration data merge failed.", type = "error")
      shinyjs::enable("quanti_launch") # Enable the launch button
      return()
    }
    
    # Retrieve standard table and process subclasses
    if (length(adducts) > 0 && length(standards) > 0) {
      standard_table <- get_standard_table_quanti(db, input$project, adducts, standards, cal_samples)
      if (nrow(standard_table) == 0) {
        showNotification("No results for the standard table.", type = "warning")
      } else {
        standard_table <- standard_table %>% distinct()
        standard_table$`total area` <- as.numeric(gsub(" ", "", standard_table$`total area`)) / 10^6
        total_area_table(standard_table)
      }
    }
    
    if (!is.null(sums_values) && !is.null(total_area_table()) && nrow(total_area_table()) > 0) {
      print("Processing subclasses:")
      
      process_subclass <- function(subclass) {       
        standard_formula <- standards_values[[subclass]]

        # Uilisation of dplyr to filter the dataframe filtered_table
        filtered_table <- total_area_table() %>%
          filter(adduct %in% adducts_values[[subclass]], formula == standard_formula)

        # Uilisation of dplyr to create a new dataframe areas_std
        areas_std <- filtered_table %>%
          mutate(`total area` = ifelse(is.na(`total area`), 0, `total area`)) %>%
          group_by(sample_id) %>%
          summarize(total_area_sum = sum(`total area`), .groups = 'drop')
        
        cal_data_df <- left_join(cal_data_df, areas_std, by = "sample_id")
        
        # Uilisation of dplyr change the dataframe cal_data_df
        cal_data_df <- cal_data_df %>%
          mutate(normalized_per_std = sum_value / total_area_sum * 1e6) %>%
          mutate(normalized_per_concentration = normalized_per_std / concentration)
        
        # csv before elimiation of NA, NaN, Inf
        # write.csv(cal_data_df, paste0("export/Quanti_data_", subclass, "_full.csv"))
        
        # Remove rows with NA, NaN, or Inf values
        cal_data_df <- cal_data_df %>%
          filter(!if_any(everything(), ~ is.na(.) | is.nan(.) | is.infinite(.)))
        
        # cal_data_df_rv(cal_data_df)
        # write.csv(cal_data_df, paste0("export/Quanti_data_", subclass, "_clean.csv"))
        
        assign(paste0("cal_data_df_", subclass), cal_data_df, envir = .GlobalEnv)

        # Calculation of the slope and average by group (dplyr)
        exponential_data <- cal_data_df %>%
          group_by(subclass, chlorination) %>%
          do({
            standard_lm <- lm(log(normalized_per_std) ~ concentration, data = .)
            data.frame(
              standard_slope = coef(standard_lm)[["concentration"]],
              response_factor = mean(.$normalized_per_concentration, na.rm = TRUE)
            )
          }) %>%
          ungroup()

        final_data <- cal_data_df %>%
        left_join(exponential_data, by = c("subclass", "chlorination"))

        graph_data(final_data)
        write.csv(final_data, paste0("export/graph_data.csv"))
      }
      
      # Update progress before subclass processing
      incProgress(0.4, detail = "Subclass processing")
      
      lapply(subclasses, process_subclass)
      
      # Finalize progress
      incProgress(0.1, detail = "Quantification complete")

      shinyjs::enable("quanti_launch") # Enable the launch button
    } else {
      showNotification("No standard values available for normalization.", type = "error")
      shinyjs::enable("quanti_launch") # Enable the launch button
    }
  })
})

observeEvent(c(input$graph_selector, graph_data()), {
  final_data <- graph_data()
  # If you have already your data saved and dont want to wait for the graphs to test the program
  # final_data <- read.csv("export/graph_data.csv") 
  if (nrow(final_data) == 0) {
    # Transformation of graph ggplot into interactive plotly graph
    output$plot_output <- renderPlotly({
      plotly_empty()
    })
  } else {
    req(input$graph_selector)

    # Determine the type of graph to display
    plot <- switch(input$graph_selector,
                   'Pre-Normalization' = {
                     ggplot(final_data, aes(x = sample_id, y = sum_value, color = subclass)) +
                       geom_point(size = 3) +
                       labs(title = "Values Before Normalization",
                            x = "Sample",
                            y = "Sum of Areas"
                            ) +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                       scale_color_viridis_d()
                   },
                   'Post-Normalization' = {
                    ggplot(final_data, aes(x = concentration, y = normalized_per_concentration)) +
                          geom_point() +
                          facet_grid(subclass ~ chlorination) +
                          labs(title = "Comparison of Individual Response Factor by Concentration",
                              x = "Concentration",
                              y = "Individual Response Factor"
                          ) +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                          scale_color_viridis_d() +
                          geom_hline(aes(yintercept = response_factor))           
                   },
                   'Regression on %Cl' = {
                     ggplot(final_data, aes(x = chlorination, y = response_factor)) +
                       facet_grid(. ~ subclass) +
                       geom_point() +
                       geom_smooth(
                         method = "glm",
                         formula = y ~ x,
                         se = FALSE,
                         method.args = list(family = gaussian(link = "log"))
                       ) +
                       labs(title = "Regression on %Cl",
                            x = "Chlorination Degree (%)",
                            y = "Response Factor"
                            ) +
                       theme_minimal() +
                       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                       scale_color_viridis_d()
                   },
                   # Default case to ensure plot is not NULL
                   {
                     ggplot() + 
                     labs(title = "Error in graph creation")
                   })

    # Convertion of graph ggplot into interactive plotly graph
    output$plot_output <- renderPlotly({
      if (inherits(plot, "gg")) {  # Vérifie si l'objet est bien un ggplot
        ggplotly(plot)
      } else {
        plotly_empty()  # Affiche un graphique vide si plot n'est pas un objet ggplot valide
      }
    })
  }
})

# Observe change in input$project to reset the cal_data_df reactive value to an empty data frame
observeEvent(input$project, {
  graph_data(data.frame())
})