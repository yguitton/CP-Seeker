#####################################
############ reactiveVal ############
#####################################

# Créer un objet réactif pour stocker les données
subclass_names <- reactiveVal()

# Réactifs pour stocker les données des échantillons et des sous-classes
cal_samples <- reactiveVal()

# Variable réactive pour stocker les données de calibration
cal_data <- reactiveVal()

# Stocker les matrices et les sélections de cellules pour chaque sous-classe
matrix_list <- reactiveVal(list())
selected_cells_list <- reactiveVal(list())

# Définir total_area_table comme une variable réactive globale
total_area_table <- reactiveVal()
sums_values <- reactiveValues()

cal_data_df_rv <- reactiveVal(data.frame())

#########################################
################## SQL ##################
#########################################

#' Ces requêtes vont devoir être transférés dans les fichiers db_get etc...

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

  # Fonction pour échapper les caractères spéciaux dans les chaînes
  escape_sql <- function(x) {
    gsub("'", "''", x)
  }

  # Échapper les caractères spéciaux
  samples <- sapply(samples, escape_sql)
  subclasses <- sapply(subclasses, escape_sql)

  # Insérer les échantillons pour chaque sous-classe dans la table cal_data
  for (sample in samples) {
    for (subclass in subclasses) {
      # Vérifier si l'échantillon existe déjà pour cette sous-classe
      check_query <- sprintf(
        "SELECT COUNT(*) FROM cal_data WHERE sample = '%s' AND subclass = '%s';",
        sample, subclass
      )
      
      existing_count <- dbGetQuery(db, check_query)$`COUNT(*)`
      
      if (existing_count == 0) {
        # Insérer l'échantillon s'il n'existe pas
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
  # Vérifier si les listes de samples et subclasses ne sont pas vides
  if (length(samples) == 0 || length(subclasses) == 0) {
    cat("No samples or subclasses provided\n")
    return(data.frame())
  }

  # Fonction pour échapper les caractères spéciaux dans les chaînes
  escape_sql <- function(x) {
    gsub("'", "''", x)
  }

  # Convertir les listes en chaînes de caractères séparées par des virgules
  samples <- sapply(samples, escape_sql)  # Échapper les caractères spéciaux
  subclasses <- sapply(subclasses, escape_sql)  # Échapper les caractères spéciaux
  samples_str <- paste(sprintf("'%s'", samples), collapse = ", ")
  subclasses_str <- paste(sprintf("'%s'", subclasses), collapse = ", ")

  # Construire la requête SQL
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

  # Construire la requête SQL pour obtenir les concentrations uniques
  query <- sprintf(
    "SELECT DISTINCT sample, concentration FROM cal_data WHERE sample IN (%s);",
    paste(sprintf("'%s'", samples), collapse = ", ")
  )

  cat("Executing query for concentration data:", query, "\n")  # Debugging
  concentration_data <- dbGetQuery(db, query)
  
  return(concentration_data)
}

get_standard_table_quanti <- function(db, project = NULL, adducts = NULL, standard_formulas = NULL, cal_samples = NULL){
  # Récupérer tous les échantillons du projet
  sample <- get_samples(db, project)
  
  # Garder uniquement les échantillons qui sont de type "CAL"
  sample <- sample[sample$sample %in% cal_samples$sample, ]
  
  table <- NULL
  
  for (adduct in adducts) {
    for (standard_formula in standard_formulas) {
      for (i in 1:length(sample$project_sample)) {
        # query of normal standard with iso = A
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

  # Eliminer les doublons
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

# Changer cette fonction reactive en fonction avec arguments
# Mettre mat() avec export = TRUE pour avoir le plus de digits
quanti_final_mat_func <- function(db, project, select_choice = 2, adducts) {
  # Initialisation de la barre de progression
  withProgress(message = 'Matrix recovery...', value = 0, {
    # Récupérer tous les échantillons du projet
    samples <- get_samples(db, project)
    print("samples")
    print(samples)
    
    # Vérifier si samples est non vide
    if (nrow(samples) == 0) {
      return(list())
    }

    # Filtrer les échantillons de calibration
    cal_samples <- get_cal_samples(db, project)
    print("cal_samples")
    print(cal_samples)
    
    # Garder uniquement les échantillons de calibration
    cal_sample_ids <- cal_samples$sample
    print("cal_sample_ids")
    print(cal_sample_ids)
    
    # Filtrer les samples pour ne garder que les échantillons de calibration
    cal_samples_filtered <- samples[samples$sample %in% cal_sample_ids, ]
    print("cal_samples_filtered")
    print(cal_samples_filtered)
    
    # Vérifier si cal_samples_filtered est non vide
    if (nrow(cal_samples_filtered) == 0) {
      return(list())
    }

    print("adducts")
    print(adducts)

    # Vérifier si all_adducts est non vide
    if (length(adducts) == 0) {
      return(list())
    }
    
    # Récupérer tous les types chimiques disponibles, sans les standards
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
    
    # Vérifier si chemical_types est non vide
    if (length(chemical_types) == 0) {
      return(list())
    }
    
    result_tables <- list()

    # Calculer le nombre total d'itérations pour la barre de progression
    total_iterations <- nrow(cal_samples_filtered) * length(chemical_types) * length(adducts)
    iteration <- 0
    
    values$export <- TRUE
    for (file in cal_samples_filtered$sample_id) {
      for (study in chemical_types) {
        for (adduct in adducts) {
          # Mise à jour de la barre de progression
          iteration <- iteration + 1
          incProgress(1 / total_iterations, detail = sprintf("Processing %s - %s - %s (%d/%d)", file, study, adduct, iteration, total_iterations))
          
          # Vérifier si l'adduct existe pour l'étude actuelle
          if (adduct %in% names(mat()[[file]][[study]])) {
            # Réduire la matrice en fonction des choix de l'utilisateur
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


# Fonction pour récupérer les valeurs des matrices de données basées sur les positions sélectionnées
get_selected_values <- function(positions, matrix) {
  values <- apply(positions, 1, function(pos) {
    row <- pos[1]
    col <- pos[2]
    return(matrix[row, col])
  })
  return(values)
}

# Fonction pour calculer la somme des valeurs numériques
calculate_numeric_sum <- function(subclass, quanti_matrices, selected_cells) {
  # Obtenir les sommes initiales pour chaque nom de fichier
  initial_sums <- sapply(names(quanti_matrices), function(name) {
    matrix <- quanti_matrices[[name]]$table
    positions <- selected_cells[[subclass]]
    selected_values <- get_selected_values(positions, matrix)
    
    # Filtrer et convertir les valeurs numériques
    numeric_values <- as.numeric(gsub("[^0-9.]", "", selected_values, perl = TRUE))
    numeric_values <- numeric_values[!is.na(numeric_values)]
    
    sum(numeric_values, na.rm = TRUE)
  })
  
  # Extraire la base commune du nom du fichier en supprimant le suffixe après le dernier underscore
  base_names <- sub("_M.*", "", names(quanti_matrices))
  
  # Créer un DataFrame pour grouper et sommer
  sums_df <- data.frame(
    file_name = base_names,
    sum_value = initial_sums,
    stringsAsFactors = FALSE
  )
  
  # Regrouper par base de nom de fichier et sommer les valeurs
  grouped_sums <- sums_df %>%
    group_by(file_name) %>%
    summarize(total_sum = sum(sum_value), .groups = 'drop')
  
  grouped_sums <- grouped_sums %>%
    mutate(file_name = sub("_PCAs.*", "", file_name))

  # Convertir en vecteur nommé pour retour
  named_sums <- setNames(grouped_sums$total_sum, grouped_sums$file_name)

  return(named_sums)
}


#####################################
############ Sample Type ############
#####################################

#' Le but ici est de laisser l'utilisateur rentrer les samples types
#' 3 types de sample type : CAL, BLANK, SAMPLE
#' Si le Type est égal à CAL alors faire rentrer les informations.
#' Les infos : subclassname SCCP, MCCP, LCCP || chlorination_degree || concentration
#' 
#' Pour l'instant l'utilisateur peut remplir ce qu'il veut dans les colonnes qu'il veut.
#' Par la suite ajouter des restrictions. (DROPDOWN MENU ...)

# Créer un objet réactif pour stocker les données
sample_type_data <- reactive({
  params <- list(
    table_selected = input$quantification_choice
  )
  tryCatch({  
    if(input$quantification_choice == "Sample Type"){  # Utiliser '==' pour la comparaison
        # Obtenir les données des samples du projet
        data <- get_samples_project_quanti(db, input$project)
        if (nrow(data) == 0) {
          print("Aucune donnée trouvée pour ce projet.")
          return(data.frame())  # Retourner un data frame vide si aucune donnée n'est trouvée
        }
        # print(data)  # Debugging: Imprimer les données récupérées
        return(data)  # Retourner les données
  }})
})

output$quanti_table_type <- DT::renderDataTable({
  DT::datatable(sample_type_data(), editable = list(target = "cell", disable = list(columns = c(0, 1))))  # Rendre certaines colonnes éditables
})

observeEvent(c(input$quanti_table_type_cell_edit, input$project), {

  project_id <- input$project
  print(project_id)

  info <- input$quanti_table_type_cell_edit
  str(info)  # Afficher les informations de l'édition pour le débeugage

  # Récupérer les données actuelles
  data_df <- isolate(sample_type_data())

  # Vérifier si info est NULL
  if (!is.null(info)) {
    # Mettre à jour la valeur dans le data.frame
    row <- info$row
    col <- info$col
    value <- info$value

    # Vérifiez que les colonnes éditées sont parmi celles qu'on veut éditer
    editable_columns <- c("sample_type")
    if (colnames(data_df)[col] %in% editable_columns) {
      data_df[row, col] <- DT::coerceValue(value, data_df[row, col])

      # Requête pour UPDATE la bdd avec les modifications
      sample_id <- data_df[row, "sample"]
      column_name <- colnames(data_df)[col]
      update_query <- sprintf("UPDATE sample SET %s = ? WHERE sample = ?", column_name)

      # Exécution de la requête
      dbExecute(db, update_query, params = list(value, sample_id))

      # Mettre à jour les échantillons de calibration après la modification
      cal_samples_data <- get_cal_samples(db, input$project)
      cal_samples(cal_samples_data)
      print(cal_samples())
    }
  }
})

#########################################
############ Sample Subclass ############
#########################################

# Observer le changement de projet et mettre à jour les sous-classes
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
  datatable(data.frame(subclass_name = subclass_names()))  # Afficher les données dans un tableau
})

observeEvent(input$add_subclass, {
  req(input$new_subclass)  # S'assurer qu'une nouvelle sous-classe est entrée
  record_subclass_quanti(db, input$project, input$new_subclass)
  updateTextInput(session, "new_subclass", value = "")  # Réinitialiser le champ de texte
  
  # Mettre à jour les sous-classes
  subclasses <- get_subclass_names(db, input$project)
  subclass_names(subclasses)
  updateSelectInput(session, "delete_subclass", choices = subclasses)

  print(subclass_names())
})

observeEvent(input$remove_subclass, {
  req(input$delete_subclass)  # S'assurer qu'une sous-classe à supprimer est sélectionnée
  delete_subclass_quanti(db, input$project, input$delete_subclass)
  
  # Mettre à jour les sous-classes
  subclasses <- get_subclass_names(db, input$project)
  subclass_names(subclasses)
  updateSelectInput(session, "delete_subclass", choices = subclasses)

  print(subclass_names())
})


#####################################
############ Calibration ############
#####################################

# Observer la sélection de quantification_choice et mettre à jour les données
observeEvent(input$quantification_choice, {
  if (input$quantification_choice == "Calibration") {
    samples <- cal_samples()
    subclasses <- subclass_names()
    
    # Enregistrer les données de calibration
    record_cal_data(db, samples, subclasses)
    
    # Mettre à jour la variable réactive avec les nouvelles données
    df <- get_cal_data(db, samples, subclasses)
    cal_data(df)
  }
})

# Afficher les données de la table de manière réactive
output$cal_samples_table <- renderDT({
  df <- cal_data()
  
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  
  datatable(df[,-1], 
            editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
            options = list(
              scrollX = TRUE,  # Activer le scroll horizontal
              scroller = TRUE,  # Activer le scroller pour améliorer les performances avec de grandes tables
              paging = FALSE  # Activer la pagination
            )
  )
})

# Observer les modifications des cellules de la table de calibration
observeEvent(input$cal_samples_table_cell_edit, {
  info <- input$cal_samples_table_cell_edit
  str(info)  # Debugging
  
  # Reconvertir l'index de la colonne en nom de colonne
  col_name <- colnames(cal_data()[,-1])[info$col]

  # Récupérer la ligne correspondant à l'info$row
  row_data <- cal_data()[info$row, ]

  # Construire et exécuter la requête UPDATE
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

#' Le but est de retourner des matrices vides pour chaque sous classe
#' L'utilisateur va selectionner les groupes d'homologue dans chaque matrice
#' Les matrices vont avoir C6-C36 première colonne et Cl3-Cl30 en première ligne
#' Les groupes d'homologues selectionnés vont être stockés par matrice

output$quanti_subclass_dropdown <- shiny::renderUI({
  bsplus::shinyInput_label_embed(
    shiny::selectInput("quanti_subclass_dropdown", "Subclass Name", choices = subclass_names(), multiple = FALSE),
    bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = 'Select Subclass')
  )
})

# Observateur pour initialiser les matrices et les cellules sélectionnées
observe({
  subclasses <- subclass_names()
  
  # Initialiser les matrices pour chaque sous-classe
  matrices <- lapply(subclasses, function(subclass) {
    matrix <- matrix(ncol = 28, nrow = 31)
    colnames(matrix) <- paste0("Cl", 3:30)
    rownames(matrix) <- paste0("C", 6:36)
    as.data.frame(matrix)
  })
  
  names(matrices) <- subclasses
  matrix_list(matrices)
  
  # Initialiser les sélections de cellules pour chaque sous-classe
  selected_cells <- lapply(subclasses, function(subclass) {
    data.frame(row = integer(0), col = integer(0))
  })
  names(selected_cells) <- subclasses
  selected_cells_list(selected_cells)
})

# Rendu de la datatable pour afficher la matrice de la sous-classe sélectionnée
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

# Observer pour mettre à jour les cellules sélectionnées
observeEvent(input$quanti_matrix_homologue_cells_selected, {
  cells <- input$quanti_matrix_homologue_cells_selected
  if (!is.null(cells)) {
    selected_cells <- selected_cells_list()
    selected_cells[[input$quanti_subclass_dropdown]] <- cells
    selected_cells_list(selected_cells)
    print("selected_cells")
    print(selected_cells)
  }
})

###########################################
############ Internal Standard ############
###########################################

# Permet de choisir un étalon interne - standard par sous-classe (dynamiquement)
# Permet de choisir un ou plusieurs adduits par sous-classe (dynamiquement)

output$quanti_dynamic_IS <- shiny::renderUI({
  # Générer les div pour chaque sous-classe
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

# Générer dynamiquement les `uiOutput` pour chaque sous-classe
observe({
  project_id <- input$project
  
  # Générer dynamiquement les `uiOutput` pour chaque sous-classe
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
  # Pour chaque sous-classe
  lapply(subclass_names(), function(subclass) {
    local({
      subclass_local <- subclass

      # Observer pour les changements dans les adducts
      observeEvent(input[[paste0("process_adduct_", subclass_local)]], {
        adduct_input <- input[[paste0("process_adduct_", subclass_local)]]
        adducts_values[[subclass_local]] <- adduct_input

        # Pour le débogage
        shiny::req(adduct_input)  # Assurez-vous que l'entrée est disponible
        print(paste("Adduct Input for", subclass_local, ":"))
        print(adduct_input)
      }, priority = 10)

      # Observer pour les changements dans les standards
      observeEvent(input[[paste0("process_standard_type_", subclass_local)]], {
        standard_input <- input[[paste0("process_standard_type_", subclass_local)]]
        standards_values[[subclass_local]] <- standard_input

        # Pour le débogage
        shiny::req(standard_input)  # Assurez-vous que l'entrée est disponible
        print(paste("Standard Input for", subclass_local, ":"))
        print(standard_input)
      }, priority = 10)
    })
  })
})


#######################################
############ Launch Quanti ############
#######################################

#' On va exécuter lorsque l'on va cliquer sur le bouton launch quanti la fonction principale quanti_final_mat()
#' Cette fonction va récupérer les matrices qui ont été calculés lors de la déconvolution.
#' C'est ici que nous devons calculer les aires normalisés par les étalons internes.
#' Une seule exécution de cette fonction est suffisante en théorie pour éxécuter tout le reste.


#' browser() R pour stopper la console et voir le contenu des variables (C pour continuer les calculs après)

# library(dplyr)

observeEvent(input$quanti_launch, {

  # Initialisation de la barre de progression
  withProgress(message = 'Quantification in progress...', value = 0, {

    subclasses <- subclass_names()
    selected_cells <- selected_cells_list()
    cal_samples <- cal_samples()

    # Récupérez toutes les combinaisons d'adducts et de standards sélectionnés
    all_adducts <- unlist(lapply(subclasses, function(subclass) {
      adducts_values[[subclass]]
    }))
    
    all_standards <- unlist(lapply(subclasses, function(subclass) {
      standards_values[[subclass]]
    }))
    
    # Filtrer les valeurs uniques
    adducts <- unique(all_adducts)
    standards <- unique(all_standards)

    incProgress(0.1, detail = sprintf("Matrix recovery"))
    quanti_matrices <- quanti_final_mat_func(db, input$project, 2, adducts)
    incProgress(0.1, detail = sprintf("Calculating numerical sums"))

    lapply(subclasses, function(subclass) {
      if (!is.null(selected_cells[[subclass]])) {       
        sums <- calculate_numeric_sum(subclass, quanti_matrices, selected_cells)
        sums_values[[subclass]] <- sums
      }
    })

    samples <- unique(unlist(cal_samples))
    cal_data_df <- get_cal_data(db, samples, subclasses)

    # Convertir sums_values en DataFrame
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
    cal_data_df$sample_id <- sub("^(neg|pos) ", "", cal_data_df$sample)
    cal_data_df <- cal_data_df %>%
      left_join(sums_df %>% select(file_name, subclass, sum_value), 
                by = c("sample_id" = "file_name", "subclass" = "subclass"))

    # Mettre à jour la barre de progression
    incProgress(0.4, detail = sprintf("Calibration data update"))

    if (length(adducts) > 0 && length(all_standards) > 0) {
      standard_table <- get_standard_table_quanti(db, input$project, adducts, all_standards, cal_samples)
      DT::datatable(as.matrix(standard_table))
      standard_table <- standard_table %>% distinct()
      standard_table$`total area` <- as.numeric(gsub(" ", "", standard_table$`total area`)) / 10^6
      total_area_table(standard_table)
    } else {
      table <- as.data.frame("No results")
      print("No results for standard table.")
      DT::datatable(table)
    }

    if (!is.null(sums_values) && !is.null(total_area_table()) && nrow(total_area_table()) > 0) {
      subclasses <- names(sums_values)

      print("Subclasses for processing:")
      print(subclasses)

      # Créez une fonction pour traiter chaque sous-classe
      process_subclass <- function(subclass) {
        if (is.null(sums_values[[subclass]])) return(NULL)

        standard_formula <- standards_values[[subclass]]
        if (is.null(standard_formula)) {
          cat("No standard formula available for subclass", subclass, "\n")
          return(NULL)
        }

        print(paste("Standard Formula for subclass", subclass, ":"))
        print(standard_formula)

        filtered_table <- total_area_table() %>%
          filter(adduct %in% adducts_values[[subclass]], formula == standards_values[[subclass]])

        if (nrow(filtered_table) == 0) {
          cat("No data available after filtering for subclass", subclass, "\n")
          return(NULL)
        }

        areas_std <- filtered_table %>%
          mutate(`total area` = ifelse(is.na(`total area`), 0, `total area`)) %>%
          group_by(sample_id) %>%
          summarize(total_area_sum = sum(`total area`), .groups = 'drop')

        cal_data_df <- left_join(cal_data_df, areas_std, by = "sample_id")

        cal_data_df <- cal_data_df %>%
          mutate(normalized_per_std = sum_value / total_area_sum * 1e6) %>%
          mutate(normalized_per_concentration = normalized_per_std / concentration)
        
        write.csv(cal_data_df, "export/Quanti_data_full.csv")

        # Filtrer les lignes qui contiennent des valeurs NA, NaN, ou Inf
        cal_data_df <- cal_data_df %>%
          filter(
            !if_any(everything(), ~ is.na(.) | is.nan(.) | is.infinite(.))
          )
        
        cal_data_df_rv(cal_data_df)
        write.csv(cal_data_df, "export/Quanti_data_clean.csv")

        assign(paste0("cal_data_df_", subclass), cal_data_df, envir = .GlobalEnv)

        cat("========================\n")
      }

      # Mettre à jour la barre de progression avant de commencer le traitement
      incProgress(0.4, detail = sprintf("Subclass processing"))

      lapply(subclasses, process_subclass)

      # Finaliser la barre de progression
      incProgress(0.1, detail = sprintf("Quantification complete"))
    } else {
      cat("No total area values available for normalization.\n")
    }
  })
})

# Observer les changements du sélecteur de graphique
observeEvent(c(input$graph_selector, cal_data_df_rv()), {
  cal_data_df <- cal_data_df_rv()

  if (nrow(cal_data_df) == 0) {
    # Convertir le graphique ggplot en graphique interactif plotly
    output$plot_output <- renderPlotly({
      plotly_empty()
    })
  } else {
    req(input$graph_selector)

        # Déterminer le type de graphique à afficher
        plot <- switch(input$graph_selector,
                      'Standard Boxplot' = {
                        ggplot(cal_data_df, aes(x = subclass, y = normalized_per_std, fill = subclass)) +
                          geom_boxplot() +
                          labs(title = "Boxplot of Normalized Values (Standard) by Subclass", x = "Subclass", y = "Normalized Value") +
                          theme_minimal()
                      },
                      'Concentration Boxplot' = {
                        ggplot(cal_data_df, aes(x = subclass, y = normalized_per_concentration, fill = subclass)) +
                          geom_boxplot() +
                          labs(title = "Boxplot of Normalized Values (Standard + Concentration) by Subclass", x = "Subclass", y = "Normalized Value") +
                          theme_minimal()
                      },
                      'Pre-Normalization' = {
                        ggplot(cal_data_df, aes(x = sample_id, y = sum_value, color = subclass)) +
                          geom_point(size = 3) +
                          labs(title = "Values Before Normalization",
                                x = "Sample",
                                y = "Sum of Areas",
                                color = "Subclass") +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                          scale_color_viridis_d()
                      },
                      'Post-Normalization' = {
                        ggplot(cal_data_df, aes(x = concentration, y = normalized_per_std)) +
                          geom_point(aes(color = 'Standard Normalized'), size = 3) +
                          geom_point(aes(y = normalized_per_concentration, color = 'Concentration Normalized'), size = 3, shape = 17) +
                          facet_grid(subclass ~ chlorination) +
                          labs(title = "Comparison of Normalized Values by Concentration",
                              x = "Concentration",
                              y = "Normalized Value",
                              color = "Metric",
                              shape = "Metric") +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                            scale_color_manual(values = c("Standard Normalized" = "blue", "Concentration Normalized" = "red"))
                      },
                      'Exponential regression' = {
                        # Calcul de la pente et de la moyenne par groupe
                        exponential_data <- cal_data_df %>%
                          group_by(subclass, chlorination) %>%
                          do({
                            standard_lm <- lm(normalized_per_std ~ concentration, data = .)
                            data.frame(
                              standard_slope = coef(standard_lm)[["concentration"]],
                              average_concentration_normalized = mean(.$normalized_per_concentration, na.rm = TRUE)
                            )
                          }) %>%
                          ungroup()

                        # Fonction pour ajuster un modèle exponentiel avec valeurs initiales ajustées
                        exponential_model <- function(data, response_var) {
                          tryCatch({
                            model <- nls(as.formula(paste(response_var, "~ a * exp(b * chlorination)")), data = data,
                                        start = list(a = max(data[[response_var]], na.rm = TRUE), b = 0.01))
                            return(model)
                          }, error = function(e) {
                            message("Erreur lors de l'ajustement du modèle exponentiel pour ", response_var, ": ", e$message)
                            return(NULL)
                          })
                        }

                        # Calculer les courbes exponentielles
                        exponential_curves <- exponential_data %>%
                          group_by(subclass) %>%
                          do({
                            data_subset <- .

                            # Ajuster les modèles exponentiels
                            exp_model_standard <- exponential_model(data_subset, "standard_slope")
                            exp_model_avg <- exponential_model(data_subset, "average_concentration_normalized")

                            # Générer les prédictions
                            chlorination_seq <- seq(min(data_subset$chlorination), max(data_subset$chlorination), length.out = 100)

                            exp_predictions_standard <- if (!is.null(exp_model_standard)) {
                              predict(exp_model_standard, newdata = data.frame(chlorination = chlorination_seq))
                            } else {
                              rep(NA, length(chlorination_seq))
                            }

                            exp_predictions_avg <- if (!is.null(exp_model_avg)) {
                              predict(exp_model_avg, newdata = data.frame(chlorination = chlorination_seq))
                            } else {
                              rep(NA, length(chlorination_seq))
                            }

                            data.frame(
                              chlorination = chlorination_seq,
                              exp_predictions_standard = exp_predictions_standard,
                              exp_predictions_avg = exp_predictions_avg,
                              subclass = unique(data_subset$subclass)
                            )
                          }) %>%
                          ungroup()

                        # Utiliser une jointure externe pour inclure toutes les valeurs de chlorination
                        exponential_df <- full_join(
                          exponential_curves,
                          exponential_data %>% select(subclass, chlorination, standard_slope, average_concentration_normalized),
                          by = c("subclass", "chlorination")
                        )

                        # Créer le graphique avec ggplot2
                        plot <- ggplot(exponential_df, aes(x = chlorination)) +
                          geom_line(aes(y = exp_predictions_standard, color = "Exp Predictions Standard")) +
                          geom_line(aes(y = exp_predictions_avg, color = "Exp Predictions Avg")) +
                          geom_point(aes(y = standard_slope, color = "Exp Predictions Standard"), shape = 16) +
                          geom_point(aes(y = average_concentration_normalized, color = "Exp Predictions Avg"), shape = 17) +
                          facet_grid(. ~ subclass) +
                          labs(title = "Exponential Regression by Chlorination Level",
                              x = "Chlorination Level",
                              y = "Value",
                              color = "Metric") +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1))
                      })
        
    # Convertir le graphique ggplot en graphique interactif plotly
    output$plot_output <- renderPlotly({
      ggplotly(plot)
    })
  }
})

observeEvent(input$project, {
  cal_data_df_rv(data.frame())
})