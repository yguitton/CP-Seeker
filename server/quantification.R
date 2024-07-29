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
adducts_values <- reactiveValues()
standards_values <- reactiveValues()

#########################################
################## SQL ##################
#########################################

# CREATE TABLE "cal_data" (
# 	"id"	INTEGER,
# 	"project"	TEXT NOT NULL,
# 	"sample"	TEXT NOT NULL,
# 	"subclass"	TEXT NOT NULL,
# 	"concentration"	REAL NOT NULL,
# 	"chlorination"	REAL NOT NULL,
# 	PRIMARY KEY("id" AUTOINCREMENT),
# 	FOREIGN KEY("subclass") REFERENCES "subclass"("subclass_name"),
# 	FOREIGN KEY("sample") REFERENCES "sample"("sample"),
# 	FOREIGN KEY("project") REFERENCES "project"("project")
# );

# CREATE TABLE "subclass" (
# 	"project"	INTEGER,
# 	"subclass_name"	TEXT,
# 	FOREIGN KEY("project") REFERENCES "project"("project")
# )

#' Ces requêtes vont devoir être transférés dans les fichiers db_get etc...

get_subclass_names <- function(db, project) {
  query <- sprintf("SELECT subclass_name FROM subclass WHERE project = '%s'", project)
  cat("Executing query:", query, "\n")  # Debugging
  dbGetQuery(db, query)$subclass_name
}

get_samples_type_quanti <- function(db, project = NULL) {
  if (is.null(project)) {
    return(data.frame())
  }

  # Requête pour obtenir les samples associés au projet donné
  project_query <- "SELECT sample FROM project_sample WHERE project = ?"
  project_samples <- dbGetPreparedQuery(db, project_query, bind.data = data.frame(project = project))

  if (nrow(project_samples) == 0) {
    return(data.frame())
  }

  samples <- project_samples$sample
  samples_str <- paste(sprintf("'%s'", samples), collapse = ", ")
  
  # Requête pour obtenir les détails des samples
  query <- sprintf("SELECT sample, sample_type FROM sample WHERE sample IN (%s)", samples_str)
  cat("Executing query:", query, "\n")  # Debugging
  
  dbGetQuery(db, query)
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

record_cal_data <- function(db, project, cal_data) {
  for (i in 1:nrow(cal_data)) {
    sample_name <- cal_data[i, "sample"]
    
    for (j in 2:ncol(cal_data)) {
      col_name <- colnames(cal_data)[j]
      if (grepl("concentration", col_name)) {
        subclass <- sub("_concentration", "", col_name)
        concentration <- cal_data[i, j]
        chlorination <- cal_data[i, paste(subclass, "chlorination", sep = "_")]
        
        query <- sprintf("INSERT INTO cal_data (project, sample, subclass, concentration, chlorination) VALUES ('%s', '%s', '%s', %f, %f)",
                         project, sample_name, subclass, concentration, chlorination)
        cat("Executing query:", query, "\n")  # Debugging
        dbExecute(db, query)
      }
    }
  }
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


#####################################
############ Sample Type ############
#####################################

#' Le but ici est de laisser l'utilisateur rentrer les samples types
#' 3 types de sample type : CAL, BLANK, SAMPLE
#' Si le Type est égal à CAL alors faire rentrer les informations.
#' Les infos : subclassname SCCP, MCCP, LCCP || chlorination_degree || concentration
#' 
#' Pour l'instant l'utilisateur peut remplir ce qu'il veut dans les colonnes qu'il veut.
#' Par la suite ajouter des restrictions.

# Créer un objet réactif pour stocker les données
sample_type_data <- reactive({
  params <- list(
    table_selected = input$quantification_choice
  )
  tryCatch({  
    if(params$table_selected == "Sample Type"){  # Utiliser '==' pour la comparaison
        # Obtenir les données des samples du projet
        data <- get_samples_type_quanti(db, input$project)
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
  
  # Initialiser la variable réactive avec les données de calibration
  cal_list <- list(sample_name = cal_samples$sample)
  for (subclass in subclass_names()) {
    cal_list[[paste(subclass, "concentration", sep = "_")]] <- rep(NA, nrow(cal_samples))
    cal_list[[paste(subclass, "chlorination", sep = "_")]] <- rep(NA, nrow(cal_samples))
  }
  cal_data(as.data.frame(cal_list))
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

# Ajouter des observateurs réactifs pour cal_samples() et subclass_names()
observe({
  samples <- cal_samples()
  subclasses <- subclass_names()
  
  if (!is.null(samples) && !is.null(subclasses)) {
    cal_list <- list(sample_name = samples$sample)
    for (subclass in subclasses) {
      cal_list[[paste(subclass, "concentration", sep = "_")]] <- rep(NA, nrow(samples))
      cal_list[[paste(subclass, "chlorination", sep = "_")]] <- rep(NA, nrow(samples))
    }
    cal_data(as.data.frame(cal_list))
  }
})

# Rendre la table des échantillons de calibration
output$cal_samples_table_df <- renderDT({
  df <- cal_data()
  
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  
  datatable(df, 
            editable = TRUE,
            options = list(
              scrollX = TRUE,  # Activer le scroll horizontal
              scroller = TRUE,  # Activer le scroller pour améliorer les performances avec de grandes tables
              paging = TRUE  # Activer la pagination
            )
  )
})

# Fonction pour vérifier si une table existe
table_exists <- function(db, table_name) {
  query <- paste("SELECT name FROM sqlite_master WHERE type='table' AND name='", table_name, "';", sep = "")
  cat("Executing query:", query, "\n")  # Debugging
  result <- dbGetQuery(db, query)
  return(nrow(result) > 0)
}

# Fonction pour supprimer une table si elle existe
drop_table_if_exists <- function(db, table_name) {
  if (table_exists(db, table_name)) {
    query <- paste("DROP TABLE ", table_name, ";", sep = "")
    cat("Executing query:", query, "\n")  # Debugging
    dbExecute(db, query)
  }
}

# Fonction pour générer dynamiquement la requête CREATE TABLE
generate_create_table_query <- function(data_list, table_name) {
  columns <- names(data_list)
  col_types <- sapply(data_list, function(col) {
    if (is.numeric(col)) {
      "REAL"
    } else {
      "TEXT"
    }
  })
  col_definitions <- paste(columns, col_types, collapse = ", ")
  query <- sprintf("CREATE TABLE %s (%s)", table_name, col_definitions)
  cat("Executing query:", query, "\n")  # Debugging
  return(query)
}

# Fonction pour créer une nouvelle table avec les données
create_cal_samples_table_df <- function(db, data_list, table_name = "cal_samples") {
  # Générer la requête CREATE TABLE
  create_table_query <- generate_create_table_query(data_list, table_name)
  # Exécuter la requête pour créer la table
  dbExecute(db, create_table_query)
  # Insérer les données dans la table
  df <- as.data.frame(data_list)
  dbWriteTable(db, table_name, df, append = TRUE, row.names = FALSE)
}

# Observer les modifications des cellules de la table de calibration
observeEvent(input$cal_samples_table_df_cell_edit, {
  info <- input$cal_samples_table_df_cell_edit
  str(info)  # Debugging
  
  df <- cal_data()
  df[info$row, info$col] <- info$value
  cal_data(df)  # Mettre à jour la variable réactive avec les nouvelles données
  print(df)  # Print the updated data frame
  
  # Optionally save the updated data frame to the database
  drop_table_if_exists(db, "cal_samples")
  create_cal_samples_table_df(db, as.list(df))
})


###### NOUVELLE VERSION AVEC LA TABLE CAL_DATA

#' Ce qu'il faut faire : 
#' Jouer avec une nouvelle table cal_data
#' Ajouter ou supprimer les échantillons CAL lorsqu'ils sont édités dans les cellules
#' Ajouter ou supprimer les sousclasses pour chaque échantillon CAL dans cal_data
#' Présenter les colonnes du tableau avec un split_str SOUSCLASSE_CONCENTRATION & SOUSCLASSE_CHLORATION

# Rendre la table des échantillons de calibration
output$cal_samples_table <- renderDT({
  df <- cal_data() # Même chose que cal_samples pour l'instant
  
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  
  datatable(df, 
            editable = TRUE,
            options = list(
              scrollX = TRUE,  # Activer le scroll horizontal
              scroller = TRUE,  # Activer le scroller pour améliorer les performances avec de grandes tables
              paging = TRUE  # Activer la pagination
            )
  )
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
  }
})

observe({
  req(input$quanti_subclass_dropdown)
  selected_cells <- selected_cells_list()
  cells <- selected_cells[[input$quanti_subclass_dropdown]]
  
  if (!is.null(cells)) {
    proxy <- dataTableProxy('quanti_matrix_homologue')
    selectCells(proxy, cells)
  }
})

# Rendu des valeurs des cellules sélectionnées pour chaque sous-classe
observe({
  subclasses <- subclass_names()
  lapply(subclasses, function(subclass) {
    output[[paste0("selected_matrices_", subclass)]] <- renderPrint({
      selected_cells <- selected_cells_list()
      
      # Vérifier la sous-classe sélectionnée et les cellules sélectionnées
      if (!is.null(selected_cells[[subclass]])) {
        cat("========================\n")
        cat("Matrice selectionnee :", subclass, "\n")
        cat("Positions des cellules selectionnees :\n")
        print(selected_cells[[subclass]])
        cat("========================\n")
      }
    })
  })
})

# Changer cette fonction reactive en fonction avec arguments
# Mettre mat() avec export = TRUE pour avoir le plus de digits
quanti_final_mat_func <- function(db, project, select_choice = 2) {
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

  # Récupérer tous les adducts disponibles
  adduct_table <- db_get_query(db, sprintf("SELECT DISTINCT adduct FROM deconvolution_param WHERE project = '%s'", project))
  adduct_list <- adduct_table$adduct
  print("adducts")
  print(adduct_list)
  
  # Vérifier si adduct_list est non vide
  if (length(adduct_list) == 0) {
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
  
  values$export <- TRUE
  for (file in cal_samples_filtered$sample_id) {
    for (study in chemical_types) {
      for (adduct in adduct_list) {
        # Vérifier si l'adduct existe pour l'étude actuelle
        if (adduct %in% names(mat()[[file]][[study]])) {
          # Réduire la matrice en fonction des choix de l'utilisateur
          table <- reduce_matrix(mat()[[file]][[study]][[adduct]], select_choice, greycells = TRUE)
          print(table)
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
  print(adduct_list)
  print("##########################################")
  print(select_choice)
  print("##########################################")
  
  return(result_tables)
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
  sums <- sapply(names(quanti_matrices), function(name) {
    matrix <- quanti_matrices[[name]]$table
    positions <- selected_cells[[subclass]]
    selected_values <- get_selected_values(positions, matrix)
    
    # Filtrer et convertir les valeurs numériques
    numeric_values <- as.numeric(gsub("[^0-9.]", "", selected_values, perl = TRUE))
    numeric_values <- numeric_values[!is.na(numeric_values)]
    
    sum(numeric_values, na.rm = TRUE)
  })
  
  return(sums)
}

###########################################
############ Internal Standard ############
###########################################

#' Permet de choisir un étalon interne (dynamiquement)
#' Permet de choisir les adduits associés (dynamiquement)

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
  # Récupérer les sous-classes depuis la base de données en filtrant les valeurs vides ou NA
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

observe({
  lapply(subclass_names(), function(subclass) {
    local({
      subclass_local <- subclass

      output[[paste0("selected_adducts_and_standards_", subclass_local)]] <- renderPrint({
        adduct_input <- input[[paste0("process_adduct_", subclass_local)]]
        standard_input <- input[[paste0("process_standard_type_", subclass_local)]]
        
        # Stocker les valeurs dans les variables réactives
        adducts_values[[subclass_local]] <- adduct_input
        standards_values[[subclass_local]] <- standard_input

        cat("========================\n")
        cat("Selected Adducts for", subclass_local, ":\n")
        print(adduct_input)
        cat("Selected Standard Formula for", subclass_local, ":\n")
        print(standard_input)
        cat("========================\n")
      })
    })
  })
})

# Déclarer les variables réactives globalement
adduct_inputs <- reactiveValues()
standard_inputs <- reactiveValues()

# Observer pour récupérer les valeurs d'adducts et de standards
observe({
  lapply(subclass_names(), function(subclass) {
    local({
      subclass_local <- subclass

      observe({
        adduct_input <- input[[paste0("process_adduct_", subclass_local)]]
        standard_input <- input[[paste0("process_standard_type_", subclass_local)]]
        
        adduct_inputs[[subclass_local]] <- adduct_input
        standard_inputs[[subclass_local]] <- standard_input

        # Pour le débogage
        print(paste("Adduct Input for", subclass_local, ":"))
        print(adduct_input)
        print(paste("Standard Input for", subclass_local, ":"))
        print(standard_input)
      })
    })
  })
})

# Function to calculate normalized areas
calculate_normalized_areas <- function(sums, total_area_table, subclass_adduct, subclass_standard) {
  normalized_areas <- numeric(length(sums))
  for (i in seq_along(sums)) {
    sample_id <- names(sums)[i]
    
    # Extract the standard area for the corresponding subclass and adduct
    total_area <- total_area_table$total_area[total_area_table$sample_id == sample_id & 
                                              total_area_table$adduct == subclass_adduct & 
                                              total_area_table$formula == subclass_standard]
    
    if (length(total_area) == 1) {
      normalized_areas[i] <- sums[i] / total_area
    } else {
      normalized_areas[i] <- NA  # Handle case where no matching total area is found
    }
  }
  return(normalized_areas)
}

#######################################
############ Apply Filters ############
#######################################

#' Ce script va appliquer des filtres sur les matrices avant de lancer le Launch Quanti.
#' Pour l'instant ne sait pas trop comment faire car il faut connaitre les arguments nécessaires pour 
#' le calcul de quanti pour éxécuter la fonction et renvoyer les bonnes data.


#######################################
############ Launch Quanti ############
#######################################

#' On va exécuter lorsque l'on va cliquer sur le bouton launch quanti la fonction principale quanti_final_mat()
#' Cette fonction va récupérer les matrices qui ont été calculés lors de la déconvolution.
#' C'est ici que nous devons calculer les aires normalisés par les étalons internes.
#' Une seule exécution de cette fonction est suffisante en théorie pour éxécuter tout le reste.


#' browser() R pour stopper la console et voir le contenu des variables (C pour continuer les calculs après)

library(dplyr)
observeEvent(input$quanti_launch, {

  subclasses <- subclass_names()
  selected_cells <- selected_cells_list()
  print("selected_cells")
  print(selected_cells)

  quanti_matrices <- quanti_final_mat_func(db, input$project, 2)

  output$quanti_profile <- renderUI({
  # Créer des sorties pour chaque table avec informations associées
  output_list <- lapply(names(quanti_matrices), function(name) {
    list(
      verbatimTextOutput(paste0("matrix_info_", name)),
      DT::dataTableOutput(name)
    )
  })
  
  # Afficher les tables et les informations dans une balise div avec style
  tagList(div(style = "overflow-x: scroll;", output_list))
  })

  lapply(names(quanti_matrices), function(name) {
    result <- quanti_matrices[[name]]
    
    output[[paste0("matrix_info_", name)]] <- renderPrint({
      cat("Sample ID:", result$sample_id, "\n")
      cat("Chemical Type:", result$chemical_type, "\n")
      cat("Adduct:", result$adduct, "\n")
    })
    
    output[[name]] <- DT::renderDataTable({
      DT::datatable(result$table, selection = "none", extensions = 'Scroller', 
                    class = 'display cell-border compact nowrap', 
                    options = list(info = FALSE, paging = FALSE, dom = 'Bfrtip', scroller = TRUE, 
                                  scrollX = TRUE, bFilter = FALSE, ordering = FALSE, 
                                  columnDefs = list(list(className = 'dt-body-center', targets = "_all")),
                                  initComplete = htmlwidgets::JS("
          function (settings, json) {
            var table = settings.oInstance.api();
            table.cells().every(function() {
              if (this.index().column == 0) {
                this.data(this.data());
              } else if (this.data() == null){
                $(this.node()).addClass('outside');
              } else if (this.data() != null){
                var splitted_cell = this.data().split('/');
                if (splitted_cell[0] == 'NA'){
                  this.data('')
                } else {
                  this.data(splitted_cell[0]);
                }
                if (splitted_cell[1] == 'outside'){
                  $(this.node()).addClass('outside');
                } else if (splitted_cell[1] == 'half'){
                  $(this.node()).addClass('half');
                } else if (splitted_cell[1] == 'inside'){
                  $(this.node()).removeClass('outside');
                  $(this.node()).removeClass('half');
                }
              }
            });
            table.columns.adjust()
          }
        ")))
    })
  })

  # Rendu dynamique des verbatimTextOutput pour chaque sous-classe
  output$selected_matrices <- renderUI({
    output_list <- lapply(subclasses, function(subclass) {
      verbatimTextOutput(paste0("selected_matrices_", subclass))
    })
    do.call(tagList, output_list)
  })

  lapply(subclasses, function(subclass) {
    output[[paste0("selected_values_", subclass)]] <- renderPrint({
      if (!is.null(selected_cells[[subclass]])) {
        cat("========================\n")
        cat("Sous-classe :", subclass, "\n")
        cat("Valeurs des cellules selectionnees :\n")
        
        values <- sapply(names(quanti_matrices), function(name) {
          matrix <- quanti_matrices[[name]]$table
          positions <- selected_cells[[subclass]]
          get_selected_values(positions, matrix)
        })
        
        print(values)
        cat("========================\n")
      }
    })
  })

  lapply(subclasses, function(subclass) {
    output[[paste0("sum_values_", subclass, "_ui")]] <- renderPrint({
      if (!is.null(selected_cells[[subclass]])) {
        cat("========================\n")
        cat("Sous-classe :", subclass, "\n")
        cat("Somme des valeurs numeriques selectionnees :\n")
        
        sums <- calculate_numeric_sum(subclass, quanti_matrices, selected_cells)
        sums_values[[subclass]] <- sums
        
        for (i in seq_along(sums)) {
          cat(names(quanti_matrices)[i], ": ", sums[i], "\n")
        }

        cat("========================\n")
      }
    })
  })
  
  lapply(subclasses, function(subclass) {
    output[[paste0("sum_values_", subclass, "_plot")]] <- renderPlot({      
      if (!is.null(selected_cells[[subclass]])) {
        # sums <- calculate_numeric_sum(subclass, quanti_matrices, selected_cells)

        data <- data.frame(
          Names = names(quanti_matrices),
          Sums = sums_values[[subclass]]
        )
        
        ggplot(data, aes(x = Names, y = Sums)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "red") +
          theme_minimal() +
          labs(title = paste("Sous-classe:", subclass), x = "Combinaison echantillon, type, adduit", y = "Sommes des aires") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
  })

  output$selected_values_ui <- renderUI({
    # Créer des sorties pour les valeurs sélectionnées pour chaque sous-classe
    output_list <- lapply(subclasses, function(subclass) {
      list(
        verbatimTextOutput(paste0("selected_values_", subclass))
      )
    })
    
    # Afficher les informations dans une balise div avec style
    tagList(div(style = "overflow-x: scroll;", output_list))
  })

  # UI pour afficher les résultats de la somme
  output$sum_values_ui <- renderUI({   
    # Créer des sorties pour les sommes des valeurs numériques pour chaque sous-classe
    output_list <- lapply(subclasses, function(subclass) {
      list(
        verbatimTextOutput(paste0("sum_values_", subclass, "_ui"))
      )
    })
    
    # Afficher les informations dans une balise div avec style
    tagList(div(style = "overflow-x: scroll;", output_list))
  })

  # UI pour afficher les graphiques des résultats de la somme
  output$sum_values_graph_ui <- renderUI({
    # Créer des sorties de graphiques pour chaque sous-classe
    plot_output_list <- lapply(subclasses, function(subclass) {
      plotOutput(outputId = paste0("sum_values_", subclass, "_plot"))
    })
    
    # Afficher les graphiques dans une balise div
    tagList(div(style = "overflow-x: scroll;", plot_output_list))
  })

  output$input_IS <- shiny::renderUI({
    tagList(
      lapply(subclass_names(), function(subclass) {
        div(
          verbatimTextOutput(paste0("selected_adducts_and_standards_", subclass))
        )
      })
    )
  })

  output$standard_table <- DT::renderDataTable({
    params <- list(
      project = input$project
    )
    
    cal_samples <- cal_samples()
    
    # Récupérer toutes les combinaisons d'adducts et de standards sélectionnés
    all_adducts <- unlist(reactiveValuesToList(adduct_inputs))
    all_standards <- unlist(reactiveValuesToList(standard_inputs))
    
    # Debugging: Print the reactive values
    print("Adduct Inputs:")
    print(all_adducts)
    print("Standard Inputs:")
    print(all_standards)

    if (length(all_adducts) > 0 && length(all_standards) > 0) {
      table <- get_standard_table_quanti(db, params$project, all_adducts, all_standards, cal_samples)
      session$sendCustomMessage("Standard", jsonlite::toJSON(as.matrix(table)))
      DT::datatable(as.matrix(table))
      print(table)
    } else {
      table <- as.data.frame("No results")
      DT::datatable(table)
    }
  })

  # Nouveau output pour afficher les valeurs de "total area"
  output$total_area_values <- renderPrint({
    params <- list(
      project = input$project
    )
    
    cal_samples <- cal_samples()
    
    # Récupérer toutes les combinaisons d'adducts et de standards sélectionnés
    all_adducts <- unlist(reactiveValuesToList(adduct_inputs))
    all_standards <- unlist(reactiveValuesToList(standard_inputs))

    if (length(all_adducts) > 0 && length(all_standards) > 0) {
      table <- get_standard_table_quanti(db, params$project, all_adducts, all_standards, cal_samples)
      
      # Éliminer les doublons après la récupération des données
      table <- unique(table)

      # Extraire et formater les valeurs de total area
      formatted_table <- table %>%
        select(sample_id, formula, adduct, `total area`) %>%
        distinct()
      
      formatted_table$`total area` <- as.numeric(gsub(" ", "", formatted_table$`total area`)) / 10^6
      
      # Mettre à jour la variable réactive
      total_area_table(formatted_table)

      # Afficher les résultats formatés
      cat("Total Area Values (in 10^6 units):\n")
      print(formatted_table)
      cat("samples")
      print(formatted_table$sample_id)
      cat("formula")
      print(formatted_table$formula)
      cat("adducts")
      print(formatted_table$adduct)
      cat("total area")
      print(formatted_table$`total area`)
    } else {
      cat("No results")
    }
  })

  output$normalisation_area <- renderPrint({
    if (!is.null(sums_values) && !is.null(total_area_table()) && nrow(total_area_table()) > 0) {
      for (subclass in names(sums_values)) {
        if (!is.null(sums_values[[subclass]])) {
          cat("========================\n")
          cat("Sous-classe :", subclass, "\n")

          cat("Adducts selectionnes pour", subclass, ":\n")
          print(adducts_values[[subclass]])
          
          cat("Formule standard selectionnee pour", subclass, ":\n")
          print(standards_values[[subclass]])

          filtered_table <- total_area_table() %>%
            filter(adduct %in% adducts_values[[subclass]], formula == standards_values[[subclass]])
          
          cat("Valeur d'aires STANDARDS", subclass, ":\n")
          print(filtered_table$sample_id)
          print(filtered_table$`total area`)

          summed_areas <- filtered_table %>%
            mutate(`total area` = ifelse(is.na(`total area`), 0, `total area`)) %>%
            group_by(sample_id) %>%
            summarize(total_area_sum = sum(`total area`), .groups = 'drop')
          
          cat("Sommes des aires STANDARDS : \n")
          print(as.data.frame(summed_areas))

          sums_df <- data.frame(
            sample_id = names(sums_values[[subclass]]),
            sum_value = unlist(sums_values[[subclass]])
          )
          sums_df <- sums_df %>%
            mutate(sample_name = sub("_M\\+Cl|_M-Cl", "", sample_id)) %>%
            mutate(sample_name = sub("_PCAs", "", sample_name)) %>%
            group_by(sample_name) %>%
            summarize(total_sum = sum(sum_value), .groups = 'drop')
          
          cat("Sommes des aires ECHANTILLONS : \n")
          print(as.data.frame(sums_df))

          summed_areas <- summed_areas %>%
            mutate(sample_name = sample_id)
          merged_df <- left_join(sums_df, summed_areas, by = "sample_name")
          
          cat("Association des deux DataFrame ECHANTILLONS + STANDARDS :\n")
          print(as.data.frame(merged_df))
          
          if (nrow(merged_df) > 0) {
            merged_df <- merged_df %>%
              mutate(normalized_value = total_sum / total_area_sum,
                     normalized_value = normalized_value * 1e6)  # Multiplier par 10^6

            cat("Valeurs normalisees ECHANTILLONS / STANDARDS (x 10^6) : \n")
            print(as.data.frame(merged_df %>% select(sample_name, normalized_value)))
            
            # Stocker le merged_df pour le graphique
            assign(paste0("merged_df_", subclass), merged_df, envir = .GlobalEnv)
          } else {
            cat("No matching sample names between sums_df and summed_areas.\n")
          }

          cat("========================\n")
        }
      }
    } else {
      cat("No total area values available for normalization.\n")
    }
  })

  output$graph_normalisation <- renderPlotly({
    # Créer et combiner les graphiques pour chaque sous-classe
    plots <- lapply(names(sums_values), function(subclass) {
      merged_df <- get(paste0("merged_df_", subclass), envir = .GlobalEnv)
      if (!is.null(merged_df)) {
        plot_ly(merged_df, x = ~reorder(sample_name, normalized_value), y = ~normalized_value, type = 'bar', name = subclass) %>%
          layout(
            title = paste("Aires cumulees normalisees des echantillons de calibration par les standards"),
            xaxis = list(title = "Nom de l'echantillon"),
            yaxis = list(title = "Valeur normalisée (x 10^6)")
          )
      }
    })
    
    # Combiner tous les graphiques
    subplot(plots, nrows = length(plots), shareX = TRUE, titleX = TRUE)
  })

  output$graph_exponential <- renderPlotly({
    # Créer et combiner les graphiques pour chaque sous-classe
    plots <- lapply(names(sums_values), function(subclass) {
      merged_df <- get(paste0("merged_df_", subclass), envir = .GlobalEnv)
      if (!is.null(merged_df)) {
        # Définir une séquence de valeurs pour l'axe des x
        x <- seq(0, 10, by = 0.1)
        # Définir la fonction exponentielle
        y <- exp(x)
        
        # Créer un graphique interactif avec plot_ly
        plot_ly(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = subclass) %>%
          layout(
            title = paste("Courbe Exponentielle pour", subclass),
            xaxis = list(title = "X"),
            yaxis = list(title = "exp(X)")
          )
      }
    })
    
    # Combiner tous les graphiques
    subplot(plots, nrows = length(plots), shareX = TRUE, titleX = TRUE)
  })
})