################################################
#' Ce qu'il reste à faire :
#' 1 - Plusieurs sous-classes possibles par échantillon. Possibilité d'en ajouter d'autres que seulement SCCP, MCCP et LCCP.
#' 2 - Ajouter du javascript dans le tableau pour ne laisser la sélection possible uniquement pour "sample type" (dynamique)
#' 3 - Ajouter un système de dropdown dans le tableau (sélection simple pour sample_type et multiple pour subclass_name)
#' 4 - Ajouter les vérifiactions nécessaire pour empécher les erreurs d'input.
#' 5 - Système d'étapes. D'abord l'intégration avant la quanti, une fois dans la quanti : Sample List > Homologue Domain > Internal Standard > Filters
#' 6 - Récupérer les matrices des aires pour chaque échantillon associés
#' 7 - Afficher les valeurs de la matrice avec les aires maximales pour chaque sous classe dans la partie Homologue Domain pour faciliter la sélection des régions.

#########################################
################## SQL ##################
#########################################

#' Ces requêtes vont devoir être transférés dans les fichiers db_get etc...

get_subclass_names <- function(db_connection, project_id) {
  subclass_names_query <- "SELECT DISTINCT s.subclass_name
                           FROM sample s
                           INNER JOIN project_sample ps ON s.sample = ps.sample
                           INNER JOIN deconvolution_param dp ON ps.project = dp.project
                           WHERE dp.project = ?"
  
  subclass_names <- dbGetQuery(db_connection, subclass_names_query, params = list(project_id))
  return(subclass_names)
}

get_samples_quanti <- function(db, project = NULL) {
  if (is.null(project)) {
    return(data.frame())
  }

  # Requête pour obtenir les samples associés au projet donné
  project_query <- "SELECT sample, sample_id FROM project_sample WHERE project = ?"
  project_samples <- dbGetPreparedQuery(db, project_query, bind.data = data.frame(project = project))

  if (nrow(project_samples) == 0) {
    return(data.frame())
  }

  samples <- project_samples$sample
  samples_str <- paste(sprintf("'%s'", samples), collapse = ", ")
  
  # Requête pour obtenir les détails des samples
  query <- sprintf("SELECT sample, sample_type, subclass_name, chlorination_degree, concentration FROM sample WHERE sample IN (%s)", samples_str)
  
  db_get_query(db, query)
}

#####################################
############ Sample List ############
#####################################

#' Le but ici est de laisser l'utilisateur rentrer les samples types
#' 3 types de sample type : CAL, BLANK, SAMPLE
#' Si le Type est égal à CAL alors faire rentrer les informations.
#' Les infos : subclassname SCCP, MCCP, LCCP || chlorination_degree || concentration
#' 
#' Pour l'instant l'utilisateur peut remplir ce qu'il veut dans les colonnes qu'il veut.
#' Par la suite ajouter des restrictions.

# Créer un objet réactif pour stocker les données
sample_list_data <- reactive({
  params <- list(
    table_selected = input$quantification_choice
  )
  tryCatch({  
    if(params$table_selected == "Sample List"){  # Utiliser '==' pour la comparaison
        # Obtenir les données des samples du projet
        data <- get_samples_quanti(db, input$project)
        if (nrow(data) == 0) {
          print("Aucune donnée trouvée pour ce projet.")
          return(data.frame())  # Retourner un data frame vide si aucune donnée n'est trouvée
        }
        # print(data)  # Debugging: Imprimer les données récupérées
        return(data)  # Retourner les données
  }})
})

output$quanti_table <- DT::renderDataTable({
  DT::datatable(sample_list_data(), editable = list(target = "cell", disable = list(columns = c(0, 1))))  # Rendre certaines colonnes éditables
})

subclass_names <- reactiveVal(NULL)

observeEvent(c(input$quanti_table_cell_edit, input$project), {

  project_id <- input$project
  print(project_id)

  info <- input$quanti_table_cell_edit
  str(info)  # Afficher les informations de l'édition pour le débeugage

  # Récupérer les données actuelles
  data_df <- isolate(sample_list_data())

  # Vérifier si info est NULL
  if (!is.null(info)) {
    # Mettre à jour la valeur dans le data.frame
    row <- info$row
    col <- info$col
    value <- info$value

    # Vérifiez que les colonnes éditées sont parmi celles qu'on veut éditer
    editable_columns <- c("sample_type", "subclass_name", "chlorination_degree", "concentration")
    if (colnames(data_df)[col] %in% editable_columns) {
      data_df[row, col] <- DT::coerceValue(value, data_df[row, col])

      # Requête pour UPDATE la bdd avec les modifications
      sample_id <- data_df[row, "sample"]
      column_name <- colnames(data_df)[col]
      update_query <- sprintf(
        "UPDATE sample SET %s = ? WHERE sample = ?", column_name
      )

      # Exécution de la requête
      dbExecute(db, update_query, params = list(value, sample_id))
    }
  }

  # Si project_id n'est pas NULL, exécuter la nouvelle requête pour mettre à jour subclass_names
  if (!is.null(project_id)) {
    subclass_names <- get_subclass_names(db, project_id)
    subclass_names <- subclass_names$subclass_name
    subclass_names(subclass_names)
  }
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

# Stocker les matrices et les sélections de cellules pour chaque sous-classe
matrix_list <- reactiveVal(list())
selected_cells_list <- reactiveVal(list())

observe({ 
  matrices <- lapply(subclass_names(), function(subclass) {
    matrix <- matrix(ncol = 28, nrow = 31)
    colnames(matrix) <- paste0("Cl", 3:30)
    rownames(matrix) <- paste0("C", 6:36)
    as.data.frame(matrix)
  })
  
  names(matrices) <- subclass_names()
  matrix_list(matrices)
  
  # Initialiser les sélections de cellules pour chaque sous-classe
  selected_cells <- lapply(subclass_names(), function(subclass) {
    data.frame(row = integer(0), col = integer(0))
  })
  names(selected_cells) <- subclass_names()
  selected_cells_list(selected_cells)
})


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


#######################################
############ Apply Filters ############
#######################################

#' Ce script va appliquer des filtres sur les matrices avant de lancer le Launch Quanti.
#' Pour l'instant ne sait pas trop comment faire car il faut connaitre les arguments nécessaires pour 
#' le calcul de quanti pour éxécuter la fonction et renvoyer les bonnes data.


#######################################
############ Launch Quanti ############
#######################################

#' Ici le code va servir pour récupérer toutes les matrices pour faire la somme
#' Fonction reduce_matrix arguments : 
#' files = samples$sample_id
#' input$process_results_study = "PCAs"
#' input$process_results_chemical_adduct = "M+Cl"
#' select_choice = 1 (matrice de score "area")
#' 
#' Donc une fois que pour 1 sample on a la bonne matrice faire une boucle pour en avoir plusieurs et faire la somme

observeEvent(input$quanti_launch, {
  output$quanti_results_profile <- DT::renderDataTable({
    samples <- get_samples(db, input$project)
    files <- samples$sample_id
    print(files) # [1] "20220611_072" "20220611_073" "20220611_081"
    for (file in files) { print(file) }

    # Initialiser une matrice pour la somme
    summed_matrix <- NULL
    # Initialiser une matrice pour les indicateurs de position
    position_matrix <- NULL

    # Boucle pour récupérer et sommer les matrices de chaque échantillon
    for (file in files) {
      # Récupérer la matrice pour l'échantillon courant
      # Réduire la matrice pour obtenir la matrice de scores (val = 2)
      matrix <- reduce_matrix(mat()[[file]][[input$process_results_study]][[input$process_results_chemical_adduct]], 2, greycells = TRUE)
      
      # Inspecter les valeurs avant sum
      print("Values before sum:")
      print(matrix) # Voir une partie de la matrice (Cl3 to Cl30).

      # Séparer les valeurs numériques et les indicateurs de position
      numeric_matrix <- apply(matrix, 2, function(x) {
        sapply(strsplit(x, "/"), function(y) {
          value <- suppressWarnings(as.numeric(y[1]))
          if (is.na(value)) 0 else value
        })
      })

      position_matrix <- apply(matrix, 2, function(x) {
        sapply(strsplit(x, "/"), function(y) {
          if (length(y) > 1) y[2] else "NA"
        })
      })
      
      # Vérifier la matrice convertie
      print("Numeric matrix:")
      print(numeric_matrix)

      # Ajouter la matrice réduite à la matrice sommée
      if (is.null(summed_matrix)) {
        summed_matrix <- numeric_matrix
      } else {
        # Additionner les matrices
        summed_matrix <- summed_matrix + numeric_matrix
      }
    }

    # Créer une matrice combinée pour l'affichage avec indicateurs de position
    combined_matrix <- mapply(function(num, pos) {
      paste0(num, "/", pos)
    }, summed_matrix, position_matrix)

    # Convertir en matrice
    combined_matrix <- matrix(combined_matrix, nrow = nrow(summed_matrix), ncol = ncol(summed_matrix),
                              dimnames = list(rownames(summed_matrix), colnames(summed_matrix)))

    # Afficher la matrice résultante
    DT::datatable(combined_matrix, selection = "none", extensions = 'Scroller', 
                  class = 'display cell-border compact nowrap', 
                  options = list(info = FALSE, paging = FALSE, dom = 'Bfrtip', scroller = TRUE, 
                                scrollX = TRUE, bFilter = FALSE, ordering = FALSE, 
                                columnDefs = list(list(className = 'dt-body-center', targets = "_all")),
                                initComplete = htmlwidgets::JS("
    function (settings, json) {
      var table = settings.oInstance.api();
      table.cells().every(function() {
        if(this.index().column == 0) {
          this.data(this.data());
        }else if (this.data() == null){
          $(this.node()).addClass('outside');
        }else if (this.data() != null){
          var splitted_cell = this.data().split('/');
          if(splitted_cell[0] == 'NA'){
            this.data('')
          }else{
            this.data(splitted_cell[0]);
          }
          if(splitted_cell[1] == 'outside'){
            $(this.node()).addClass('outside');
          }else if(splitted_cell[1] == 'half'){
            $(this.node()).addClass('half');
          }else if(splitted_cell[1] == 'inside'){
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
