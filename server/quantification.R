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
data <- reactive({
  params <- list(
    table_selected = input$quantification_choice
  )

  tryCatch({
    if (params$table_selected == "Sample List") {
      # Obtenir les données des samples du projet
      data <- get_samples_quanti(db, input$project)
      if (nrow(data) == 0) {
        print("Aucune donnée trouvée pour ce projet.")
        return(data.frame())  # Retourner un data frame vide si aucune donnée n'est trouvée
      }
      print(data)  # Debugging: Imprimer les données récupérées
      data  # Retourner les données
    } else {
      print("Sélection non valide")  # Debugging: Cas où la sélection n'est pas "Sample List"
      data.frame()  # Retourner un data frame vide si rien n'est trouvé
    }
  }, error = function(e) {
    print("ERR quanti_table")
    print(e)
    sweet_alert_error('Cannot display the table', e$message)
    data.frame()  # Retourner un data frame vide en cas d'erreur
  })
})

output$quanti_table <- DT::renderDataTable({
  DT::datatable(data(), editable = list(target = "cell", disable = list(columns = c(0, 1))))  # Rendre certaines colonnes éditables
})

observeEvent(input$quanti_table_cell_edit, {
  info <- input$quanti_table_cell_edit
  str(info)  # Afficher les informations de l'édition pour le débogage

  # Récupérer les données actuelles
  data_df <- isolate(data())

  # Mettre à jour la valeur dans le data.frame
  row <- info$row
  col <- info$col
  value <- info$value

  # Vérifiez que les colonnes éditées sont parmi celles qu'on veut éditer
  editable_columns <- c("sample_type", "subclass_name", "chlorination_degree", "concentration")
  if (colnames(data_df)[col] %in% editable_columns) {
    data_df[row, col] <- DT::coerceValue(value, data_df[row, col])

    # Construire la requête de mise à jour
    sample_id <- data_df[row, "sample"]
    column_name <- colnames(data_df)[col]
    update_query <- sprintf(
      "UPDATE sample SET %s = ? WHERE sample = ?", column_name
    )

    # Exécuter la mise à jour
    dbExecute(db, update_query, params = list(value, sample_id))

    # Mettre à jour l'objet réactif data
    assign("data", reactive({ data_df }), envir = .GlobalEnv)
  }
})

##########################################
############ Homologue Domain ############
##########################################

#' Le but est de retourner une matrice vide
#' L'utilisateur va selectionner les groupes d'homologue dans la matrice
#' La matrice va avoir C6-C36 première colonne et Cl3-Cl30 en première ligne

# Les groupes d'homologues selectionnés vont être stockés

# Créer une matrice vide avec les dimensions désirées

# Créer les matrices avec les noms de colonnes et de lignes définis
sccp_matrix_initial <- matrix(ncol = 31, nrow = 28)
colnames(sccp_matrix_initial) <- paste0("C", 6:36)
rownames(sccp_matrix_initial) <- paste0("Cl", 3:30)

mccp_matrix_initial <- matrix(ncol = 31, nrow = 28)
colnames(mccp_matrix_initial) <- paste0("C", 6:36)
rownames(mccp_matrix_initial) <- paste0("Cl", 3:30)

lccp_matrix_initial <- matrix(ncol = 31, nrow = 28)
colnames(lccp_matrix_initial) <- paste0("C", 6:36)
rownames(lccp_matrix_initial) <- paste0("Cl", 3:30)

# Réactifs pour stocker les matrices vides pour chaque type
sccp_matrix <- reactiveVal(as.data.frame(sccp_matrix_initial))
mccp_matrix <- reactiveVal(as.data.frame(mccp_matrix_initial))
lccp_matrix <- reactiveVal(as.data.frame(lccp_matrix_initial))

# Réactifs pour stocker les sélections pour chaque type
selected_cells_sccp <- reactiveVal(data.frame(row = integer(0), col = integer(0)))
selected_cells_mccp <- reactiveVal(data.frame(row = integer(0), col = integer(0)))
selected_cells_lccp <- reactiveVal(data.frame(row = integer(0), col = integer(0)))

# Render DataTable en fonction du type sélectionné
output$quanti_matrix_homologue <- DT::renderDataTable({
  matrix_to_display <- switch(input$quanti_subclass_dropdown,
                              "SCCP" = sccp_matrix(),
                              "MCCP" = mccp_matrix(),
                              "LCCP" = lccp_matrix())
  
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

# Observer pour stocker les sélections en fonction du type
observeEvent(input$quanti_matrix_homologue_cells_selected, {
  cells <- input$quanti_matrix_homologue_cells_selected
  if (!is.null(cells)) {
    if (input$quanti_subclass_dropdown == "SCCP") {
      selected_cells_sccp(cells)
    } else if (input$quanti_subclass_dropdown == "MCCP") {
      selected_cells_mccp(cells)
    } else if (input$quanti_subclass_dropdown == "LCCP") {
      selected_cells_lccp(cells)
    }
  }
})

# Observer pour réappliquer les sélections lors du changement de sous-classe
observe({
  cells <- switch(input$quanti_subclass_dropdown,
                  "SCCP" = selected_cells_sccp(),
                  "MCCP" = selected_cells_mccp(),
                  "LCCP" = selected_cells_lccp())
  
  if (!is.null(cells)) {
    proxy <- dataTableProxy('quanti_matrix_homologue')
    selectCells(proxy, cells)
  }
})

output$quanti_matrix_IS <- DT::renderDataTable({
  # Create an empty matrix with the desired dimensions
  quanti_matrix_vide <- matrix(ncol = 31, nrow = 28)
  colnames(quanti_matrix_vide) <- paste0("C", 6:36)
  rownames(quanti_matrix_vide) <- paste0("Cl", 3:30)
  
  # Return the empty matrix as a data frame
  datatable(as.data.frame(quanti_matrix_vide), 
            selection = list(mode = 'multiple', target = 'cell'),
            class = 'display cell-border compact nowrap',
            options = list(info = FALSE, paging = FALSE, dom = 'Bfrtip', 
                           scrollX = TRUE, scroller = TRUE, bFilter = FALSE, 
                           ordering = FALSE, 
                           columnDefs = list(list(className = 'dt-body-justify', 
                                                  targets = "_all")))
  )
})