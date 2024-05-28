################################################
#' Ce qu'il reste à faire :
#' 1 -  Plusieurs sous-classes possibles par échantillon.
#' 2 - Ajouter du javascript dans le tableau pour ne laisser la sélection possible uniquement pour "sample type"
#' 3 - Ajouter un système de dropdown dans le tableau (sélection simple pour sample_type et multiple pour subclass_name)
#' 4 - Ajouter les vérifiactions nécessaire pour empécher les erreurs d'input.
#' 5 - 


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
        print(data)  # Debugging: Imprimer les données récupérées
        return(data)  # Retourner les données
  }})
})

output$quanti_table <- DT::renderDataTable({
  DT::datatable(sample_list_data(), editable = list(target = "cell", disable = list(columns = c(0, 1))))  # Rendre certaines colonnes éditables
})

observeEvent(input$quanti_table_cell_edit, {
  info <- input$quanti_table_cell_edit
  str(info)  # Afficher les informations de l'édition pour le débeugage

  # Récupérer les données actuelles
  data_df <- isolate(sample_list_data())

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
})

##########################################
############ Homologue Domain ############
##########################################

#' Le but est de retourner 3 matrices vides
#' L'utilisateur va selectionner les groupes d'homologue dans chaque matrice
#' Les matrices vont avoir C6-C36 première colonne et Cl3-Cl30 en première ligne

# Les groupes d'homologues selectionnés vont être stockés par matrice (SCCP, MCCP, LCCP)

# Créer une matrice vide avec les dimensions et paramètres désirées
sccp_matrix_initial <- matrix(ncol = 28, nrow = 31)
colnames(sccp_matrix_initial) <- paste0("Cl", 3:30)
rownames(sccp_matrix_initial) <- paste0("C", 6:36)

mccp_matrix_initial <- matrix(ncol = 28, nrow = 31)
colnames(mccp_matrix_initial) <- paste0("Cl", 3:30)
rownames(mccp_matrix_initial) <- paste0("C", 6:36)

lccp_matrix_initial <- matrix(ncol = 28, nrow = 31)
colnames(lccp_matrix_initial) <- paste0("Cl", 3:30)
rownames(lccp_matrix_initial) <- paste0("C", 6:36)

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


###########################################
############ Internal Standard ############
###########################################

#' Permet de choisir un étalon interne.
#' Choisir 1 ou plusieurs ?
#' Sur une famille d'homologue ?
#' Sur un type de chaine (SCCP, MCCP, LCCP) ?
#' En fonction des adduits ?

# Adduct choices for chemical type from DB
output$quanti_IS_chemical_adduct_SCCP <- shiny::renderUI({
  table <- unique(db_get_query(db, "SELECT adduct, chemical_ion_family FROM chemical_ion"))
  splitTable <- split(table$adduct, table$chemical_ion_family)
  for (x in names(splitTable)) {
    if (length(splitTable[[x]]) < 2) {
      names(splitTable[[x]]) <- splitTable[[x]]
    }
  }
  bsplus::shinyInput_label_embed(
    shiny::selectInput("quanti_IS_chemical_adduct_SCCP", "Adduct(s) for SCCP", choices = splitTable, multiple = TRUE),
    bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = 'Adduct(s) to use for SCCP')
  )
})

output$quanti_IS_standard_formula_SCCP <- shiny::renderUI({
  table <- unique(db_get_query(db, "SELECT chemical_type, chemical_familly FROM chemical"))
  table <- table[which(table$chemical_familly == "Standard"), ]
  std_list <- table$chemical_type
  bsplus::shinyInput_label_embed(
    shinyWidgets::pickerInput("quanti_IS_standard_formula_SCCP", "Standard formula for SCCP", choices = std_list),
    bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = "Formula of the standard for SCCP")
  )
})

output$quanti_IS_chemical_adduct_MCCP <- shiny::renderUI({
  table <- unique(db_get_query(db, "SELECT adduct, chemical_ion_family FROM chemical_ion"))
  splitTable <- split(table$adduct, table$chemical_ion_family)
  for (x in names(splitTable)) {
    if (length(splitTable[[x]]) < 2) {
      names(splitTable[[x]]) <- splitTable[[x]]
    }
  }
  bsplus::shinyInput_label_embed(
    shiny::selectInput("quanti_IS_chemical_adduct_MCCP", "Adduct(s) for MCCP", choices = splitTable, multiple = TRUE),
    bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = 'Adduct(s) to use for MCCP')
  )
})

output$quanti_IS_standard_formula_MCCP <- shiny::renderUI({
  table <- unique(db_get_query(db, "SELECT chemical_type, chemical_familly FROM chemical"))
  table <- table[which(table$chemical_familly == "Standard"), ]
  std_list <- table$chemical_type
  bsplus::shinyInput_label_embed(
    shinyWidgets::pickerInput("quanti_IS_standard_formula_MCCP", "Standard formula for MCCP", choices = std_list),
    bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = "Formula of the standard for MCCP")
  )
})

output$quanti_IS_chemical_adduct_LCCP <- shiny::renderUI({
  table <- unique(db_get_query(db, "SELECT adduct, chemical_ion_family FROM chemical_ion"))
  splitTable <- split(table$adduct, table$chemical_ion_family)
  for (x in names(splitTable)) {
    if (length(splitTable[[x]]) < 2) {
      names(splitTable[[x]]) <- splitTable[[x]]
    }
  }
  bsplus::shinyInput_label_embed(
    shiny::selectInput("quanti_IS_chemical_adduct_LCCP", "Adduct(s) for LCCP", choices = splitTable, multiple = TRUE),
    bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = 'Adduct(s) to use for LCCP')
  )
})

output$quanti_IS_standard_formula_LCCP <- shiny::renderUI({
  table <- unique(db_get_query(db, "SELECT chemical_type, chemical_familly FROM chemical"))
  table <- table[which(table$chemical_familly == "Standard"), ]
  std_list <- table$chemical_type
  bsplus::shinyInput_label_embed(
    shinyWidgets::pickerInput("quanti_IS_standard_formula_LCCP", "Standard formula for LCCP", choices = std_list),
    bsplus::bs_embed_tooltip(bsplus::shiny_iconlink(), placement = 'top', title = "Formula of the standard for LCCP")
  )
})


# Stockage des sélections dans des variables réactives
selected_adducts <- reactiveValues(SCCP = NULL, MCCP = NULL, LCCP = NULL)
selected_standards <- reactiveValues(SCCP = NULL, MCCP = NULL, LCCP = NULL)

observeEvent(input$quanti_IS_chemical_adduct_SCCP, {
  selected_adducts$SCCP <- input$quanti_IS_chemical_adduct_SCCP
  print(selected_adducts$SCCP)
})

observeEvent(input$quanti_IS_standard_formula_SCCP, {
  selected_standards$SCCP <- input$quanti_IS_standard_formula_SCCP
  print(selected_standards$SCCP)
})

observeEvent(input$quanti_IS_chemical_adduct_MCCP, {
  selected_adducts$MCCP <- input$quanti_IS_chemical_adduct_MCCP
  print(selected_adducts$MCCP)
})

observeEvent(input$quanti_IS_standard_formula_MCCP, {
  selected_standards$MCCP <- input$quanti_IS_standard_formula_MCCP
  print(selected_standards$MCCP)
})

observeEvent(input$quanti_IS_chemical_adduct_LCCP, {
  selected_adducts$LCCP <- input$quanti_IS_chemical_adduct_LCCP
  print(selected_adducts$LCCP)
})

observeEvent(input$quanti_IS_standard_formula_LCCP, {
  selected_standards$LCCP <- input$quanti_IS_standard_formula_LCCP
  print(selected_standards$LCCP)
})

# Utiliser les sélections pour des calculs ou autres actions
# observeEvent(input$run_calculation, {
#   # Par exemple, exécuter un calcul basé sur les sélections
#   adducts_SCCP <- selected_adducts$SCCP
#   standard_SCCP <- selected_standards$SCCP
#   # Faites des calculs avec adducts_SCCP et standard_SCCP
#   print("SCCP Adducts:")
#   print(adducts_SCCP)
#   print("SCCP Standard:")
#   print(standard_SCCP)
  
#   adducts_MCCP <- selected_adducts$MCCP
#   standard_MCCP <- selected_standards$MCCP
#   # Faites des calculs avec adducts_MCCP et standard_MCCP
#   print("MCCP Adducts:")
#   print(adducts_MCCP)
#   print("MCCP Standard:")
#   print(standard_MCCP)
  
#   adducts_LCCP <- selected_adducts$LCCP
#   standard_LCCP <- selected_standards$LCCP
#   # Faites des calculs avec adducts_LCCP et standard_LCCP
#   print("LCCP Adducts:")
#   print(adducts_LCCP)
#   print("LCCP Standard:")
#   print(standard_LCCP)
# })



#######################################
############ Apply Filters ############
#######################################

#' Ce script va appliquer des filtres sur les matrices avant de lancer le script de quantification.
#' Pour l'instant ne sait pas trop comment faire car il faut connaitre les arguments nécessaires pour 
#' le calcul de quanti pour éxécuter la fonction et renvoyer les bonnes data.