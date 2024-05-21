# Récupérer les données des samples du projet
output$quanti_table <- DT::renderDataTable({
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
    DT::datatable(data, editable = TRUE)  # Rendre les colonnes éditables
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

# Observer les modifications dans le tableau
observeEvent(input$quanti_table_cell_edit, {
info <- input$quanti_table_cell_edit
str(info)  # Afficher les informations de l'édition pour le débogage

# Récupérer les données actuelles
data <- get_samples_quanti(db, input$project)

# Mettre à jour la valeur dans le data.frame
row <- info$row
col <- info$col + 1  # +1 parce que DataTables est basé sur des index 0
value <- info$value

# Vérifiez que les colonnes éditées sont parmi celles qu'on veut éditer
editable_columns <- c("sample_type", "subclass_name", "chlorination_degree", "concentration")
if (colnames(data)[col] %in% editable_columns) {
    data[row, col] <<- DT::coerceValue(value, data[row, col])
    
    # Construire la requête de mise à jour
    sample_id <- data[row, "sample"]
    column_name <- colnames(data)[col]
    update_query <- sprintf(
    "UPDATE sample SET %s = ? WHERE sample = ?", column_name
    )
    
    # Exécuter la mise à jour
    dbExecute(db, update_query, params = list(value, sample_id))
}
})
