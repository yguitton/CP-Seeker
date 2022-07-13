shinydashboard::tabItem(tabName = 'manage',
	shinydashboard::box(id = 'manage_db', title = 'Database', width = 12, 
		shinyWidgets::radioGroupButtons('manage_select', 'Select table', justified = TRUE,
			choices = c('Sequence', 'Sample'), 
			checkIcon = list(
				yes = shiny::tags$i(
					class = "fa fa-circle", 
					style = "color: steelblue"
				), 
				no = shiny::tags$i(
					class = "fa fa-circle-o", 
					style = "color: steelblue"
				)
			)),
		shiny::tags$div(style="float:right;", 
			shinyWidgets::actionBttn('manage_add', "Add new entry", 
				style = "bordered", color = "primary")
		),
		shiny::tags$div(style = "float:right;", 
			shinyWidgets::actionBttn("manage_delete", "Delete entry(ies)", 
				style = "bordered", color="danger")
		),
		shiny::tags$br(), 
		shiny::tags$br(), 
		shiny::tags$br(), 
		DT::dataTableOutput('manage_table')
	)
)