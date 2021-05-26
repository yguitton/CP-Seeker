shinydashboard::tabItem(tabName = 'process_results',
	shinydashboard::box(width = 12, 
		shiny::tags$div(class = "params-inline", 
			shiny::selectInput("process_results_file", "Select sample", 
				choices = c(), multiple = FALSE, width = "40vw"),
			shiny::selectInput("process_results_adduct", 
				"Adduct", choices = c(), width = "40vw") 
		)
	),
	shinyWidgets::radioGroupButtons('process_results_selected_matrix', '', justified = TRUE,
	  choices = c('Scores', 'Standardized intensities', 'Deviations'), 
	  checkIcon = list(
	  yes = shiny::tags$i(
	    class = "fa fa-circle", 
	    style = "color: steelblue"
	  ), 
	  no = shiny::tags$i(
	    class = "fa fa-circle-o", 
	    style = "color: steelblue"
	  ),
	  no = shiny::tags$i(
	    class = "fa fa-circle-o", 
	    style = "color: steelblue"
	  )
	)),
	shinydashboard::box(width = 9,  
		shinycssloaders::withSpinner(
			DT::dataTableOutput('process_results_profile')
		)
	), 
	shiny::column(width = 3, 
		shinydashboard::box(width = 12, 
			shinycssloaders::withSpinner(
				plotly::plotlyOutput("process_results_eic")
			)
		), 
		shinydashboard::box(width = 12, 
			shinycssloaders::withSpinner(
				plotly::plotlyOutput("process_results_ms")
			)
		)
	)
)