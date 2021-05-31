shinydashboard::tabItem(tabName = 'process_results',
	shinydashboard::box(width = 12, 
		shiny::tags$div(class = "params-inline", 
			shiny::selectInput("process_results_file", "Select sample", 
				choices = c(), multiple = FALSE, width = "40vw"),
			shiny::selectInput("process_results_adduct", 
				"Adduct", choices = c(), width = "40vw"),
			shiny::selectInput("process_results_study",
			  "Study", choices = c("chemical", "standard"), width = "40vw"),
			shiny::tags$div(id = "process_results_chemical",
			  shiny::selectInput("process_results_chemical_type",
			    "Chemical", choices = c("CPs", "COs", "CdiOs"), width = "20vw")
			),
			shinyjs::hidden(
			  shiny::tags$div(id = "process_results_standard",
			    shiny::selectInput("process_results_standard_formula",
			      "Standard formula", choices = c("C12D18Br6", "[13]C12H18Br6"), width = "20vw")
			  )
			)
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
	  )
	)),
	shinydashboard::box(width = 9,
	  shiny::downloadButton('process_results_download', 'Download matrix'
	),
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