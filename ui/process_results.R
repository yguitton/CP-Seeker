shinydashboard::tabItem(tabName = 'process_results',
	shinydashboard::box(width = 12, 
		shiny::tags$div(class = "params-inline", 
			shiny::selectInput("process_results_file", "Select sample", 
				choices = c(), multiple = FALSE, width = "20vw"),
			shiny::selectInput("process_results_study",
			  "Type", choices = c("chemical", "standard"), width = "20vw"),
			shiny::tags$div(id = "process_results_chemical",
			  shiny::selectInput("process_results_chemical_type",
			    "Family", choices = c(), width = "20vw")
			),
			shinyjs::hidden(
			  shiny::tags$div(id = "process_results_standard",
			    shiny::selectInput("process_results_standard_formula",
			      "Standard formula", choices = c(), width = "20vw")
			  )
			),
			shiny::tags$div(id = "process_results_adduct",
			  shiny::selectInput("process_results_chemical_adduct", 
				  "Adduct", choices = c(), width = "20vw")
			),
			shinyjs::hidden(
			  shiny::tags$div(id = "process_results_adduct2",
			    shiny::selectInput("process_results_standard_adduct",
			      "Adduct", choices = c(), width = "20vw")
			  )
			),
			shinyWidgets::actionBttn("process_results_matrix", "Show matrix", 
			  style = "minimal", color = "primary"
      )
		)
	),
	shinyWidgets::radioGroupButtons('process_results_selected_matrix', '', justified = TRUE,
	  choices = c('Normalized intensities (xE6)', 'Scores', 'Deviations (mDa, xE-4)'), 
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
	  shiny::column(width = 7, style = 'margin-top: 20px; margin-bottom: 20px;',
	    shiny::downloadButton('process_results_download', 'Export matrix')
	  ),
    shiny::column(width = 2,
      shiny::numericInput('process_results_score_min', 'score min', value = 0)
    ),
	  shiny::column(width = 2,
	    shiny::numericInput('process_results_score_max', 'score max', value = 100)
	  ),
	  shiny::column(width = 1, style = 'margin-top: 20px; margin-bottom: 20px;',
	    shiny::actionButton('process_results_apply', 'apply')
	  ),
    shiny::column(width = 12, 
      shinycssloaders::withSpinner(
    			DT::dataTableOutput('process_results_profile')
    	)
    )
	), 
	shiny::column(width = 3, 
		shinydashboard::box(width = 12, 
			shinycssloaders::withSpinner(
				plotly::plotlyOutput("process_results_eic")
			),
			shiny::actionButton('process_results_reintegration', 'reintegration')
		), 
		shinydashboard::box(width = 12, 
			shinycssloaders::withSpinner(
				plotly::plotlyOutput("process_results_ms")
			)
		)
	)
)