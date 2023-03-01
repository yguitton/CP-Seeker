shinydashboard::tabItem(tabName = 'process_results',
	shinydashboard::box(width = 12, 
		shiny::tags$div(class = "params-inline", 
			shiny::selectInput("process_results_study",
				"Type", choices = c(), width = "30vw"),
			shinyjs::hidden(
				# To show when standard choose
				shiny::tags$div(id = "process_results_standard",
			    shiny::selectInput("process_results_standard_formula",
			      "Standard formula", choices = c(), width = "30vw")
			  ),
			  shiny::tags$div(id = "process_results_adduct2",
			    shiny::selectInput("process_results_standard_adduct",
			      "Adduct", choices = c(), width = "30vw")
			  ),
			  # To show when chemical family choose
			  shiny::tags$div(id = "process_result_sample",
			  	shiny::selectInput("process_results_file", "Select sample", 
						choices = c(), multiple = FALSE, width = "30vw")
			  ),
			  shiny::tags$div(id = "process_results_adduct",
			  	shiny::selectInput("process_results_chemical_adduct", 
				  	"Adduct", choices = c(), width = "30vw")
				)
			)
		)
	),
	shinydashboard::box(width = 9,
	  shiny::column(width = 3, style = 'margin-top: 20px; margin-bottom: 20px;',
	    shiny::actionButton('process_results_download', 'Export matrix', 
	      icon = shiny::icon("download"))
	  ),
	  shiny::column(width = 2),
	  shiny::column(width = 3,
      shiny::numericInput('process_results_deviation_max', 'Deviation max (absolute value)', value = 0, step = 0.01)
    ),
    shiny::column(width = 3,
      shiny::numericInput('process_results_score_min', 'Score min', value = 0, min = 0, max = 100, step = 1)
    ),
	  shiny::column(width = 1, style = 'margin-top: 20px; margin-bottom: 20px;',
	    shiny::actionButton('process_results_apply', 'Apply')
	  ),
	  shinyWidgets::radioGroupButtons('process_results_selected_matrix', '', justified = TRUE,
	    choices = c('Normalized intensity (xE6)', 'Score (%)', 'Deviation (mDa)'), 
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
    shiny::column(width = 12, 
      shinyjs::hidden(
        shiny::tags$div(id = "process_results_profile_div",
      	  shinycssloaders::withSpinner(
    			  DT::dataTableOutput('process_results_profile')
    		  )
    	  )
      )
    ),
	  shiny::column(width = 12, 
	    DT::dataTableOutput('process_results_standard_table')
	  )
	), 
	shiny::column(width = 3, 
		shinydashboard::box(width = 12, 
			shinycssloaders::withSpinner(
				plotly::plotlyOutput("process_results_eic")
			),
			#shiny::actionButton('process_results_reintegration', 'reintegration')
		), 
		shinydashboard::box(width = 12, 
			shinycssloaders::withSpinner(
				plotly::plotlyOutput("process_results_ms")
			)
		)
	)
)