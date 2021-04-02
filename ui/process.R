shinydashboard::tabItem(tabName = 'process', 
	shinydashboard::box(width = 3, 
		shiny::column(width = 12, style = "margin-bottom: 20px; text-align: center;", 
			shinyWidgets::actionBttn('process_launch', 'Launch deconvolution process', 
				style = 'minimal', color = 'primary')
		),
		bsplus::shinyInput_label_embed(
			shiny::selectInput("process_adduct", 
				"Adduct", choices = available_adducts),
			bsplus::bs_embed_tooltip(
				bsplus::shiny_iconlink(),
				placement = 'top', 
				title = 'Adducts to use for ion formula generation', 
			)
		), 
		shiny::selectInput('process_instrument', 'Instrument', 
			choices = c("Orbitrap", "QTOF_XevoG2-S", "Sciex_TripleTOF5600", 
				"Sciex_TripleTOF6600", "Sciex_QTOFX500R", "Agilent_QTOF6550")),
		shiny::tags$div(id = "process_orbitrap", 
			shiny::tags$label(class = "control-label", "Resolution"), 
			shiny::tags$table(style = "text-align: center; white-space: nowrap; margi-top: -20px;", 
				shiny::tags$tr(
					shiny::tags$td(
						shiny::numericInput('process_resolution', '', value = 140)
					), 
					shiny::tags$td(style = "padding-left: 5px; padding-right: 5px;", 
						shiny::tags$h4(tags$b('k @'))
					),
					shiny::tags$td(
						shiny::numericInput('process_resolution_mz', '', value = 200)
					)
				)
			)
		),
		shinyjs::hidden(
			shiny::selectInput('process_resolution_index', 
				'Resolution', choices = setNames(25, "25k@200"))
		), 
		shiny::column(width = 12, 
			shiny::tags$div(style = "margin-right: 0px;", 
				shiny::numericInput("process_mz_tol", "mass tolerance", 
					value = 5)
			), 
			shiny::tags$div(style = "margin-left: 0px;", 
				shiny::tags$br(), 
				shinyWidgets::switchInput("process_mz_tol_unit", "", 
					value = TRUE, onLabel = "ppm", offLabel = "mDa")
			)
		), 
		shiny::column(width = 12, 
			shiny::column(width = 6, 
				bsplus::shinyInput_label_embed(
					shiny::numericInput('process_peakwidth_min', 
						'Peakwidth min (s)', value = 5),
					bsplus::bs_embed_tooltip(
						bsplus::shiny_iconlink(),
						placement = 'top', 
						title = 'Expected approximate peak width in chromatographic space'
					)
				)
			),
			shiny::column(width = 6, 
				bsplus::shinyInput_label_embed(
					shiny::numericInput('process_peakwidth_max', 
						'Peakwidth max (s)', value = 60),
					bsplus::bs_embed_tooltip(
						bsplus::shiny_iconlink(),
						placement = 'top', 
						title = 'Expected approximate peak width in chromatographic space'
					)
				)
			)
		), 
		bsplus::shinyInput("process_missing_scans", 
			shiny::numericInput("process_missing_scans", "missing scans", 
				value = 1), 
			bsplus::bs_embed_tooltip(
				bsplus::shiny_iconlink(),
				placement = 'top', 
				title = 'Maximim number of scans to consider them consecutive.'
			)
		)
	), 
	
	shinydashboard::box(width = 9, 
		shinycssloaders::withSpinner(plotly::plotlyOutput('process_TIC')),
		tags$hr(),
		shinycssloaders::withSpinner(plotly::plotlyOutput('process_MS'))
	)
)
