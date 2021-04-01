shinydashboard::tabItem(tabName='EIC',
	shinydashboard::box(width = 3, id = "eic_params", 
		shiny::selectizeInput("eic_files", "" , choices = c(), multiple = TRUE, 
			options = list(plugins = list("remove_button"))),
		shiny::tags$table(class = "table-params", 
			shiny::tags$tr(
				shiny::tags$td(
					bsplus::shinyInput_label_embed(
						shiny::numericInput('eic_mz', 'm/z', value = ''),
						bsplus::bs_embed_tooltip(
							bsplus::shiny_iconlink(),
							placement = 'top', 
							title = "m/z for EIC generation, leave empty to draw only the TIC"
						)
					)
				),
				shiny::tags$td(
					bsplus::shinyInput_label_embed(
						shiny::numericInput('eic_mz_tol', 
							'm/z tolerance (mDa)', value = 0.5),
						bsplus::bs_embed_tooltip(
							bsplus::shiny_iconlink(),
							placement = 'top', 
							title = "m/z tolerance (mDa) to use for EIC generation"
						)
					)
				)
			),
			shiny::tags$tr(
				shiny::tags$td(style = 'text-align: center;', 
					shinyWidgets::actionBttn('eic_draw_eic', 'EIC', 
						style = 'minimal', color = 'primary')
				),
				shiny::tags$td(style = 'text-align: center;', 
					shinyWidgets::actionBttn('eic_draw_tic', 'TIC', 
						style = 'minimal', color = 'primary')
				)
			)
		)
	),
	shinydashboard::box(width = 9, 
		shinycssloaders::withSpinner(
			plotly::plotlyOutput('eic_chromato')
		),
		tags$hr(),
		shinycssloaders::withSpinner(
			plotly::plotlyOutput('eic_ms')
		)
	)
)