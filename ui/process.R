shinydashboard::tabItem(tabName = 'process',
  shinydashboard::box(width = 3,
    shiny::column(width = 12, style = "margin-bottom: 20px; text-align: center;",
      shinyWidgets::actionBttn('process_launch', 'Launch deconvolution process',
        style = 'minimal', color = 'primary')
    ),
    shinyWidgets::radioGroupButtons('process_chemical_standard', '', justified = TRUE,
      choices = c('General', 'Target analyte', 'Standard'),
      checkIcon = list(
        yes = shiny::tags$i(
          class = "fa fa-circle",
          style = "color: steelblue"
        ),
        no = shiny::tags$i(
          class = "fa fa-circle-o",
          style = "color: steelblue"
        )
      )
    ),
    shiny::tags$div(id = "process_general",
      shiny::tags$div(style = "display: flex; align-items: center;",
        shiny::tags$div(style = "margin-right: 0px;",
          shiny::numericInput("process_mz_tol", "Mass tolerance", value = 3)
        ),
        shiny::tags$div(style = "margin-left: 0px;",
          shiny::tags$br(),
          shinyWidgets::switchInput("process_mz_tol_unit", "",
            value = TRUE, onLabel = "ppm", offLabel = "mDa", offStatus = "primary")
        )
      ),
      shiny::selectInput('process_instrument', 'Instrument',
        choices = c("Orbitrap", "QTOF_XevoG2-S", "Sciex_TripleTOF5600",
          "Sciex_TripleTOF6600", "Sciex_QTOFX500R", "Agilent_QTOF6550")
      ),
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
            'Peakwidth max (s)', value = 300),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = 'top',
            title = 'Expected approximate peak width in chromatographic space'
          )
        )
      ),
      shiny::column(width = 6,
        bsplus::shinyInput_label_embed(
          shiny::numericInput("process_retention_time_min", "Retention time min (min)",
            value = 0),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = 'top',
            title = 'Expected approximate retention time'
          )
        )
      ),
      shiny::column(width = 6,
        bsplus::shinyInput_label_embed(
          shiny::numericInput("process_retention_time_max", "Retention time max (min)",
            value = 20),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = 'top',
            title = 'Expected approximate retention time'
          )
        )
      ),
      shiny::column(width = 6,
        bsplus::shinyInput_label_embed(
          shiny::numericInput("process_missing_scans", "Missing scans",
            value = 10),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = 'top',
            title = 'Maximim number of scans to consider them consecutive.'
          )
        )
      )
    ),
	#essaie pour faire des choice par opgroup
	#shinyjs::hidden(
		#shiny::tags$div(id = "process_chemical",
			#shiny::uiOutput("ui_process_adduct")
		#	 shiny::column(id = "Adduct", width = 12, shinydashboard::box(width = 12, title = 'Add adduct(s)',
		#		shiny::column(width = 12, 
		#			shiny::tags$label('adducts'),
		#			shiny::fluidRow(
		#				shiny::column(width = 12, 
		#					shiny::selectInput('adduct', label = NULL, 
		#					choices = ""
								#("Unknown coast 1", "Unknown coast 2:",
								#'ECNI'= 
								#choices = c(setnames(esiapci_adducts$adduct)),#"M-Cl", "M-HCl", "M-Br","M-HBr"),
								#'ESI/APCI' = esiapci_adducts),#c("M-H", "M+Br", "M+Cl","M+Ac-H")), #multiple = TRUE,
								#choices = c(setNames(choices$project_sample, choices$sample_id)), 
								#selected = c(setNames(choices$project_sample, choices$sample_id)))
								#choices = c(setNames(ecni_adducts()$adduct), #list(ecni_adducts), #c("NY", "NJ", "CT"),
								#choices = c(setnames(esiapci_adducts()$adduct)),multiple = TRUE,##list(esiapci_adducts),#c("WA", "OR", "CA")), multiple = TRUE,
								
								#options = list(searchField = c("text", "optgroup"))
				#			multiple = TRUE,	
							#options = list(searchField = c("text", "optgroup"))
								#options = list(searchField = c("text", "optgroup"))
							#),
							#shiny::uiOutput("ui_process_adduct")
						#)
				#	),
					#shiny::fluidRow(
						#shiny::column(width = 12, style = "text-align: center", 
						#	shinyWidgets::actionBttn('file_associate', 'Valid', 
						#		style = 'minimal', color = 'primary')
						#)
						#	)
				#)
			#)
		#)
	
	#),
	#code original de l'application avec les commentaires en # comme ceux de sebastien
    shinyjs::hidden(
  		shiny::tags$div(id = "process_chemical",
       shiny::uiOutput("ui_process_chemical_type"),
    	bsplus::shinyInput_label_embed(
    			shiny::selectInput("process_adduct",
    				"Adduct(s)", list(
                         'ECNI' = c("M-Cl", "M-HCl", "M-Br","M-HBr"),
                         'ESI/APCI' = c("M-H", "M+Br", "M+Cl","M+Ac-H")
							), multiple = TRUE),
						 #options = list(searchField = c("text", "optgroup"))),
						 #choices = available_adducts, multiple = TRUE),
    			bsplus::bs_embed_tooltip(
    				bsplus::shiny_iconlink(),
    				placement = 'top',
    				title = 'Adducts to use for ion formula generation'
					)
			#shiny::uiOutput("ui_result")
				
    		)
  		)
	),
    shinyjs::hidden(
      shiny::tags$div(id = "process_standard",
        shinyWidgets::switchInput("process_standard_study", "standard study",
          value = FALSE, onLabel = "yes", offLabel = "no"),
        shiny::tags$div(id = "process_standard_params",
          bsplus::shinyInput_label_embed(
            shiny::selectInput("process_standard_formula", "Standard formula",
              choices = c("C12[2]H18Br6", "[13]C12H18Br6"), multiple = TRUE),
            bsplus::bs_embed_tooltip(
              bsplus::shiny_iconlink(),
              placement = 'top',
              title = "Formula of the standard"
            )
          ),
          bsplus::shinyInput_label_embed(
            shiny::selectInput("process_standard_adduct", "Adduct",
              choices = c("M-H (or M-D)", "M+Cl"), multiple = TRUE),
            bsplus::bs_embed_tooltip(
              bsplus::shiny_iconlink(),
              placement = 'top',
              title = "Adduct to use"
            )
          ),
          bsplus::shinyInput_label_embed(
            shiny::numericInput("process_standard_retention_time_1", "Retention time +/- 2 (min)",
              value = ""),
            bsplus::bs_embed_tooltip(
              bsplus::shiny_iconlink(),
              placement = 'top',
              title = "Retention time to use for the standard study"
            )
          ),
          shiny::uiOutput("process_standard_rt")
        )
      )
    )
	),

	shinydashboard::box(width = 9,
		shinycssloaders::withSpinner(plotly::plotlyOutput('process_TIC')),
		tags$hr(),
		shinycssloaders::withSpinner(plotly::plotlyOutput('process_MS'))
	)
)
