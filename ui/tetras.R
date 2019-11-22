tabItem(tabName='tetras', 
	column(width = 3, 
		box(title = "Load Data", width = 12, 
			tabsetPanel(id = "tetrasTab", 
				tabPanel('Database', value = "tetrasDBTab", 
					uiOutput('uiTetrasSample')
				),
				tabPanel('Excel File', value = "tetrasXlsxTab", 
					tags$div(class="form-group shiny-input-container", style = "padding-top: 5%;", 
						tags$div(class="input-group", 
							tags$label(class="input-group-btn", 
								tags$span(id="tetrasXlsxSpan", class="btn btn-default btn-file action-button", "Browse...",
									shinyFilesButton("tetrasXlsxImport", 'select file(s)', class="shinyjs-resettable", 
										multiple=FALSE, label="", style="display: none;")
								)
							),
							tags$input(id = "tetrasXlsxFileName", type = "text", class = "form-control", 
								placeholder = "no file selected", readonly = "readonly")
						),
						div(style='font-style:italic; font-weight:lighter',
							"NB: 3 columns are required (C, Cl and Int)")
					)
				)
			),
			sliderInput('tetrasVTarget', 'volume targeted', min = 0, max = 100, 
				value = 90, step = 1, round = TRUE, post = "%"),
			div(style = "text-align: center;",
				actionBttn('tetrasCompute', 'Compute')
			)
		),
		box(title = "Compare", width = 12, 
			uiOutput('uiTetrasCompare')
		),
		box(title="Record profile", width = 12, 
			textInput('tetrasName', 'Name of the profile', 
				value ='', placeholder = '15% SCPP 30% MCPP'),
			div(style = "text-align: center;",
				actionBttn('tetrasRecord', 'Record')
			)
		)
	),
	column(width = 9,
		tabBox(width = 12, 
			tabPanel("2D", 
				plotlyOutput('tetras2D') %>% withSpinner(),
				dataTableOutput('tetrasTableScores') %>% withSpinner()
			),
			tabPanel("3D", 
				plotlyOutput('tetras3D', height= '800px') %>% withSpinner()
			)
		)
	)
)