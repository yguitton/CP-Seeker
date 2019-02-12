tabBox(width=6, 
	tabPanel('from database',
		fluidRow(
			column(width=4, uiOutput('uiDbProfileSample')),
			column(width=2, uiOutput('uiDbProfileAdduct')),
			column(width=2, style="padding-top:3%;", actionBttn('dbProfileLaunch', 'Launch'))
		),
		dataTableOutput('dbProfileContent') %>% withSpinner()
	),
	
	tabPanel('from xlsx file',
		fluidRow(
			column(width=2, 
				tags$div(class="form-group shiny-input-container", 
					tags$div(class="input-group", 
						tags$label(class="input-group-btn", 
							tags$span(id="xlsxChooseSpan", class="btn btn-default btn-file action-button", "Browse...",
								actionButton(class="shinyjs-resettable",
									"data-shinyjs-resettable-value"="", "data-shinyjs-resettable-type"="File", 
									"data-shinyjs-resettable-id"="file", inputId="xlsxChoose", label="", name="file", 
									style="display: none;")
							)
						)
					)
				)
			),
			column(width=8, 
				verbatimTextOutput('xlsxPath', placeholder=TRUE)
			),
			column(width=2, actionBttn('xlsxLaunch', 'Launch'))
		),
		dataTableOutput('xlsxContent') %>% withSpinner()
	),
	
	tabPanel('additionnal parameters', 
		fluidRow(
			column(width=3, numericInput('vTarget', 'target volume (%)', 90, min=0, max=100, step=1)),
			column(width=2, numericInput('vDigits', 'precision', value=2, min=0, max=9, step=1))
		)
	)
)