box(width=6, 
	box(title='from database', width=12, 
		column(width=4, uiOutput('uiDbProfileSampleAdduct'))
	),
	box(title='from xlsx file', width=12, 
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
		)
	),
	box(title='additionnal parameters', width=12, 
		column(width=3, numericInput('vTarget', 'target volume (%)', 90, min=0, max=100, step=1)),
		column(width=2, numericInput('vDigits', 'precision', value=2, min=0, max=9, step=1))
	),
	tags$div(style="text-align:center;", actionBttn('profileLaunch', 'Launch'))
)