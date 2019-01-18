box(width=6, 
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
	tags$div(style="text-align:center;", actionBttn('launch', 'launch')),
	dataTableOutput('xlsxContent') %>% withSpinner()
)