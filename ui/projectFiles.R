tabItem(tabName='projectFiles', 
	box(title="Create project", width=3,
		textInput('projectName', 'Name', placeholder='name of the project'),
		textInput('projectComment', 'Comment', placeholder='(facultative)'),
		div(style="text-align:center;", actionBttn('projectCreate', 'create project'))
	),
	box(title='Import files', width=6, 
		fluidRow(
			column(width=2, style="padding-top:3.8%; padding-left:5%;",
				tags$div(class="form-group shiny-input-container", 
					tags$div(class="input-group", 
						tags$label(class="input-group-btn", 
							tags$span(id="rawFilesSpan", class="btn btn-default btn-file action-button", "Browse...",
								shinyFilesButton("fileImportRaw", title='select file(s)', class="shinyjs-resettable", 
									multiple=TRUE, label="", style="display: none;")
							)
						)
					)
				)
			),
			column(width=8, 
				uiOutput('uiRawFiles')
			)
		),
		dataTableOutput('sampleTable')
	)#,
	# box(title='Standards', width=6,
		# dataTableOutput('standardTable')
	# )
)