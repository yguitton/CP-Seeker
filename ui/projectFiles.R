tabItem(tabName='projectFiles', 
	box(title="Create project", width=3,
		textInput('projectName', 'Name', placeholder='name of the project'),
		textInput('projectComment', 'Comment', placeholder='(facultative)'),
		div(style="text-align:center;", actionBttn('projectCreate', 'create project'))
	),
	box(id='filesBox', width=7, title='Add file(s)',
		box(width=12, title='from the database',
			column(width=3, uiOutput('uiFileDBProject')),
			column(width=6, uiOutput('uiFilesDB')),
			column(width=3, style="padding-top:2.6%;", actionBttn('fileDBAdd', 'Valid'))
		),
		
		box(width=12, title='Import file(s)',
			tags$div(class="form-group shiny-input-container", 
				tags$div(class="input-group", 
					tags$label(class="input-group-btn", 
						tags$span(id="rawFilesSpan", class="btn btn-default btn-file action-button", "Browse...",
							shinyFilesButton("filesImport", 'select file(s)', class="shinyjs-resettable", 
								multiple=TRUE, label="", style="display: none;")
						)
					)
				)
			)
		)
	)#,
	# box(title='Standards', width=6,
		# dataTableOutput('standardTable')
	# )
)