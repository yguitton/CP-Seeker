tabItem(tabName='projectFiles', 
	box(title="Create project", width=3,
		textInput('projectName', 'Name', placeholder='name of the project'),
		textInput('projectComment', 'Comment', placeholder='(facultative)'),
		div(style="text-align:center;", actionBttn('projectCreate', 'create project'))
	),
	box(id='filesBox', width=7, title='Add file(s)',
		box(width=6, title='from the database',
			fluidRow(
				column(width = 12, 
					uiOutput('uiFilesDB')
				)
			),
			fluidRow(
				column(width = 12, style = "text-align: center", 
					actionBttn('fileDBAdd', 'Valid')
				)
			)
		),
		
		box(width=6, title='from hard drive',
			tags$div(class="form-group shiny-input-container", style = "text-align:center",  
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
	),
	
	box(width = 12, title = 'Delete records',
		tabsetPanel(
			tabPanel('Project(s)', 
				tags$div(style = "float: right;", 
					actionBttn('deleteProjects', 'Delete', style = 'unite', color = 'danger')
				),
				DT::dataTableOutput('deleteProjectsTable')
			),
			tabPanel('Sample(s)', 
				tags$div(style = "float: right;", 
					actionBttn('deleteSamples', 'Delete', style = 'unite', color = 'danger')
				),
				DT::dataTableOutput('deleteSamplesTable')
			)
		)
	)
)
