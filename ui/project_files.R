shinydashboard::tabItem(tabName = 'project_files',
	shiny::column(id = "user_box", width = 2, shinydashboard::box(width = 12, title = "New user", 
		shiny::textInput('login', 'Name or intials', 
			placeholder = "Enter a name"),
		shiny::column(width = 12, style = "text-align:center", 
			shinyWidgets::actionBttn('user_create', 'Create', style = 'minimal', 
				color = 'primary')
		)
	)),
	shiny::column(id = "project_box", width = 4, shinydashboard::box(width = 12, title = "New sequence", 
		shiny::fluidRow(
			shiny::column(width = 6, 
				shiny::textInput('project_name', 'Name', 
					placeholder = 'Enter a name')
			),
			shiny::column(width = 6, 
				shiny::textInput('project_comment', 'Comment', 
					placeholder = '(optional)')
			)
		),
		shiny::fluidRow(
			shiny::column(width = 12, style = "text-align:center", 
				shinyWidgets::actionBttn('project_create', 'Create', 
					style = 'minimal', color = 'primary')
			)
		)
	)),

	shiny::column(id = "file_box", width = 6, shinydashboard::box(width = 12, title = 'Add file(s)',
		shiny::column(width = 6, 
			shiny::tags$label('From a previous project'),
			shiny::fluidRow(
				shiny::column(width = 12, 
					shiny::selectizeInput('files_from_db', label = NULL, 
						choices = c(), multiple = TRUE, 
						options = list(searchField = c("text", "optgroup")))
				)
			),
			shiny::fluidRow(
				shiny::column(width = 12, style = "text-align: center", 
					shinyWidgets::actionBttn('file_associate', 'Valid', 
						style = 'minimal', color = 'primary')
				)
			)
		),
		
		shiny::column(width = 6, 
			shiny::tags$label('From hard drive'),
			tags$div(class="form-group shiny-input-container", 
				style = "text-align:center",  
				tags$div(class="input-group", 
					tags$label(class="input-group-btn", 
						shiny::tags$a(class = "bttn bttn-no-fill bttn-primary bttn-no-outline", 
							shiny::icon("upload"),
							shinyFiles::shinyFilesButton("file_import", 
								'select file(s)', class = "shinyjs-resettable ", 
								multiple = TRUE, label = "", 
								style = "display: none;"
							)
						)
					)
				)
			)
		)
	))
)