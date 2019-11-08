tabItem(tabName='table',
	box(width=12, 
		tags$table(class = "table3", 
			tags$tr(
				tags$td(style = "width: 10%;", 
					uiOutput('uiDetailsSample')
				),
				tags$td(style = "width: 5%;",
					uiOutput('uiDetailsAdduct')
				),
				tags$td(style = "width: 5%;", 
					numericInput('detailsTolPpm', 'tol ppm', 5, min=0, step=1)
				),
				tags$td(style = "width: 10%;", 
					pickerInput('detailsMachine', 'machine', choices=
						names(resolution_list), option=list(`live-search`=TRUE))
				),
				tags$td(style = "width: 5%;", 
					tags$div(class = "form-group shiny-input-container", 
						actionBttn('detailsErase', 'Erase')
					)
				),
				tags$td(style = "width: 5%;", 
					tags$div(class = "form-group shiny-input-container", 
						switchInput('detailsSwitch', onLabel='Scores', offLabel='tR', 
							value=TRUE)
					)
				)
			)
		)			
	),
	
	box(width=12, 
		DT::dataTableOutput('detailsTable')
	),
	column(width=6,
		plotlyOutput('detailsMS') %>% withSpinner()
	), 
	column(width = 6, 
		plotlyOutput('detailsEIC') %>% withSpinner()
	)	
)