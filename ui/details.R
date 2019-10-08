tabItem(tabName='table',
	box(width=12, 
		column(width=2, uiOutput('uiDetailsSample')),
		column(width=1, uiOutput('uiDetailsAdduct')),
		column(width=1, numericInput('detailsTolPpm', 'tol ppm', 5, min=0, step=1)),
		column(width=2, pickerInput('detailsMachine', 'machine', choices=
					names(resolution_list), option=list(`live-search`=TRUE))),
		column(width=1, style="padding-top: 1.6%;", actionBttn('detailsErase', 'Erase')),
		column(width=1, style="padding-top: 2%;", switchInput('detailsSwitch', onLabel='Scores', offLabel='tR', value=TRUE))
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