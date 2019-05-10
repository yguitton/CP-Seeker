tabItem(tabName='details',
	box(width=12, 
		column(width=2, uiOutput('uiDetailsSample')),
		column(width=1, uiOutput('uiDetailsAdduct')),
		column(width=1, numericInput('detailsTolPpm', 'tol ppm', 5, min=0, step=1)),
		column(width=3, pickerInput('detailsMachine', 'machine', choices=
					setNames(1:length(resolution_list), names(resolution_list)), option=list(`live-search`=TRUE))),
		column(width=1, style="padding-top: 1.6%;", actionBttn('detailsErase', 'Erase')),
		column(width=1, style="padding-top: 2%;", switchInput('detailsSwitch', onLabel='Scores', offLabel='tR', value=TRUE))
	),
	
	box(width=12, 
		dataTableOutput('detailsTable')
	),
	column(width=12,
		plotlyOutput('detailsEic', height='800px') %>% withSpinner())
)