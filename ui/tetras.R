tabItem(tabName='tetras', 
	column(width = 3, 
		box(title = "Load Data", width = 12, 
			tabsetPanel( 
				tabPanel('Database', 
					uiOutput('uiTetrasSample'),
					uiOutput('uiTetrasAdduct')
				),
				tabPanel('xlsxFile'
				)
			),
			sliderInput('tetrasVTarget', 'volume targeted', min = 0, max = 100, 
				value = 90, step = 1, round = TRUE, post = "%"),
			div(style = "text-align: center;",
				actionBttn('tetrasCompute', 'Compute')
			)
		),
		box(title = "Compare", width = 12, 
			selectInput('tetrasCompare', 'Profiles to compare', 
				choices = c())
		),
		box(title="Record profile", width = 12, 
			textInput('tetrasName', 'Name of the profile', 
				value ='', placeholder = '15% SCPP 30% MCPP'),
			div(style = "text-align: center;",
				actionBttn('tetrasRecord', 'Record')
			)
		)
	),
	column(width = 9,
		tabBox(width = 12, 
			tabPanel("2D", 
				plotlyOutput('tetras2D') %>% withSpinner()
			),
			tabPanel("3D", 
				plotlyOutput('tetras3D', height= '800px') %>% withSpinner()
			),
			tabPanel("Table")
		)
	)
)