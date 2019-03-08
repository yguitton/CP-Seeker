tabItem(tabName='tetras', 
	source('ui/loadProfile.R', local=TRUE)$value,
	
	tabBox(width=6,
		tabPanel(title="3D", 
			uiOutput('uiTetrasSample'),
			plotlyOutput('tetrahedras', height="600px")
		),
		tabPanel(title="scores", 
			dataTableOutput('profilesScores')
		)
	),

	box(title="map", width=12, 
		plotlyOutput('map', height="800px")
	)
)