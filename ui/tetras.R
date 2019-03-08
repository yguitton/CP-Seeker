tabItem(tabName='tetras', 
	source('ui/loadProfile.R', local=TRUE)$value,
	
	# box(title="3D", width=6,
	box(title="scores", width=6,
		# uiOutput('uiTetrasSample'),
		# plotlyOutput('tetrahedras', height="600px")
		dataTableOutput('profilesScores')
	),

	box(title="map", width=12, 
		plotlyOutput('map', height="800px")
	)
)