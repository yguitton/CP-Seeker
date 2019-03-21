tabItem(tabName='tetras', 
	source('ui/loadProfile.R', local=TRUE)$value,
	
	box(width=6,	
		uiOutput('uiTetrasSample'),
		plotlyOutput('tetrahedras', height="600px")
	),

	box(title="map", width=12, 
		uiOutput('uiZoneSamples'),
		plotlyOutput('map', height="800px")
	)
)