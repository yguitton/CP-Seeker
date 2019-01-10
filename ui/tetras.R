tabItem(tabName='tetras', 
	source('ui/loadXlsx.R', local=TRUE)$value,
	
	box(title="3D", width=6, 
		plotlyOutput('tetrahedras', height="600px")
	),

	box(title="map", width=12, 
		plotlyOutput('map', height="800px")
	)
)