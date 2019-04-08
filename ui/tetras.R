tabItem(tabName='tetras', 
	source('ui/loadProfile.R', local=TRUE)$value,
	
	box(width=6,	
		uiOutput('uiTetrasSample'),
		plotlyOutput('tetrahedras', height="600px")
	),

	box(title="map", width=12, 
		column(width=2, uiOutput('uiZoneSamples')),
		column(width=1, style="padding-top:1.7%;", actionBttn('zoneDraw', 'Draw')),
		column(width=12, plotlyOutput('map', height="800px") %>% withSpinner())
	)
)