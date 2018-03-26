tabItem(tabName = 'TargetFull',
	box(width=3,
		uiOutput('uiTargetFullSelectFiles'),
		sliderInput("targetFullPpm", "tolerance m/z (ppm)", min=0, max=25, value=5, round=TRUE),
		sliderInput('targetFullRt', 'rt range (min)', min=1, max=30, value=c(2, 13), round=TRUE),	
		numericInput('targetFullThreshold', 'Enter a threshold', value=0, min=0),
		numericInput('targetFullTolAbd', 'Abundance tolerance in %', value=20, min=0, max=100, step=1),
		selectInput('targetFullAdduct', 'Select adduct', choices=c('[M+CH3COO]-', '[M+Cl]-', '[M-H]-', '[M-HCl]-'), multiple=FALSE),
		actionButton("targetFullSubmit", "Submit")
	),
	box(width=9,
		plotlyOutput('targetFullGraph', height='800px')
	)
)