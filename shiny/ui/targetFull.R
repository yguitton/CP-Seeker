tabItem(tabName = 'TargetFull',
	box(width=3, style='vertical-align:text:bottom;', 
		uiOutput('uiTargetFullSelectFiles'),
		pickerInput('targetFullAdduct', 'Select adduct', choices=c('[M+CH3COO]-', '[M+Cl]-', '[M-H]-', '[M-HCl]-'), multiple=FALSE),
		sliderInput("targetFullPpm", "tolerance m/z (ppm)", min=1, max=25, value=5, round=TRUE),
		sliderInput('targetFullPrefilterS', 'prefilter step', min=1, max=10, value=3),
		column(width=6, numericInput('targetFullPrefilterL', 'prefilter level', value=10000)),
		column(width=6, numericInput('targetFullTolAbd', 'abundance tol (%)', value=20, min=0, max=100, step=1)),
		div(style='text-align:center;', actionBttn("targetFullSubmit", "Submit", style='unite', color='primary'))
	),
	box(width=9,
		plotlyOutput('targetFullGraph', height='800px')
	)
)