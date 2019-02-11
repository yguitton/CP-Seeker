tabItem(tabName='target',
	box(title='Parameters', width=4,
		column(width=12,
			uiOutput('uiTargetSamples'),
			column(width=6, 
				pickerInput('targetAdduct', 'adduct', choices=adducts$Name, options=list(`live-search` = TRUE))
			),
			column(width=6, 
				numericInput('targetTolPpm', 'tol ppm', value=5, min=0, step=1)
			),
			column(width=6, 
				sliderInput('targetPeakwidth', 'min peakwidth(sec)', value=15, min=0, max=60, step=1)
			),
			column(width=6,
				numericInput('targetSnthresh', 'snthresh', value=1, min=0, step=1)
			)
		),
		tags$div(style="text-align:center;", actionBttn('target', 'target'))
	),
	box(title="TIC", width=8,
		uiOutput('uiTargetSampleTic'),
		plotlyOutput('targetTic')
	)
)