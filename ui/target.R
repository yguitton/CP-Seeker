tabItem(tabName='target',
	box(title='Parameters', width=4,
		column(width=12,
			uiOutput('uiTargetSamples'),
			column(width=6, 
				pickerInput('targetAdducts', 'adduct', choices=
					setNames(c('M+Cl', 'M-H', 'M+Hac-H', 'M-Cl', 'M-H-Cl'), 
						c('[M+Cl]-', '[M-H]-', '[M+Hac-H]-', '[M-Cl]-', '[M-H-Cl]-')),
					multiple=TRUE)
			),
			column(width=6, 
				pickerInput('targetMachine', 'machine', choices=
					names(resolution_list), option=list(`live-search`=TRUE))
			),
			column(width=12, 
				sliderInput('targetRT', 'Range rT', min=0, max=20, value=c(2, 14), step=1)
			),
			column(width=12, 
				sliderInput('targetTolPpm', 'tol ppm', value=5, min=0, max=50, step=1)
			),
			
			column(width=12, 
				sliderInput('targetPeakwidth', 'peakwidth(sec)', value=c(25, 270), 
					min=0, max=300, step=1)
			)
		),
		tags$div(style="text-align:center;", actionBttn('target', 'target'))
	),
	box(title="TIC", width=8,
		uiOutput('uiTargetSampleTic'),
		plotlyOutput('targetTic')
	)
)