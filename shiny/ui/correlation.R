tabItem('Correlation', 
	box(width=3,
		uiOutput('uiCorrelationStandardFile'),
		uiOutput('uiCorrelationFile')
	),
	box(width=9,
		plotlyOutput('correlationGraph'),
		textInput('correlationText', '', '')
		# plotlyOutput('correlationResidues')
	)
)