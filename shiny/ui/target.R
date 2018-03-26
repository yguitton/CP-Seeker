tabItem(tabName='Target',
	box(width=12,
		tags$table(id='inputs-table', style='width:100%',
			tags$tr(
				tags$td(style="vertical-align: bottom;",
					uiOutput('uiTargetFile')
				),
				tags$td(style="vertical-align: bottom;",
					uiOutput('uiTargetAdduct')
				),
				tags$td(style="vertical-align: bottom;",
					numericInput('targetPpm', 'tol mz (ppm)', value=5, min=0, max=50)
				),
				tags$td(style="vertical-align: bottom;",
					numericInput('targetRtMin', 'rt min (min)', value=0, min=0, max=25)
				),
				tags$td(style="vertical-align: bottom;",
					numericInput('targetRtMax', 'rt max (min)', value=0, min=0, max=25)
				),
				tags$td(style="vertical-align: bottom;",
					numericInput('targetThreshold', 'threshold', value=10000, min=0, max=1000000)
				),
				tags$td(style="vertical-align: bottom;",
					numericInput('targetTolAbd', 'tol abd (%)', value=20, min=0, max=100)
				),
				tags$td(
					actionButton('targetSubmit', 'Re-integrate')
				),
				tags$td(
					actionButton('targetDelete', 'Delete')
				)
			)
		)
	),
	box(width=7,
		tabsetPanel(
			tabPanel('ppm Deviation', 
				dataTableOutput('targetTablePpmDeviation')
			),
			tabPanel('Score',
				dataTableOutput('targetTableScore')
			),
			tabPanel('AUC',	
				column(width=4, sliderInput('targetC', 'C', min=8, max=36, value=c(8, 36), step=1)),
				column(width=4, sliderInput('targetCl', 'Cl', 4, 30, value=c(4, 30), step=1)),
				column(width=3, verbatimTextOutput('targetSumAUC')),
				dataTableOutput('targetTableInto')
			)
		)
	),
	box(width=5, 
		plotlyOutput('targetEIC')
	),
	tags$div(style="visibility:hidden;", downloadButton('targetDownload', 'Download'))
)