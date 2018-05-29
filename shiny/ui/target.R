tabItem(tabName='Target',
	box(width=12,
		tags$table(id='inputs-table', 
			tags$tr(style='width:100%; vertical-align:bottom',
				tags$td(style="width:15%;",
					uiOutput('uiTargetFile')
				),
				tags$td(style="width:10%",
					uiOutput('uiTargetAdduct')
				),
				tags$td(style="width:7.5%;",
					numericInput('targetPpm', 'tol mz (ppm)', value=5, min=0, max=50)
				),
				tags$td(style="width:7.5%;",
					numericInput('targetRtMin', 'rt min (min)', value=0, min=0, max=25)
				),
				tags$td(style="width:7.5%;",
					numericInput('targetRtMax', 'rt max (min)', value=0, min=0, max=25)
				),
				tags$td(style='width:7.5%;',
					numericInput('targetPrefilterS', 'Prefilter step', value=3)
				),
				tags$td(style='width:7.5%;', 
					numericInput('targetPrefilterL', 'Prefilter level', value=100)
				),
				tags$td(style="width:7.5%;",
					numericInput('targetTolAbd', 'tol abd (%)', value=20, min=0, max=100)
				),
				tags$td(style="vertical-align:100%; width:10%; text-align:center;",
					actionBttn('targetSubmit', 'Re-integrate', style='unite')
				),
				tags$td(style="vertical-align:100%; width:6%; text-align:center;",
					actionBttn('targetDelete', 'Delete', style='unite', color='warning')
				),
				tags$td(style="vertical-align:100%; width:4%; text-align:center;", 
					actionBttn('targetDownload', '', style='unite', icon=icon('download'))
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
				fluidRow(
					column(width=4, sliderInput('targetC', 'C', min=8, max=36, value=c(8, 36), step=1)),
					column(width=4, sliderInput('targetCl', 'Cl', 4, 30, value=c(4, 30), step=1)),
					column(width=3, style='vertical-align:text-bottom;', br(), verbatimTextOutput('targetSumAUC'))
				),
				dataTableOutput('targetTableInto')
			)
		)
	),
	box(width=5, 
		plotlyOutput('targetEIC')
	)
)