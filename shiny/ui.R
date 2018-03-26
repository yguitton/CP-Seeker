library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(shinyFiles)

appCSS <- "#loader { color: #0000ff; font-size: 90px; text-indent: -9999em; overflow: hidden; width: 1em; height: 1em; border-radius: 50%; margin: 200px auto; position: relative; -webkit-transform: translateZ(0); -ms-transform: translateZ(0); transform: translateZ(0); -webkit-animation: load6 1.7s infinite ease, round 1.7s infinite ease; animation: load6 1.7s infinite ease, round 1.7s infinite ease; } @-webkit-keyframes load6 { 0% { box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em; } 5%, 95% { box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em; } 10%, 59% { box-shadow: 0 -0.83em 0 -0.4em, -0.087em -0.825em 0 -0.42em, -0.173em -0.812em 0 -0.44em, -0.256em -0.789em 0 -0.46em, -0.297em -0.775em 0 -0.477em; } 20% { box-shadow: 0 -0.83em 0 -0.4em, -0.338em -0.758em 0 -0.42em, -0.555em -0.617em 0 -0.44em, -0.671em -0.488em 0 -0.46em, -0.749em -0.34em 0 -0.477em; } 38% { box-shadow: 0 -0.83em 0 -0.4em, -0.377em -0.74em 0 -0.42em, -0.645em -0.522em 0 -0.44em, -0.775em -0.297em 0 -0.46em, -0.82em -0.09em 0 -0.477em; } 100% { box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em; } } @keyframes load6 { 0% { box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em; } 5%, 95% { box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em; } 10%, 59% { box-shadow: 0 -0.83em 0 -0.4em, -0.087em -0.825em 0 -0.42em, -0.173em -0.812em 0 -0.44em, -0.256em -0.789em 0 -0.46em, -0.297em -0.775em 0 -0.477em; } 20% { box-shadow: 0 -0.83em 0 -0.4em, -0.338em -0.758em 0 -0.42em, -0.555em -0.617em 0 -0.44em, -0.671em -0.488em 0 -0.46em, -0.749em -0.34em 0 -0.477em; } 38% { box-shadow: 0 -0.83em 0 -0.4em, -0.377em -0.74em 0 -0.42em, -0.645em -0.522em 0 -0.44em, -0.775em -0.297em 0 -0.46em, -0.82em -0.09em 0 -0.477em; } 100% { box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em; } } @-webkit-keyframes round { 0% { -webkit-transform: rotate(0deg); transform: rotate(0deg);} 100% { -webkit-transform: rotate(360deg); transform: rotate(360deg); } } @keyframes round { 0% { -webkit-transform: rotate(0deg); transform: rotate(0deg); } 100% { -webkit-transform: rotate(360deg); transform: rotate(360deg); } }
			.shiny-output-error { visibility: hidden; }.shiny-output-error:before {  visibility: visible;  title: ''; }}
			#inputs-table{ border-collapse: separate; } #inputs-table td{ align:center; display: inline-block;}
			#targetDownload{ visibility:hidden; }"

header <- dashboardHeader(
	title = "Target ROI"
)

sidebar <- dashboardSidebar(
	sidebarMenu(id='app',
		menuItem('Files', icon=icon('cog'), tabName='Files'),
		menuItem('TargetFull', icon=icon('list-alt'), tabName = 'TargetFull'),
		menuItem('Target', icon=icon('list-alt'), tabName='Target'),
		menuItem('Correlation', icon=icon('bar-chart-o'), tabName='Correlation'),
		uiOutput('uiSelectProject')
	)
)

body <- dashboardBody(
	tags$div(id = "loader"),
	hidden(div(id='app-content',
    tabItems(
		tabItem(tabName = 'Files',
			box(width=6, title='Import files',
				column(width=3, shinyFilesButton('fileImportmzXML', label='Import mzXML(s)', 'select mzXML(s) or mzXML(s) ', multiple=TRUE)),
				column(width=3, shinyFilesButton('fileImportRaw', label='Import raw(s)', 'select raw(s)', multiple=TRUE))
			),
			box(width=6, 'Informations',
				uiOutput('uiFileInfo'),
				DT::dataTableOutput("tableInfoConversion")
			)
		),
		
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
		),
		
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
						tags$td(style='vertical-align:75%;',
							actionButton('targetSubmit', 'Re-integrate')
						),
						tags$td(style='vertical-align: 75%;',
							actionButton('targetDelete', 'Delete')
						),
						tags$td(style='width:0%',
							downloadButton('targetDownload', 'Download')
						)
					)
				)
			),
			box(width=7,
				tabsetPanel(
					tabPanel('ppm Deviation', 
						DT::dataTableOutput('targetTablePpmDeviation')
					),
					tabPanel('Score',
						DT::dataTableOutput('targetTableScore')
					),
					tabPanel('AUC',	
						column(width=4, sliderInput('targetC', 'C', min=8, max=36, value=c(8, 36), step=1)),
						column(width=4, sliderInput('targetCl', 'Cl', 4, 30, value=c(4, 30), step=1)),
						column(width=3, verbatimTextOutput('targetSumAUC')),
						DT::dataTableOutput('targetTableInto')
					)
				)
			),
			box(width=5, 
				plotlyOutput('targetEIC')
			)
		),
		
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
	)
)))
	

shinyUI(fillPage(
	useShinyjs(),
	inlineCSS(appCSS),
	dashboardPage(
	header,
	sidebar,
	body
	)
))
