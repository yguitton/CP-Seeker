header <- dashboardHeader(
	title = "targetROI", 
	tags$li(class="dropdown", dropdownButton(circle=TRUE, status="primary", icon=icon('gear'), right=TRUE, tooltip=tooltipOptions(title='Hidden parameters'),
		knobInput('vTarget', 'target volume %', value=90),
		tags$div(style="text-align:center;", numericInput('vDigits', 'precision', value=2, width='50%'))
	))
)

sidebar <- dashboardSidebar(disable=TRUE
)

body <- dashboardBody(fluidPage(
	tags$head(
		includeCSS("www/shinyCSS.css"),
		# tags$link(rel = "stylesheet", type = "text/css", href = "shinyWidgets/sweetAlert/css/sweetalert.min.css"),
		includeScript("www/shinyJS.js"),
		# tags$script(src = "shinyWidgets/sweetAlert/js/sweetalert.min.js"),
		useShinyjs(),
		introjsUI(),
		useShinyFeedback(),
		useToastr(),
		useSweetAlert()
	),
	
	source('ui/loadXlsx.R', local=TRUE)$value,
	
	box(title="3D", width=6, 
		plotlyOutput('tetrahedras', height="600px")
	),

	box(title="map", width=12, 
		plotlyOutput('map', height="800px")
	)
	
))

shinyUI(
	dashboardPage(
		header,
		sidebar,
		body
	)
)
