header <- dashboardHeader(
	title = "targetROI", 
	tags$li(class="dropdown", dropdownButton(circle=TRUE, status="primary", icon=icon('gear'), right=TRUE, tooltip=tooltipOptions(title='Hidden parameters'),
		knobInput('vTarget', 'target volume %', value=90),
		tags$div(style="text-align:center;", numericInput('vDigits', 'precision', value=2, width='50%'))
	))
)

sidebar <- dashboardSidebar(
	sidebarMenu(id='tabs', 
		menuItem('Projects & Files', icon=icon('cog'), tabName='projectFiles'),
		menuItem('Tetrahedras', icon=icon('bar-chart-o'), tabName='tetras')
	)
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
	
	tabItems(
		
		source('ui/projectFiles.R', local=TRUE)$value,
		
		source('ui/tetras.R', local=TRUE)$value
		
		
	
)))

shinyUI(
	dashboardPage(
		header,
		sidebar,
		body
	)
)
