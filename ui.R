header <- dashboardHeader(
	title = appname
)

sidebar <- dashboardSidebar(
	sidebarMenu(id='tabs', 
		menuItem('Projects & Files', icon=icon('cog'), tabName='projectFiles'),
		menuItem('Target chloropara', icon=icon('tasks'), tabName='target'),
		menuItem('Details', icon=icon('table'), tabName='details'),
		menuItem('Tetrahedras', icon=icon('bar-chart-o'), tabName='tetras'),
		uiOutput('uiProject'),
		tags$div(style="padding-left: 30%; padding-top:10%;", 
			downloadButton('downloadProject', '')
		)
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
		
		source('ui/target.R', local=TRUE)$value,
		
		source('ui/tetras.R', local=TRUE)$value,
		
		source('ui/details.R', local=TRUE)$value
		
		
	
)))

shinyUI(
	dashboardPage(
		header,
		sidebar,
		body
	)
)
