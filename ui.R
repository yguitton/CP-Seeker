header <- dashboardHeader(
	title = "targetROI"
)

sidebar <- dashboardSidebar()

body <- dashboardBody(
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
	)
	
)

shinyUI(
	dashboardPage(
		header,
		sidebar,
		body
	)
)
