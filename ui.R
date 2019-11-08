header <- tags$header(class = "main-header", tags$span(class = "logo", ""), 
	useShinyjs(),
	useShinyFeedback(),
	useToastr(),
	useSweetAlert(),
	includeCSS("www/shinyCSS.css"),
	includeScript("www/shinyJS.js"),
	
	tags$nav(class="navbar navbar-static-top", role="navigation", 
		tags$form(class = "form-inline", 
			tags$div(class = "form-group", style = "float: left", 
				tags$span(id = "titleApp", class = "logo", appname)
			), 
			tags$div(class = "form-group", style = "float: right;",
				tags$a(id = 'download', class = "bttn bttn-default bttn-lg bttn-bordered bttn-no-outline shiny-download-link", 
					href = "", target = "_blank", download = NA, icon("download"))
			), 
			tags$div(class = "form-group", style = "float: right; padding-top: 0.55%",
				uiOutput('uiProject')
			)
		)
	)
)

sidebar <- dashboardSidebar(collapsed = TRUE, disable = TRUE, 
	sidebarMenu(id='tabs', 
		menuItem('Projects & Files', icon=icon('cog'), tabName='projectFiles'),
		menuItem('Target chloropara', icon=icon('tasks'), tabName='target'),
		menuItem('Table', icon=icon('table'), tabName='table'),
		menuItem('Tetrahedras', icon=icon('bar-chart-o'), tabName='tetras')
	)
)

body <- dashboardBody(fluidPage(
	tabItems(
		
		source('ui/projectFiles.R', local=TRUE)$value,
		
		source('ui/target.R', local=TRUE)$value,
		
		source('ui/tetras.R', local=TRUE)$value,
		
		source('ui/details.R', local=TRUE)$value
	)
))

shinyUI(
	dashboardPage(
		header,
		sidebar,
		body
	)
)
