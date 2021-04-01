header <- shiny::tags$header(
	class = "main-header", 
	shiny::tags$span(class = "logo", ""), 
	shinyjs::useShinyjs(), 
	shinyjs::extendShinyjs(
		text = "shinyjs.collapse = function(boxId) {
			$('#' + boxId).closest('.box').find('[data-widget=collapse]').click();
		}", functions = "collapse"),
	shinyFeedback::useShinyFeedback(),
	shinyWidgets::useSweetAlert(),
	bsplus::use_bs_tooltip(),
	# bsplus::use_bs_popover(),
	shiny::includeCSS("www/CPSeeker.css"),
	shiny::includeScript("www/CPSeeker.js"),
	
	shiny::tags$nav(class = "navbar navbar-static-top", role = "navigation", 
		shiny::tags$form(class = "form-inline", 
			shiny::tags$div(class = "form-group", style = "float:left", 
				shiny::tags$span(id = "titleApp", class = "logo", appname)
			), 
			shiny::tags$div(class = "form-group", 
				style = "float: right; padding-top: 0.55%; padding-right: 10px",
				shiny::tags$table(
					shiny::tags$tr(
						shiny::tags$td(style = "color: white; padding-right: 3px;", 
							tags$b('Active project ')
						), 
						tags$td(
							shinyWidgets::pickerInput('project', 
								label = NULL, choices = c(), width = "100%")
						)
					)
				)
			), 
			shiny::tags$div(class = "form-group", 
				style = "float: right; padding-top: 0.55%; padding-right: 10px",
				shiny::tags$table(
					shiny::tags$tr(
						shiny::tags$td(style = "color: white; padding-right: 3px;", 
							tags$b('Active user ')
						), 
						tags$td(
							shinyWidgets::pickerInput('user', 
								label = NULL, choices = c(), width = "100%")
						)
					)
				)
			)
		)
	)
)

sidebar <- shinydashboard::dashboardSidebar(collapsed = TRUE, disable = TRUE, 
	shinydashboard::sidebarMenu(id = 'tabs', 
		shinydashboard::menuItem('Projects & Files', icon = shiny::icon('database'), 
			shinydashboard::menuSubItem("New project", icon = shiny::icon('plus'), tabName = 'project_files'),
			shinydashboard::menuSubItem("Database tables", icon = shiny::icon('database'), tabName = 'manage')
		),
		# shinydashboard::menuItem("Deconvolution", icon = shiny::icon("cog"), 
			# shinydashboard::menuSubItem("Process", icon = shiny::icon("cog"), tabName = "process")
		# ),
		shinydashboard::menuItem('Explore data', icon = shiny::icon('bar-chart-o'), 
			shinydashboard::menuSubItem("TIC/EIC & MS", icon = shiny::icon("bar-chart-o"), tabName = "EIC")
		)		
	)
)

body <- shinydashboard::dashboardBody(
	shiny::fluidPage(
		tags$div(id = "loader", class = "lds-dual-ring"), 
		shinyjs::hidden(
			shiny::div(id = 'app-content',
				shinydashboard::tabItems(
					source(file.path('ui', 'project_files.R'), local = TRUE)$value,
					source(file.path('ui', 'manage.R'), local = TRUE)$value,
					# source(file.path('ui', 'process.R'), local = TRUE)$value,
					source(file.path("ui", "EIC.R"), local = TRUE)$value
				)
			)
		)
	)
)

shiny::shinyUI(
	shinydashboard::dashboardPage(
		title = appname, 
		header,
		sidebar,
		body
	)
)