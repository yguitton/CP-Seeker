shinydashboard::tabItem(
  tabName = 'quantification',
  shinydashboard::box(
    width = 12,
    shinyWidgets::radioGroupButtons(
      inputId = 'quantification_choice',
      label = '',
      choices = c('Sample List', 'Homologue Domain', 'Internal Standard', 'Filters'),
      justified = TRUE,
      checkIcon = list(
        yes = shiny::tags$i(class = "fa fa-circle", style = "color: steelblue"),
        no = shiny::tags$i(class = "fa fa-circle-o", style = "color: steelblue")
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Sample List'",
    div(
      h3("Sample List"),
      DT::dataTableOutput('quanti_table')
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Homologue Domain'",
    div(
      h3("Homologue Domain")
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Internal Standard'",
    div(
      h3("Internal Standard")
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Filters'",
    div(
      h3("Filters Form")
    ),
    shiny::fluidRow(
      shiny::column(width = 6, 
        shiny::numericInput('filter_intensity', 'Minimum Normalized Intensity (xE6)', value = 0, min = 0, max = 100, step = 1)
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 6, 
        shiny::numericInput('filter_score', 'Minimum Pattern Score (%)', value = 0, min = 0, max = 100, step = 1)
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 6, 
        shiny::numericInput('filter_deviation', 'Minimum Deviation (mDa)', value = 0, step = 0.01)
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 6,
        shiny::actionButton("apply_filter", "Apply Filter")
      )
    )
  )
)
