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
    shinydashboard::box(
      width = 12,
      div(
        h3("Sample List"),
        DT::dataTableOutput('quanti_table')
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Homologue Domain'",
    shinydashboard::box(
      width = 12,
      shiny::column(width = 12,
        div(
          h3("Homologue Domain"),
          shiny::column(width = 3, 
            shiny::selectInput('quanti_subclass_dropdown', 'Subclass',
              choices = c("SCCP", "MCCP", "LCCP")
            )
          ),
          shiny::column(width = 9,
            DT::dataTableOutput('quanti_matrix_homologue')
          )
        )
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Internal Standard'",
    shinydashboard::box(
      width = 12,
      div(
        h3("Internal Standard"),
        DT::dataTableOutput('quanti_matrix_IS')
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Filters'",
    shinydashboard::box(
      width = 12,
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
)