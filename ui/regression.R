shinydashboard::tabItem('regression',
  shiny::column(width = 3,
    shinydashboard::box(width = 12, id = 'regression_params',
      shinyWidgets::radioGroupButtons('regression_files_params', '', justified = TRUE, 
        choices = c('Observed', 'Library'),
        checkIcon = list(
          yes = shiny::tags$i(
            class = "fa fa-circle", 
            style = "color: steelblue"
          ), 
          no = shiny::tags$i(
            class = "fa fa-circle-o", 
            style = "color: steelblue"
          )
        )
      ),
      shiny::tags$div(id = 'regression_observed',
        shiny::selectInput('regression_observed_file', 'Sample', choices = c(),
          multiple = FALSE),
        shiny::selectizeInput('regression_observed_family', 'Family', choices = c()),
        shiny::selectizeInput('regression_observed_adduct', 'Adduct', choices = c())
      ),
      shinyjs::hidden(
        shiny::tags$div(id = 'regression_theoretic',
          shiny::selectInput('regression_theoretic_files', 'Samples', choices = c(),
            multiple = TRUE),
          shiny::selectizeInput('regression_theoretic_family', 'Family', choices = c()),
          shiny::selectizeInput('regression_theoretic_adduct', 'Adduct', choices = c())
        )
      ),
      shiny::column(width = 12, style = "margin-bottom: 20px; text-align: center;",
        shinyWidgets::actionBttn('regression_launch', 'Launch Regression', 
          style = 'minimal', color = 'primary')
      )
    ),
    shinydashboard::box(width = 12, id = 'regression_table_results',
      DT::dataTableOutput('regression_table')
    )
  ),
  shinydashboard::box(width = 9, id = 'regression_results',
    shiny::downloadButton('regression_export', 'Export regression matrix'),
    shinyWidgets::radioGroupButtons('regression_matrix_choice', '', justified = TRUE,
      choices = c('Observed (xE^3)', 'Theoretical (xE^3)'),
      checkIcon = list(
        yes = shiny::tags$i(
          class = "fa fa-circle",
          style = "color: steelblue"
        ),
        no = shiny::tags$i(
          class = "fa fa-circle-o",
          style = "color: steelblue"
        )
      )
    ),
    DT::dataTableOutput('regression_matrix')
  )
)