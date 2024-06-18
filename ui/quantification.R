shinydashboard::tabItem(
  tabName = 'quantification',
  shinydashboard::box(
    width = 12,
    shinyWidgets::radioGroupButtons(
      inputId = 'quantification_choice',
      label = '',
      choices = c('Sample Type', 'Subclass List', 'Calibration', 'Homologue Domain', 'Internal Standard', 'Filters'),
      justified = TRUE,
      checkIcon = list(
        yes = shiny::tags$i(class = "fa fa-circle", style = "color: steelblue"),
        no = shiny::tags$i(class = "fa fa-circle-o", style = "color: steelblue")
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Sample Type'",
    shinydashboard::box(
      width = 12,
      title = "Select sample type for each sample : CAL, SAMPLE, BLANK",
      div(
        DT::dataTableOutput('quanti_table_type')
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Subclass List'",
    shinydashboard::box(
      width = 12,
      title = "SCCP, MCCP and LCCP subclass but you can add more",
      div(
        DT::dataTableOutput('quanti_table_subclass')
      ),
      br(),
      shiny::textInput("new_subclass", "Add a new subclass"),
      shiny::actionButton("add_subclass", "Add"),
      br(),
      br(),
      shiny::selectInput("delete_subclass", "Select the subclass you want to delete", choices = NULL),
      shiny::actionButton("remove_subclass", "Remove")
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Calibration'",
    shinydashboard::box(
      width = 12,
      title = "For each CAL enter concentration and chlorination degree",
      div(
        DT::dataTableOutput('cal_samples_table')
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Homologue Domain'",
    shinydashboard::box(
      width = 12,
      title = "Click on the matrix to select homologues domains according to carbon chain length type.",
      shiny::column(width = 12,
        div(
          shiny::column(width = 3, 
            shiny::uiOutput("quanti_subclass_dropdown")
            )
          ),
          shiny::column(width = 9,
            DT::dataTableOutput('quanti_matrix_homologue')
          )
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Internal Standard'",
    shinydashboard::box(
      width = 12,
      title = "For each homologue family selected, choose an internal standard.",
      shiny::uiOutput("quanti_dynamic_IS")
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Filters'",
    shinydashboard::box(
      width = 12,
      title = "Form to start quantification calculation.",
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 8, 
          shiny::numericInput('filter_intensity', 'Minimum Normalized Intensity (xE6)', value = 0, min = 0, max = 100, step = 1)
        )
      ),
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 8, 
          shiny::numericInput('filter_score', 'Minimum Pattern Score (%)', value = 0, min = 0, max = 100, step = 1)
        )
      ),
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 8, 
          shiny::numericInput('filter_deviation', 'Minimum Deviation (mDa)', value = 0, step = 0.01)
        )
      ),
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 8, 
          style = "margin-bottom: 20px; text-align: center;",
          shinyWidgets::actionBttn('quanti_launch', 'Launch Quantification process',
            style = 'minimal', color = 'primary')
        )
      )
    ),
    shinydashboard::box(
      width = 12, 
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shinycssloaders::withSpinner(DT::dataTableOutput('quanti_results_profile'))
        )
      )
    )
  )
)