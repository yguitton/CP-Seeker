shinydashboard::tabItem(
  tabName = 'quantification',
  shinydashboard::box(
    width = 12,
    shinyWidgets::radioGroupButtons(
      inputId = 'quantification_choice',
      label = '',
      choices = c('Sample Type', 'Subclass List', 'Calibration', 'Homologue Domain', 'Internal Standard', 'Filters', 'Results'),
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
    )
  ),
  shiny::conditionalPanel(
    condition = "input.quantification_choice == 'Results'",
    shinydashboard::box(
      width = 12, 
      title = "Matrices des echantillons de calibration pour chaque type et chaque adduit selectionne",
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::uiOutput("quanti_profile")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Valeurs des positions des groupes d'homologues selectionnes par sousclasse",
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::uiOutput("selected_matrices")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Recherche de toutes les valeurs dans les matrices correspondantes aux groupes d'homologue choisi par sous classe",
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::uiOutput("selected_values_ui")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Somme des valeurs d'homologues",
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::uiOutput("sum_values_ui")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Somme des valeurs d'homologues",
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::uiOutput("sum_values_graph_ui")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Valeur input de l'adduit et du standard en fonction de la sous classe ",
      shiny::fluidRow(
        shiny::column(width = 12,
          shiny::uiOutput("input_IS")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Tableau des valeurs d'aires, scores, deviations pour les standards",
      shiny::fluidRow(
        shiny::column(width = 12, 
          DT::dataTableOutput('standard_table')
        )
      )
    ), 
    shinydashboard::box(
      width = 12,
      title = "Recuperation des aires totales (E10^6) pour le calcul",
      shiny::fluidRow(
        shiny::column(width = 12, 
          verbatimTextOutput("total_area_values")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Normalisation des aires de calibration par celles des standard",
      shiny::fluidRow(
        shiny::column(width = 12, 
          verbatimTextOutput("normalisation_area")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Representation graphique des aires normalisees",
      shiny::fluidRow(
        shiny::column(width = 12, 
          plotlyOutput("graph_normalisation")
        )
      )
    ),
    shinydashboard::box(
      width = 12,
      title = "Representation graphique des aires normalisees exponentiel",
      shiny::fluidRow(
        shiny::column(width = 12, 
          plotlyOutput("graph_exponential")
        )
      )
    )
  )
)