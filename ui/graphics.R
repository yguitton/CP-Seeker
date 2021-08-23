shinydashboard::tabItem(tabName='graphics',
  shinydashboard::box(width = 3, id = "graphics_params", 
    shiny::selectInput("graphics_file", "Select sample" , choices = c(), 
      multiple = FALSE),
    shiny::selectizeInput("graphics_chemical", "Family", choices = c()),
    shiny::selectizeInput("graphics_adduct", "Adduct", choices = c()),
    shiny::checkboxInput("graphics_histogram", "3D histogram"),
    shiny::checkboxInput("graphics_pics", "Pics"),
    shiny::checkboxInput("graphics_bubble", "Bubble chart"),
    shinyjs::hidden(
      shiny::tags$div(id = 'graphics_bubble_params', style = "margin-left: 20px",
        bsplus::shinyInput_label_embed(
          shiny::radioButtons('graphics_bubble_ref', 'Reference',
            choices = c('Total intensity', 'Maximum intensity'), inline = TRUE),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = 'top', 
            title = "Choose the reference for the bubble size"
          )
        )
      )
    ),
    shiny::checkboxInput("graphics_contours", "Contour plot"),
    shinyjs::hidden(
      shiny::tags$div(id = 'graphics_contours_params', style = "margin-left: 20px",
        bsplus::shinyInput_label_embed(
          shiny::radioButtons('graphics_contours_ref', 'Reference',
            choices = c('Total intensity', 'Maximum intensity', 'Intensity'), inline = TRUE),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = 'top',
            title = 'Reference for the cut'
          )
        ),
        bsplus::shinyInput_label_embed(
          shiny::numericInput('graphics_contours_value', 'Value', value = 0),
          bsplus::bs_embed_tooltip(
            bsplus::shiny_iconlink(),
            placement = 'top', 
            title = "Value where to cut, in % for Total and Maximum intensity, in xE-6 for Intensity"
          )
        )
      )
    ),
    shiny::column(width = 12, style = "margin-bottom: 20px; text-align: center;", 
      shinyWidgets::actionBttn('graphics_draw', 'Draw graphic(s)', 
        style = 'minimal', color = 'primary')
    )
  ),
  shinydashboard::box(width = 9, 
    shiny::column(width = 6, style = "border-right : 1px solid #ddd",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput('graphics_histogram_plot')
      ),
      tags$hr(),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput('graphics_bubble_plot')
      )
    ),
    shiny::column(width = 6,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput('graphics_pics_plot')
      ),
      tags$hr(),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput('graphics_contours_plot')
      )
    )
  )
)