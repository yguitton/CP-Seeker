#' @title Event when select bubble graphic
#' 
#' @description
#' The selection of a graph will show the associate parameters
#' 
#' @param input$graphics_bubble boolean, choice 
shiny::observeEvent(input$graphics_bubble, {
  params <- list(active = input$graphics_bubble)
  if(params$active){
    shinyjs::show("graphics_bubble_params")
  }
  else if(!params$active){
    shinyjs::hide("graphics_bubble_params")
  }
})

#' @title Event when select contours graphic
#' 
#' @description
#' The selection of a graph will show the associate parameters
#' 
#' @param input$graphics_contours boolean, choice 
shiny::observeEvent(input$graphics_contours, {
  params <- list(active = input$graphics_contours)
  if(params$active) {
    shinyjs::show("graphics_contours_params")
  }
  else if(!params$active) {
    shinyjs::hide("graphics_contours_params")
  }
})

#' @title Plot selected graphics
#' 
#' @description 
#' Will plot selected graphics when show graphics button is clicked
shiny::observeEvent(input$graphics_draw, {
  actualize$graphics_bubble <<- runif(1)
  actualize$graphics_pic <<- runif(1)
  actualize$graphics_contours <<- runif(1)
  actualize$graphics_histogram <<- runif(1)
})

#' @title Plot bubble plot
#' 
#' @description 
#' Will plot bubble plot according to parameters chosen
#' 
#' @param db sqlite connection
#' @param input$graphics_bubble boolean choice
#' @param input$graphics_file integer project_sample ID
#' @param input$graphics_chemical string family studied
#' @param input$graphics_adduct string adduct name
#' @param input$graphics_bubble_ref string reference to use for the plot
output$graphics_bubble_plot <- plotly::renderPlotly({
  actualize$graphics_bubble
  params <- list(
    active = isolate(input$graphics_bubble),
    project_sample = isolate(input$graphics_file),
    chemical = isolate(input$graphics_chemical),
    adduct = isolate(input$graphics_adduct),
    reference = input$graphics_bubble_ref
  )
  if(!params$active) return(plot_empty_plot('Bubble plot'))
  if(length(params$project_sample) == 0) return(plot_empty_plot('Bubble plot'))
  plot_bubble_plot(db, params$project_sample, params$adduct, params$chemical, 
    params$reference)
})

#' @title Plot pic graphic
#' 
#' @description 
#' Will plot a pic graphic, according to parameters chosen
#' 
#' @param db sqlite connection
#' @param input$graphics_pics boolean choice
#' @param input$graphics_file integer project_sample ID
#' @param input$graphics_chemical string family studied
#' @param input$graphics_adduct string adduct name
output$graphics_pics_plot <- plotly::renderPlotly({
  actualize$graphics_pic
  params <- list(
    active = isolate(input$graphics_pics),
    project_sample = isolate(input$graphics_file),
    chemical = isolate(input$graphics_chemical),
    adduct = isolate(input$graphics_adduct)
  )
  if(!params$active) return(plot_empty_3d_plot('Pic plot'))
  plot_pic_plot(db, params$project_sample, params$adduct, params$chemical)
})

#' @title Plot contours graphic
#' 
#' @description 
#' Will plot contours graphic according to parameters chosen
#' 
#' @param db sqlite connection
#' @param input$graphics_contours boolean choice
#' @param input$graphics_file integer project_sample ID
#' @param input$graphics_chemical string family studied
#' @param input$graphics_adduct string adduct name
#' @param input$graphics_contours_ref string reference to use for the plot
#' @param input$graphics_contours_value integer value for the minimal value
output$graphics_contours_plot <- renderPlotly({
  actualize$graphics_contours
  params <- list(
    active = isolate(input$graphics_contours),
    project_sample = isolate(input$graphics_file),
    chemical = isolate(input$graphics_chemical),
    adduct = isolate(input$graphics_adduct),
    reference = input$graphics_contours_ref,
    value = input$graphics_contours_value
  )
  if(!params$active) return(plot_empty_plot('Contour plot'))
  if(length(params$project_sample) == 0) return(plot_empty_plot('Contour plot'))
  plot_contour_plot(db, params$project_sample, params$adduct, 
    params$chemical, params$reference, params$value)
})

#' @title Plot histogram graphic
#' 
#' @description 
#' Will plot histogram graphic according to parameters chosen
#' 
#' @param db sqlite connection
#' @param input$graphics_histogram boolean choice
#' @param input$graphics_file integer project_sample ID
#' @param input$graphics_chemical string family studied
#' @param input$graphics_adduct string adduct name
output$graphics_histogram_plot <- renderPlotly({
  actualize$graphics_histogram
  params <- list(
    active = isolate(input$graphics_histogram),
    project_sample = isolate(input$graphics_file),
    chemical = isolate(input$graphics_chemical),
    adduct = isolate(input$graphics_adduct)
  )
  if(!params$active) return(plot_empty_3d_plot('3D histogram'))
  plot_3d_histogram(db, params$project_sample, params$adduct, params$chemical)
})