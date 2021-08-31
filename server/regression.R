#' @title Event when switch between observed and library
#' 
#' @description 
#' Will display parameters for observed sample or theoretical samples 
shiny::observeEvent(input$regression_files_params, {
  params <- list(
    choice = input$regression_files_params
  )
  if(params$choice == 'Observed'){
    shinyjs::show('regression_observed')
    shinyjs::hide('regression_theoretic')
  }
  else if(params$choice == 'Library'){
    shinyjs::hide('regression_observed')
    shinyjs::show('regression_theoretic')
  }
})

#' @title Launch regression process
#' 
#' @description 
#' Launch regression process
#' 
#' @param db sqlite connection
#' @param input$project integer, project ID
#' @param input$regression_observed_file integer, observed file ID
#' @param input$regression_observed_family string, family chosen for observed sample
#' @param input$regression_observed_adduct string, adduct chosen for observed sample
#' @param input$regression_theoretic_files vector, theoretic files ID
#' @param input$regression_theoretic_family vector, family chosen for theoretic files
#' @param input$regression_theoretic_adduct vector, adduct chosen for theoretic files
shiny::observeEvent(input$regression_launch, {
  params <- list(
    project = input$project
  )
  params_observed <- list(
    file = input$regression_observed_file,
    family = input$regression_observed_family,
    adduct = input$regression_observed_adduct
  )
  params_theoretic <- list(
    files = input$regression_theoretic_files,
    family = input$regression_theoretic_family,
    adduct = input$regression_theoretic_adduct
  )
  tryCatch({
    if (is.null(params$project)) custom_stop("minor_error", 
      "A project with files is needed for processing")
    else if (params$project == "") custom_stop("minor_error", 
      "A project with files is needed for processing")
    params$samples <- project_samples()[which(
      project_samples()$project == params$project), "sample"]
    if (length(params$samples) == 0) custom_stop("minor_error", 
      "you need to import files in project to process them")
    
    inputs <- c("regression_observed_file", "regression_observed_family",
      "regression_observed_adduct")
    conditions <- c(length(params_observed$file) > 0, length(params_observed$family) > 0,
      length(params_observed$adduct) > 0)
    messages <- c("An observed file must be chosen", "A family must be chosen", 
      "An adduct must be chosen")
    check_inputs(inputs, conditions, messages)
    
    inputs <- c("regression_theoretic_files", "regression_theoretic_family", 
      "regression_theoretic_adduct")
    conditions <- c(length(params_theoretic$files) > 1, length(params_theoretic$family) > 0,
      length(params_theoretic$adduct > 0))
    messages <- c("At least two theoretical files must be chosen", "A family must be chosen",
      "An adduct must be chosen")
    check_inputs(inputs, conditions, messages)
    
    for(i in 1:length(params_theoretic$files)){
      test <- get_features(db, params_theoretic$files[i], params_theoretic$adduct, 
        params_theoretic$family)
      inputs <- c("regression_theoretic_files")
      conditions <- c(nrow(test) > 0)
      messages <- c("Files must be integrate before be use for regression")
      check_inputs(inputs, conditions, messages)
    }
  
  mat_observed <- get_profile_matrix(db, params_observed$file, params_observed$adduct, 
    params_observed$family, simplify = FALSE, table = TRUE)
  observed <- mat_observed$intensities 
  observed[which(observed == "NA" | is.na(observed))] <- 0
  observed_norm <- observed/sum(observed)
  
  theoretic <- NULL
  theoretic_norm <- NULL
  for(i in 1:length(params_theoretic$files)){
    mat_theoretic <- get_profile_matrix(db, params_theoretic$file[i], params_theoretic$adduct, 
      params_theoretic$family, simplify = FALSE, table = TRUE)
    th2 <- mat_theoretic$intensities
    th2[which(th2 == 'NA' | is.na(th2))] <- 0
    theoretic <- cbind(theoretic, th2)
    th2 <- th2/sum(th2)
    theoretic_norm <- cbind(theoretic_norm, th2)
  }
  
  # obtention of residuals & vector of factor for each theoretic file 
  solution <- pracma::lsqnonneg(theoretic_norm, observed_norm)
  factor_vector <- round(solution$x, digits = 2)
  res <- solution$resid.norm
  
  # reconstruction of pattern & theoretic matrix
  theoretic_pattern <- t(factor_vector * t(theoretic))
  theoretic_mat <- matrix(0, nrow = nrow(theoretic_pattern))
  for(i in 1:nrow(theoretic_pattern)){
    theoretic_mat[i,] <- sum(theoretic_pattern[i,])
  }
  C <- c(min(mat_observed$C), max(mat_observed$C))
  Cl <- c(min(mat_observed$Cl), max(mat_observed$Cl))
  finale_matrix <- matrix(NA, nrow = C[2] - C[1] + 1, ncol = Cl[2] - Cl[1] + 1, 
    dimnames = list(paste0("C", C[1]:C[2]), paste0("Cl", Cl[1]:Cl[2])))
  for(row in seq(nrow(theoretic_mat))) {
    data <- if(theoretic_mat[row] != 0) paste(round(mat_observed[row, "intensities"]/10**6, digits = 2),
      round(theoretic_mat[row]/10**6, digits = 2), sep = '/')
    else paste("NA", "NA", sep = '/')
    finale_matrix[mat_observed[row, "C"] - C[1] + 1, 
      mat_observed[row, "Cl"] - Cl[1] + 1] <- data 
  }
  
  files <- project_samples()[which(project_samples()$project_sample %in% params_theoretic$files), 
    c("sample_id", "project_sample")]
  params_theoretic$files_names <- sapply(1:length(params_theoretic$files), function(x) 
    files[which(files$project_sample == params_theoretic$files[x]), "sample_id"])
  
  # construction of table of results
  result_table <- cbind(params_theoretic$files_names, factor_vector)
  result_table <- rbind(result_table, c("Determination coeff.", 
    round(sum(factor_vector), digits = 2)))
  result_table <- rbind(result_table, c("Residuals", 
    scales::scientific(res, digits = 3)))
  colnames(result_table) <- c(' ', ' ')
  
  val <- list(
    matrix = finale_matrix,
    table = result_table
  )
  share_vars$regression_values <- val
  session$sendCustomMessage("regression", jsonlite::toJSON(finale_matrix))
  }, invalid = function(i) NULL
  , minor_error = function(e) {
    print(e)
    shinyWidgets::closeSweetAlert(session)
    toastr_error(e$message)
  }, error = function(e) {
    print(e)
    shinyWidgets::closeSweetAlert(session)
    sweet_alert_error(e$message)
  })
})

#' @title Theoretic matrix
#' 
#' @description 
#' Display the observed and theoretic matrices
#' 
#' @param share_vars reactive value, results of the regression
output$regression_matrix <- DT::renderDataTable({
  if(!is.null(share_vars$regression_values$matrix)) 
    share_vars$regression_values$matrix
  else {
    C <- c(7, 36)
    Cl <- c(3, 30)
    matrix(NA, nrow = C[2] - C[1] + 1, ncol = Cl[2] - Cl[1] + 1, 
      dimnames = list(paste0("C", C[1]:C[2]), paste0("Cl", Cl[1]:Cl[2])))
  }
}, selection = "none", server = FALSE, extensions = 'Scroller', 
class = 'display cell-border compact nowrap', 
options = list(info = FALSE, paging = FALSE, dom = 'frtip', scoller = TRUE, 
scrollX = TRUE, bFilter = FALSE, ordering = FALSE, columnDefs = list(list(
  className = 'dt-body-center', targets = "_all")),
initComplete = htmlwidgets::JS("
  function (settings, json) {
    var table = settings.oInstance.api();
    var button = $('#regression_matrix_choice .active').text();
    var selected_button = button.includes('Observed') ? 0 : 1;
    table.cells().every(function(){
      if(this.index().column == 0){
        this.data(this.data());
      }
      else if (this.data() != null){
        var splitted_cell = this.data().split('/');
        if(splitted_cell[selected_button] == 'NA'){
          this.data('');
        }
        else{
          this.data(splitted_cell[selected_button]);
        }
      }
    });
    table.columns.adjust();
  }
")
)
, callback = htmlwidgets::JS("
  $('#regression_matrix_choice').on('click', 'div button', function(){
    if($(this).hasClass('active')) return(null);
    $('#regression_matrix_choice button .active').removeClass('active');
	  $(this).addClass('active');
	  var old_table = regression_mat;
    var selected_button = $(this).text().includes('Observed') ? 0 : 1;
    var table = $('#regression_matrix').data('datatable');
    table.cells().every(function(){
      var row = this.index().row;
      var col = this.index().column - 1;
      if(this.index().column == 0){
        this.data(this.data());
      }
      else if(old_table[row][col] == null){
        this.data('');
      }
      else if(old_table[row][col] != null){
        var splitted_cell = old_table[row][col].split('/');
        if(splitted_cell[selected_button] == 'NA'){
          this.data('');
        }
        else{
          this.data(splitted_cell[selected_button]);
        }
      }
    });
    table.columns.adjust();
  });
")
)

#' @title Regression results table
#' 
#' @description 
#' Display the factor for each file, the coefficient of determination and the residuals
#' 
#' @param share_vars reactive value, results of the regression
output$regression_table <- DT::renderDataTable({
  share_vars$regression_values$table
}, selection = "none", server = FALSE, extensions = 'Scroller', 
class = 'display cell-border compact nowrap', 
options = list(info = FALSE, paging = FALSE, dom = 'frtip', scoller = TRUE, 
  scrollX = TRUE, bFilter = FALSE, ordering = FALSE, columnDefs = list(list(
    className = 'dt-body-center', targets = "_all"))
  )
)

#' @title Download matrix
#' 
#' @description 
#' Download the matrix at the xlsx format
#' 
#' @param input$project integer project ID
#' 
#' @return xlsx file
output$regression_export <- shiny::downloadHandler(
  filename = function(){
    params <- list(
      project = input$project
    )
    name <- get_project_name(db, params$project)
    paste("CPSeeker0.1_", name, "_regression.xlsx", sep = "")
  },
  content = function(file){
    mat <- share_vars$regression_values$matrix
    for(row in 1:nrow(mat)){
      for(col in 1 :ncol(mat)){
        cell = mat[row,col]
        if(is.na(cell)) next
        splitted_cell = unlist(stringr::str_split(cell, "/"))[2]
        if(splitted_cell == "NA"){
          mat[row,col] = ""
        }
        else{
          mat[row,col] = splitted_cell
        } 
      }
    }
    openxlsx::write.xlsx(mat, file)
  }
)