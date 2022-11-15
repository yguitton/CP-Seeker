# Export xlsx functions

export_PCA <- function(user, project_informations, file, output = ""){
  # Template PCAs & PBAs
  browser()
  library(openxlsx)
  #library(XLConnect) # for some more functions

  # Create the work book for Excel
  wb <- openxlsx::createWorkbook()

  # Create work sheets without lines (all backgound is blank)
  addWorksheet(wb=wb, sheetName='Sequence', gridLines=FALSE)
  addWorksheet(wb=wb, sheetName='Parameters', gridLines=FALSE)
  #setSheetColor(wb, sheet = 1:4, color = c("red","blue","green","yellow"))

  # Create styles
  hStyle <- createStyle(fontColour = "#129adb", textDecoration = "bold")
  tableStyle <- createStyle(fgFill = "#def1fa", border = "TopBottomLeftRight")
  hTableStyle <- createStyle(textDecoration = "bold", fgFill = "#bfbfbf", border = c("top", "bottom"))
  bodyTableStyle <- createStyle(fgFill = "#def1fa")
  boldStyle <- createStyle(textDecoration = "bold")
  sh2EndTableStyle <- createStyle(halign = "center", fgFill = "#fcd0a9", border = "TopBottomLeftRight")
  sh2DisplayStyle1 <- createStyle(fgFill = "#878787")
  sh2DisplayStyle2 <- createStyle(fgFill = "#d9d9d9")
  sh2LegendStyle <- createStyle(fontColour = "#d1cfcf")
  italicStyle <- createStyle(textDecoration = "italic")
  sh2TableStyle <- createStyle(halign = "center", fgFill = "#def1fa", border = "TopBottomLeftRight")
  noiseActiveStyle <- createStyle(halign = "center", fgFill = "#def1fa", fontColour = "#f20505", textDecoration = "bold")
  noiseStopStyle <- createStyle(halign = "center", fgFill = "#def1fa")
  topBorderStyle <- createStyle(halign = "center", valign = "center", border = c("top","left","right"),fgFill = "#9dd4e3", textDecoration = "bold")
  bottomBorderStyle <- createStyle(halign = "center", valign = "center", border = c("bottom","left","right"), fgFill = "#9dd4e3", textDecoration = "bold")
  middleBorderStyle <- createStyle(halign = "center", valign = "center", border = c("left","right"), fgFill = "#9dd4e3", textDecoration = "bold")
  startBorderStyle <- createStyle(halign = "center", valign = "center", border = c("left","top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  endBorderStyle <- createStyle(halign = "center", valign = "center", border = c("right","top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  topMiddleBorderStyle <- createStyle(halign = "center", valign = "center", border = c("top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  bottomBlankBorderStyle <- createStyle(border = "bottom")
  rightBlankBoderStyle <- createStyle(border = "right")
  cornerRightBottomBlankStyle <- createStyle(border = c("right","bottom"))

  ################################################################################
  # Write the first sheet = table for each sample
  openxlsx::writeData(wb, 1, config$appname, startRow = 1)
  openxlsx::writeData(wb, 1, paste("Sequence"), startRow = 2)
  addStyle(wb, sheet = 1, hStyle, rows = c(1,2), cols = 1)
  setColWidths(wb, sheet = 1, cols = 1, widths = 4)

  openxlsx::writeData(wb, 1, paste("User"), startRow = 4, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Sequence"),startRow = 5, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Comments"),startRow = 6, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Creation date"),startRow = 7, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Last modified"),startRow = 8, startCol = 2)
  setColWidths(wb, sheet = 1, cols = 2, widths = 50)

  openxlsx::writeData(wb, 1, user, startRow = 4, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$name, startRow = 5, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$comments, startRow = 6, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$creation, startRow = 7, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$modified, startRow = 8, startCol = 3)
  addStyle(wb, sheet = 1, tableStyle, rows = 4:8, cols = 3)
  setColWidths(wb, sheet = 1, cols = 3, widths = 50)

  openxlsx::writeData(wb, 1, c("File"), startRow = 10, startCol = 2)
  openxlsx::writeData(wb, 1, c("Label"), startRow = 10, startCol = 3)
  openxlsx::writeData(wb, 1, c("Polarity"), startRow = 10, startCol = 4)
  openxlsx::writeData(wb, 1, c("m/z Range"), startRow = 10, startCol = 5)
  openxlsx::writeData(wb, 1, c("Size (MB)"), startRow = 10, startCol = 6)
  openxlsx::writeData(wb, 1, c("Instrument model"), startRow = 10, startCol = 7)
  setColWidths(wb, sheet = 1, cols = 7, widths = 20)
  openxlsx::writeData(wb, 1, c("Ion source"), startRow = 10, startCol = 8)
  setColWidths(wb, sheet = 1, cols = 8, widths = 30)
  openxlsx::writeData(wb, 1, c("Original path"), startRow = 10, startCol = 9)
  addStyle(wb, sheet = 1, hTableStyle, rows = 10, cols = 2:9)
  setColWidths(wb, sheet = 1, cols = 9, widths = 100)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  line <- 11
  for(smpl in allFiles$sample){
    sample_info <- samples()[which(samples()$sample == smpl),]
    openxlsx::writeData(wb, 1, sample_info$sample, startRow = line, startCol = 2)
    openxlsx::writeData(wb, 1, allFiles[which(allFiles$sample == smpl), "sample_id"], startRow = line, startCol = 3)
    openxlsx::writeData(wb, 1, sample_info$polarity, startRow = line, startCol = 4)
    openxlsx::writeData(wb, 1, "????", startRow = line, startCol = 5) ##### a modifier (mz range) non inclus (thermorawdump lors de l'ajout des fichiers?)
    openxlsx::writeData(wb, 1, paste0(sample_info$size," MB"), startRow = line, startCol = 6)
    openxlsx::writeData(wb, 1, sample_info$instrument_model, startRow = line, startCol = 7)
    openxlsx::writeData(wb, 1, sample_info$ion_source, startRow = line, startCol = 8)
    openxlsx::writeData(wb, 1, sample_info$raw_path, startRow = line, startCol = 9)
    addStyle(wb, sheet = 1, bodyTableStyle, rows = line, cols = 2:9)
    line <- line + 1
  }
  
  ################################################################################
  # Write the second sheet
  openxlsx::writeData(wb, 2, config$appname, startRow = 1)
  openxlsx::writeData(wb, 2, paste("Parameters"), startRow = 2)
  addStyle(wb, 2, hStyle, rows = c(1,2), cols = 1)
  setColWidths(wb, 2, cols = 1, widths = 4)
  addStyle(wb, 2, sh2DisplayStyle1, rows = 27, cols = 1)
  addStyle(wb, 2, sh2DisplayStyle2, rows = 28, cols = 1)

  openxlsx::writeData(wb, 2, "General", startRow = 4, startCol = 2)
  openxlsx::writeData(wb, 2, c("Mass tolerance","Instrument","Peakwidth","Retention time","Missing scans"), startRow = 5, startCol = 2)
  openxlsx::writeData(wb, 2, "Target analyte", startRow = 11, startCol = 2)
  openxlsx::writeData(wb, 2, c("Family","Adduct"), startRow = 12, startCol = 2)
  openxlsx::writeData(wb, 2, "Standard", startRow = 15, startCol = 2)
  openxlsx::writeData(wb, 2, c("Formula","Adduct","Retention time 1","Retention time 2"), startRow = 16, startCol = 2)
  openxlsx::writeData(wb, 2, "Deconvolution process", startRow = 21, startCol = 2)
  openxlsx::writeData(wb, 2, c("Start time","Computer","Duration"), startRow = 22, startCol = 2)
  openxlsx::writeData(wb, 2, "Display format", startRow = 26, startCol = 2)
  openxlsx::writeData(wb, 2, "Out of m/z range, or number of halogen higher than number of carbon plus 3, or not processed", startRow = 27, startCol = 2)
  openxlsx::writeData(wb, 2, "Stadding (at least the 2 most intense isotopomer groups)", startRow = 28, startCol = 2)
  openxlsx::writeData(wb, 2, "Score threshold", startRow = 29, startCol = 2)
  openxlsx::writeData(wb, 2, "Deviation tolerance (+/- mDa)", startRow = 30, startCol = 2)
  addStyle(wb, 2, boldStyle, rows = c(4,11,15,21,26), cols = 2)
  setColWidths(wb, 2, cols = 2, widths = 25)

  # Mass tolerance
  decParams <- deconvolution_params()[which("PCAs" %in% deconvolution_params()$chemical_type || "PBAs" %in% deconvolution_params()$chemical_type),]
  if(decParams$ppm > 0){
    openxlsx::writeData(wb, 2, paste0(decParams$ppm," ppm"), startCol = 3, startRow = 5)
  }else if(decParams$mda > 0){
    openxlsx::writeData(wb, 2, paste0(decParams$mda," mDa"), startCol = 3, startRow = 5)
  }
  if(decParams$instrument == "Orbitrap"){
    openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution/1000,"k@",decParams$resolution_mz), startCol = 3, startRow = 6)
  }else if(grep("ToF", decParams$instrument)){
    openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution), startCol = 3, startRow = 6)
  }
  # Peakwidth
  openxlsx::writeData(wb, 2, paste0(decParams$peakwidth_min," to ",decParams$peakwidth_max," s"), startCol = 3, startRow = 7)
  # Retention time
  openxlsx::writeData(wb, 2, paste0(decParams$retention_time_min," to ",decParams$retention_time_max," min"), startCol = 3, startRow = 8)
  # Missing scans
  openxlsx::writeData(wb, 2, decParams$missing_scans, startCol = 3, startRow = 9)
  # Chemical type
  openxlsx::writeData(wb, 2, decParams$chemical_type, startCol = 3, startRow = 12)
  # Adduct(s)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
  if(unique(allSamples$polarity == "negative")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(decParams$adduct)," ]-"), startCol = 3, startRow = 13)
  }else if(unique(allSamples$polarity == "positive")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(decParams$adduct)," ]+"), startCol = 3, startRow = 13)
  }else{
    openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
  }
  #################################
  # Standard(s) information
  std <- deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"]
  std <- std[-which(std %in% c("PCAs", "PBAs", "PCOs", "PCdiOs"))]
  std <- std[-grep("PXAs", std)]
  usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% std),]
  myStd <- NULL
  for(s in 1:nrow(usedStd)){
    if(s < nrow(usedStd)){
      myStd <- paste0(myStd, paste0(usedStd$chemical_type[s],";"))
    }else{
      myStd <- paste0(myStd, usedStd$chemical_type[s])
    }
  }
  openxlsx::writeData(wb, 2, myStd, startCol = 3, startRow = 16)
  # Standard adduct(s)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
  if(unique(allSamples$polarity == "negative")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(usedStd$adduct)," ]-"), startCol = 3, startRow = 17)
  }else if(unique(allSamples$polarity == "positive")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(usedStd$adduct)," ]+"), startCol = 3, startRow = 17)
  }else{
    openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 17)
  }
  # Standard RT 1 (given by user)
  stdInfo <- deconvolution_params()[which(deconvolution_params()$project == input$project), ]
  stdInfo <- stdInfo[-which(stdInfo$chemical_type %in% c("PCAs", "PCOs", "PCdiOs")),]
  stdInfo <- stdInfo[-grep("PXAs", stdInfo$chemical_type),]
  openxlsx::writeData(wb, 2, paste0(mean(c(stdInfo$retention_time_min[which(stdInfo$chemical_type == std[1])], 
                                          stdInfo$retention_time_max[which(stdInfo$chemical_type == std[1])]))," min"), 
                      startCol = 3, startRow = 18)
  # Standard RT 2 (given by user)
  openxlsx::writeData(wb, 2, paste0(mean(c(stdInfo$retention_time_min[which(stdInfo$chemical_type == std[2])], 
                                          stdInfo$retention_time_max[which(stdInfo$chemical_type == std[2])]))," min"), 
                      startCol = 3, startRow = 19)
  #################################
  openxlsx::writeData(wb, 2, "time when started", startCol = 3, startRow = 22) # surement à créer
  openxlsx::writeData(wb, 2, "computer name", startCol = 3, startRow = 23) # surement à créer
  openxlsx::writeData(wb, 2, "time of duration", startCol = 3, startRow = 24) # surement à créer
  openxlsx::writeData(wb, 2, "score of threshold", startCol = 3, startRow = 29) # pas compris
  openxlsx::writeData(wb, 2, "tolerance in deviation", startCol = 3, startRow = 30) # pas compris
  setColWidths(wb, 2, cols = 3, widths = 50)
  addStyle(wb, 2, sh2TableStyle, rows = c(5:9,12:13,16:19,22:24), cols = 3)
  addStyle(wb, 2, sh2EndTableStyle, rows = 29:30, cols = 3)

  openxlsx::writeData(wb, 2, "Possibly includes processing of other family and adduct types", startCol = 4, startRow = 24)
  openxlsx::writeData(wb, 2, "Number in grey police if below", startCol = 4, startRow = 29)
  openxlsx::writeData(wb, 2, "Number in grey police if outside", startCol = 4, startRow = 30)
  addStyle(wb, 2, italicStyle, rows = 24, cols = 4)
  addStyle(wb, 2, sh2LegendStyle, rows = 29:30, cols = 4)

  ################################################################################
  # Write the third sheet X times with X is the number of standard
  sheet <- 3
  for(s in std){
    # Add one sheet per standard
    addWorksheet(wb=wb, sheetName=paste0('Standard',sheet-2), gridLines=FALSE)
    # Write the sheet
    openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
    usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type == s),]
    openxlsx::writeData(wb, sheet, usedStd$chemical_type, startRow = 2)
    allFiles <- project_samples()[which(project_samples()$project == input$project),]
    allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
    if(unique(allSamples$polarity == "negative")){
      openxlsx::writeData(wb, sheet, paste0("[ ",unique(usedStd$adduct)," ]-"), startRow = 3)
    }else if(unique(allSamples$polarity == "positive")){
      openxlsx::writeData(wb, sheet, paste0("[ ",unique(usedStd$adduct)," ]+"), startRow = 3)
    }else{
      openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
    }
    addStyle(wb, sheet, hStyle, rows = 1:3, cols = 1)
    setColWidths(wb, sheet, cols = 1, widths = 4)

    openxlsx::writeData(wb, sheet, "File", startCol = 2, startRow = 5)
    setColWidths(wb, sheet, cols = 2, widths = 50)
    openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
    setColWidths(wb, sheet, cols = 3, widths = 50)
    openxlsx::writeData(wb, sheet, "Total area", startCol = 4, startRow = 5)
    openxlsx::writeData(wb, sheet, "Noise", startCol = 5, startRow = 5)
    setColWidths(wb, sheet, cols = 5, widths = 6)
    openxlsx::writeData(wb, sheet, "Score (%)", startCol = 6, startRow = 5)
    openxlsx::writeData(wb, sheet, "Deviation (mDa)", startCol = 7, startRow = 5)
    setColWidths(wb, sheet, cols = 7, widths = 20)
    addStyle(wb, sheet, hTableStyle, rows = 5, cols = 2:7)
    

    allFiles <- project_samples()[which(project_samples()$project == input$project),]
    line <- 6
    for(smpl in allFiles$sample){
      thisSmpl <- allFiles[which(allFiles$sample == smpl),]
      # Sample ID
      openxlsx::writeData(wb, sheet, thisSmpl$sample_id, startCol = 2, startRow = line)
      # Sample label (as named by user)
      openxlsx::writeData(wb, sheet, allFiles[which(allFiles$sample == smpl),"sample_id"], startCol = 3, startRow = line)
      # Total area
      query <- sprintf('select chemical_type, adduct from deconvolution_param where project == %s and
      chemical_type in (select formula from chemical where chemical_type == "Standard");',
      input$project)
      standard <- db_get_query(db, query)
      table_params <- list(
        standard = unique(standard[which(unique(standard$chemical_type) == s), "chemical_type"]),
        adduct = unique(standard[which(unique(standard$chemical_type) == s), "adduct"])
      )
      table <- get_standard_table(db, input$project, table_params$adduct, table_params$standard)
      table <- table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),]
      openxlsx::writeData(wb, sheet, table$'total area', startCol = 4, startRow = line)
      addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:7)
      # Is there noise ? Calculate with total area ABOVE baseline (script from home)
      if(table$'total area' > table$'area above baseline'){
        noiseParam <- "YES"
        openxlsx::writeData(wb, sheet, noiseParam, startCol = 5, startRow = line)
        addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
      }else{
        noiseParam <- "No"
        openxlsx::writeData(wb, sheet, noiseParam, startCol = 5, startRow = line)
        addStyle(wb, sheet, noiseStopStyle, rows = line, cols = 5)
      }
      # Score 
      openxlsx::writeData(wb, sheet, table$score, startCol = 6, startRow = line)
      # Deviation (mDa)
      openxlsx::writeData(wb, sheet, table$'deviation(mDa)', startCol = 7, startRow = line)
      line <- line + 1
    }
    sheet <- sheet + 1
  }
  

  ################################################################################
  # Write the label's sheet(s)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  decParams <- deconvolution_params()[which("PCAs" %in% deconvolution_params()$chemical_type || "PBAs" %in% deconvolution_params()$chemical_type),]
  for(file in allFiles$sample){
    myActualFile <- allFiles[which(allFiles$sample == file),] 
    # Create the sheet of the file label
    addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

    openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
    openxlsx::writeData(wb, sheet, paste("Label"), startRow = 2) # cest quoi le file label ?
    openxlsx::writeData(wb, sheet, paste0("[ ",decParams$adduct," ]"), startRow = 3) # le signe derriere ?
    openxlsx::writeData(wb, sheet, decParams$chemical_type, startRow = 5)
    addStyle(wb, sheet, hStyle, rows = 1:5, cols = 1)
    setColWidths(wb, sheet, cols = 1:1000, widths = 4.5)
    setRowHeights(wb, sheet, rows = 1:1000, heights = 18)

    # Save the table with all values for this file 
    table <- get_profile_matrix(db, myActualFile$project_sample, adduct = decParams$adduct, chemical_type = decParams$chemical_type)
    
    # Table 1 : area (x 1 M)
    openxlsx::writeData(wb, sheet, "Area (x1,000,000)", startRow = 4, startCol = 3)
    addStyle(wb, sheet, boldStyle, rows = 4, cols = 3)
    table1 <- as.data.frame(reduce_matrix(table, 2, na_empty = FALSE))
    openxlsx::writeData(wb, sheet, table1, startCol = 3, startRow = 6)
    openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 7, startCol = 2)
    for(i in 3:30){
      if(decParams$chemical_type == "PCAs"){
        openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i)
      }else if(decParams$chemical_type == "PBAs"){
        openxlsx::writeData(wb, sheet, paste0("Br",i), startRow = 6, startCol = i)
      }
      if(i == 3){
        addStyle(wb, sheet, startBorderStyle, rows = 6, cols = i)
      }else if(i == 30){
        addStyle(wb, sheet, endBorderStyle, rows = 6, cols = i)
      }else{
        addStyle(wb, sheet, topMiddleBorderStyle, rows = 6, cols = i)
      }
    }
    addStyle(wb, sheet, topBorderStyle, rows = 7, cols = 2)
    addStyle(wb, sheet, bottomBorderStyle, rows = 37, cols = 2)
    for(r in 8:36){
      addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 2)
    }
    addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 3:30)
    
    # Table 2 : score %
    openxlsx::writeData(wb, sheet, "Score (%)", startRow = 4, startCol = 32)
    addStyle(wb, sheet, boldStyle, rows = 4, cols = 32)
    table2 <- as.data.frame(reduce_matrix(table, 1, na_empty = FALSE))
    openxlsx::writeData(wb, sheet, table2, startCol = 32, startRow = 6)
    openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 7, startCol = 31)
    for(i in 3:30){
      if(decParams$chemical_type == "PCAs"){
        openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i+29)
      }else if(decParams$chemical_type == "PBAs"){
        openxlsx::writeData(wb, sheet, paste0("Br",i), startRow = 6, startCol = i+29)
      }
      if(i == 3){
        addStyle(wb, sheet, startBorderStyle, rows = 6, cols = i+29)
      }else if(i == 30){
        addStyle(wb, sheet, endBorderStyle, rows = 6, cols = i+29)
      }else{
        addStyle(wb, sheet, topMiddleBorderStyle, rows = 6, cols = i+29)
      }
    }
    addStyle(wb, sheet, topBorderStyle, rows = 7, cols = 31)
    addStyle(wb, sheet, bottomBorderStyle, rows = 37, cols = 31)
    for(r in 8:36){
      addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 31)
    }
    addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 32:59)
    
    # Table 3 : deviation (mDa) # penser à changer mDa ou ppm selon choix uilisateur !!
    openxlsx::writeData(wb, sheet, "Deviation (mDa)", startRow = 4, startCol = 61)
    addStyle(wb, sheet, boldStyle, rows = 4, cols = 61)
    table3 <- as.data.frame(reduce_matrix(table, 3, na_empty = FALSE))
    openxlsx::writeData(wb, sheet, table3, startCol = 61, startRow = 6)
    openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 7, startCol = 60)
    for(i in 3:30){
      if(decParams$chemical_type == "PCAs"){
        openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i+58)
      }else if(decParams$chemical_type == "PBAs"){
        openxlsx::writeData(wb, sheet, paste0("Br",i), startRow = 6, startCol = i+58)
      }
      if(i == 3){
        addStyle(wb, sheet, startBorderStyle, rows = 6, cols = i+58)
      }else if(i == 30){
        addStyle(wb, sheet, endBorderStyle, rows = 6, cols = i+58)
      }else{
        addStyle(wb, sheet, topMiddleBorderStyle, rows = 6, cols = i+58)
      }
    }
    addStyle(wb, sheet, topBorderStyle, rows = 7, cols = 60)
    addStyle(wb, sheet, bottomBorderStyle, rows = 37, cols = 60)
    for(r in 8:36){
      addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 60)
    }
    addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 61:88)
    addStyle(wb, sheet, rightBlankBoderStyle, rows = 7:37, cols = 88)
    addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 37, cols = 88)

    sheet <- sheet + 1
  }
  
  ################################################################################
  # Save the workbook
  saveWorkbook(wb, paste0(config_dir,"/",file,"_testPCAs.xlsx"), overwrite = TRUE)
  #openXL("c:/Users/JSA/Documents/LABERCA/CP-Seeker_datatest/testPCAs.xlsx") # to read it when tested
}

export_PCO <- function(user, project_informations, file, output = ""){
  library(openxlsx)
  #library(XLConnect) # for some more functions

  # Create the work book for Excel
  wb <- openxlsx::createWorkbook()

  # Create all work sheets without lines (all backgound is blank)
  addWorksheet(wb=wb, sheetName='Sequence', gridLines=FALSE)
  addWorksheet(wb=wb, sheetName='Parameters', gridLines=FALSE)
  addWorksheet(wb=wb, sheetName='Standard', gridLines=FALSE)
  addWorksheet(wb=wb, sheetName='Label', gridLines=FALSE)
  #setSheetColor(wb, sheet = 1:4, color = c("red","blue","green","yellow"))

  # Create styles
  hStyle <- createStyle(fontColour = "#129adb", textDecoration = "bold")
  tableStyle <- createStyle(fgFill = "#def1fa", border = "TopBottomLeftRight")
  hTableStyle <- createStyle(textDecoration = "bold", fgFill = "#bfbfbf", border = c("top", "bottom"))
  bodyTableStyle <- createStyle(fgFill = "#def1fa")
  boldStyle <- createStyle(textDecoration = "bold")
  sh2EndTableStyle <- createStyle(halign = "center", fgFill = "#fcd0a9", border = "TopBottomLeftRight")
  sh2DisplayStyle1 <- createStyle(fgFill = "#878787")
  sh2DisplayStyle2 <- createStyle(fgFill = "#d9d9d9")
  sh2LegendStyle <- createStyle(fontColour = "#d1cfcf")
  italicStyle <- createStyle(textDecoration = "italic")
  sh2TableStyle <- createStyle(halign = "center", fgFill = "#def1fa", border = "TopBottomLeftRight")
  noiseActiveStyle <- createStyle(halign = "center", fgFill = "#def1fa", fontColour = "#f20505", textDecoration = "bold")
  noiseStopStyle <- createStyle(halign = "center", fgFill = "#def1fa")
  topBorderStyle <- createStyle(halign = "center", valign = "center", border = c("top","left","right"),fgFill = "#9dd4e3", textDecoration = "bold")
  bottomBorderStyle <- createStyle(halign = "center", valign = "center", border = c("bottom","left","right"), fgFill = "#9dd4e3", textDecoration = "bold")
  middleBorderStyle <- createStyle(halign = "center", valign = "center", border = c("left","right"), fgFill = "#9dd4e3", textDecoration = "bold")
  startBorderStyle <- createStyle(halign = "center", valign = "center", border = c("left","top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  endBorderStyle <- createStyle(halign = "center", valign = "center", border = c("right","top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  topMiddleBorderStyle <- createStyle(halign = "center", valign = "center", border = c("top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  bottomBlankBorderStyle <- createStyle(border = "bottom")
  rightBlankBoderStyle <- createStyle(border = "right")
  cornerRightBottomBlankStyle <- createStyle(border = c("right","bottom"))

  ################################################################################
  # Write the first sheet = table for each sample
  openxlsx::writeData(wb, 1, config$appname, startRow = 1)
  openxlsx::writeData(wb, 1, paste("Sequence"), startRow = 2)
  addStyle(wb, sheet = 1, hStyle, rows = c(1,2), cols = 1)
  setColWidths(wb, sheet = 1, cols = 1, widths = 4)

  openxlsx::writeData(wb, 1, paste("User"), startRow = 4, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Sequence"),startRow = 5, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Comments"),startRow = 6, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Creation date"),startRow = 7, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Last modified"),startRow = 8, startCol = 2)
  setColWidths(wb, sheet = 1, cols = 2, widths = 50)

  openxlsx::writeData(wb, 1, user, startRow = 4, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$name, startRow = 5, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$comments, startRow = 6, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$creation, startRow = 7, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$modified, startRow = 8, startCol = 3)
  addStyle(wb, sheet = 1, tableStyle, rows = 4:8, cols = 3)
  setColWidths(wb, sheet = 1, cols = 3, widths = 50)

  openxlsx::writeData(wb, 1, c("File"), startRow = 10, startCol = 2)
  openxlsx::writeData(wb, 1, c("Label"), startRow = 10, startCol = 3)
  openxlsx::writeData(wb, 1, c("Polarity"), startRow = 10, startCol = 4)
  openxlsx::writeData(wb, 1, c("m/z Range"), startRow = 10, startCol = 5)
  openxlsx::writeData(wb, 1, c("Size (MB)"), startRow = 10, startCol = 6)
  openxlsx::writeData(wb, 1, c("Instrument model"), startRow = 10, startCol = 7)
  setColWidths(wb, sheet = 1, cols = 7, widths = 20)
  openxlsx::writeData(wb, 1, c("Ion source"), startRow = 10, startCol = 8)
  setColWidths(wb, sheet = 1, cols = 8, widths = 30)
  openxlsx::writeData(wb, 1, c("Original path"), startRow = 10, startCol = 9)
  addStyle(wb, sheet = 1, hTableStyle, rows = 10, cols = 2:9)
  setColWidths(wb, sheet = 1, cols = 9, widths = 100)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  line <- 11
  for(smpl in allFiles$sample){
    sample_info <- samples()[which(samples()$sample == smpl),]
    openxlsx::writeData(wb, 1, sample_info$sample, startRow = line, startCol = 2)
    openxlsx::writeData(wb, 1, allFiles[which(allFiles$sample == smpl), "sample_id"], startRow = line, startCol = 3)
    openxlsx::writeData(wb, 1, sample_info$polarity, startRow = line, startCol = 4)
    openxlsx::writeData(wb, 1, "????", startRow = line, startCol = 5) ##### a modifier (mz range) non inclus (thermorawdump lors de l'ajout des fichiers?)
    openxlsx::writeData(wb, 1, paste0(sample_info$size," MB"), startRow = line, startCol = 6)
    openxlsx::writeData(wb, 1, sample_info$instrument_model, startRow = line, startCol = 7)
    openxlsx::writeData(wb, 1, sample_info$ion_source, startRow = line, startCol = 8)
    openxlsx::writeData(wb, 1, sample_info$raw_path, startRow = line, startCol = 9)
    addStyle(wb, sheet = 1, bodyTableStyle, rows = line, cols = 2:9)
    line <- line + 1
  }

  ################################################################################
  # Write the second sheet
  openxlsx::writeData(wb, 2, config$appname, startRow = 1)
  openxlsx::writeData(wb, 2, paste("Parameters"), startRow = 2)
  addStyle(wb, 2, hStyle, rows = c(1,2), cols = 1)
  setColWidths(wb, 2, cols = 1, widths = 4)
  addStyle(wb, 2, sh2DisplayStyle1, rows = 27, cols = 1)
  addStyle(wb, 2, sh2DisplayStyle2, rows = 28, cols = 1)

  openxlsx::writeData(wb, 2, "General", startRow = 4, startCol = 2)
  openxlsx::writeData(wb, 2, c("Mass tolerance","Instrument","Peakwidth","Retention time","Missing scans"), startRow = 5, startCol = 2)
  openxlsx::writeData(wb, 2, "Target analyte", startRow = 11, startCol = 2)
  openxlsx::writeData(wb, 2, c("Family","Adduct"), startRow = 12, startCol = 2)
  openxlsx::writeData(wb, 2, "Standard", startRow = 15, startCol = 2)
  openxlsx::writeData(wb, 2, c("Formula","Adduct","Retention time 1","Retention time 2"), startRow = 16, startCol = 2)
  openxlsx::writeData(wb, 2, "Deconvolution process", startRow = 21, startCol = 2)
  openxlsx::writeData(wb, 2, c("Start time","Computer","Duration"), startRow = 22, startCol = 2)
  openxlsx::writeData(wb, 2, "Display format", startRow = 26, startCol = 2)
  openxlsx::writeData(wb, 2, "Out of m/z range, or number of halogen higher than number of carbon plus 3, or not processed", startRow = 27, startCol = 2)
  openxlsx::writeData(wb, 2, "Stadding (at least the 2 most intense isotopomer groups)", startRow = 28, startCol = 2)
  openxlsx::writeData(wb, 2, "Score threshold", startRow = 29, startCol = 2)
  openxlsx::writeData(wb, 2, "Deviation tolerance (+/- mDa)", startRow = 30, startCol = 2)
  addStyle(wb, 2, boldStyle, rows = c(4,11,15,21,26), cols = 2)
  setColWidths(wb, 2, cols = 2, widths = 25)

  # Mass tolerance
  decParams <- deconvolution_params()[which("PCAs" %in% deconvolution_params()$chemical_type || "PBAs" %in% deconvolution_params()$chemical_type),]
  if(decParams$ppm > 0){
    openxlsx::writeData(wb, 2, paste0(decParams$ppm," ppm"), startCol = 3, startRow = 5)
  }else if(decParams$mda > 0){
    openxlsx::writeData(wb, 2, paste0(decParams$mda," mDa"), startCol = 3, startRow = 5)
  }
  if(decParams$instrument == "Orbitrap"){
    openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution/1000,"k@",decParams$resolution_mz), startCol = 3, startRow = 6)
  }else if(grep("ToF", decParams$instrument)){
    openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution), startCol = 3, startRow = 6)
  }
  # Peakwidth
  openxlsx::writeData(wb, 2, paste0(decParams$peakwidth_min," to ",decParams$peakwidth_max," s"), startCol = 3, startRow = 7)
  # Retention time
  openxlsx::writeData(wb, 2, paste0(decParams$retention_time_min," to ",decParams$retention_time_max," min"), startCol = 3, startRow = 8)
  # Missing scans
  openxlsx::writeData(wb, 2, decParams$missing_scans, startCol = 3, startRow = 9)
  # Chemical type
  openxlsx::writeData(wb, 2, decParams$chemical_type, startCol = 3, startRow = 12)
  # Adduct(s)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
  if(unique(allSamples$polarity == "negative")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(decParams$adduct)," ]-"), startCol = 3, startRow = 13)
  }else if(unique(allSamples$polarity == "positive")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(decParams$adduct)," ]+"), startCol = 3, startRow = 13)
  }else{
    openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
  }
  #################################
  # Standard(s) information
  std <- deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"]
  std <- std[-which(std %in% c("PCAs", "PBAs", "PCOs", "PCdiOs"))]
  std <- std[-grep("PXAs", std)]
  usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% std),]
  myStd <- NULL
  for(s in 1:nrow(usedStd)){
    if(s < nrow(usedStd)){
      myStd <- paste0(myStd, paste0(usedStd$chemical_type[s],";"))
    }else{
      myStd <- paste0(myStd, usedStd$chemical_type[s])
    }
  }
  openxlsx::writeData(wb, 2, myStd, startCol = 3, startRow = 16)
  # Standard adduct(s)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
  if(unique(allSamples$polarity == "negative")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(usedStd$adduct)," ]-"), startCol = 3, startRow = 17)
  }else if(unique(allSamples$polarity == "positive")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(usedStd$adduct)," ]+"), startCol = 3, startRow = 17)
  }else{
    openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 17)
  }
  # Standard RT 1 (given by user)
  stdInfo <- deconvolution_params()[which(deconvolution_params()$project == input$project), ]
  stdInfo <- stdInfo[-which(stdInfo$chemical_type %in% c("PCAs", "PCOs", "PCdiOs")),]
  stdInfo <- stdInfo[-grep("PXAs", stdInfo$chemical_type),]
  openxlsx::writeData(wb, 2, paste0(mean(c(stdInfo$retention_time_min[which(stdInfo$chemical_type == std[1])], 
                                          stdInfo$retention_time_max[which(stdInfo$chemical_type == std[1])]))," min"), 
                      startCol = 3, startRow = 18)
  # Standard RT 2 (given by user)
  openxlsx::writeData(wb, 2, paste0(mean(c(stdInfo$retention_time_min[which(stdInfo$chemical_type == std[2])], 
                                          stdInfo$retention_time_max[which(stdInfo$chemical_type == std[2])]))," min"), 
                      startCol = 3, startRow = 19)
  #################################
  openxlsx::writeData(wb, 2, "time when started", startCol = 3, startRow = 22) # surement à créer
  openxlsx::writeData(wb, 2, "computer name", startCol = 3, startRow = 23) # surement à créer
  openxlsx::writeData(wb, 2, "time of duration", startCol = 3, startRow = 24) # surement à créer
  openxlsx::writeData(wb, 2, "score of threshold", startCol = 3, startRow = 29) # pas compris
  openxlsx::writeData(wb, 2, "tolerance in deviation", startCol = 3, startRow = 30) # pas compris
  setColWidths(wb, 2, cols = 3, widths = 50)
  addStyle(wb, 2, sh2TableStyle, rows = c(5:9,12:13,16:19,22:24), cols = 3)
  addStyle(wb, 2, sh2EndTableStyle, rows = 29:30, cols = 3)

  openxlsx::writeData(wb, 2, "Possibly includes processing of other family and adduct types", startCol = 4, startRow = 24)
  openxlsx::writeData(wb, 2, "Number in grey police if below", startCol = 4, startRow = 29)
  openxlsx::writeData(wb, 2, "Number in grey police if outside", startCol = 4, startRow = 30)
  addStyle(wb, 2, italicStyle, rows = 24, cols = 4)
  addStyle(wb, 2, sh2LegendStyle, rows = 29:30, cols = 4)

    ################################################################################
  # Write the third sheet X times with X is the number of standard
  sheet <- 3
  for(s in std){
    # Add one sheet per standard
    addWorksheet(wb=wb, sheetName=paste0('Standard',sheet-2), gridLines=FALSE)
    # Write the sheet
    openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
    usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type == s),]
    openxlsx::writeData(wb, sheet, usedStd$chemical_type, startRow = 2)
    allFiles <- project_samples()[which(project_samples()$project == input$project),]
    allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
    if(unique(allSamples$polarity == "negative")){
      openxlsx::writeData(wb, sheet, paste0("[ ",unique(usedStd$adduct)," ]-"), startRow = 3)
    }else if(unique(allSamples$polarity == "positive")){
      openxlsx::writeData(wb, sheet, paste0("[ ",unique(usedStd$adduct)," ]+"), startRow = 3)
    }else{
      openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
    }
    addStyle(wb, sheet, hStyle, rows = 1:3, cols = 1)
    setColWidths(wb, sheet, cols = 1, widths = 4)

    openxlsx::writeData(wb, sheet, "File", startCol = 2, startRow = 5)
    setColWidths(wb, sheet, cols = 2, widths = 50)
    openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
    setColWidths(wb, sheet, cols = 3, widths = 50)
    openxlsx::writeData(wb, sheet, "Total area", startCol = 4, startRow = 5)
    openxlsx::writeData(wb, sheet, "Noise", startCol = 5, startRow = 5)
    setColWidths(wb, sheet, cols = 5, widths = 6)
    openxlsx::writeData(wb, sheet, "Score (%)", startCol = 6, startRow = 5)
    openxlsx::writeData(wb, sheet, "Deviation (mDa)", startCol = 7, startRow = 5)
    setColWidths(wb, sheet, cols = 7, widths = 20)
    addStyle(wb, sheet, hTableStyle, rows = 5, cols = 2:7)
    

    allFiles <- project_samples()[which(project_samples()$project == input$project),]
    line <- 6
    for(smpl in allFiles$sample){
      thisSmpl <- allFiles[which(allFiles$sample == smpl),]
      # Sample ID
      openxlsx::writeData(wb, sheet, thisSmpl$sample_id, startCol = 2, startRow = line)
      # Sample label (as named by user)
      openxlsx::writeData(wb, sheet, allFiles[which(allFiles$sample == smpl),"sample_id"], startCol = 3, startRow = line)
      # Total area
      query <- sprintf('select chemical_type, adduct from deconvolution_param where project == %s and
      chemical_type in (select formula from chemical where chemical_type == "Standard");',
      input$project)
      standard <- db_get_query(db, query)
      table_params <- list(
        standard = unique(standard[which(unique(standard$chemical_type) == s), "chemical_type"]),
        adduct = unique(standard[which(unique(standard$chemical_type) == s), "adduct"])
      )
      table <- get_standard_table(db, input$project, table_params$adduct, table_params$standard)
      table <- table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),]
      openxlsx::writeData(wb, sheet, table$'total area', startCol = 4, startRow = line)
      addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:7)
      # Is there noise ? Calculate with total area ABOVE baseline (script from home)
      if(table$'total area' > table$'area above baseline'){
        noiseParam <- "YES"
        openxlsx::writeData(wb, sheet, noiseParam, startCol = 5, startRow = line)
        addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
      }else{
        noiseParam <- "No"
        openxlsx::writeData(wb, sheet, noiseParam, startCol = 5, startRow = line)
        addStyle(wb, sheet, noiseStopStyle, rows = line, cols = 5)
      }
      # Score 
      openxlsx::writeData(wb, sheet, table$score, startCol = 6, startRow = line)
      # Deviation (mDa)
      openxlsx::writeData(wb, sheet, table$'deviation(mDa)', startCol = 7, startRow = line)
      line <- line + 1
    }
    sheet <- sheet + 1
  }

  ################################################################################
  # Write the fourth sheet
  openxlsx::writeData(wb, 4, paste("CP-Seeker Version"), startRow = 1)
  openxlsx::writeData(wb, 4, paste("Label"), startRow = 2)
  openxlsx::writeData(wb, 4, paste("Name of the adduct"), startRow = 3)
  openxlsx::writeData(wb, 4, "PCOs", startRow = 5)
  openxlsx::writeData(wb, 4, "PCdiOs", startRow = 39)
  openxlsx::writeData(wb, 4, "PCtriOs", startRow = 73)
  addStyle(wb, 4, hStyle, rows = 1:73, cols = 1)
  setColWidths(wb, 4, cols = 1:1000, widths = 4.5)
  setRowHeights(wb, 4, rows = 1:1000, heights = 18)
  # Table 1
  openxlsx::writeData(wb, 4, "Area (x1,000,000)", startRow = 4, startCol = 3)
  addStyle(wb, 4, boldStyle, rows = 4, cols = 3)
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 7, startCol = 2)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 6, startCol = i)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows = 6, cols = i)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 6, cols = i)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 6, cols = i)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 7, cols = 2)
  addStyle(wb, 4, bottomBorderStyle, rows = 37, cols = 2)
  for(r in 8:36){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 2)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 37, cols = 3:30)
  # Table 2
  openxlsx::writeData(wb, 4, "Score (%)", startRow = 4, startCol = 32)
  addStyle(wb, 4, boldStyle, rows = 4, cols = 32)
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 7, startCol = 31)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 6, startCol = i+29)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows = 6, cols = i+29)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 6, cols = i+29)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 6, cols = i+29)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 7, cols = 31)
  addStyle(wb, 4, bottomBorderStyle, rows = 37, cols = 31)
  for(r in 8:36){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 31)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 37, cols = 32:59)
  # Table 3
  openxlsx::writeData(wb, 4, "Deviation (mDa)", startRow = 4, startCol = 61)
  addStyle(wb, 4, boldStyle, rows = 4, cols = 61)
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 7, startCol = 60)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 6, startCol = i+58)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows = 6, cols = i+58)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 6, cols = i+58)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 6, cols = i+58)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 7, cols = 60)
  addStyle(wb, 4, bottomBorderStyle, rows = 37, cols = 60)
  for(r in 8:36){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 60)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 37, cols = 61:88)
  addStyle(wb, 4, rightBlankBoderStyle, rows = 7:37, cols = 88)
  addStyle(wb, 4, cornerRightBottomBlankStyle, rows = 37, cols = 88)
  #Table 4
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 41, startCol = 2)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 40, startCol = i)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows = 40, cols = i)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 40, cols = i)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 40, cols = i)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 41, cols = 2)
  addStyle(wb, 4, bottomBorderStyle, rows = 71, cols = 2)
  for(r in 42:70){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 2)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 71, cols = 3:30)
  # Table 5
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 41, startCol = 31)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 40, startCol = i+29)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows = 40, cols = i+29)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 40, cols = i+29)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 40, cols = i+29)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 41, cols = 31)
  addStyle(wb, 4, bottomBorderStyle, rows = 71, cols = 31)
  for(r in 42:70){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 31)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 71, cols = 32:59)
  # Table 6
  openxlsx::writeData(wb, 4, "Deviation (mDa)", startRow = 4, startCol = 61)
  addStyle(wb, 4, boldStyle, rows = 4, cols = 61)
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 41, startCol = 60)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 40, startCol = i+58)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows = 40, cols = i+58)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 40, cols = i+58)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 40, cols = i+58)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 41, cols = 60)
  addStyle(wb, 4, bottomBorderStyle, rows = 71, cols = 60)
  for(r in 42:70){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 60)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 71, cols = 61:88)
  addStyle(wb, 4, rightBlankBoderStyle, rows = 41:71, cols = 88)
  addStyle(wb, 4, cornerRightBottomBlankStyle, rows = 71, cols = 88)
  # Table 7
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 75, startCol = 2)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 74, startCol = i)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows = 74, cols = i)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 74, cols = i)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 74, cols = i)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 75, cols = 2)
  addStyle(wb, 4, bottomBorderStyle, rows = 105, cols = 2)
  for(r in 76:104){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 2)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 105, cols = 3:30)
  # Table 8
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 75, startCol = 31)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 74, startCol = i+29)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows = 74, cols = i+29)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 74, cols = i+29)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 74, cols = i+29)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 75, cols = 31)
  addStyle(wb, 4, bottomBorderStyle, rows = 105, cols = 31)
  for(r in 76:104){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 31)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 105, cols = 32:59)
  # Table 9
  openxlsx::writeData(wb, 4, "Deviation (mDa)", startRow = 4, startCol = 61)
  addStyle(wb, 4, boldStyle, rows = 4, cols = 61)
  openxlsx::writeData(wb, 4, c("C6","C7","C8","C9","C10","C11","C12","C13","C14",
                               "C15","C16","C17","C18","C19","C20","C21","C22","C23","C24",
                               "C25","C26","C27","C28","C29","C30","C31","C32","C33","C34","C35","C36"), 
                      startRow = 75, startCol = 60)
  for(i in 3:30){
      openxlsx::writeData(wb, 4, paste0("Cl",i), startRow = 74, startCol = i+58)
    if(i == 3){
      addStyle(wb, 4, startBorderStyle, rows =74, cols = i+58)
    }else if(i == 30){
      addStyle(wb, 4, endBorderStyle, rows = 74, cols = i+58)
    }else{
      addStyle(wb, 4, topMiddleBorderStyle, rows = 74, cols = i+58)
    }
  }
  addStyle(wb, 4, topBorderStyle, rows = 75, cols = 60)
  addStyle(wb, 4, bottomBorderStyle, rows = 105, cols = 60)
  for(r in 76:104){
    addStyle(wb, 4, middleBorderStyle, rows = r, cols = 60)
  }
  addStyle(wb, 4, bottomBlankBorderStyle, rows = 105, cols = 61:88)
  addStyle(wb, 4, rightBlankBoderStyle, rows = 75:105, cols = 88)
  addStyle(wb, 4, cornerRightBottomBlankStyle, rows = 105, cols = 88)


  ################################################################################
  # Save the workbook
  saveWorkbook(wb, paste0(config_dir,"/",file,"_testPCOs.xlsx"), overwrite = TRUE)
  #openXL("c:/Users/JSA/Documents/LABERCA/CP-Seeker_datatest/testPCOs.xlsx")
}

export_PXA <- function(user, project_informations, file, output = ""){
  library(openxlsx)
  #library(XLConnect) # for some more functions

  # Create the work book for Excel
  wb <- openxlsx::createWorkbook()

  # Create all work sheets without lines (all backgound is blank)
  addWorksheet(wb=wb, sheetName='Sequence', gridLines=FALSE)
  addWorksheet(wb=wb, sheetName='Parameters', gridLines=FALSE)
  addWorksheet(wb=wb, sheetName='Standard', gridLines=FALSE)
  addWorksheet(wb=wb, sheetName='Label', gridLines=FALSE)
  #setSheetColor(wb, sheet = 1:4, color = c("red","blue","green","yellow"))

  # Create styles
  hStyle <- createStyle(fontColour = "#129adb", textDecoration = "bold")
  tableStyle <- createStyle(fgFill = "#def1fa", border = "TopBottomLeftRight")
  hTableStyle <- createStyle(textDecoration = "bold", fgFill = "#bfbfbf", border = c("top", "bottom"))
  bodyTableStyle <- createStyle(fgFill = "#def1fa")
  boldStyle <- createStyle(textDecoration = "bold")
  sh2EndTableStyle <- createStyle(halign = "center", fgFill = "#fcd0a9", border = "TopBottomLeftRight")
  sh2DisplayStyle1 <- createStyle(fgFill = "#878787")
  sh2DisplayStyle2 <- createStyle(fgFill = "#d9d9d9")
  sh2LegendStyle <- createStyle(fontColour = "#d1cfcf")
  italicStyle <- createStyle(textDecoration = "italic")
  sh2TableStyle <- createStyle(halign = "center", fgFill = "#def1fa", border = "TopBottomLeftRight")
  noiseActiveStyle <- createStyle(halign = "center", fgFill = "#def1fa", fontColour = "#f20505", textDecoration = "bold")
  noiseStopStyle <- createStyle(halign = "center", fgFill = "#def1fa")
  topBorderStyle <- createStyle(halign = "center", valign = "center", border = c("top","left","right"),fgFill = "#9dd4e3", textDecoration = "bold")
  bottomBorderStyle <- createStyle(halign = "center", valign = "center", border = c("bottom","left","right"), fgFill = "#9dd4e3", textDecoration = "bold")
  middleBorderStyle <- createStyle(halign = "center", valign = "center", border = c("left","right"), fgFill = "#9dd4e3", textDecoration = "bold")
  startBorderStyle <- createStyle(halign = "center", valign = "center", border = c("left","top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  endBorderStyle <- createStyle(halign = "center", valign = "center", border = c("right","top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  topMiddleBorderStyle <- createStyle(halign = "center", valign = "center", border = c("top","bottom"), fgFill = "#9dd4e3", textDecoration = "bold")
  bottomBlankBorderStyle <- createStyle(border = "bottom")
  rightBlankBoderStyle <- createStyle(border = "right")
  cornerRightBottomBlankStyle <- createStyle(border = c("right","bottom"))

  ################################################################################
  # Write the first sheet = table for each sample
  openxlsx::writeData(wb, 1, config$appname, startRow = 1)
  openxlsx::writeData(wb, 1, paste("Sequence"), startRow = 2)
  addStyle(wb, sheet = 1, hStyle, rows = c(1,2), cols = 1)
  setColWidths(wb, sheet = 1, cols = 1, widths = 4)

  openxlsx::writeData(wb, 1, paste("User"), startRow = 4, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Sequence"),startRow = 5, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Comments"),startRow = 6, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Creation date"),startRow = 7, startCol = 2)
  openxlsx::writeData(wb, 1, paste("Last modified"),startRow = 8, startCol = 2)
  setColWidths(wb, sheet = 1, cols = 2, widths = 50)

  openxlsx::writeData(wb, 1, user, startRow = 4, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$name, startRow = 5, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$comments, startRow = 6, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$creation, startRow = 7, startCol = 3)
  openxlsx::writeData(wb, 1, project_informations$modified, startRow = 8, startCol = 3)
  addStyle(wb, sheet = 1, tableStyle, rows = 4:8, cols = 3)
  setColWidths(wb, sheet = 1, cols = 3, widths = 50)

  openxlsx::writeData(wb, 1, c("File"), startRow = 10, startCol = 2)
  openxlsx::writeData(wb, 1, c("Label"), startRow = 10, startCol = 3)
  openxlsx::writeData(wb, 1, c("Polarity"), startRow = 10, startCol = 4)
  openxlsx::writeData(wb, 1, c("m/z Range"), startRow = 10, startCol = 5)
  openxlsx::writeData(wb, 1, c("Size (MB)"), startRow = 10, startCol = 6)
  openxlsx::writeData(wb, 1, c("Instrument model"), startRow = 10, startCol = 7)
  setColWidths(wb, sheet = 1, cols = 7, widths = 20)
  openxlsx::writeData(wb, 1, c("Ion source"), startRow = 10, startCol = 8)
  setColWidths(wb, sheet = 1, cols = 8, widths = 30)
  openxlsx::writeData(wb, 1, c("Original path"), startRow = 10, startCol = 9)
  addStyle(wb, sheet = 1, hTableStyle, rows = 10, cols = 2:9)
  setColWidths(wb, sheet = 1, cols = 9, widths = 100)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  line <- 11
  for(smpl in allFiles$sample){
    sample_info <- samples()[which(samples()$sample == smpl),]
    openxlsx::writeData(wb, 1, sample_info$sample, startRow = line, startCol = 2)
    openxlsx::writeData(wb, 1, allFiles[which(allFiles$sample == smpl), "sample_id"], startRow = line, startCol = 3)
    openxlsx::writeData(wb, 1, sample_info$polarity, startRow = line, startCol = 4)
    openxlsx::writeData(wb, 1, "????", startRow = line, startCol = 5) ##### a modifier (mz range) non inclus (thermorawdump lors de l'ajout des fichiers?)
    openxlsx::writeData(wb, 1, paste0(sample_info$size," MB"), startRow = line, startCol = 6)
    openxlsx::writeData(wb, 1, sample_info$instrument_model, startRow = line, startCol = 7)
    openxlsx::writeData(wb, 1, sample_info$ion_source, startRow = line, startCol = 8)
    openxlsx::writeData(wb, 1, sample_info$raw_path, startRow = line, startCol = 9)
    addStyle(wb, sheet = 1, bodyTableStyle, rows = line, cols = 2:9)
    line <- line + 1
  }

  ################################################################################
  # Write the second sheet
  openxlsx::writeData(wb, 2, config$appname, startRow = 1)
  openxlsx::writeData(wb, 2, paste("Parameters"), startRow = 2)
  addStyle(wb, 2, hStyle, rows = c(1,2), cols = 1)
  setColWidths(wb, 2, cols = 1, widths = 4)
  addStyle(wb, 2, sh2DisplayStyle1, rows = 27, cols = 1)
  addStyle(wb, 2, sh2DisplayStyle2, rows = 28, cols = 1)

  openxlsx::writeData(wb, 2, "General", startRow = 4, startCol = 2)
  openxlsx::writeData(wb, 2, c("Mass tolerance","Instrument","Peakwidth","Retention time","Missing scans"), startRow = 5, startCol = 2)
  openxlsx::writeData(wb, 2, "Target analyte", startRow = 11, startCol = 2)
  openxlsx::writeData(wb, 2, c("Family","Adduct"), startRow = 12, startCol = 2)
  openxlsx::writeData(wb, 2, "Standard", startRow = 15, startCol = 2)
  openxlsx::writeData(wb, 2, c("Formula","Adduct","Retention time 1","Retention time 2"), startRow = 16, startCol = 2)
  openxlsx::writeData(wb, 2, "Deconvolution process", startRow = 21, startCol = 2)
  openxlsx::writeData(wb, 2, c("Start time","Computer","Duration"), startRow = 22, startCol = 2)
  openxlsx::writeData(wb, 2, "Display format", startRow = 26, startCol = 2)
  openxlsx::writeData(wb, 2, "Out of m/z range, or number of halogen higher than number of carbon plus 3, or not processed", startRow = 27, startCol = 2)
  openxlsx::writeData(wb, 2, "Stadding (at least the 2 most intense isotopomer groups)", startRow = 28, startCol = 2)
  openxlsx::writeData(wb, 2, "Score threshold", startRow = 29, startCol = 2)
  openxlsx::writeData(wb, 2, "Deviation tolerance (+/- mDa)", startRow = 30, startCol = 2)
  addStyle(wb, 2, boldStyle, rows = c(4,11,15,21,26), cols = 2)
  setColWidths(wb, 2, cols = 2, widths = 25)

  # Mass tolerance
  decParams <- deconvolution_params()[grep("PXAs", deconvolution_params()$chemical_type),]
  if(decParams$ppm > 0){
    openxlsx::writeData(wb, 2, paste0(decParams$ppm," ppm"), startCol = 3, startRow = 5)
  }else if(decParams$mda > 0){
    openxlsx::writeData(wb, 2, paste0(decParams$mda," mDa"), startCol = 3, startRow = 5)
  }
  if(decParams$instrument == "Orbitrap"){
    openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution/1000,"k@",decParams$resolution_mz), startCol = 3, startRow = 6)
  }else if(grep("ToF", decParams$instrument)){
    openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution), startCol = 3, startRow = 6)
  }
  # Peakwidth
  openxlsx::writeData(wb, 2, paste0(decParams$peakwidth_min," to ",decParams$peakwidth_max," s"), startCol = 3, startRow = 7)
  # Retention time
  openxlsx::writeData(wb, 2, paste0(decParams$retention_time_min," to ",decParams$retention_time_max," min"), startCol = 3, startRow = 8)
  # Missing scans
  openxlsx::writeData(wb, 2, decParams$missing_scans, startCol = 3, startRow = 9)
  # Chemical type
  openxlsx::writeData(wb, 2, decParams$chemical_type, startCol = 3, startRow = 12)
  # Adduct(s)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
  if(unique(allSamples$polarity == "negative")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(decParams$adduct)," ]-"), startCol = 3, startRow = 13)
  }else if(unique(allSamples$polarity == "positive")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(decParams$adduct)," ]+"), startCol = 3, startRow = 13)
  }else{
    openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
  }
  #################################
  # Standard(s) information
  std <- deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"]
  std <- std[-which(std %in% c("PCAs", "PBAs", "PCOs", "PCdiOs"))]
  std <- std[-grep("PXAs", std)]
  usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% std),]
  myStd <- NULL
  for(s in 1:nrow(usedStd)){
    if(s < nrow(usedStd)){
      myStd <- paste0(myStd, paste0(usedStd$chemical_type[s],";"))
    }else{
      myStd <- paste0(myStd, usedStd$chemical_type[s])
    }
  }
  openxlsx::writeData(wb, 2, myStd, startCol = 3, startRow = 16)
  # Standard adduct(s)
  allFiles <- project_samples()[which(project_samples()$project == input$project),]
  allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
  if(unique(allSamples$polarity == "negative")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(usedStd$adduct)," ]-"), startCol = 3, startRow = 17)
  }else if(unique(allSamples$polarity == "positive")){
    openxlsx::writeData(wb, 2, paste0("[ ",unique(usedStd$adduct)," ]+"), startCol = 3, startRow = 17)
  }else{
    openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 17)
  }
  # Standard RT 1 (given by user)
  stdInfo <- deconvolution_params()[which(deconvolution_params()$project == input$project), ]
  stdInfo <- stdInfo[-which(stdInfo$chemical_type %in% c("PCAs", "PCOs", "PCdiOs")),]
  stdInfo <- stdInfo[-grep("PXAs", stdInfo$chemical_type),]
  openxlsx::writeData(wb, 2, paste0(mean(c(stdInfo$retention_time_min[which(stdInfo$chemical_type == std[1])], 
                                          stdInfo$retention_time_max[which(stdInfo$chemical_type == std[1])]))," min"), 
                      startCol = 3, startRow = 18)
  # Standard RT 2 (given by user)
  openxlsx::writeData(wb, 2, paste0(mean(c(stdInfo$retention_time_min[which(stdInfo$chemical_type == std[2])], 
                                          stdInfo$retention_time_max[which(stdInfo$chemical_type == std[2])]))," min"), 
                      startCol = 3, startRow = 19)
  #################################
  openxlsx::writeData(wb, 2, "time when started", startCol = 3, startRow = 22) # surement à créer
  openxlsx::writeData(wb, 2, "computer name", startCol = 3, startRow = 23) # surement à créer
  openxlsx::writeData(wb, 2, "time of duration", startCol = 3, startRow = 24) # surement à créer
  openxlsx::writeData(wb, 2, "score of threshold", startCol = 3, startRow = 29) # pas compris
  openxlsx::writeData(wb, 2, "tolerance in deviation", startCol = 3, startRow = 30) # pas compris
  setColWidths(wb, 2, cols = 3, widths = 50)
  addStyle(wb, 2, sh2TableStyle, rows = c(5:9,12:13,16:19,22:24), cols = 3)
  addStyle(wb, 2, sh2EndTableStyle, rows = 29:30, cols = 3)

  openxlsx::writeData(wb, 2, "Possibly includes processing of other family and adduct types", startCol = 4, startRow = 24)
  openxlsx::writeData(wb, 2, "Number in grey police if below", startCol = 4, startRow = 29)
  openxlsx::writeData(wb, 2, "Number in grey police if outside", startCol = 4, startRow = 30)
  addStyle(wb, 2, italicStyle, rows = 24, cols = 4)
  addStyle(wb, 2, sh2LegendStyle, rows = 29:30, cols = 4)

    ################################################################################
  # Write the third sheet X times with X is the number of standard
  sheet <- 3
  for(s in std){
    # Add one sheet per standard
    addWorksheet(wb=wb, sheetName=paste0('Standard',sheet-2), gridLines=FALSE)
    # Write the sheet
    openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
    usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type == s),]
    openxlsx::writeData(wb, sheet, usedStd$chemical_type, startRow = 2)
    allFiles <- project_samples()[which(project_samples()$project == input$project),]
    allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
    if(unique(allSamples$polarity == "negative")){
      openxlsx::writeData(wb, sheet, paste0("[ ",unique(usedStd$adduct)," ]-"), startRow = 3)
    }else if(unique(allSamples$polarity == "positive")){
      openxlsx::writeData(wb, sheet, paste0("[ ",unique(usedStd$adduct)," ]+"), startRow = 3)
    }else{
      openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
    }
    addStyle(wb, sheet, hStyle, rows = 1:3, cols = 1)
    setColWidths(wb, sheet, cols = 1, widths = 4)

    openxlsx::writeData(wb, sheet, "File", startCol = 2, startRow = 5)
    setColWidths(wb, sheet, cols = 2, widths = 50)
    openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
    setColWidths(wb, sheet, cols = 3, widths = 50)
    openxlsx::writeData(wb, sheet, "Total area", startCol = 4, startRow = 5)
    openxlsx::writeData(wb, sheet, "Noise", startCol = 5, startRow = 5)
    setColWidths(wb, sheet, cols = 5, widths = 6)
    openxlsx::writeData(wb, sheet, "Score (%)", startCol = 6, startRow = 5)
    openxlsx::writeData(wb, sheet, "Deviation (mDa)", startCol = 7, startRow = 5)
    setColWidths(wb, sheet, cols = 7, widths = 20)
    addStyle(wb, sheet, hTableStyle, rows = 5, cols = 2:7)
    

    allFiles <- project_samples()[which(project_samples()$project == input$project),]
    line <- 6
    for(smpl in allFiles$sample){
      thisSmpl <- allFiles[which(allFiles$sample == smpl),]
      # Sample ID
      openxlsx::writeData(wb, sheet, thisSmpl$sample_id, startCol = 2, startRow = line)
      # Sample label (as named by user)
      openxlsx::writeData(wb, sheet, allFiles[which(allFiles$sample == smpl),"sample_id"], startCol = 3, startRow = line)
      # Total area
      query <- sprintf('select chemical_type, adduct from deconvolution_param where project == %s and
      chemical_type in (select formula from chemical where chemical_type == "Standard");',
      input$project)
      standard <- db_get_query(db, query)
      table_params <- list(
        standard = unique(standard[which(unique(standard$chemical_type) == s), "chemical_type"]),
        adduct = unique(standard[which(unique(standard$chemical_type) == s), "adduct"])
      )
      table <- get_standard_table(db, input$project, table_params$adduct, table_params$standard)
      table <- table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),]
      openxlsx::writeData(wb, sheet, table$'total area', startCol = 4, startRow = line)
      addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:7)
      # Is there noise ? Calculate with total area ABOVE baseline (script from home)
      if(table$'total area' > table$'area above baseline'){
        noiseParam <- "YES"
        openxlsx::writeData(wb, sheet, noiseParam, startCol = 5, startRow = line)
        addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
      }else{
        noiseParam <- "No"
        openxlsx::writeData(wb, sheet, noiseParam, startCol = 5, startRow = line)
        addStyle(wb, sheet, noiseStopStyle, rows = line, cols = 5)
      }
      # Score 
      openxlsx::writeData(wb, sheet, table$score, startCol = 6, startRow = line)
      # Deviation (mDa)
      openxlsx::writeData(wb, sheet, table$'deviation(mDa)', startCol = 7, startRow = line)
      line <- line + 1
    }
    sheet <- sheet + 1
  }

  ################################################################################
  # Write the fourth sheet
  openxlsx::writeData(wb, 4, paste("CP-Seeker Version"), startRow = 1)
  openxlsx::writeData(wb, 4, paste("Label"), startRow = 2)
  openxlsx::writeData(wb, 4, paste("Name of the adduct"), startRow = 3)
  addStyle(wb, 4, hStyle, rows = 1:1000, cols = 1)
  setColWidths(wb, 4, cols = 1:1000, widths = 4.5)
  setRowHeights(wb, 4, rows = 1:1000, heights = 15)
  openxlsx::writeData(wb, 4, "Area (x1,000,000)", startRow = 4, startCol = 3)
  addStyle(wb, 4, boldStyle, rows = 4, cols = 3)
  openxlsx::writeData(wb, 4, "Score (%)", startRow = 4, startCol = 34)
  addStyle(wb, 4, boldStyle, rows = 4, cols = 34)
  openxlsx::writeData(wb, 4, "Deviation (mDa)", startRow = 4, startCol = 66)
  addStyle(wb, 4, boldStyle, rows = 4, cols = 66)

  previousEnd <- 4
  for(i in 5:35){
   
    # Title of tables
    openxlsx::writeData(wb, 4, paste0("C",i+1,"-PXAs"), startRow = previousEnd+1)
    
    # Rownames of tables
    lineNames <- c()
    if(i+4 > 30){
      for(nb in 0:30){
        lineNames <- c(lineNames, paste0("BR",nb))
      }
    }else{
      for(nb in 0:(i+4)){
        lineNames <- c(lineNames, paste0("BR",nb))
      }
    }
    
    # Table 1
    openxlsx::writeData(wb ,4, lineNames, startRow = previousEnd+3, startCol = 2)
    addStyle(wb, 4, topBorderStyle, rows = previousEnd+3, cols = 2)
    addStyle(wb, 4, bottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 2)
    for(r in (previousEnd+4):(previousEnd+4+length(lineNames)-3)){
      addStyle(wb, 4, middleBorderStyle, rows = r, cols = 2)
    }
    addStyle(wb, 4, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 3:(3+length(lineNames)-1))
    addStyle(wb, 4, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (3+length(lineNames)-1))
    addStyle(wb, 4, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (3+length(lineNames)-1))
    # Table 2
    openxlsx::writeData(wb, 4, lineNames, startRow = previousEnd+3, startCol = 34)
    addStyle(wb, 4, topBorderStyle, rows = previousEnd+3, cols = 34)
    addStyle(wb, 4, bottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 34)
    for(r in (previousEnd+4):(previousEnd+4+length(lineNames)-3)){
      addStyle(wb, 4, middleBorderStyle, rows = r, cols = 34)
    }
    addStyle(wb, 4, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 35:(35+length(lineNames)-1))
    addStyle(wb, 4, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (35+length(lineNames)-1))
    addStyle(wb, 4, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (35+length(lineNames)-1))
    # Table 3
    openxlsx::writeData(wb, 4, lineNames, startRow = previousEnd+3, startCol = 66)
    addStyle(wb, 4, topBorderStyle, rows = previousEnd+3, cols = 66)
    addStyle(wb, 4, bottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 66)
    for(r in (previousEnd+4):(previousEnd+4+length(lineNames)-3)){
      addStyle(wb, 4, middleBorderStyle, rows = r, cols = 66)
    }
    addStyle(wb, 4, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 67:(67+length(lineNames)-1))
    addStyle(wb, 4, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (67+length(lineNames)-1))
    addStyle(wb, 4, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (67+length(lineNames)-1))
    
    
    # Colnames of tables
    c1 <- 3
    c2 <- 35
    c3 <- 67
    for(c in 0:(length(lineNames)-1)){
      openxlsx::writeData(wb, 4, paste0("Cl",c), startRow = previousEnd+2, startCol = c1+c)
      openxlsx::writeData(wb, 4, paste0("Cl",c), startRow = previousEnd+2, startCol = c2+c)
      openxlsx::writeData(wb, 4, paste0("Cl",c), startRow = previousEnd+2, startCol = c3+c)
      if(c == 0){
        addStyle(wb, 4, startBorderStyle, rows = previousEnd+2, cols = c1+c)
        addStyle(wb, 4, startBorderStyle, rows = previousEnd+2, cols = c2+c)
        addStyle(wb, 4, startBorderStyle, rows = previousEnd+2, cols = c3+c)
      }else if(c == (length(lineNames)-1)){
        addStyle(wb, 4, endBorderStyle, rows = previousEnd+2, cols = c1+c)
        addStyle(wb, 4, endBorderStyle, rows = previousEnd+2, cols = c2+c)
        addStyle(wb, 4, endBorderStyle, rows = previousEnd+2, cols = c3+c)
      }else{
        addStyle(wb, 4, topMiddleBorderStyle, rows = previousEnd+2, cols = c1+c)
        addStyle(wb, 4, topMiddleBorderStyle, rows = previousEnd+2, cols = c2+c)
        addStyle(wb, 4, topMiddleBorderStyle, rows = previousEnd+2, cols = c3+c)
      }
    }
    previousEnd <- previousEnd+1+length(lineNames)+2
  }


  ################################################################################
  # Save the workbook
  saveWorkbook(wb, paste0(config_dir,"/",file,"_testPXA.xlsx"), overwrite = TRUE)
  #openXL("c:/Users/JSA/Documents/LABERCA/CP-Seeker_datatest/testPXA.xlsx")
}