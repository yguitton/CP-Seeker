# Export xlsx functions

export_PCA <- function(user, chem_type, adducts, project_informations, pbValue, output = ""){
  # Template PCAs & PBAs
  library(openxlsx)
  #library(XLConnect) # for some more functions
  for(chem in chem_type){
    for(adduct in adducts){
      shinyWidgets::updateProgressBar(session, id = "exportBar",
        value = (pbValue + 1)/8*100, 
        title = paste0("Exportation of ", chem, "..."))
      pbValue <- pbValue + 1
      print("######################################################################################")
      print(paste0("Run for ",chem," and ",adduct))
      if(adduct %in% deconvolution_params()[which(deconvolution_params()$chemical_type == chem),"adduct"]){
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
        tableNumberStyle <- createStyle(fgFill = "#def1fa", numFmt = "### ### ### ### ### ### ###")
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
        headerRightAlignStd <- createStyle(textDecoration = "bold", fgFill = "#bfbfbf", border = c("top", "bottom"), halign = "right")

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
          openxlsx::writeData(wb, 1, strsplit(sample_info$sample,"neg ")[[1]][2], startRow = line, startCol = 2)
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
        decParams <- deconvolution_params()[which(chem %in% deconvolution_params()$chemical_type),]
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
        openxlsx::writeData(wb, 2, chem, startCol = 3, startRow = 12)
        # Adduct(s)
        allFiles <- project_samples()[which(project_samples()$project == input$project),]
        allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
        if(unique(allSamples$polarity == "negative")){
          openxlsx::writeData(wb, 2, paste0("[ ",adduct," ]-"), startCol = 3, startRow = 13)
        }else if(unique(allSamples$polarity == "positive")){
          openxlsx::writeData(wb, 2, paste0("[ ",adduct," ]+"), startCol = 3, startRow = 13)
        }else{
          openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
        }
        #################################
        # Standard(s) information
        std <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"])
        std <- std[-which(std %in% c("PBAs"))]
        std <- std[-c(grep("PXAs", std),grep("PCAs",std),grep("Os",std))]
        usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% std),]
        myStd <- NULL
        for(s in 1:length(unique(usedStd$chemical_type))){
          if(s < length(unique(usedStd$chemical_type))){
            myStd <- paste0(myStd, paste0(unique(usedStd$chemical_type)[s],"; "))
          }else{
            myStd <- paste0(myStd, unique(usedStd$chemical_type)[s])
          }
        }
        openxlsx::writeData(wb, 2, myStd, startCol = 3, startRow = 16)
        # Standard adduct(s)
        allFiles <- project_samples()[which(project_samples()$project == input$project),]
        allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
        if(unique(allSamples$polarity == "negative")){
          myStdAdduct <- NULL
          for(a in 1:length(unique(usedStd$adduct))){
            if(a < length(unique(usedStd$adduct))){
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-; "))
            }else{
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-"))
            }
          }
          openxlsx::writeData(wb, 2, myStdAdduct, startCol = 3, startRow = 17)
        }else if(unique(allSamples$polarity == "positive")){
          myStdAdduct <- NULL
          for(a in 1:length(unique(usedStd$adduct))){
            if(a < length(unique(usedStd$adduct))){
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+; "))
            }else{
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+"))
            }
          }
          openxlsx::writeData(wb, 2, myStdAdduct, startCol = 3, startRow = 17)
        }else{
          openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 17)
        }
        # Standard RT 1 (given by user)
        stdInfo <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), ])
        stdInfo <- stdInfo[-which(stdInfo$chemical_type %in% c("PBAs")),]
        stdInfo <- stdInfo[-c(grep("PXAs", stdInfo$chemical_type),grep("PCAs",stdInfo$chemical_type),grep("Os",stdInfo$chemical_type)),]
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
          openxlsx::writeData(wb, sheet, unique(usedStd$chemical_type), startRow = 2)
          allFiles <- project_samples()[which(project_samples()$project == input$project),]
          allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
          if(unique(allSamples$polarity == "negative")){
            myStdAdduct <- NULL
            for(a in 1:length(unique(usedStd$adduct))){
              if(a < length(unique(usedStd$adduct))){
                myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-; "))
              }else{
                myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-"))
              }
            }
            openxlsx::writeData(wb, sheet, myStdAdduct, startRow = 3)
          }else if(unique(allSamples$polarity == "positive")){
            myStdAdduct <- NULL
            for(a in 1:length(unique(usedStd$adduct))){
              if(a < length(unique(usedStd$adduct))){
                myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+; "))
              }else{
                myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+"))
              }
            }
            openxlsx::writeData(wb, sheet, myStdAdduct, startRow = 3)
          }else{
            openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
          }
          addStyle(wb, sheet, hStyle, rows = 1:3, cols = 1)
          setColWidths(wb, sheet, cols = 1, widths = 4)

          openxlsx::writeData(wb, sheet, "File", startCol = 2, startRow = 5)
          setColWidths(wb, sheet, cols = 2, widths = 50)
          openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
          setColWidths(wb, sheet, cols = 3, widths = 50)
          openxlsx::writeData(wb, sheet, "Adduct", startCol = 4, startRow = 5)
          setColWidths(wb, sheet, cols = 4, widths = 15)
          openxlsx::writeData(wb, sheet, "Total area", startCol = 5, startRow = 5)
          setColWidths(wb, sheet, cols = 5, widths = 20)
          openxlsx::writeData(wb, sheet, "Noise", startCol = 6, startRow = 5)
          setColWidths(wb, sheet, cols = 6, widths = 6)
          openxlsx::writeData(wb, sheet, "Score (%)", startCol = 7, startRow = 5)
          setColWidths(wb, sheet, cols = 7, widths = 15)
          openxlsx::writeData(wb, sheet, "Deviation (mDa)", startCol = 8, startRow = 5)
          setColWidths(wb, sheet, cols = 8, widths = 20)
          addStyle(wb, sheet, hTableStyle, rows = 5, cols = 2:8)
          addStyle(wb, sheet, headerRightAlignStd, rows = 5, cols = c(5,7,8))
          
          allFiles <- project_samples()[which(project_samples()$project == input$project),]
          line <- 6
          for(stdAdduct in unique(usedStd$adduct)){
            for(smpl in allFiles$sample){
              thisSmpl <- allFiles[which(allFiles$sample == smpl),]
              # Sample ID
              openxlsx::writeData(wb, sheet, thisSmpl$sample_id, startCol = 2, startRow = line)
              # Sample label (as named by user)
              openxlsx::writeData(wb, sheet, allFiles[which(allFiles$sample == smpl),"sample_id"], startCol = 3, startRow = line)
              # Adduct
              openxlsx::writeData(wb, sheet, paste0("[",stdAdduct,"]"), startCol = 4, startRow = line)
              # Total area
              table <- get_standard_table(db, input$project, stdAdduct, s)
              table <- table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),]
              addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:8)
              # Is there noise ? Calculate with total area ABOVE baseline (script from home)
              if(is.na(table$'total area')){
                noiseParam <- "Not detected"
                openxlsx::writeData(wb, sheet, "Not detected", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 6)
                # Score 
                openxlsx::writeData(wb, sheet, "", startCol = 7, startRow = line)
                # Deviation (mDa)
                openxlsx::writeData(wb, sheet, "", startCol = 8, startRow = line)
              }else{
                totArea <- gsub(" ", "", table$'total area')
                aboveArea <- gsub(" ", "", table$'area above baseline')
                if(as.numeric(totArea) > as.numeric(aboveArea)){
                  openxlsx::writeData(wb, sheet, as.numeric(totArea), startCol = 5, startRow = line)
                  addStyle(wb, sheet, tableNumberStyle, rows = line, cols = 5)
                  noiseParam <- "YES"
                  openxlsx::writeData(wb, sheet, noiseParam, startCol = 6, startRow = line)
                  addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 6)
                }else{
                  openxlsx::writeData(wb, sheet, as.numeric(totArea), startCol = 5, startRow = line)
                  addStyle(wb, sheet, tableNumberStyle, rows = line, cols = 5)
                  noiseParam <- "No"
                  openxlsx::writeData(wb, sheet, noiseParam, startCol = 6, startRow = line)
                  addStyle(wb, sheet, noiseStopStyle, rows = line, cols = 6)
                }
               # Score 
                openxlsx::writeData(wb, sheet, table$score, startCol = 7, startRow = line)
                # Deviation (mDa)
                openxlsx::writeData(wb, sheet, table$'deviation(mDa)', startCol = 8, startRow = line)
              }
              line <- line + 1
            }
          }
          
          sheet <- sheet + 1
        }
        
        ################################################################################
        # Write the label's sheet(s)
        allFiles <- project_samples()[which(project_samples()$project == input$project),]
        decParams <- deconvolution_params()[which(chem %in% deconvolution_params()$chemical_type),]
        for(file in allFiles$sample_id){
          myActualFile <- allFiles[which(allFiles$sample_id == file),] 
          # Create the sheet of the file label
          addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

          openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
          openxlsx::writeData(wb, sheet, paste("Label"), startRow = 2) # cest quoi le file label ?
          mySample <- samples()[which(samples()$sample %in% myActualFile$sample),]
          if(unique(mySample$polarity == "negative")){
            openxlsx::writeData(wb, sheet, paste0("[",adduct,"]-"), startRow = 3)
          }else if(unique(mySample$polarity == "positive")){
            openxlsx::writeData(wb, sheet, paste0("[",adduct,"]+"), startRow = 3)
          }else{
            openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
          }
          openxlsx::writeData(wb, sheet, chem, startRow = 5)
          addStyle(wb, sheet, hStyle, rows = 1:5, cols = 1)
          setColWidths(wb, sheet, cols = 1:1000, widths = 4.5)
          setRowHeights(wb, sheet, rows = 1:1000, heights = 18)

          # Save the table with all values for this file 
          table <- get_profile_matrix(db, myActualFile$project_sample, adduct = decParams$adduct, chemical_type = chem)
          
          # Table 1 : area (x 1 M)
          openxlsx::writeData(wb, sheet, "Area (x1,000,000)", startRow = 4, startCol = 3)
          addStyle(wb, sheet, boldStyle, rows = 4, cols = 3)
          table1 <- as.data.frame(reduce_matrix(table, 2, na_empty = FALSE))
          openxlsx::writeData(wb, sheet, table1, startCol = 3, startRow = 6)
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 7, startCol = 2)
          for(i in 3:30){
            if(length(grep("PCAs",chem)) > 0){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i)
            }else if(chem == "PBAs"){
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
            if(length(grep("PCAs",chem)) > 0){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i+29)
            }else if(chem == "PBAs"){
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
            if(length(grep("PCAs",chem)) > 0){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i+58)
            }else if(chem == "PBAs"){
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
        saveWorkbook(wb, paste0(config_dir,"/",project_informations$name,"_",project_informations$creation,"_[",adduct,"]_",chem,".xlsx"), overwrite = TRUE) # add start time
        #openXL("c:/Users/JSA/Documents/LABERCA/CP-Seeker_datatest/testPCAs.xlsx") # to read it when tested
      }
    }
  }
}
  

export_PCO <- function(user, chem_type, adducts, project_informations, pbValue, output = ""){
  library(openxlsx)
  #library(XLConnect) # for some more functions
  for(adduct in adducts){
    shinyWidgets::updateProgressBar(session, id = "exportBar",
      value = (pbValue + 1)/8*100, 
      title = paste0("Exportation of PCOs ..."))
    pbValue <- pbValue + 1
    print("######################################################################################")
    print(paste0("Run for PCOs and ",adduct))
    if(adduct %in% deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),"adduct"]){
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
      tableNumberStyle <- createStyle(fgFill = "#def1fa", numFmt = "### ### ### ### ### ### ###")
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
      fullGreyCell <- createStyle(fgFill = "#878787")
      greyBottomBorderStyle <- createStyle(border = "bottom", fgFill = "#878787")
      greyRightBorderStyle <- createStyle(border = "right", fgFill = "#878787")
      greyCornerRightBottomStyle <- createStyle(border = c("right","bottom"), fgFill = "#878787")
      headerRightAlignStd <- createStyle(textDecoration = "bold", fgFill = "#bfbfbf", border = c("top", "bottom"), halign = "right")

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
        openxlsx::writeData(wb, 1, strsplit(sample_info$sample,"neg ")[[1]][2], startRow = line, startCol = 2)
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
      decParams <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
      decParams <- decParams[which(decParams$adduct == adduct),]
      if(unique(decParams$ppm) > 0){
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
      openxlsx::writeData(wb, 2, chem_type, startCol = 3, startRow = 12)
      # Adduct(s)
      allFiles <- project_samples()[which(project_samples()$project == input$project),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      if(unique(allSamples$polarity == "negative")){
        openxlsx::writeData(wb, 2, paste0("[ ",adduct," ]-"), startCol = 3, startRow = 13)
      }else if(unique(allSamples$polarity == "positive")){
        openxlsx::writeData(wb, 2, paste0("[ ",adduct," ]+"), startCol = 3, startRow = 13)
      }else{
        openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
      }
      #################################
      # Standard(s) information
      std <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"])
      std <- std[-which(std %in% c("PBAs"))]
      std <- std[-c(grep("PXAs", std),grep("PCAs",std),grep("Os",std))]
      usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% std),]
      myStd <- NULL
      for(s in 1:length(unique(usedStd$chemical_type))){
        if(s < length(unique(usedStd$chemical_type))){
          myStd <- paste0(myStd, paste0(unique(usedStd$chemical_type)[s],"; "))
        }else{
          myStd <- paste0(myStd, unique(usedStd$chemical_type)[s])
        }
      }
      openxlsx::writeData(wb, 2, myStd, startCol = 3, startRow = 16)
      # Standard adduct(s)
      allFiles <- project_samples()[which(project_samples()$project == input$project),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      if(unique(allSamples$polarity == "negative")){
        myStdAdduct <- NULL
        for(a in 1:length(unique(usedStd$adduct))){
          if(a < length(unique(usedStd$adduct))){
            myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-; "))
          }else{
            myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-"))
          }
        }
        openxlsx::writeData(wb, 2, myStdAdduct, startCol = 3, startRow = 17)
      }else if(unique(allSamples$polarity == "positive")){
        myStdAdduct <- NULL
        for(a in 1:length(unique(usedStd$adduct))){
          if(a < length(unique(usedStd$adduct))){
            myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+; "))
          }else{
            myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+"))
          }
        }
        openxlsx::writeData(wb, 2, myStdAdduct, startCol = 3, startRow = 17)
      }else{
        openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 17)
      }
      # Standard RT 1 (given by user)
      stdInfo <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), ])
      stdInfo <- stdInfo[-which(stdInfo$chemical_type %in% c("PBAs")),]
      stdInfo <- stdInfo[-c(grep("PXAs", stdInfo$chemical_type),grep("PCAs",stdInfo$chemical_type),grep("Os",stdInfo$chemical_type)),]
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
        openxlsx::writeData(wb, sheet, unique(usedStd$chemical_type), startRow = 2)
        allFiles <- project_samples()[which(project_samples()$project == input$project),]
        allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
        if(unique(allSamples$polarity == "negative")){
          myStdAdduct <- NULL
          for(a in 1:length(unique(usedStd$adduct))){
            if(a < length(unique(usedStd$adduct))){
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-; "))
            }else{
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-"))
            }
          }
          openxlsx::writeData(wb, sheet, myStdAdduct, startRow = 3)
        }else if(unique(allSamples$polarity == "positive")){
          myStdAdduct <- NULL
          for(a in 1:length(unique(usedStd$adduct))){
            if(a < length(unique(usedStd$adduct))){
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+; "))
            }else{
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+"))
            }
          }
          openxlsx::writeData(wb, sheet, myStdAdduct, startRow = 3)
        }else{
          openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
        }
        addStyle(wb, sheet, hStyle, rows = 1:3, cols = 1)
        setColWidths(wb, sheet, cols = 1, widths = 4)

        openxlsx::writeData(wb, sheet, "File", startCol = 2, startRow = 5)
        setColWidths(wb, sheet, cols = 2, widths = 50)
        openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
        setColWidths(wb, sheet, cols = 3, widths = 50)
        openxlsx::writeData(wb, sheet, "Adduct", startCol = 4, startRow = 5)
        setColWidths(wb, sheet, cols = 4, widths = 15)
        openxlsx::writeData(wb, sheet, "Total area", startCol = 5, startRow = 5)
        setColWidths(wb, sheet, cols = 5, widths = 20)
        openxlsx::writeData(wb, sheet, "Noise", startCol = 6, startRow = 5)
        setColWidths(wb, sheet, cols = 6, widths = 6)
        openxlsx::writeData(wb, sheet, "Score (%)", startCol = 7, startRow = 5)
        setColWidths(wb, sheet, cols = 7, widths = 15)
        openxlsx::writeData(wb, sheet, "Deviation (mDa)", startCol = 8, startRow = 5)
        setColWidths(wb, sheet, cols = 8, widths = 20)
        addStyle(wb, sheet, hTableStyle, rows = 5, cols = 2:8)
        addStyle(wb, sheet, headerRightAlignStd, rows = 5, cols = c(5,7,8))
          
        allFiles <- project_samples()[which(project_samples()$project == input$project),]
        line <- 6
        for(stdAdduct in unique(usedStd$adduct)){
          for(smpl in allFiles$sample){
            thisSmpl <- allFiles[which(allFiles$sample == smpl),]
            # Sample ID
            openxlsx::writeData(wb, sheet, thisSmpl$sample_id, startCol = 2, startRow = line)
            # Sample label (as named by user)
            openxlsx::writeData(wb, sheet, allFiles[which(allFiles$sample == smpl),"sample_id"], startCol = 3, startRow = line)
            # Adduct
            openxlsx::writeData(wb, sheet, paste0("[",stdAdduct,"]"), startCol = 4, startRow = line)
            # Total area
            table <- get_standard_table(db, input$project, stdAdduct, s)
            table <- table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),]
            addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:8)
            # Is there noise ? Calculate with total area ABOVE baseline (script from home)
            if(is.na(table$'total area')){
              noiseParam <- "Not detected"
              openxlsx::writeData(wb, sheet, "Not detected", startCol = 5, startRow = line)
              addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
              openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
              addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 6)
              # Score 
              openxlsx::writeData(wb, sheet, "", startCol = 7, startRow = line)
              # Deviation (mDa)
              openxlsx::writeData(wb, sheet, "", startCol = 8, startRow = line)
            }else{
              totArea <- gsub(" ", "", table$'total area')
              aboveArea <- gsub(" ", "", table$'area above baseline')
              if(as.numeric(totArea) > as.numeric(aboveArea)){
                openxlsx::writeData(wb, sheet, as.numeric(totArea), startCol = 5, startRow = line)
                addStyle(wb, sheet, tableNumberStyle, rows = line, cols = 5)
                noiseParam <- "YES"
                openxlsx::writeData(wb, sheet, noiseParam, startCol = 6, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 6)
              }else{
                openxlsx::writeData(wb, sheet, as.numeric(totArea), startCol = 5, startRow = line)
                addStyle(wb, sheet, tableNumberStyle, rows = line, cols = 5)
                noiseParam <- "No"
                openxlsx::writeData(wb, sheet, noiseParam, startCol = 6, startRow = line)
                addStyle(wb, sheet, noiseStopStyle, rows = line, cols = 6)
              }
             # Score 
              openxlsx::writeData(wb, sheet, table$score, startCol = 7, startRow = line)
              # Deviation (mDa)
              openxlsx::writeData(wb, sheet, table$'deviation(mDa)', startCol = 8, startRow = line)
            }
            line <- line + 1
          }
        }
        sheet <- sheet + 1
      }
      ################################################################################
      # Write the label's sheet(s)
      allFiles <- project_samples()[which(project_samples()$project == input$project),]
      decParams <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
      decParams <- decParams[which(decParams$adduct == adduct),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]

      for(file in allFiles$sample_id){
        myActualFile <- allFiles[which(allFiles$sample_id == file),] 
        # Create the sheet of the file label
        addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

        openxlsx::writeData(wb, sheet, paste("CP-Seeker Version"), startRow = 1)
        openxlsx::writeData(wb, sheet, paste("Label"), startRow = 2)
        mySample <- samples()[which(samples()$sample %in% myActualFile$sample),]
        if(unique(mySample$polarity == "negative")){
          openxlsx::writeData(wb, sheet, paste0("[",adduct,"]-"), startRow = 3)
        }else if(unique(mySample$polarity == "positive")){
          openxlsx::writeData(wb, sheet, paste0("[",adduct,"]+"), startRow = 3)
        }else{
          openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
        }
        openxlsx::writeData(wb, sheet, "PCOs", startRow = 5)
        openxlsx::writeData(wb, sheet, "PCdiOs", startRow = 39)
        openxlsx::writeData(wb, sheet, "PCtriOs", startRow = 73)
        addStyle(wb, sheet, hStyle, rows = 1:73, cols = 1)
        setColWidths(wb, sheet, cols = 1:1000, widths = 4.5)
        setRowHeights(wb, sheet, rows = 1:1000, heights = 18)

        for(chem in chem_type){
          # Save the table with all values for this file 
          table <- get_profile_matrix(db, myActualFile$project_sample, adduct = adduct, chemical_type = chem)
          openxlsx::writeData(wb, sheet, "Area (x1,000,000)", startRow = 4, startCol = 3)
          addStyle(wb, sheet, boldStyle, rows = 4, cols = 3)
          openxlsx::writeData(wb, sheet, "Score (%)", startRow = 4, startCol = 32)
          addStyle(wb, sheet, boldStyle, rows = 4, cols = 32)
          openxlsx::writeData(wb, sheet, "Deviation (mDa)", startRow = 4, startCol = 61)
          addStyle(wb, sheet, boldStyle, rows = 4, cols = 61)
          # Table 1
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 7, startCol = 2)
          for(i in 3:30){
            openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i)
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
          # Table 2
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 7, startCol = 31)
          for(i in 3:30){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i+29)
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
          # Table 3
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 7, startCol = 60)
          for(i in 3:30){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 6, startCol = i+58)
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
          if(chem == "PCOs"){
            print("There is PCOs")
            # Table 1
            table1 <- as.data.frame(reduce_matrix(table, 2, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table1, startCol = 3, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 3:30)
            # Table 2
            table2 <- as.data.frame(reduce_matrix(table, 1, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table2, startCol = 32, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 32:59)
            # Table 3
            table3 <- as.data.frame(reduce_matrix(table, 3, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table3, startCol = 61, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 7:37, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 37, cols = 88)
          }else{
            # Grey all tables
            # Table 1
            addStyle(wb, sheet, fullGreyCell, rows = 7:37, cols = 3:30, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 37, cols = 3:30)
            # Table 2
            addStyle(wb, sheet, fullGreyCell, rows = 7:37, cols = 32:59, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 37, cols = 32:59)
            # Table 3
            addStyle(wb, sheet, fullGreyCell, rows = 7:37, cols = 61:88, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 37, cols = 61:88)
            addStyle(wb, sheet, greyRightBorderStyle, rows = 7:37, cols = 88)
            addStyle(wb, sheet, greyCornerRightBottomStyle, rows = 37, cols = 88)
          }
          # Table 4
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 41, startCol = 2)
          for(i in 3:30){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 40, startCol = i)
            if(i == 3){
              addStyle(wb, sheet, startBorderStyle, rows = 40, cols = i)
            }else if(i == 30){
              addStyle(wb, sheet, endBorderStyle, rows = 40, cols = i)
            }else{
              addStyle(wb, sheet, topMiddleBorderStyle, rows = 40, cols = i)
            }
          }
          addStyle(wb, sheet, topBorderStyle, rows = 41, cols = 2)
          addStyle(wb, sheet, bottomBorderStyle, rows = 71, cols = 2)
          for(r in 42:70){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 2)
          }
          # Table 5
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 41, startCol = 31)
          for(i in 3:30){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 40, startCol = i+29)
            if(i == 3){
              addStyle(wb, sheet, startBorderStyle, rows = 40, cols = i+29)
            }else if(i == 30){
              addStyle(wb, sheet, endBorderStyle, rows = 40, cols = i+29)
            }else{
              addStyle(wb, sheet, topMiddleBorderStyle, rows = 40, cols = i+29)
            }
          }
          addStyle(wb, sheet, topBorderStyle, rows = 41, cols = 31)
          addStyle(wb, sheet, bottomBorderStyle, rows = 71, cols = 31)
          for(r in 42:70){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 31)
          }
          # Table 6
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 41, startCol = 60)
          for(i in 3:30){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 40, startCol = i+58)
            if(i == 3){
              addStyle(wb, sheet, startBorderStyle, rows = 40, cols = i+58)
            }else if(i == 30){
              addStyle(wb, sheet, endBorderStyle, rows = 40, cols = i+58)
            }else{
              addStyle(wb, sheet, topMiddleBorderStyle, rows = 40, cols = i+58)
            }
          }
          addStyle(wb, sheet, topBorderStyle, rows = 41, cols = 60)
          addStyle(wb, sheet, bottomBorderStyle, rows = 71, cols = 60)
          for(r in 42:70){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 60)
          }
          if(chem == "PCdiOs"){
            print("There is PCdiOs")
            #Table 4
            table4 <- as.data.frame(reduce_matrix(table, 2, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table4, startCol = 3, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 3:30)
            # Table 5
            table5 <- as.data.frame(reduce_matrix(table, 1, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table5, startCol = 32, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 32:59)
            # Table 6
            table6 <- as.data.frame(reduce_matrix(table, 3, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table6, startCol = 61, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 41:71, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 71, cols = 88)
          }else{
            # Grey all tables
            # Table 4
            addStyle(wb, sheet, fullGreyCell, rows = 41:71, cols = 3:30, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 71, cols = 3:30)
            # Table 5
            addStyle(wb, sheet, fullGreyCell, rows = 41:71, cols = 32:59, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 71, cols = 32:59)
            # Table 6
            addStyle(wb, sheet, fullGreyCell, rows = 41:71, cols = 61:88, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 71, cols = 61:88)
            addStyle(wb, sheet, greyRightBorderStyle, rows = 41:71, cols = 88)
            addStyle(wb, sheet, greyCornerRightBottomStyle, rows = 71, cols = 88)
          }
          # Table 7
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 75, startCol = 2)
          for(i in 3:30){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 74, startCol = i)
            if(i == 3){
              addStyle(wb, sheet, startBorderStyle, rows = 74, cols = i)
            }else if(i == 30){
              addStyle(wb, sheet, endBorderStyle, rows = 74, cols = i)
            }else{
              addStyle(wb, sheet, topMiddleBorderStyle, rows = 74, cols = i)
            }
          }
          addStyle(wb, sheet, topBorderStyle, rows = 75, cols = 2)
          addStyle(wb, sheet, bottomBorderStyle, rows = 105, cols = 2)
          for(r in 76:104){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 2)
          }
          # Table 8
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 75, startCol = 31)
          for(i in 3:30){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 74, startCol = i+29)
            if(i == 3){
              addStyle(wb, sheet, startBorderStyle, rows = 74, cols = i+29)
            }else if(i == 30){
              addStyle(wb, sheet, endBorderStyle, rows = 74, cols = i+29)
            }else{
              addStyle(wb, sheet, topMiddleBorderStyle, rows = 74, cols = i+29)
            }
          }
          addStyle(wb, sheet, topBorderStyle, rows = 75, cols = 31)
          addStyle(wb, sheet, bottomBorderStyle, rows = 105, cols = 31)
          for(r in 76:104){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 31)
          }
          # Table 9
          openxlsx::writeData(wb, sheet, rep(paste0("C",6:36)), startRow = 75, startCol = 60)
          for(i in 3:30){
              openxlsx::writeData(wb, sheet, paste0("Cl",i), startRow = 74, startCol = i+58)
            if(i == 3){
              addStyle(wb, sheet, startBorderStyle, rows =74, cols = i+58)
            }else if(i == 30){
              addStyle(wb, sheet, endBorderStyle, rows = 74, cols = i+58)
            }else{
              addStyle(wb, sheet, topMiddleBorderStyle, rows = 74, cols = i+58)
            }
          }
          addStyle(wb, sheet, topBorderStyle, rows = 75, cols = 60)
          addStyle(wb, sheet, bottomBorderStyle, rows = 105, cols = 60)
          for(r in 76:104){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 60)
          }
          if(chem == "PCtriOs"){
            print("There is PCtriOs")
            # Table 7
            table7 <- as.data.frame(reduce_matrix(table, 2, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table7, startCol = 3, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 3:30)
            # Table 8
            table8 <- as.data.frame(reduce_matrix(table, 1, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table8, startCol = 32, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 32:59)
            # Table 9
            table9 <- as.data.frame(reduce_matrix(table, 3, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table9, startCol = 61, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 75:105, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 105, cols = 88)
          }else{
            # Grey all tables
            # Table 7
            addStyle(wb, sheet, fullGreyCell, rows = 75:105, cols = 3:30, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 105, cols = 3:30)
            # Table 8
            addStyle(wb, sheet, fullGreyCell, rows = 75:105, cols = 32:59, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 105, cols = 32:59)
            # Table 9
            addStyle(wb, sheet, fullGreyCell, rows = 75:105, cols = 61:88, gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = 105, cols = 61:88)
            addStyle(wb, sheet, greyRightBorderStyle, rows = 75:105, cols = 88)
            addStyle(wb, sheet, greyCornerRightBottomStyle, rows = 105, cols = 88)
          }
        }
        sheet <- sheet + 1
      }
      ################################################################################
      # Save the workbook
      saveWorkbook(wb, paste0(config_dir,"/",project_informations$name,"_",project_informations$creation,"_[",adduct,"]_PCOs.xlsx"), overwrite = TRUE)
      #openXL("c:/Users/JSA/Documents/LABERCA/CP-Seeker_datatest/testPCOs.xlsx")
    }
  }
}


export_PXA <- function(user, chem_type, adducts, project_informations, pbValue, output = ""){
  library(openxlsx)
  #library(XLConnect) # for some more functions
  for(adduct in adducts){
    shinyWidgets::updateProgressBar(session, id = "exportBar",
      value = (pbValue + 1)/8*100, 
      title = paste0("Exportation of PXAs ..."))
    pbValue <- pbValue + 1
    print("######################################################################################")
    print(paste0("Run for PXAs and ",adduct))
    if(adduct %in% deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),"adduct"]){
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
      tableNumberStyle <- createStyle(fgFill = "#def1fa", numFmt = "### ### ### ### ### ### ###")
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
      fullGreyCell <- createStyle(fgFill = "#878787")
      greyBottomBorderStyle <- createStyle(border = "bottom", fgFill = "#878787")
      greyRightBorderStyle <- createStyle(border = "right", fgFill = "#878787")
      greyCornerRightBottomStyle <- createStyle(border = c("right","bottom"), fgFill = "#878787")
      headerRightAlignStd <- createStyle(textDecoration = "bold", fgFill = "#bfbfbf", border = c("top", "bottom"), halign = "right")

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
        openxlsx::writeData(wb, 1, strsplit(sample_info$sample,"neg ")[[1]][2], startRow = line, startCol = 2)
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
      decParams <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
      decParams <- decParams[which(decParams$adduct == adduct),]
      if(decParams$ppm > 0){
        openxlsx::writeData(wb, 2, paste0(unique(decParams$ppm)," ppm"), startCol = 3, startRow = 5)
      }else if(decParams$mda > 0){
        openxlsx::writeData(wb, 2, paste0(unique(decParams$mda)," mDa"), startCol = 3, startRow = 5)
      }
      if(decParams$instrument == "Orbitrap"){
        openxlsx::writeData(wb, 2, paste0(unique(decParams$instrument)," ",unique(decParams$resolution)/1000,"k@",unique(decParams$resolution_mz)), startCol = 3, startRow = 6)
      }else if(grep("ToF", decParams$instrument)){
        openxlsx::writeData(wb, 2, paste0(unique(decParams$instrument)," ",unique(decParams$resolution)), startCol = 3, startRow = 6)
      }
      # Peakwidth
      openxlsx::writeData(wb, 2, paste0(unique(decParams$peakwidth_min)," to ",unique(decParams$peakwidth_max)," s"), startCol = 3, startRow = 7)
      # Retention time
      openxlsx::writeData(wb, 2, paste0(unique(decParams$retention_time_min)," to ",unique(decParams$retention_time_max)," min"), startCol = 3, startRow = 8)
      # Missing scans
      openxlsx::writeData(wb, 2, unique(decParams$missing_scans), startCol = 3, startRow = 9)
      # Chemical type
      saveChem <- ""
      listNum <- list()
      for(c in 1:length(chem_type)){
        chemC <- strsplit(chem_type[c], "-")[[1]][1]
        numSave <- strsplit(chemC, "C")[[1]][2]
        listNum <- c(listNum, numSave)
        if(c == 1){
          saveChem <- paste0("C", numSave)
        }else{
          if(numSave == (as.numeric(listNum[c-1]) + 1)){
            if(c == length(chem_type)){
              saveChem <- paste0(saveChem, " to C", numSave)
            }
          }else{
            print(paste0(as.numeric(listNum[c-1]) + 1, " != ", numSave))
            if(c == length(chem_type)){
              saveChem <- paste0(saveChem, ", C", numSave)
            }else{
              saveChem <- paste0(saveChem, " to C", listNum[c-1], ", C", numSave)
            }
          }
        }
      }
      openxlsx::writeData(wb, 2, paste0("PXAs (", saveChem, ")"), startCol = 3, startRow = 12)
      # Adduct(s)
      allFiles <- project_samples()[which(project_samples()$project == input$project),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      if(unique(allSamples$polarity == "negative")){
        openxlsx::writeData(wb, 2, paste0("[ ",adduct," ]-"), startCol = 3, startRow = 13)
      }else if(unique(allSamples$polarity == "positive")){
        openxlsx::writeData(wb, 2, paste0("[ ",adduct," ]+"), startCol = 3, startRow = 13)
      }else{
        openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
      }
      #################################
      # Standard(s) information
      std <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"])
      std <- std[-which(std %in% c("PBAs"))]
      std <- std[-c(grep("PXAs", std),grep("PCAs",std),grep("Os",std))]
      usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% std),]
      myStd <- NULL
      for(s in 1:length(unique(usedStd$chemical_type))){
        if(s < length(unique(usedStd$chemical_type))){
          myStd <- paste0(myStd, paste0(unique(usedStd$chemical_type)[s],"; "))
        }else{
          myStd <- paste0(myStd, unique(usedStd$chemical_type)[s])
        }
      }
      openxlsx::writeData(wb, 2, myStd, startCol = 3, startRow = 16)
      # Standard adduct(s)
      allFiles <- project_samples()[which(project_samples()$project == input$project),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      if(unique(allSamples$polarity == "negative")){
        myStdAdduct <- NULL
        for(a in 1:length(unique(usedStd$adduct))){
          if(a < length(unique(usedStd$adduct))){
            myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-; "))
          }else{
            myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-"))
          }
        }
        openxlsx::writeData(wb, 2, myStdAdduct, startCol = 3, startRow = 17)
      }else if(unique(allSamples$polarity == "positive")){
        myStdAdduct <- NULL
        for(a in 1:length(unique(usedStd$adduct))){
          if(a < length(unique(usedStd$adduct))){
            myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+; "))
          }else{
            myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+"))
          }
        }
        openxlsx::writeData(wb, 2, myStdAdduct, startCol = 3, startRow = 17)
      }else{
        openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 17)
      }
      # Standard RT 1 (given by user)
      stdInfo <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), ])
      stdInfo <- stdInfo[-which(stdInfo$chemical_type %in% c("PBAs")),]
      stdInfo <- stdInfo[-c(grep("PXAs", stdInfo$chemical_type),grep("PCAs",stdInfo$chemical_type),grep("Os",stdInfo$chemical_type)),]
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
        openxlsx::writeData(wb, sheet, unique(usedStd$chemical_type), startRow = 2)
        allFiles <- project_samples()[which(project_samples()$project == input$project),]
        allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
        if(unique(allSamples$polarity == "negative")){
          myStdAdduct <- NULL
          for(a in 1:length(unique(usedStd$adduct))){
            if(a < length(unique(usedStd$adduct))){
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-; "))
            }else{
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]-"))
            }
          }
          openxlsx::writeData(wb, sheet, myStdAdduct, startRow = 3)
        }else if(unique(allSamples$polarity == "positive")){
          myStdAdduct <- NULL
          for(a in 1:length(unique(usedStd$adduct))){
            if(a < length(unique(usedStd$adduct))){
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+; "))
            }else{
              myStdAdduct <- paste0(myStdAdduct, paste0("[",unique(usedStd$adduct)[a],"]+"))
            }
          }
          openxlsx::writeData(wb, sheet, myStdAdduct, startRow = 3)
        }else{
          openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
        }
        addStyle(wb, sheet, hStyle, rows = 1:3, cols = 1)
        setColWidths(wb, sheet, cols = 1, widths = 4)

        openxlsx::writeData(wb, sheet, "File", startCol = 2, startRow = 5)
        setColWidths(wb, sheet, cols = 2, widths = 50)
        openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
        setColWidths(wb, sheet, cols = 3, widths = 50)
        openxlsx::writeData(wb, sheet, "Adduct", startCol = 4, startRow = 5)
        setColWidths(wb, sheet, cols = 4, widths = 15)
        openxlsx::writeData(wb, sheet, "Total area", startCol = 5, startRow = 5)
        setColWidths(wb, sheet, cols = 5, widths = 20)
        openxlsx::writeData(wb, sheet, "Noise", startCol = 6, startRow = 5)
        setColWidths(wb, sheet, cols = 6, widths = 6)
        openxlsx::writeData(wb, sheet, "Score (%)", startCol = 7, startRow = 5)
        setColWidths(wb, sheet, cols = 7, widths = 15)
        openxlsx::writeData(wb, sheet, "Deviation (mDa)", startCol = 8, startRow = 5)
        setColWidths(wb, sheet, cols = 8, widths = 20)
        addStyle(wb, sheet, hTableStyle, rows = 5, cols = 2:8)
        addStyle(wb, sheet, headerRightAlignStd, rows = 5, cols = c(5,7,8))
        
        allFiles <- project_samples()[which(project_samples()$project == input$project),]
        line <- 6
        for(stdAdduct in unique(usedStd$adduct)){
          for(smpl in allFiles$sample){
            thisSmpl <- allFiles[which(allFiles$sample == smpl),]
            # Sample ID
            openxlsx::writeData(wb, sheet, thisSmpl$sample_id, startCol = 2, startRow = line)
            # Sample label (as named by user)
            openxlsx::writeData(wb, sheet, allFiles[which(allFiles$sample == smpl),"sample_id"], startCol = 3, startRow = line)
            # Adduct
            openxlsx::writeData(wb, sheet, paste0("[",stdAdduct,"]"), startCol = 4, startRow = line)
            # Total area
            table <- get_standard_table(db, input$project, stdAdduct, s)
            table <- table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),]
            addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:8)
            # Is there noise ? Calculate with total area ABOVE baseline (script from home)
            if(is.na(table$'total area')){
              noiseParam <- "Not detected"
              openxlsx::writeData(wb, sheet, "Not detected", startCol = 5, startRow = line)
              addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
              openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
              addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 6)
              # Score 
              openxlsx::writeData(wb, sheet, "", startCol = 7, startRow = line)
              # Deviation (mDa)
              openxlsx::writeData(wb, sheet, "", startCol = 8, startRow = line)
            }else{
              totArea <- gsub(" ", "", table$'total area')
              aboveArea <- gsub(" ", "", table$'area above baseline')
              if(as.numeric(totArea) > as.numeric(aboveArea)){
                openxlsx::writeData(wb, sheet, as.numeric(totArea), startCol = 5, startRow = line)
                addStyle(wb, sheet, tableNumberStyle, rows = line, cols = 5)
                noiseParam <- "YES"
                openxlsx::writeData(wb, sheet, noiseParam, startCol = 6, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 6)
              }else{
                openxlsx::writeData(wb, sheet, as.numeric(totArea), startCol = 5, startRow = line)
                addStyle(wb, sheet, tableNumberStyle, rows = line, cols = 5)
                noiseParam <- "No"
                openxlsx::writeData(wb, sheet, noiseParam, startCol = 6, startRow = line)
                addStyle(wb, sheet, noiseStopStyle, rows = line, cols = 6)
              }
             # Score 
              openxlsx::writeData(wb, sheet, table$score, startCol = 7, startRow = line)
              # Deviation (mDa)
              openxlsx::writeData(wb, sheet, table$'deviation(mDa)', startCol = 8, startRow = line)
            }
            line <- line + 1
          }
        }
        sheet <- sheet + 1
      }
      
      ################################################################################
      # Write the label's sheet(s)
      allFiles <- project_samples()[which(project_samples()$project == input$project),]
      decParams <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
      decParams <- decParams[which(decParams$adduct == adduct),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      
      for(file in allFiles$sample_id){
        myActualFile <- allFiles[which(allFiles$sample_id == file),] 
        # Create the sheet of the file label
        addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        openxlsx::writeData(wb, sheet, paste("Label"), startRow = 2)
        mySample <- samples()[which(samples()$sample %in% myActualFile$sample),]
        if(unique(mySample$polarity == "negative")){
          openxlsx::writeData(wb, sheet, paste0("[",adduct,"]-"), startRow = 3)
        }else if(unique(mySample$polarity == "positive")){
          openxlsx::writeData(wb, sheet, paste0("[",adduct,"]+"), startRow = 3)
        }else{
          openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
        }
        addStyle(wb, sheet, hStyle, rows = 1:1000, cols = 1)
        setColWidths(wb, sheet, cols = 1:1000, widths = 4.5)
        setRowHeights(wb, sheet, rows = 1:1000, heights = 15)
        openxlsx::writeData(wb, sheet, "Area (x1,000,000)", startRow = 4, startCol = 3)
        addStyle(wb, sheet, boldStyle, rows = 4, cols = 3)
        openxlsx::writeData(wb, sheet, "Score (%)", startRow = 4, startCol = 34)
        addStyle(wb, sheet, boldStyle, rows = 4, cols = 34)
        openxlsx::writeData(wb, sheet, "Deviation (mDa)", startRow = 4, startCol = 66)
        addStyle(wb, sheet, boldStyle, rows = 4, cols = 66)
        previousEnd <- 4
        for(i in 5:35){ # Should be better to search in DB all PXA and determine from which to which number they exist
          # That is for each PXA possible
          # Title of tables
          openxlsx::writeData(wb, sheet, paste0("C",i+1,"-PXAs"), startRow = previousEnd+1)
          table <- get_profile_matrix(db, myActualFile$project_sample, adduct = adduct, chemical_type = paste0("C",i+1,"-PXAs")) 
          # Rownames of tables
          lineNames <- c()
          if(i+4 > 30){
            for(nb in 0:30){
              lineNames <- c(lineNames, paste0("Br",nb))
            }
          }else{
            for(nb in 0:(i+4)){
              lineNames <- c(lineNames, paste0("Br",nb))
            }
          }
          openxlsx::writeData(wb , sheet, lineNames, startRow = previousEnd+3, startCol = 2)
          addStyle(wb, sheet, topBorderStyle, rows = previousEnd+3, cols = 2)
          addStyle(wb, sheet, bottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 2)
          for(r in (previousEnd+4):(previousEnd+4+length(lineNames)-3)){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 2)
          }
          openxlsx::writeData(wb, sheet, lineNames, startRow = previousEnd+3, startCol = 34)
          addStyle(wb, sheet, topBorderStyle, rows = previousEnd+3, cols = 34)
          addStyle(wb, sheet, bottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 34)
          for(r in (previousEnd+4):(previousEnd+4+length(lineNames)-3)){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 34)
          }
          openxlsx::writeData(wb, sheet, lineNames, startRow = previousEnd+3, startCol = 66)
          addStyle(wb, sheet, topBorderStyle, rows = previousEnd+3, cols = 66)
          addStyle(wb, sheet, bottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 66)
          for(r in (previousEnd+4):(previousEnd+4+length(lineNames)-3)){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 66)
          }

          # Colnames of tables
          c1 <- 3
          c2 <- 35
          c3 <- 67
          for(c in 0:(length(lineNames)-1)){
            openxlsx::writeData(wb, sheet, paste0("Cl",c), startRow = previousEnd+2, startCol = c1+c)
            openxlsx::writeData(wb, sheet, paste0("Cl",c), startRow = previousEnd+2, startCol = c2+c)
            openxlsx::writeData(wb, sheet, paste0("Cl",c), startRow = previousEnd+2, startCol = c3+c)
            if(c == 0){
              addStyle(wb, sheet, startBorderStyle, rows = previousEnd+2, cols = c1+c)
              addStyle(wb, sheet, startBorderStyle, rows = previousEnd+2, cols = c2+c)
              addStyle(wb, sheet, startBorderStyle, rows = previousEnd+2, cols = c3+c)
            }else if(c == (length(lineNames)-1)){
              addStyle(wb, sheet, endBorderStyle, rows = previousEnd+2, cols = c1+c)
              addStyle(wb, sheet, endBorderStyle, rows = previousEnd+2, cols = c2+c)
              addStyle(wb, sheet, endBorderStyle, rows = previousEnd+2, cols = c3+c)
            }else{
              addStyle(wb, sheet, topMiddleBorderStyle, rows = previousEnd+2, cols = c1+c)
              addStyle(wb, sheet, topMiddleBorderStyle, rows = previousEnd+2, cols = c2+c)
              addStyle(wb, sheet, topMiddleBorderStyle, rows = previousEnd+2, cols = c3+c)
            }
          }
          
          if(paste0("C",i+1,"-PXAs") %in% chem_type){
            # Table 1
            table1 <- as.data.frame(reduce_matrix(table, 2, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table1, startCol = c1, startRow = previousEnd+2)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 3:(3+length(lineNames)-1))
            addStyle(wb, sheet, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (3+length(lineNames)-1))
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (3+length(lineNames)-1))
            # Table 2
            table2 <- as.data.frame(reduce_matrix(table, 1, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table2, startCol = c2, startRow = previousEnd+2)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 35:(35+length(lineNames)-1))
            addStyle(wb, sheet, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (35+length(lineNames)-1))
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (35+length(lineNames)-1))
            # Table 3
            table3 <- as.data.frame(reduce_matrix(table, 3, na_empty = FALSE))
            openxlsx::writeData(wb, sheet, table3, startCol = c3, startRow = previousEnd+2)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 67:(67+length(lineNames)-1))
            addStyle(wb, sheet, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (67+length(lineNames)-1))
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (67+length(lineNames)-1))
          }else{
            # Grey all tables
            # Table 1
            addStyle(wb, sheet, fullGreyCell, rows = (previousEnd+3):(previousEnd+3+length(lineNames)-1), cols = 3:(3+length(lineNames)-1), gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 3:(3+length(lineNames)-1))
            addStyle(wb, sheet, greyRightBorderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (3+length(lineNames)-1))
            addStyle(wb, sheet, greyCornerRightBottomStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (3+length(lineNames)-1))
            #Table 2
            addStyle(wb, sheet, fullGreyCell, rows = (previousEnd+3):(previousEnd+3+length(lineNames)-1), cols = 35:(35+length(lineNames)-1), gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 35:(35+length(lineNames)-1))
            addStyle(wb, sheet, greyRightBorderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (35+length(lineNames)-1))
            addStyle(wb, sheet, greyCornerRightBottomStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (35+length(lineNames)-1))
            # Table 3
            addStyle(wb, sheet, fullGreyCell, rows = (previousEnd+3):(previousEnd+3+length(lineNames)-1), cols = 67:(67+length(lineNames)-1), gridExpand = TRUE)
            addStyle(wb, sheet, greyBottomBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 67:(67+length(lineNames)-1))
            addStyle(wb, sheet, greyRightBorderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (67+length(lineNames)-1))
            addStyle(wb, sheet, greyCornerRightBottomStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (67+length(lineNames)-1))
          }
          previousEnd <- previousEnd+1+length(lineNames)+2
        }
        sheet <- sheet + 1
      }
      ################################################################################
      # Save the workbook
      saveWorkbook(wb, paste0(config_dir,"/",project_informations$name,"_",project_informations$creation,"_[",adduct,"]_PXAs.xlsx"), overwrite = TRUE)
      #openXL("c:/Users/JSA/Documents/LABERCA/CP-Seeker_datatest/testPXA.xlsx")
    }
  }
}