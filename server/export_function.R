# Export xlsx functions

export_PCA <- function(user, maxBar, chem_type, adducts, project_informations, pbValue, output = ""){
  # Template PCAs & PBAs
  for(chem in chem_type){
    for(adduct in adducts){
      shinyWidgets::updateProgressBar(session, id = "exportBar",
      value = (pbValue + 1)/maxBar*100, 
      title = paste0("Exporting ", chem, "..."))
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
        noiseOrangeStyle <- createStyle(halign = "center", fgFill = "#def1fa", fontColour = "#f1a251", textDecoration = "bold")
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
        setColWidths(wb, sheet = 1, cols = 2, widths = 55)

        openxlsx::writeData(wb, 1, user, startRow = 4, startCol = 3)
        openxlsx::writeData(wb, 1, project_informations$name, startRow = 5, startCol = 3)
        openxlsx::writeData(wb, 1, project_informations$comments, startRow = 6, startCol = 3)
        openxlsx::writeData(wb, 1, project_informations$creation, startRow = 7, startCol = 3)
        openxlsx::writeData(wb, 1, project_informations$modified, startRow = 8, startCol = 3)
        addStyle(wb, sheet = 1, tableStyle, rows = 4:8, cols = 3)
        setColWidths(wb, sheet = 1, cols = 3, widths = 55)

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
          mzrange <- get_project_mz_range(db, input$project)
          openxlsx::writeData(wb, 1, paste0(round(mzrange[1], digits =0), " - ", round(mzrange[2], digits = 0)), startRow = line, startCol = 5) ##### a modifier (mz range) non inclus (thermorawdump lors de l'ajout des fichiers?)
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
        openxlsx::writeData(wb, 2, c("Start time","Duration (minutes)"), startRow = 22, startCol = 2)
        openxlsx::writeData(wb, 2, "Display format", startRow = 25, startCol = 2)
        openxlsx::writeData(wb, 2, "Out of m/z range, or number of halogen higher than number of carbon plus 3, or not processed", startRow = 26, startCol = 2)
        openxlsx::writeData(wb, 2, "Stadding (at least the 2 most intense isotopomer groups)", startRow = 27, startCol = 2)
        openxlsx::writeData(wb, 2, "Score threshold", startRow = 28, startCol = 2)
        openxlsx::writeData(wb, 2, "Deviation tolerance (\u00b1 mDa)", startRow = 29, startCol = 2)
        openxlsx::writeData(wb, 2, "Computer Hardware", startRow = 31, startCol = 2)
        openxlsx::writeData(wb, 2, c("Computer Manufacturer","Computer Model","Operating System","System Type", "CPU Manufacturer", "CPU Name", "Number of CPU Cores", "CPU Speed", "Installed Memory","RAM Speed"), startRow = 32, startCol = 2)
        addStyle(wb, 2, boldStyle, rows = c(4,11,15,21,25,31), cols = 2)
        setColWidths(wb, 2, cols = 2, widths = 25)

        # Mass tolerance
        decParams <- deconvolution_params()[which(deconvolution_params()$chemical_type == chem),]
        decParams <- decParams[which(decParams$project == as.numeric(input$project)),]
        decParams <- decParams[which(decParams$adduct == adduct),]
        if(decParams$ppm > 0){
          openxlsx::writeData(wb, 2, paste0("\u00b1 ", decParams$ppm," ppm"), startCol = 3, startRow = 5)
        }else if(decParams$mda > 0){
          openxlsx::writeData(wb, 2, paste0("\u00b1 ", decParams$mda," mDa"), startCol = 3, startRow = 5)
        }
        if(unique(decParams$instrument == "Orbitrap")){
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
          openxlsx::writeData(wb, 2, paste0("[",adduct,"]-"), startCol = 3, startRow = 13)
        }else if(unique(allSamples$polarity == "positive")){
          openxlsx::writeData(wb, 2, paste0("[",adduct,"]+"), startCol = 3, startRow = 13)
        }else{
          openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
        }
        #################################
        # Standard(s) information
        std <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"])
        # Search for all families we have
        family <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
        # Merge our type and their family
        std <- family[which(family$chemical_type %in% std),]
        std <- std[which(std$chemical_familly == "Standard"),]
        std <- std$chemical_type
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
        if(length(grep("PBAs", stdInfo$chemical_type)) > 0) stdInfo <- stdInfo[-grep("PBAs", stdInfo$chemical_typ),]
        stdInfo <- stdInfo[-c(grep("PXAs", stdInfo$chemical_type),grep("PCAs",stdInfo$chemical_type),grep("Os",stdInfo$chemical_type)),]
        openxlsx::writeData(wb, 2, paste0(mean(c(stdInfo$retention_time_min[which(stdInfo$chemical_type == std[1])], 
                                                stdInfo$retention_time_max[which(stdInfo$chemical_type == std[1])]))," min"), 
                            startCol = 3, startRow = 18)
        # Standard RT 2 (given by user)
        openxlsx::writeData(wb, 2, paste0(mean(c(stdInfo$retention_time_min[which(stdInfo$chemical_type == std[2])], 
                                                stdInfo$retention_time_max[which(stdInfo$chemical_type == std[2])]))," min"), 
                            startCol = 3, startRow = 19)
        #################################
        info_table <- get_infos(db, input$project)
        openxlsx::writeData(wb, 2, info_table$time_start, startCol = 3, startRow = 22)
        openxlsx::writeData(wb, 2, info_table$time_diff, startCol = 3, startRow = 23)
        openxlsx::writeData(wb, 2, paste0(as.numeric(80),"%"), startCol = 3, startRow = 28)
        openxlsx::writeData(wb, 2, as.numeric(2), startCol = 3, startRow = 29)
        openxlsx::writeData(wb, 2, info_table$computer_manufacturer, startCol = 3, startRow = 32)
        openxlsx::writeData(wb, 2, info_table$computer_model, startCol = 3, startRow = 33)
        openxlsx::writeData(wb, 2, info_table$os_info, startCol = 3, startRow = 34)
        openxlsx::writeData(wb, 2, info_table$system_type, startCol = 3, startRow = 35)
        openxlsx::writeData(wb, 2, info_table$cpu_manufacturer, startCol = 3, startRow = 36)
        openxlsx::writeData(wb, 2, info_table$processor_info, startCol = 3, startRow = 37)
        openxlsx::writeData(wb, 2, paste0(info_table$cpu_cores, " (unit: cores)"), startCol = 3, startRow = 38)
        openxlsx::writeData(wb, 2, paste0(info_table$cpu_speed, " (unit: MHz)"), startCol = 3, startRow = 39)
        openxlsx::writeData(wb, 2, paste0(info_table$memory_info, " (unit: GB)"), startCol = 3, startRow = 40)
        openxlsx::writeData(wb, 2, paste0(info_table$memory_speed, " (unit: MHz)"), startCol = 3, startRow = 41)

        setColWidths(wb, 2, cols = 3, widths = 55)
        addStyle(wb, 2, sh2TableStyle, rows = c(5:9,12:13,16:19,22:23,32:41), cols = 3)
        addStyle(wb, 2, sh2EndTableStyle, rows = 28:29, cols = 3)

        openxlsx::writeData(wb, 2, "Possibly includes processing of other family and adduct types", startCol = 4, startRow = 23)
        openxlsx::writeData(wb, 2, "Number in grey police if below", startCol = 4, startRow = 28)
        openxlsx::writeData(wb, 2, "Number in grey police if outside", startCol = 4, startRow = 29)
        addStyle(wb, 2, italicStyle, rows = 23, cols = 4)
        addStyle(wb, 2, sh2LegendStyle, rows = 28:29, cols = 4)

        ################################################################################
        # Write the third sheet X times with X is the number of standard
        sheet <- 3
        for(s in std){
          print(paste0("Standard : ", s))
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
          setColWidths(wb, sheet, cols = 2, widths = 55)
          openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
          setColWidths(wb, sheet, cols = 3, widths = 55)
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
              table <- unique(table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),])
              addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:8)
              # Is there noise ? Calculate with total area ABOVE baseline (script from home)
              if(is.na(table$'total area') || table$'total area' == 0){
                openxlsx::writeData(wb, sheet, "Not detected", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseOrangeStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
                # Score 
                openxlsx::writeData(wb, sheet, "", startCol = 7, startRow = line)
                # Deviation (mDa)
                openxlsx::writeData(wb, sheet, "", startCol = 8, startRow = line)
              }else if(length(grep("not possible", table$score)) > 0){
                openxlsx::writeData(wb, sheet, "Not possible", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
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
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
        decParams <- deconvolution_params()[which(deconvolution_params()$chemical_type == chem),]
        decParams <- decParams[which(decParams$project == as.numeric(input$project)),]
        decParams <- decParams[which(decParams$adduct == adduct),]
        for(file in unique(allFiles$sample_id)){
          myActualFile <- allFiles[which(allFiles$sample_id == file),] 
          # Create the sheet of the file label
          addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

          openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
          openxlsx::writeData(wb, sheet, file, startRow = 2) # cest quoi le file label ?
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
          table <- get_profile_matrix(db, myActualFile$project_sample, adduct = decParams$adduct, chemical_type = chem, export = TRUE)
          # Table 1 : area (x 1 M)
          openxlsx::writeData(wb, sheet, "Area (x1,000,000)", startRow = 4, startCol = 3)
          addStyle(wb, sheet, boldStyle, rows = 4, cols = 3)
          addStyle(wb, sheet, topBorderStyle, rows = 7, cols = 2)
          addStyle(wb, sheet, bottomBorderStyle, rows = 37, cols = 2)
          for(r in 8:36){
            addStyle(wb, sheet, middleBorderStyle, rows = r, cols = 2)
          }
          addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 3:30)
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
          table1ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
          table1Values <- as.data.frame(reduce_matrix(table1ALL, 1))
          openxlsx::writeData(wb, sheet, table1Values, startCol = 3, startRow = 6)
          table1Status <- as.data.frame(reduce_matrix(table1ALL, 2))
          for(col in 3:(ncol(table1Status)+2)){
            for(row in 7:(nrow(table1Status)+6)){
              if(table1Status[row-6,col-2] == "half"){
                addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
              }else if(table1Status[row-6,col-2] == "outside"){
                addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
              }
            }
          }
          
          # Table 2 : score %
          openxlsx::writeData(wb, sheet, "Score (%)", startRow = 4, startCol = 32)
          addStyle(wb, sheet, boldStyle, rows = 4, cols = 32)
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
          table2ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
          table2Values <- as.data.frame(reduce_matrix(table2ALL, 1))
          openxlsx::writeData(wb, sheet, table2Values, startCol = 32, startRow = 6)
          table2Status <- as.data.frame(reduce_matrix(table2ALL, 2))        
          for(col in 32:(ncol(table2Status)+31)){
            for(row in 7:(nrow(table2Status)+6)){
              if(table2Status[row-6,col-31] == "half"){
                addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
              }else if(table2Status[row-6,col-31] == "outside"){
                addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
              }
            }
          }

          # Table 3 : deviation (mDa) # penser à changer mDa ou ppm selon choix uilisateur !!
          openxlsx::writeData(wb, sheet, "Deviation (mDa)", startRow = 4, startCol = 61)
          addStyle(wb, sheet, boldStyle, rows = 4, cols = 61)
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
          table3ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
          table3Values <- as.data.frame(reduce_matrix(table3ALL, 1))
          openxlsx::writeData(wb, sheet, table3Values, startCol = 61, startRow = 6)
          table3Status <- as.data.frame(reduce_matrix(table3ALL, 2))        
          for(col in 61:(ncol(table3Status)+60)){
            for(row in 7:(nrow(table3Status)+6)){
              if(table3Status[row-6,col-60] == "half"){
                addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
              }else if(table3Status[row-6,col-60] == "outside"){
                addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
              }
            }
          }

          sheet <- sheet + 1
        }
        
        ################################################################################
        # Save the workbook
        saveWorkbook(wb, paste0(config_dir,"/",project_informations$name,"_",project_informations$creation,"_[",adduct,"]_",chem,".xlsx"), overwrite = TRUE) # add Start time
      }
    }
  }
}
  

export_PCO <- function(user, maxBar, chem_type, adducts, project_informations, pbValue, output = ""){
  allProj <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
  myProjDeconv <- allProj[which(allProj$project == project_informations$project),]
  for(adduct in adducts){
    shinyWidgets::updateProgressBar(session, id = "exportBar",
      value = (pbValue + 1)/maxBar*100,
      title = paste0("Exporting PCOs ..."))
    pbValue <- pbValue + 1
    print("######################################################################################")
    print(paste0("Run for PCOs and ",adduct))
    if(adduct %in% myProjDeconv$adduct){
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
      noiseOrangeStyle <- createStyle(halign = "center", fgFill = "#def1fa", fontColour = "#f1a251", textDecoration = "bold")
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
      setColWidths(wb, sheet = 1, cols = 2, widths = 55)

      openxlsx::writeData(wb, 1, user, startRow = 4, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$name, startRow = 5, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$comments, startRow = 6, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$creation, startRow = 7, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$modified, startRow = 8, startCol = 3)
      addStyle(wb, sheet = 1, tableStyle, rows = 4:8, cols = 3)
      setColWidths(wb, sheet = 1, cols = 3, widths = 55)

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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      line <- 11
      for(smpl in allFiles$sample){
        sample_info <- samples()[which(samples()$sample == smpl),]
        openxlsx::writeData(wb, 1, strsplit(sample_info$sample,"neg ")[[1]][2], startRow = line, startCol = 2)
        openxlsx::writeData(wb, 1, allFiles[which(allFiles$sample == smpl), "sample_id"], startRow = line, startCol = 3)
        openxlsx::writeData(wb, 1, sample_info$polarity, startRow = line, startCol = 4)
        mzrange <- get_project_mz_range(db, input$project)
        openxlsx::writeData(wb, 1, paste0(round(mzrange[1], digits =0), " - ", round(mzrange[2], digits = 0)), startRow = line, startCol = 5) ##### a modifier (mz range) non inclus (thermorawdump lors de l'ajout des fichiers?)
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
      openxlsx::writeData(wb, 2, c("Start time","Duration (minutes)"), startRow = 22, startCol = 2)
      openxlsx::writeData(wb, 2, "Display format", startRow = 25, startCol = 2)
      openxlsx::writeData(wb, 2, "Out of m/z range, or number of halogen higher than number of carbon plus 3, or not processed", startRow = 26, startCol = 2)
      openxlsx::writeData(wb, 2, "Stadding (at least the 2 most intense isotopomer groups)", startRow = 27, startCol = 2)
      openxlsx::writeData(wb, 2, "Score threshold", startRow = 28, startCol = 2)
      openxlsx::writeData(wb, 2, "Deviation tolerance (\u00b1 mDa)", startRow = 29, startCol = 2)
      openxlsx::writeData(wb, 2, "Computer Hardware", startRow = 31, startCol = 2)
      openxlsx::writeData(wb, 2, c("Computer Manufacturer","Computer Model","Operating System","System Type", "CPU Manufacturer", "CPU Name", "Number of CPU Cores", "CPU Speed", "Installed Memory","RAM Speed"), startRow = 32, startCol = 2)
      addStyle(wb, 2, boldStyle, rows = c(4,11,15,21,25,31), cols = 2)
      setColWidths(wb, 2, cols = 2, widths = 25)

      # Mass tolerance
      decParams <- myProjDeconv[which(myProjDeconv$adduct == adduct),]
      if(unique(decParams$ppm) > 0){
        openxlsx::writeData(wb, 2, paste0("\u00b1 ", decParams$ppm," ppm"), startCol = 3, startRow = 5)
      }else if(any(decParams$mda > 0)){
        openxlsx::writeData(wb, 2, paste0("\u00b1 ", decParams$mda," mDa"), startCol = 3, startRow = 5)
      }
      if(unique(decParams$instrument == "Orbitrap")){
        openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution/1000,"k@",decParams$resolution_mz), startCol = 3, startRow = 6)
      }else if(grep("ToF", decParams$instrument)){
        openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution), startCol = 3, startRow = 6)
      }
      # Peakwidth
      openxlsx::writeData(wb, 2, paste0(decParams$peakwidth_min," to ",decParams$peakwidth_max," s"), startCol = 3, startRow = 7)
      # Retention time
      if(length(unique(decParams$retention_time_min)) == 1 && length(unique(decParams$retention_time_max)) == 1){
        openxlsx::writeData(wb, 2, paste0(unique(decParams$retention_time_min)," to ",unique(decParams$retention_time_max)," min"), startCol = 3, startRow = 8)
      }else{
        if(length(unique(decParams$retention_time_min)) == 1){
          for(max in 1:length(unique(decParams$retention_time_max))){
            my_rt <- paste0(unique(decParams$retention_time_min), " to (", unique(decParams$retention_time_max), ") min")
          }
        }
        if(length(unique(decParams$retention_time_max)) == 1){
          for(min in 1:length(unique(decParams$retention_time_min))){
            my_rt <- paste0("(", unique(decParams$retention_time_min), ") to ", unique(decParams$retention_time_max), " min")
          }
        }
        openxlsx::writeData(wb, 2, my_rt, startCol = 3, startRow = 8)
      }
      # Missing scans
      if(length(unique(decParams$missing_scans)) == 1){
        openxlsx::writeData(wb, 2, unique(decParams$missing_scans), startCol = 3, startRow = 9)
      }else{
        miss_scan <- paste(decParams$missing_scans)
        openxlsx::writeData(wb, 2, miss_scan, startCol = 3, startRow = 9)
      }
      # Chemical type
      my_chemtype <- NULL
      whichone <- rep(0, 3)
      if(length(chem_type == 3)){
        my_chemtype <- "PCOs (mono, di, tri)"
      }else if(length(chem_type) == 1){
        my_chemtype <- chem_type
      }else{
        if(grep("PCOs", chem_type)) whichone[1] <- 1
        if(grep("di", chem_type)) whichone[2] <- 1
        if(grep("tri", chem_type)) whichone[3] <- 1
        if(whichone == c(1,1,0)) my_chemtype <- "PCOs (mono, di)"
        if(whichone == c(1,0,1)) my_chemtype <- "PCOs (mono, tri)"
        if(whichone == c(0,1,1)) my_chemtype <- "PCOs (di, tri)"
      }
      openxlsx::writeData(wb, 2, my_chemtype, startCol = 3, startRow = 12)
      # Adduct(s)
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      if(unique(allSamples$polarity == "negative")){
        openxlsx::writeData(wb, 2, paste0("[",adduct,"]-"), startCol = 3, startRow = 13)
      }else if(unique(allSamples$polarity == "positive")){
        openxlsx::writeData(wb, 2, paste0("[",adduct,"]+"), startCol = 3, startRow = 13)
      }else{
        openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
      }
      #################################
      # Standard(s) information
      std <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"])
      # Search for all families we have
      family <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
      # Merge our type and their family
      std <- family[which(family$chemical_type %in% std),]
      std <- std[which(std$chemical_familly == "Standard"),]
      std <- std$chemical_type
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
      info_table <- get_infos(db, input$project)
      openxlsx::writeData(wb, 2, info_table$time_start, startCol = 3, startRow = 22)
      openxlsx::writeData(wb, 2, info_table$time_diff, startCol = 3, startRow = 23)
      openxlsx::writeData(wb, 2, paste0(as.numeric(80),"%"), startCol = 3, startRow = 28)
      openxlsx::writeData(wb, 2, as.numeric(2), startCol = 3, startRow = 29)
      openxlsx::writeData(wb, 2, info_table$computer_manufacturer, startCol = 3, startRow = 32)
      openxlsx::writeData(wb, 2, info_table$computer_model, startCol = 3, startRow = 33)
      openxlsx::writeData(wb, 2, info_table$os_info, startCol = 3, startRow = 34)
      openxlsx::writeData(wb, 2, info_table$system_type, startCol = 3, startRow = 35)
      openxlsx::writeData(wb, 2, info_table$cpu_manufacturer, startCol = 3, startRow = 36)
      openxlsx::writeData(wb, 2, info_table$processor_info, startCol = 3, startRow = 37)
      openxlsx::writeData(wb, 2, paste0(info_table$cpu_cores, " (unit: cores)"), startCol = 3, startRow = 38)
      openxlsx::writeData(wb, 2, paste0(info_table$cpu_speed, " (unit: MHz)"), startCol = 3, startRow = 39)
      openxlsx::writeData(wb, 2, paste0(info_table$memory_info, " (unit: GB)"), startCol = 3, startRow = 40)
      openxlsx::writeData(wb, 2, paste0(info_table$memory_speed, " (unit: MHz)"), startCol = 3, startRow = 41)

      setColWidths(wb, 2, cols = 3, widths = 55)
      addStyle(wb, 2, sh2TableStyle, rows = c(5:9,12:13,16:19,22:23,32:41), cols = 3)
      addStyle(wb, 2, sh2EndTableStyle, rows = 28:29, cols = 3)

      openxlsx::writeData(wb, 2, "Possibly includes processing of other family and adduct types", startCol = 4, startRow = 23)
      openxlsx::writeData(wb, 2, "Number in grey police if below", startCol = 4, startRow = 28)
      openxlsx::writeData(wb, 2, "Number in grey police if outside", startCol = 4, startRow = 29)
      addStyle(wb, 2, italicStyle, rows = 23, cols = 4)
      addStyle(wb, 2, sh2LegendStyle, rows = 28:29, cols = 4)

      ################################################################################
      # Write the third sheet X times with X is the number of standard
      sheet <- 3
      for(s in std){
        print(paste0("Standard : ", s))
        # Add one sheet per standard
        addWorksheet(wb=wb, sheetName=paste0('Standard',sheet-2), gridLines=FALSE)
        # Write the sheet
        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type == s),]
        openxlsx::writeData(wb, sheet, unique(usedStd$chemical_type), startRow = 2)
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
        setColWidths(wb, sheet, cols = 2, widths = 55)
        openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
        setColWidths(wb, sheet, cols = 3, widths = 55)
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
          
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
            table <- unique(table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),])
            addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:8)
            if(is.na(table$'total area') || table$'total area' == 0){
                openxlsx::writeData(wb, sheet, "Not detected", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseOrangeStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
                # Score 
                openxlsx::writeData(wb, sheet, "", startCol = 7, startRow = line)
                # Deviation (mDa)
                openxlsx::writeData(wb, sheet, "", startCol = 8, startRow = line)
              }else if(length(grep("not possible", table$score)) > 0){
                openxlsx::writeData(wb, sheet, "Not possible", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      decParams <- myProjDeconv[which(myProjDeconv$adduct == adduct),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]

      for(file in unique(allFiles$sample_id)){
        myActualFile <- allFiles[which(allFiles$sample_id == file),] 
        # Create the sheet of the file label
        addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        openxlsx::writeData(wb, sheet, file, startRow = 2)
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
          table <- get_profile_matrix(db, myActualFile$project_sample, adduct = adduct, chemical_type = chem, export = TRUE)
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
            table1ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table1Values <- as.data.frame(reduce_matrix(table1ALL, 1))
            openxlsx::writeData(wb, sheet, table1Values, startCol = 3, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 3:30)
            table1Status <- as.data.frame(reduce_matrix(table1ALL, 2))
            for(col in 3:(ncol(table1Status)+2)){
              for(row in 7:(nrow(table1Status)+6)){
                if(table1Status[row-6,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table1Status[row-6,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 2
            table2ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table2Values <- as.data.frame(reduce_matrix(table2ALL, 1))
            openxlsx::writeData(wb, sheet, table2Values, startCol = 32, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 32:59)
            table2Status <- as.data.frame(reduce_matrix(table2ALL, 2))
            for(col in 32:(ncol(table2Status)+31)){
              for(row in 7:(nrow(table2Status)+6)){
                if(table2Status[row-6,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table2Status[row-6,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 3
            table3 <- as.data.frame(reduce_matrix(table, 3, na_empty = FALSE))
            table3ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table3Values <- as.data.frame(reduce_matrix(table3ALL, 1))
            openxlsx::writeData(wb, sheet, table3Values, startCol = 61, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 7:37, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 37, cols = 88)
            table3Status <- as.data.frame(reduce_matrix(table3ALL, 2))
            for(col in 61:(ncol(table3Status)+60)){
              for(row in 7:(nrow(table3Status)+6)){
                if(table3Status[row-6,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table3Status[row-6,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("PCOs", chem_type)) == 0){
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
            print("There is PCdiOs") # verify grey cells !
            #Table 4
            table4ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table4Values <- as.data.frame(reduce_matrix(table4ALL, 1))
            openxlsx::writeData(wb, sheet, table4Values, startCol = 3, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 3:30)
            table4Status <- as.data.frame(reduce_matrix(table4ALL, 2))
            for(col in 3:(ncol(table4Status)+2)){
              for(row in 41:(nrow(table4Status)+40)){
                if(table1Status[row-40,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table4Status[row-40,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 5
            table5ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table5Values <- as.data.frame(reduce_matrix(table5ALL, 1))
            openxlsx::writeData(wb, sheet, table5Values, startCol = 32, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 32:59)
            table5Status <- as.data.frame(reduce_matrix(table5ALL, 2))
            for(col in 32:(ncol(table5Status)+31)){
              for(row in 41:(nrow(table5Status)+40)){
                if(table5Status[row-40,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table5Status[row-40,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 6
            table6ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table6Values <- as.data.frame(reduce_matrix(table6ALL, 1))
            openxlsx::writeData(wb, sheet, table6Values, startCol = 61, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 41:71, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 71, cols = 88)            
            table6Status <- as.data.frame(reduce_matrix(table6ALL, 2))
            for(col in 61:(ncol(table6Status)+60)){
              for(row in 41:(nrow(table6Status)+40)){
                if(table6Status[row-40,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table6Status[row-40,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("PCdiOs", chem_type)) == 0){
            # Grey all tables
            print("Grey all diOS")
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
            print("There is PCtriOs") # verify grey cells !
            # Table 7
            table7ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table7Values <- as.data.frame(reduce_matrix(table7ALL, 1))
            openxlsx::writeData(wb, sheet, table7Values, startCol = 3, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 3:30)
            table7Status <- as.data.frame(reduce_matrix(table7ALL, 2))
            for(col in 3:(ncol(table7Status)+2)){
              for(row in 75:(nrow(table7Status)+74)){
                if(table7Status[row-74,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table7Status[row-74,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 8
            table8ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table8Values <- as.data.frame(reduce_matrix(table8ALL, 1))
            openxlsx::writeData(wb, sheet, table8Values, startCol = 32, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 32:59)
            table8Status <- as.data.frame(reduce_matrix(table8ALL, 2))
            for(col in 32:(ncol(table8Status)+31)){
              for(row in 75:(nrow(table8Status)+74)){
                if(table8Status[row-74,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table8Status[row-74,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 9
            table9ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table9Values <- as.data.frame(reduce_matrix(table9ALL, 1))
            openxlsx::writeData(wb, sheet, table9Values, startCol = 61, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 75:105, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 105, cols = 88)
            table9Status <- as.data.frame(reduce_matrix(table9ALL, 2))
            for(col in 61:(ncol(table9Status)+60)){
              for(row in 75:(nrow(table9Status)+74)){
                if(table9Status[row-74,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table9Status[row-74,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("PCtriOs", chem_type)) == 0){
            # Grey all tables
            print("Grey all triOS")
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
    }
  }
}


export_PXA <- function(user, maxBar, chem_type, adducts, project_informations, pbValue, output = ""){
  allProj <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
  myProjDeconv <- allProj[which(allProj$project == project_informations$project),]
  for(adduct in adducts){
    shinyWidgets::updateProgressBar(session, id = "exportBar",
      value = (pbValue + 1)/maxBar*100, 
      title = paste0("Exporting PXAs ..."))
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
      noiseOrangeStyle <- createStyle(halign = "center", fgFill = "#def1fa", fontColour = "#f1a251", textDecoration = "bold")
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
      setColWidths(wb, sheet = 1, cols = 2, widths = 55)

      openxlsx::writeData(wb, 1, user, startRow = 4, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$name, startRow = 5, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$comments, startRow = 6, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$creation, startRow = 7, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$modified, startRow = 8, startCol = 3)
      addStyle(wb, sheet = 1, tableStyle, rows = 4:8, cols = 3)
      setColWidths(wb, sheet = 1, cols = 3, widths = 55)

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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      line <- 11
      for(smpl in allFiles$sample){
        sample_info <- samples()[which(samples()$sample == smpl),]
        openxlsx::writeData(wb, 1, strsplit(sample_info$sample,"neg ")[[1]][2], startRow = line, startCol = 2)
        openxlsx::writeData(wb, 1, allFiles[which(allFiles$sample == smpl), "sample_id"], startRow = line, startCol = 3)
        openxlsx::writeData(wb, 1, sample_info$polarity, startRow = line, startCol = 4)
        mzrange <- get_project_mz_range(db, input$project)
        openxlsx::writeData(wb, 1, paste0(round(mzrange[1], digits =0), " - ", round(mzrange[2], digits = 0)), startRow = line, startCol = 5) ##### a modifier (mz range) non inclus (thermorawdump lors de l'ajout des fichiers?)
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
      openxlsx::writeData(wb, 2, c("Start time","Duration (minutes)"), startRow = 22, startCol = 2)
      openxlsx::writeData(wb, 2, "Display format", startRow = 25, startCol = 2)
      openxlsx::writeData(wb, 2, "Out of m/z range, or number of halogen higher than number of carbon plus 3, or not processed", startRow = 26, startCol = 2)
      openxlsx::writeData(wb, 2, "Stadding (at least the 2 most intense isotopomer groups)", startRow = 27, startCol = 2)
      openxlsx::writeData(wb, 2, "Score threshold", startRow = 28, startCol = 2)
      openxlsx::writeData(wb, 2, "Deviation tolerance (\u00b1 mDa)", startRow = 29, startCol = 2)
      openxlsx::writeData(wb, 2, "Computer Hardware", startRow = 31, startCol = 2)
      openxlsx::writeData(wb, 2, c("Computer Manufacturer","Computer Model","Operating System","System Type", "CPU Manufacturer", "CPU Name", "Number of CPU Cores", "CPU Speed", "Installed Memory","RAM Speed"), startRow = 32, startCol = 2)
      addStyle(wb, 2, boldStyle, rows = c(4,11,15,21,25,31), cols = 2)
      setColWidths(wb, 2, cols = 2, widths = 25)

      # Mass tolerance
      decParams <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
      decParams <- decParams[which(decParams$adduct == adduct),]
      if(decParams$ppm > 0){
        openxlsx::writeData(wb, 2, paste0("\u00b1 ", unique(decParams$ppm)," ppm"), startCol = 3, startRow = 5)
      }else if(decParams$mda > 0){
        openxlsx::writeData(wb, 2, paste0("\u00b1 ", unique(decParams$mda)," mDa"), startCol = 3, startRow = 5)
      }
      if(unique(decParams$instrument == "Orbitrap")){
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      if(unique(allSamples$polarity == "negative")){
        openxlsx::writeData(wb, 2, paste0("[",adduct,"]-"), startCol = 3, startRow = 13)
      }else if(unique(allSamples$polarity == "positive")){
        openxlsx::writeData(wb, 2, paste0("[",adduct,"]+"), startCol = 3, startRow = 13)
      }else{
        openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
      }
      #################################
      # Standard(s) information
      std <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"])
      # Search for all families we have
      family <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
      # Merge our type and their family
      std <- family[which(family$chemical_type %in% std),]
      std <- std[which(std$chemical_familly == "Standard"),]
      std <- std$chemical_type
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
      info_table <- get_infos(db, input$project)
      openxlsx::writeData(wb, 2, info_table$time_start, startCol = 3, startRow = 22)
      openxlsx::writeData(wb, 2, info_table$time_diff, startCol = 3, startRow = 23)
      openxlsx::writeData(wb, 2, paste0(as.numeric(80),"%"), startCol = 3, startRow = 28)
      openxlsx::writeData(wb, 2, as.numeric(2), startCol = 3, startRow = 29)
      openxlsx::writeData(wb, 2, info_table$computer_manufacturer, startCol = 3, startRow = 32)
      openxlsx::writeData(wb, 2, info_table$computer_model, startCol = 3, startRow = 33)
      openxlsx::writeData(wb, 2, info_table$os_info, startCol = 3, startRow = 34)
      openxlsx::writeData(wb, 2, info_table$system_type, startCol = 3, startRow = 35)
      openxlsx::writeData(wb, 2, info_table$cpu_manufacturer, startCol = 3, startRow = 36)
      openxlsx::writeData(wb, 2, info_table$processor_info, startCol = 3, startRow = 37)
      openxlsx::writeData(wb, 2, paste0(info_table$cpu_cores, " (unit: cores)"), startCol = 3, startRow = 38)
      openxlsx::writeData(wb, 2, paste0(info_table$cpu_speed, " (unit: MHz)"), startCol = 3, startRow = 39)
      openxlsx::writeData(wb, 2, paste0(info_table$memory_info, " (unit: GB)"), startCol = 3, startRow = 40)
      openxlsx::writeData(wb, 2, paste0(info_table$memory_speed, " (unit: MHz)"), startCol = 3, startRow = 41)

      setColWidths(wb, 2, cols = 3, widths = 55)
      addStyle(wb, 2, sh2TableStyle, rows = c(5:9,12:13,16:19,22:23,32:41), cols = 3)
      addStyle(wb, 2, sh2EndTableStyle, rows = 28:29, cols = 3)

      openxlsx::writeData(wb, 2, "Possibly includes processing of other family and adduct types", startCol = 4, startRow = 23)
      openxlsx::writeData(wb, 2, "Number in grey police if below", startCol = 4, startRow = 28)
      openxlsx::writeData(wb, 2, "Number in grey police if outside", startCol = 4, startRow = 29)
      addStyle(wb, 2, italicStyle, rows = 23, cols = 4)
      addStyle(wb, 2, sh2LegendStyle, rows = 28:29, cols = 4)

      ################################################################################
      # Write the third sheet X times with X is the number of standard
      sheet <- 3
      for(s in std){
        print(paste0("Standard : ", s))
        # Add one sheet per standard
        addWorksheet(wb=wb, sheetName=paste0('Standard',sheet-2), gridLines=FALSE)
        # Write the sheet
        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type == s),]
        openxlsx::writeData(wb, sheet, unique(usedStd$chemical_type), startRow = 2)
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
        setColWidths(wb, sheet, cols = 2, widths = 55)
        openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
        setColWidths(wb, sheet, cols = 3, widths = 55)
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
        
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
            table <- unique(table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),])
            addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:8)
            if(is.na(table$'total area') || table$'total area' == 0){
                openxlsx::writeData(wb, sheet, "Not detected", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseOrangeStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
                # Score 
                openxlsx::writeData(wb, sheet, "", startCol = 7, startRow = line)
                # Deviation (mDa)
                openxlsx::writeData(wb, sheet, "", startCol = 8, startRow = line)
              }else if(length(grep("not possible", table$score)) > 0){
                openxlsx::writeData(wb, sheet, "Not possible", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      decParams <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
      decParams <- decParams[which(decParams$adduct == adduct),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      
      for(file in unique(allFiles$sample_id)){
        myActualFile <- allFiles[which(allFiles$sample_id == file),] 
        # Create the sheet of the file label
        addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        openxlsx::writeData(wb, sheet, file, startRow = 2)
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
          table <- get_profile_matrix(db, myActualFile$project_sample, adduct = adduct, chemical_type = paste0("C",i+1,"-PXAs"), export = TRUE) 
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
            table1ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table1Values <- as.data.frame(reduce_matrix(table1ALL, 1))
            openxlsx::writeData(wb, sheet, table1Values, startCol = c1, startRow = previousEnd+2)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 3:(3+length(lineNames)-1))
            addStyle(wb, sheet, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (3+length(lineNames)-1))
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (3+length(lineNames)-1))
            table1Status <- as.data.frame(reduce_matrix(table1ALL, 2))
            for(col in 3:(3+length(lineNames)-1)){
              for(row in (previousEnd+3):(previousEnd+4+length(lineNames)-2)){
                if(table1Status[row-(previousEnd+2),col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table1Status[row-(previousEnd+2),col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 2
            table2ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table2Values <- as.data.frame(reduce_matrix(table2ALL, 1))
            openxlsx::writeData(wb, sheet, table2Values, startCol = c2, startRow = previousEnd+2)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 35:(35+length(lineNames)-1))
            addStyle(wb, sheet, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (35+length(lineNames)-1))
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (35+length(lineNames)-1))
            table2Status <- as.data.frame(reduce_matrix(table2ALL, 2))
            for(col in 3:(3+length(lineNames)-1)){
              for(row in (previousEnd+3):(previousEnd+4+length(lineNames)-2)){
                if(table2Status[row-(previousEnd+2),col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col+32) # add 32 to correspond to the good column (35 - 3)
                }else if(table2Status[row-(previousEnd+2),col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col+32) # add 32 to correspond to the good column (35 - 3)
                }
              }
            }
            # Table 3
            table3ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table3Values <- as.data.frame(reduce_matrix(table3ALL, 1))
            openxlsx::writeData(wb, sheet, table3Values, startCol = c3, startRow = previousEnd+2)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = previousEnd+3+length(lineNames)-1, cols = 67:(67+length(lineNames)-1))
            addStyle(wb, sheet, rightBlankBoderStyle, rows = (previousEnd+3):(previousEnd+4+length(lineNames)-2), cols = (67+length(lineNames)-1))
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = (previousEnd+4+length(lineNames)-2), cols = (67+length(lineNames)-1))
            table3Status <- as.data.frame(reduce_matrix(table3ALL, 2))
            for(col in 3:(3+length(lineNames)-1)){
              for(row in (previousEnd+3):(previousEnd+4+length(lineNames)-2)){
                if(table3Status[row-(previousEnd+2),col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col+64) # add 64 to correspond to the good column (67 - 3)
                }else if(table3Status[row-(previousEnd+2),col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col+64) # add 64 to correspond to the good column (67 - 3)
                }
              }
            }
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
    }
  }
}


export_phase1 <- function(user, maxBar, chem_type, adducts, project_informations, pbValue, output = ""){
  allProj <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
  myProjDeconv <- allProj[which(allProj$project == project_informations$project),]
  for(adduct in adducts){
    shinyWidgets::updateProgressBar(session, id = "exportBar",
      value = (pbValue + 1)/maxBar*100,
      title = paste0("Exporting Phase 1 metabolites ..."))
    pbValue <- pbValue + 1
    print("######################################################################################")
    print(paste0("Run for Phase 1 metabolites and ",adduct))
    if(adduct %in% myProjDeconv$adduct){
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
      noiseOrangeStyle <- createStyle(halign = "center", fgFill = "#def1fa", fontColour = "#f1a251", textDecoration = "bold")
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
      setColWidths(wb, sheet = 1, cols = 2, widths = 55)

      openxlsx::writeData(wb, 1, user, startRow = 4, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$name, startRow = 5, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$comments, startRow = 6, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$creation, startRow = 7, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$modified, startRow = 8, startCol = 3)
      addStyle(wb, sheet = 1, tableStyle, rows = 4:8, cols = 3)
      setColWidths(wb, sheet = 1, cols = 3, widths = 55)

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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      line <- 11
      for(smpl in allFiles$sample){
        sample_info <- samples()[which(samples()$sample == smpl),]
        openxlsx::writeData(wb, 1, strsplit(sample_info$sample,"neg ")[[1]][2], startRow = line, startCol = 2)
        openxlsx::writeData(wb, 1, allFiles[which(allFiles$sample == smpl), "sample_id"], startRow = line, startCol = 3)
        openxlsx::writeData(wb, 1, sample_info$polarity, startRow = line, startCol = 4)
        mzrange <- get_project_mz_range(db, input$project)
        openxlsx::writeData(wb, 1, paste0(round(mzrange[1], digits =0), " - ", round(mzrange[2], digits = 0)), startRow = line, startCol = 5) ##### a modifier (mz range) non inclus (thermorawdump lors de l'ajout des fichiers?)
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
      openxlsx::writeData(wb, 2, c("Start time","Duration (minutes)"), startRow = 22, startCol = 2)
      openxlsx::writeData(wb, 2, "Display format", startRow = 25, startCol = 2)
      openxlsx::writeData(wb, 2, "Out of m/z range, or number of halogen higher than number of carbon plus 3, or not processed", startRow = 26, startCol = 2)
      openxlsx::writeData(wb, 2, "Stadding (at least the 2 most intense isotopomer groups)", startRow = 27, startCol = 2)
      openxlsx::writeData(wb, 2, "Score threshold", startRow = 28, startCol = 2)
      openxlsx::writeData(wb, 2, "Deviation tolerance (\u00b1 mDa)", startRow = 29, startCol = 2)
      openxlsx::writeData(wb, 2, "Computer Hardware", startRow = 31, startCol = 2)
      openxlsx::writeData(wb, 2, c("Computer Manufacturer","Computer Model","Operating System","System Type", "CPU Manufacturer", "CPU Name", "Number of CPU Cores", "CPU Speed", "Installed Memory","RAM Speed"), startRow = 32, startCol = 2)
      addStyle(wb, 2, boldStyle, rows = c(4,11,15,21,25,31), cols = 2)
      setColWidths(wb, 2, cols = 2, widths = 25)

      # Mass tolerance
      decParams <- myProjDeconv[which(myProjDeconv$adduct == adduct),]
      if(unique(decParams$ppm) > 0){
        openxlsx::writeData(wb, 2, paste0("\u00b1 ", decParams$ppm," ppm"), startCol = 3, startRow = 5)
      }else if(any(decParams$mda > 0)){
        openxlsx::writeData(wb, 2, paste0("\u00b1 ", decParams$mda," mDa"), startCol = 3, startRow = 5)
      }
      if(unique(decParams$instrument == "Orbitrap")){
        openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution/1000,"k@",decParams$resolution_mz), startCol = 3, startRow = 6)
      }else if(grep("ToF", decParams$instrument)){
        openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution), startCol = 3, startRow = 6)
      }
      # Peakwidth
      openxlsx::writeData(wb, 2, paste0(decParams$peakwidth_min," to ",decParams$peakwidth_max," s"), startCol = 3, startRow = 7)
      # Retention time
      if(length(unique(decParams$retention_time_min)) == 1 && length(unique(decParams$retention_time_max)) == 1){
        openxlsx::writeData(wb, 2, paste0(unique(decParams$retention_time_min)," to ",unique(decParams$retention_time_max)," min"), startCol = 3, startRow = 8)
      }else{
        if(length(unique(decParams$retention_time_min)) == 1){
          for(max in 1:length(unique(decParams$retention_time_max))){
            my_rt <- paste0(unique(decParams$retention_time_min), " to (", unique(decParams$retention_time_max), ") min")
          }
        }
        if(length(unique(decParams$retention_time_max)) == 1){
          for(min in 1:length(unique(decParams$retention_time_min))){
            my_rt <- paste0("(", unique(decParams$retention_time_min), ") to ", unique(decParams$retention_time_max), " min")
          }
        }
        openxlsx::writeData(wb, 2, my_rt, startCol = 3, startRow = 8)
      }
      # Missing scans
      if(length(unique(decParams$missing_scans)) == 1){
        openxlsx::writeData(wb, 2, unique(decParams$missing_scans), startCol = 3, startRow = 9)
      }else{
        miss_scan <- paste(decParams$missing_scans)
        openxlsx::writeData(wb, 2, miss_scan, startCol = 3, startRow = 9)
      }
      # Chemical type
      my_chemtype <- "Phase I metabolites ("
      for(met in 1:length(chem_type)){
        if(met < length(chem_type)){
          my_chemtype <- paste0(my_chemtype, chem_type[met], ", ")
        }else{
          my_chemtype <- paste0(my_chemtype, chem_type[met], ")")
        }
      }
      openxlsx::writeData(wb, 2, my_chemtype, startCol = 3, startRow = 12)
      # Adduct(s)
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      if(unique(allSamples$polarity == "negative")){
        openxlsx::writeData(wb, 2, paste0("[",adduct,"]-"), startCol = 3, startRow = 13)
      }else if(unique(allSamples$polarity == "positive")){
        openxlsx::writeData(wb, 2, paste0("[",adduct,"]+"), startCol = 3, startRow = 13)
      }else{
        openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
      }
      #################################
      # Standard(s) information
      std <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"])
      # Search for all families we have
      family <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
      # Merge our type and their family
      std <- family[which(family$chemical_type %in% std),]
      std <- std[which(std$chemical_familly == "Standard"),]
      std <- std$chemical_type
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
      info_table <- get_infos(db, input$project)
      openxlsx::writeData(wb, 2, info_table$time_start, startCol = 3, startRow = 22)
      openxlsx::writeData(wb, 2, info_table$time_diff, startCol = 3, startRow = 23)
      openxlsx::writeData(wb, 2, paste0(as.numeric(80),"%"), startCol = 3, startRow = 28)
      openxlsx::writeData(wb, 2, as.numeric(2), startCol = 3, startRow = 29)
      openxlsx::writeData(wb, 2, info_table$computer_manufacturer, startCol = 3, startRow = 32)
      openxlsx::writeData(wb, 2, info_table$computer_model, startCol = 3, startRow = 33)
      openxlsx::writeData(wb, 2, info_table$os_info, startCol = 3, startRow = 34)
      openxlsx::writeData(wb, 2, info_table$system_type, startCol = 3, startRow = 35)
      openxlsx::writeData(wb, 2, info_table$cpu_manufacturer, startCol = 3, startRow = 36)
      openxlsx::writeData(wb, 2, info_table$processor_info, startCol = 3, startRow = 37)
      openxlsx::writeData(wb, 2, paste0(info_table$cpu_cores, " (unit: cores)"), startCol = 3, startRow = 38)
      openxlsx::writeData(wb, 2, paste0(info_table$cpu_speed, " (unit: MHz)"), startCol = 3, startRow = 39)
      openxlsx::writeData(wb, 2, paste0(info_table$memory_info, " (unit: GB)"), startCol = 3, startRow = 40)
      openxlsx::writeData(wb, 2, paste0(info_table$memory_speed, " (unit: MHz)"), startCol = 3, startRow = 41)

      setColWidths(wb, 2, cols = 3, widths = 55)
      addStyle(wb, 2, sh2TableStyle, rows = c(5:9,12:13,16:19,22:23,32:41), cols = 3)
      addStyle(wb, 2, sh2EndTableStyle, rows = 28:29, cols = 3)

      openxlsx::writeData(wb, 2, "Possibly includes processing of other family and adduct types", startCol = 4, startRow = 23)
      openxlsx::writeData(wb, 2, "Number in grey police if below", startCol = 4, startRow = 28)
      openxlsx::writeData(wb, 2, "Number in grey police if outside", startCol = 4, startRow = 29)
      addStyle(wb, 2, italicStyle, rows = 23, cols = 4)
      addStyle(wb, 2, sh2LegendStyle, rows = 28:29, cols = 4)

      ################################################################################
      # Write the third sheet X times with X is the number of standard
      sheet <- 3
      for(s in std){
        print(paste0("Standard : ", s))
        # Add one sheet per standard
        addWorksheet(wb=wb, sheetName=paste0('Standard',sheet-2), gridLines=FALSE)
        # Write the sheet
        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type == s),]
        openxlsx::writeData(wb, sheet, unique(usedStd$chemical_type), startRow = 2)
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
        setColWidths(wb, sheet, cols = 2, widths = 55)
        openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
        setColWidths(wb, sheet, cols = 3, widths = 55)
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
          
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
            table <- unique(table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),])
            addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:8)
            if(is.na(table$'total area') || table$'total area' == 0){
                openxlsx::writeData(wb, sheet, "Not detected", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseOrangeStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
                # Score 
                openxlsx::writeData(wb, sheet, "", startCol = 7, startRow = line)
                # Deviation (mDa)
                openxlsx::writeData(wb, sheet, "", startCol = 8, startRow = line)
              }else if(length(grep("not possible", table$score)) > 0){
                openxlsx::writeData(wb, sheet, "Not possible", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      decParams <- myProjDeconv[which(myProjDeconv$adduct == adduct),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]

      for(file in unique(allFiles$sample_id)){
        myActualFile <- allFiles[which(allFiles$sample_id == file),] 
        # Create the sheet of the file label
        addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        openxlsx::writeData(wb, sheet, file, startRow = 2)
        mySample <- samples()[which(samples()$sample %in% myActualFile$sample),]
        if(unique(mySample$polarity == "negative")){
          openxlsx::writeData(wb, sheet, paste0("[",adduct,"]-"), startRow = 3)
        }else if(unique(mySample$polarity == "positive")){
          openxlsx::writeData(wb, sheet, paste0("[",adduct,"]+"), startRow = 3)
        }else{
          openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
        }
        openxlsx::writeData(wb, sheet, "OH-PCAs", startRow = 5)
        openxlsx::writeData(wb, sheet, "COOH-PCAs", startRow = 39)
        openxlsx::writeData(wb, sheet, "oxo-PCAs", startRow = 73)
        addStyle(wb, sheet, hStyle, rows = 1:73, cols = 1)
        setColWidths(wb, sheet, cols = 1:1000, widths = 4.5)
        setRowHeights(wb, sheet, rows = 1:1000, heights = 18)
        for(chem in chem_type){
          # Save the table with all values for this file 
          table <- get_profile_matrix(db, myActualFile$project_sample, adduct = adduct, chemical_type = chem, export = TRUE)
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
          if(chem == "OH-PCAs"){
            print("There is OH-PCAs")
            # Table 1
            table1ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table1Values <- as.data.frame(reduce_matrix(table1ALL, 1))
            openxlsx::writeData(wb, sheet, table1Values, startCol = 3, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 3:30)
            table1Status <- as.data.frame(reduce_matrix(table1ALL, 2))
            for(col in 3:(ncol(table1Status)+2)){
              for(row in 7:(nrow(table1Status)+6)){
                if(table1Status[row-6,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table1Status[row-6,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 2
            table2ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table2Values <- as.data.frame(reduce_matrix(table2ALL, 1))
            openxlsx::writeData(wb, sheet, table2Values, startCol = 32, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 32:59)
            table2Status <- as.data.frame(reduce_matrix(table2ALL, 2))
            for(col in 32:(ncol(table2Status)+31)){
              for(row in 7:(nrow(table2Status)+6)){
                if(table2Status[row-6,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table2Status[row-6,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 3
            table3 <- as.data.frame(reduce_matrix(table, 3, na_empty = FALSE))
            table3ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table3Values <- as.data.frame(reduce_matrix(table3ALL, 1))
            openxlsx::writeData(wb, sheet, table3Values, startCol = 61, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 7:37, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 37, cols = 88)
            table3Status <- as.data.frame(reduce_matrix(table3ALL, 2))
            for(col in 61:(ncol(table3Status)+60)){
              for(row in 7:(nrow(table3Status)+6)){
                if(table3Status[row-6,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table3Status[row-6,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("OH-PCAs", chem_type)) == 0){
            # Grey all tables
            print("Grey all OH-PCAs")
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
          if(chem == "COOH-PCAs"){
            print("There is COOH-PCAs") # verify grey cells !
            #Table 4
            table4ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table4Values <- as.data.frame(reduce_matrix(table4ALL, 1))
            openxlsx::writeData(wb, sheet, table4Values, startCol = 3, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 3:30)
            table4Status <- as.data.frame(reduce_matrix(table4ALL, 2))
            for(col in 3:(ncol(table4Status)+2)){
              for(row in 41:(nrow(table4Status)+40)){
                if(table1Status[row-40,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table4Status[row-40,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 5
            table5ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table5Values <- as.data.frame(reduce_matrix(table5ALL, 1))
            openxlsx::writeData(wb, sheet, table5Values, startCol = 32, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 32:59)
            table5Status <- as.data.frame(reduce_matrix(table5ALL, 2))
            for(col in 32:(ncol(table5Status)+31)){
              for(row in 41:(nrow(table5Status)+40)){
                if(table5Status[row-40,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table5Status[row-40,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 6
            table6ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table6Values <- as.data.frame(reduce_matrix(table6ALL, 1))
            openxlsx::writeData(wb, sheet, table6Values, startCol = 61, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 41:71, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 71, cols = 88)            
            table6Status <- as.data.frame(reduce_matrix(table6ALL, 2))
            for(col in 61:(ncol(table6Status)+60)){
              for(row in 41:(nrow(table6Status)+40)){
                if(table6Status[row-40,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table6Status[row-40,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("COOH-PCAs", chem_type)) == 0){
            # Grey all tables
            print("Grey all COOH-PCAs")
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
          if(chem == "oxo-PCAs"){
            print("There is oxo-PCAs") # verify grey cells !
            # Table 7
            table7ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table7Values <- as.data.frame(reduce_matrix(table7ALL, 1))
            openxlsx::writeData(wb, sheet, table7Values, startCol = 3, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 3:30)
            table7Status <- as.data.frame(reduce_matrix(table7ALL, 2))
            for(col in 3:(ncol(table7Status)+2)){
              for(row in 75:(nrow(table7Status)+74)){
                if(table7Status[row-74,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table7Status[row-74,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 8
            table8ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table8Values <- as.data.frame(reduce_matrix(table8ALL, 1))
            openxlsx::writeData(wb, sheet, table8Values, startCol = 32, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 32:59)
            table8Status <- as.data.frame(reduce_matrix(table8ALL, 2))
            for(col in 32:(ncol(table8Status)+31)){
              for(row in 75:(nrow(table8Status)+74)){
                if(table8Status[row-74,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table8Status[row-74,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 9
            table9ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table9Values <- as.data.frame(reduce_matrix(table9ALL, 1))
            openxlsx::writeData(wb, sheet, table9Values, startCol = 61, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 75:105, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 105, cols = 88)
            table9Status <- as.data.frame(reduce_matrix(table9ALL, 2))
            for(col in 61:(ncol(table9Status)+60)){
              for(row in 75:(nrow(table9Status)+74)){
                if(table9Status[row-74,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table9Status[row-74,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("oxo-PCAs", chem_type)) == 0){
            # Grey all tables
            print("Grey all oxo-PCAs")
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
      saveWorkbook(wb, paste0(config_dir,"/",project_informations$name,"_",project_informations$creation,"_[",adduct,"]_Phase1metabolites.xlsx"), overwrite = TRUE)
    }
  }
}

export_phase2 <- function(user, maxBar, chem_type, adducts, project_informations, pbValue, output = ""){
  allProj <- deconvolution_params()[which(deconvolution_params()$chemical_type %in% chem_type),]
  myProjDeconv <- allProj[which(allProj$project == project_informations$project),]
  for(adduct in adducts){
    shinyWidgets::updateProgressBar(session, id = "exportBar",
      value = (pbValue + 1)/maxBar*100,
      title = paste0("Exporting Phase 2 metabolites ..."))
    pbValue <- pbValue + 1
    print("######################################################################################")
    print(paste0("Run for Phase 2 metabolites and ",adduct))
    if(adduct %in% myProjDeconv$adduct){
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
      noiseOrangeStyle <- createStyle(halign = "center", fgFill = "#def1fa", fontColour = "#f1a251", textDecoration = "bold")
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
      setColWidths(wb, sheet = 1, cols = 2, widths = 55)

      openxlsx::writeData(wb, 1, user, startRow = 4, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$name, startRow = 5, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$comments, startRow = 6, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$creation, startRow = 7, startCol = 3)
      openxlsx::writeData(wb, 1, project_informations$modified, startRow = 8, startCol = 3)
      addStyle(wb, sheet = 1, tableStyle, rows = 4:8, cols = 3)
      setColWidths(wb, sheet = 1, cols = 3, widths = 55)

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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      line <- 11
      for(smpl in allFiles$sample){
        sample_info <- samples()[which(samples()$sample == smpl),]
        openxlsx::writeData(wb, 1, strsplit(sample_info$sample,"neg ")[[1]][2], startRow = line, startCol = 2)
        openxlsx::writeData(wb, 1, allFiles[which(allFiles$sample == smpl), "sample_id"], startRow = line, startCol = 3)
        openxlsx::writeData(wb, 1, sample_info$polarity, startRow = line, startCol = 4)
        mzrange <- get_project_mz_range(db, input$project)
        openxlsx::writeData(wb, 1, paste0(round(mzrange[1], digits =0), " - ", round(mzrange[2], digits = 0)), startRow = line, startCol = 5) ##### a modifier (mz range) non inclus (thermorawdump lors de l'ajout des fichiers?)
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
      openxlsx::writeData(wb, 2, c("Start time","Duration (minutes)"), startRow = 22, startCol = 2)
      openxlsx::writeData(wb, 2, "Display format", startRow = 25, startCol = 2)
      openxlsx::writeData(wb, 2, "Out of m/z range, or number of halogen higher than number of carbon plus 3, or not processed", startRow = 26, startCol = 2)
      openxlsx::writeData(wb, 2, "Stadding (at least the 2 most intense isotopomer groups)", startRow = 27, startCol = 2)
      openxlsx::writeData(wb, 2, "Score threshold", startRow = 28, startCol = 2)
      openxlsx::writeData(wb, 2, "Deviation tolerance (\u00b1 mDa)", startRow = 29, startCol = 2)
      openxlsx::writeData(wb, 2, "Computer Hardware", startRow = 31, startCol = 2)
      openxlsx::writeData(wb, 2, c("Computer Manufacturer","Computer Model","Operating System","System Type", "CPU Manufacturer", "CPU Name", "Number of CPU Cores", "CPU Speed", "Installed Memory","RAM Speed"), startRow = 32, startCol = 2)
      addStyle(wb, 2, boldStyle, rows = c(4,11,15,21,25,31), cols = 2)
      setColWidths(wb, 2, cols = 2, widths = 25)

      # Mass tolerance
      decParams <- myProjDeconv[which(myProjDeconv$adduct == adduct),]
      if(unique(decParams$ppm) > 0){
        openxlsx::writeData(wb, 2, paste0("\u00b1 ", decParams$ppm," ppm"), startCol = 3, startRow = 5)
      }else if(any(decParams$mda > 0)){
        openxlsx::writeData(wb, 2, paste0("\u00b1 ", decParams$mda," mDa"), startCol = 3, startRow = 5)
      }
      if(unique(decParams$instrument == "Orbitrap")){
        openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution/1000,"k@",decParams$resolution_mz), startCol = 3, startRow = 6)
      }else if(grep("ToF", decParams$instrument)){
        openxlsx::writeData(wb, 2, paste0(decParams$instrument," ",decParams$resolution), startCol = 3, startRow = 6)
      }
      # Peakwidth
      openxlsx::writeData(wb, 2, paste0(decParams$peakwidth_min," to ",decParams$peakwidth_max," s"), startCol = 3, startRow = 7)
      # Retention time
      if(length(unique(decParams$retention_time_min)) == 1 && length(unique(decParams$retention_time_max)) == 1){
        openxlsx::writeData(wb, 2, paste0(unique(decParams$retention_time_min)," to ",unique(decParams$retention_time_max)," min"), startCol = 3, startRow = 8)
      }else{
        if(length(unique(decParams$retention_time_min)) == 1){
          for(max in 1:length(unique(decParams$retention_time_max))){
            my_rt <- paste0(unique(decParams$retention_time_min), " to (", unique(decParams$retention_time_max), ") min")
          }
        }
        if(length(unique(decParams$retention_time_max)) == 1){
          for(min in 1:length(unique(decParams$retention_time_min))){
            my_rt <- paste0("(", unique(decParams$retention_time_min), ") to ", unique(decParams$retention_time_max), " min")
          }
        }
        openxlsx::writeData(wb, 2, my_rt, startCol = 3, startRow = 8)
      }
      # Missing scans
      if(length(unique(decParams$missing_scans)) == 1){
        openxlsx::writeData(wb, 2, unique(decParams$missing_scans), startCol = 3, startRow = 9)
      }else{
        miss_scan <- paste(decParams$missing_scans)
        openxlsx::writeData(wb, 2, miss_scan, startCol = 3, startRow = 9)
      }
      # Chemical type
      my_chemtype <- "Phase II metabolites ("
      for(met in 1:length(chem_type)){
        if(met < length(chem_type)){
          my_chemtype <- paste0(my_chemtype, chem_type[met], ", ")
        }else{
          my_chemtype <- paste0(my_chemtype, chem_type[met], ")")
        }
      }
      openxlsx::writeData(wb, 2, my_chemtype, startCol = 3, startRow = 12)
      # Adduct(s)
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]
      if(unique(allSamples$polarity == "negative")){
        openxlsx::writeData(wb, 2, paste0("[",adduct,"]-"), startCol = 3, startRow = 13)
      }else if(unique(allSamples$polarity == "positive")){
        openxlsx::writeData(wb, 2, paste0("[",adduct,"]+"), startCol = 3, startRow = 13)
      }else{
        openxlsx::writeData(wb, 2, paste0("Polarity problem"), startCol = 3, startRow = 13)
      }
      #################################
      # Standard(s) information
      std <- unique(deconvolution_params()[which(deconvolution_params()$project == input$project), "chemical_type"])
      # Search for all families we have
      family <- unique(db_get_query(db, "select chemical_type, chemical_familly from chemical"))
      # Merge our type and their family
      std <- family[which(family$chemical_type %in% std),]
      std <- std[which(std$chemical_familly == "Standard"),]
      std <- std$chemical_type
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
      info_table <- get_infos(db, input$project)
      openxlsx::writeData(wb, 2, info_table$time_start, startCol = 3, startRow = 22)
      openxlsx::writeData(wb, 2, info_table$time_diff, startCol = 3, startRow = 23)
      openxlsx::writeData(wb, 2, paste0(as.numeric(80),"%"), startCol = 3, startRow = 28)
      openxlsx::writeData(wb, 2, as.numeric(2), startCol = 3, startRow = 29)
      openxlsx::writeData(wb, 2, info_table$computer_manufacturer, startCol = 3, startRow = 32)
      openxlsx::writeData(wb, 2, info_table$computer_model, startCol = 3, startRow = 33)
      openxlsx::writeData(wb, 2, info_table$os_info, startCol = 3, startRow = 34)
      openxlsx::writeData(wb, 2, info_table$system_type, startCol = 3, startRow = 35)
      openxlsx::writeData(wb, 2, info_table$cpu_manufacturer, startCol = 3, startRow = 36)
      openxlsx::writeData(wb, 2, info_table$processor_info, startCol = 3, startRow = 37)
      openxlsx::writeData(wb, 2, paste0(info_table$cpu_cores, " (unit: cores)"), startCol = 3, startRow = 38)
      openxlsx::writeData(wb, 2, paste0(info_table$cpu_speed, " (unit: MHz)"), startCol = 3, startRow = 39)
      openxlsx::writeData(wb, 2, paste0(info_table$memory_info, " (unit: GB)"), startCol = 3, startRow = 40)
      openxlsx::writeData(wb, 2, paste0(info_table$memory_speed, " (unit: MHz)"), startCol = 3, startRow = 41)

      setColWidths(wb, 2, cols = 3, widths = 55)
      addStyle(wb, 2, sh2TableStyle, rows = c(5:9,12:13,16:19,22:23,32:41), cols = 3)
      addStyle(wb, 2, sh2EndTableStyle, rows = 28:29, cols = 3)

      openxlsx::writeData(wb, 2, "Possibly includes processing of other family and adduct types", startCol = 4, startRow = 23)
      openxlsx::writeData(wb, 2, "Number in grey police if below", startCol = 4, startRow = 28)
      openxlsx::writeData(wb, 2, "Number in grey police if outside", startCol = 4, startRow = 29)
      addStyle(wb, 2, italicStyle, rows = 23, cols = 4)
      addStyle(wb, 2, sh2LegendStyle, rows = 28:29, cols = 4)
      ################################################################################
      # Write the third sheet X times with X is the number of standard
      sheet <- 3
      for(s in std){
        print(paste0("Standard : ", s))
        # Add one sheet per standard
        addWorksheet(wb=wb, sheetName=paste0('Standard',sheet-2), gridLines=FALSE)
        # Write the sheet
        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        usedStd <- deconvolution_params()[which(deconvolution_params()$chemical_type == s),]
        openxlsx::writeData(wb, sheet, unique(usedStd$chemical_type), startRow = 2)
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
        setColWidths(wb, sheet, cols = 2, widths = 55)
        openxlsx::writeData(wb, sheet, "Label", startCol = 3, startRow = 5)
        setColWidths(wb, sheet, cols = 3, widths = 55)
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
          
        allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
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
            table <- unique(table[which(table$sample_id == allFiles[which(allFiles$sample == smpl),"sample_id"]),])
            addStyle(wb, sheet = sheet, bodyTableStyle, rows = line, cols = 2:8)
            if(is.na(table$'total area') || table$'total area' == 0){
                openxlsx::writeData(wb, sheet, "Not detected", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseOrangeStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
                # Score 
                openxlsx::writeData(wb, sheet, "", startCol = 7, startRow = line)
                # Deviation (mDa)
                openxlsx::writeData(wb, sheet, "", startCol = 8, startRow = line)
              }else if(length(grep("not possible", table$score)) > 0){
                openxlsx::writeData(wb, sheet, "Not possible", startCol = 5, startRow = line)
                addStyle(wb, sheet, noiseActiveStyle, rows = line, cols = 5)
                openxlsx::writeData(wb, sheet, "", startCol = 6, startRow = line)
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
      allFiles <- unique(project_samples()[which(project_samples()$project == input$project),])
      decParams <- myProjDeconv[which(myProjDeconv$adduct == adduct),]
      allSamples <- samples()[which(samples()$sample %in% allFiles$sample),]

      for(file in unique(allFiles$sample_id)){
        myActualFile <- allFiles[which(allFiles$sample_id == file),] 
        # Create the sheet of the file label
        addWorksheet(wb = wb, sheetName = file, gridLines = FALSE)

        openxlsx::writeData(wb, sheet, config$appname, startRow = 1)
        openxlsx::writeData(wb, sheet, file, startRow = 2)
        mySample <- samples()[which(samples()$sample %in% myActualFile$sample),]
        if(unique(mySample$polarity == "negative")){
          openxlsx::writeData(wb, sheet, paste0("[",adduct,"]-"), startRow = 3)
        }else if(unique(mySample$polarity == "positive")){
          openxlsx::writeData(wb, sheet, paste0("[",adduct,"]+"), startRow = 3)
        }else{
          openxlsx::writeData(wb, sheet, paste0("Polarity problem"), startRow = 3)
        }
        openxlsx::writeData(wb, sheet, "GSH-OH-PCAs", startRow = 5)
        openxlsx::writeData(wb, sheet, "SCys-OH-PCAs", startRow = 39)
        openxlsx::writeData(wb, sheet, "Mercapturic-OH-PCAs", startRow = 73)
        addStyle(wb, sheet, hStyle, rows = 1:73, cols = 1)
        setColWidths(wb, sheet, cols = 1:1000, widths = 4.5)
        setRowHeights(wb, sheet, rows = 1:1000, heights = 18)
        for(chem in chem_type){
          # Save the table with all values for this file 
          table <- get_profile_matrix(db, myActualFile$project_sample, adduct = adduct, chemical_type = chem, export = TRUE)
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
          if(chem == "GSH-OH-PCAs"){
            print("There is GSH-OH-PCAs")
            # Table 1
            table1ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table1Values <- as.data.frame(reduce_matrix(table1ALL, 1))
            openxlsx::writeData(wb, sheet, table1Values, startCol = 3, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 3:30)
            table1Status <- as.data.frame(reduce_matrix(table1ALL, 2))
            for(col in 3:(ncol(table1Status)+2)){
              for(row in 7:(nrow(table1Status)+6)){
                if(table1Status[row-6,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table1Status[row-6,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 2
            table2ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table2Values <- as.data.frame(reduce_matrix(table2ALL, 1))
            openxlsx::writeData(wb, sheet, table2Values, startCol = 32, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 32:59)
            table2Status <- as.data.frame(reduce_matrix(table2ALL, 2))
            for(col in 32:(ncol(table2Status)+31)){
              for(row in 7:(nrow(table2Status)+6)){
                if(table2Status[row-6,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table2Status[row-6,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 3
            table3 <- as.data.frame(reduce_matrix(table, 3, na_empty = FALSE))
            table3ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table3Values <- as.data.frame(reduce_matrix(table3ALL, 1))
            openxlsx::writeData(wb, sheet, table3Values, startCol = 61, startRow = 6)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 37, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 7:37, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 37, cols = 88)
            table3Status <- as.data.frame(reduce_matrix(table3ALL, 2))
            for(col in 61:(ncol(table3Status)+60)){
              for(row in 7:(nrow(table3Status)+6)){
                if(table3Status[row-6,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table3Status[row-6,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("GSH-OH-PCAs", chem_type)) == 0){
            # Grey all tables
            print("Grey all GSH-OH-PCAs")
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
          if(chem == "SCys-OH-PCAs"){
            print("There is SCys-OH-PCAs") # verify grey cells !
            #Table 4
            table4ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table4Values <- as.data.frame(reduce_matrix(table4ALL, 1))
            openxlsx::writeData(wb, sheet, table4Values, startCol = 3, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 3:30)
            table4Status <- as.data.frame(reduce_matrix(table4ALL, 2))
            for(col in 3:(ncol(table4Status)+2)){
              for(row in 41:(nrow(table4Status)+40)){
                if(table1Status[row-40,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table4Status[row-40,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 5
            table5ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table5Values <- as.data.frame(reduce_matrix(table5ALL, 1))
            openxlsx::writeData(wb, sheet, table5Values, startCol = 32, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 32:59)
            table5Status <- as.data.frame(reduce_matrix(table5ALL, 2))
            for(col in 32:(ncol(table5Status)+31)){
              for(row in 41:(nrow(table5Status)+40)){
                if(table5Status[row-40,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table5Status[row-40,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 6
            table6ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table6Values <- as.data.frame(reduce_matrix(table6ALL, 1))
            openxlsx::writeData(wb, sheet, table6Values, startCol = 61, startRow = 40)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 71, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 41:71, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 71, cols = 88)            
            table6Status <- as.data.frame(reduce_matrix(table6ALL, 2))
            for(col in 61:(ncol(table6Status)+60)){
              for(row in 41:(nrow(table6Status)+40)){
                if(table6Status[row-40,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table6Status[row-40,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("SCys-OH-PCAs", chem_type)) == 0){
            # Grey all tables
            print("Grey all SCys-OH-PCAs")
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
          if(chem == "Mercapturic-OH-PCAs"){
            print("There is Mercapturic-OH-PCAs") # verify grey cells !
            # Table 7
            table7ALL <- as.data.frame(reduce_matrix(table, 2, greycells = TRUE, na_empty = FALSE))
            table7Values <- as.data.frame(reduce_matrix(table7ALL, 1))
            openxlsx::writeData(wb, sheet, table7Values, startCol = 3, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 3:30)
            table7Status <- as.data.frame(reduce_matrix(table7ALL, 2))
            for(col in 3:(ncol(table7Status)+2)){
              for(row in 75:(nrow(table7Status)+74)){
                if(table7Status[row-74,col-2] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table7Status[row-74,col-2] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 8
            table8ALL <- as.data.frame(reduce_matrix(table, 1, greycells = TRUE, na_empty = FALSE))
            table8Values <- as.data.frame(reduce_matrix(table8ALL, 1))
            openxlsx::writeData(wb, sheet, table8Values, startCol = 32, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 32:59)
            table8Status <- as.data.frame(reduce_matrix(table8ALL, 2))
            for(col in 32:(ncol(table8Status)+31)){
              for(row in 75:(nrow(table8Status)+74)){
                if(table8Status[row-74,col-31] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table8Status[row-74,col-31] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
            # Table 9
            table9ALL <- as.data.frame(reduce_matrix(table, 3, greycells = TRUE, na_empty = FALSE))
            table9Values <- as.data.frame(reduce_matrix(table9ALL, 1))
            openxlsx::writeData(wb, sheet, table9Values, startCol = 61, startRow = 74)
            addStyle(wb, sheet, bottomBlankBorderStyle, rows = 105, cols = 61:88)
            addStyle(wb, sheet, rightBlankBoderStyle, rows = 75:105, cols = 88)
            addStyle(wb, sheet, cornerRightBottomBlankStyle, rows = 105, cols = 88)
            table9Status <- as.data.frame(reduce_matrix(table9ALL, 2))
            for(col in 61:(ncol(table9Status)+60)){
              for(row in 75:(nrow(table9Status)+74)){
                if(table9Status[row-74,col-60] == "half"){
                  addStyle(wb, sheet, sh2DisplayStyle2, rows = row, cols = col)
                }else if(table9Status[row-74,col-60] == "outside"){
                  addStyle(wb, sheet, sh2DisplayStyle1, rows = row, cols = col)
                }
              }
            }
          }else if(length(grep("Mercapturic-OH-PCAs", chem_type)) == 0){
            # Grey all tables
            print("Grey all Mercapturic-OH-PCAs")
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
      saveWorkbook(wb, paste0(config_dir,"/",project_informations$name,"_",project_informations$creation,"_[",adduct,"]_Phase2metabolites.xlsx"), overwrite = TRUE)
    }
  }
}