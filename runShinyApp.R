Sys.setenv(PATH = paste(file.path(getwd(), "R-Portable/Rtools/bin"), Sys.getenv("PATH"), sep=";"))
madhalors <- file.path(getwd(), 'shiny')
shiny::runApp(madhalors, launch.browser=TRUE)
