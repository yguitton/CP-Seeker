Sys.setenv(PATH = paste(file.path(getwd(), "R-Portable/Rtools/bin"), Sys.getenv("PATH"), sep=";"))
madhalors <- file.path(getwd(), 'shiny/app.R')
shiny::runApp(madhalors, launch.browser=TRUE)
