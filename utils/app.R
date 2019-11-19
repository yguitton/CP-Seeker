# app launching code

launch_browser = TRUE	

setWinProgressBar(pb, 1.00, label = "Starting app")  

close(pb)

data(isotopes)
data(adducts)
data(resolution_list)

options(browser = chrome)
options(stringsAsFactors = FALSE)
options(shiny.reactlog = TRUE)
shiny::runApp("./", launch.browser = launch_browser)

