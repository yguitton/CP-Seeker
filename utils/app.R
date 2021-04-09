data("isotopes", package = "enviPat")
data("adducts", package = "enviPat")
data("resolution_list", package = "enviPat")

# order isotopes to have first carbons, then hydrogens, then elements in alphabetical order
elts_CH <- unlist(lapply(c("C", "[12]C", "[13]C", "H", "D", "[1]H", "[2]H"), function(elt)
	which(isotopes$element == elt)))
isotopes_CH <- isotopes[elts_CH, ]
isotopes_not_CH <- isotopes[-elts_CH, ]
isotopes <- rbind(isotopes_CH, isotopes_not_CH[order(
	isotopes_not_CH$element), ])

options(warn = -1) # deactivate warnings cause of plotly (multiplication)
options(stringsAsFactors = FALSE)
options(browser = chromium)
options(show.error.messages = TRUE)
volumes <- c(home = tools::file_path_as_absolute("~"), shinyFiles::getVolumes()())
shiny::runApp(".", launch.browser = TRUE)