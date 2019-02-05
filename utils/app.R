# app launching code
message('test browser...')
setWinProgressBar(pb, 1.00, label = "test browser...")
if(user_browser == 'chrome') result <- testBrowser(user_browser, chrome) else 
if(user_browser == 'firefox') result <- testBrowser(user_browser, ff) else 
if(user_browser == 'ie') result <- testBrowser(user_browser, ie) else 
if(user_browser == 'none') result <- list(msg = 'use default browser', browser = '') else 
stop('user_browser must be chrome, firefox, ie or none')
	
msg <- result[[1]]
browser <- result[[2]]
if(grepl('not founded', msg)){
	message(msg)
	tcltk::tk_messageBox(
		type = "ok",
		message = msg,
		icon = "error")
	msg <- 'use default browser'
}
if(msg == 'use default browser') browser <- shell(
	"reg QUERY HKEY_CLASSES_ROOT\\http\\shell\\open\\command", intern=TRUE) else options(browser = browser)
message(msg)
message(browser)
launch_browser = TRUE	

setWinProgressBar(pb, 1.00, label = "Starting app")  

close(pb)

# load enviPat data
data(isotopes)
data(adducts)

options(stringsAsFactors=FALSE)
shiny::runApp("./", launch.browser = launch_browser)

