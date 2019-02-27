options(show.error.messages = TRUE)
shinyServer(function(input, output, session) {

#to close the connection
session$onSessionEnded(function() {
	stopApp()
	# comment line above because it's still in developement
	# q('no')
})

values <- reactiveValues()
updateOutput <- reactiveValues()

minC <- 4
maxC <- 36
minCl <- 2
maxCl <- 30

# create the adducts [M-Cl]- & [M-H-Cl]-
adducts <- adducts %>% rbind(data.frame(
	Name = c("M-Cl", "M-H-Cl"), calc = c("M-34.9683", "M-35.97613"),
	Charge = c(-1, -1), Mult = c(1, 1), Mass = c(-34.9683, -35.97613),
	Ion_mode = c('negative', 'negative'), Formula_add = c("FALSE", "FALSE"),
	Formula_ded = c('Cl1', 'Cl1H1'), Multi = c(1, 1)))

source('server/func.R', local=TRUE)$value

source('server/loadProfile.R', local=TRUE)$value

source('server/profils.R', local=TRUE)$value

source('server/tetrahedrization.R', local=TRUE)$value

source('server/projectFiles.R', local=TRUE)$value

source('server/target.R', local=TRUE)$value

source('server/details.R', local=TRUE)$value

})	