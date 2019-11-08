#  This file is part of the HaloSeeker software for analyzing LC-MS data
#  
#  Copyright (C) 2018  Sébastien HUTINET, Ronan CARIOU, Alexis LÉON, Julie HUREL, Yann GUITTON, Céline TIXIER, Catherine MUNSCHY, Jean-Philippe ANTIGNAC, Gaud DERVILLY-PINEL, Bruno LE BIZEC
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.

#the function if the user create a new project
observeEvent(input$projectCreate, {
	print('############################################################')
	print('######################### CREATE PROJECT ###################')
	print('############################################################')
	tryCatch({
	print(list(name=input$projectName, comment=input$projectComment))
	
	args <- c('projectName', 'projectComment')
	titles <- c('name', 'comment')
	conditions <- c(!is.character(input$projectName) | input$projectName == '', !is.character(input$projectComment))
	messages <- c('Invalid project name', 'Invalid project comment')
	if(!inputsTest(args, conditions, titles, messages)) custom_stop('invalid', 'Invalid args')
	args <- c('projectName', 'projectComment')
	titles <- c('name', 'comment')
	conditions <- c(str_detect(input$projectName, specialChars), 
		str_detect(input$projectComment, specialChars))
	messages <- c('Name cannot contain any special character ([\\:*?\"<>|])', 
		'comment cannot contain any special character ([\\:*?\"<>|])')
	if(!inputsTest(args, conditions, titles, messages)) custom_stop('invalid', 'Invalid args')
	
	recordProject(input$projectName, input$projectComment)
  	
	actualize$projects <- TRUE
	toastr_success(paste('Project', input$projectName, 'created!'))
	
	}, invalid = function(i) NULL
	, minor_error = function(e) toastr_warning(paste(e$message))
	, error = function(e){
		print(e)
		sendSweetAlert("Cannot create project", paste(e$message))
	})
	print('############################################################')
	print('######################### END CREATE PROJECT ###############')
	print('############################################################')
})
