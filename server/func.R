`%!in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

#conversion factor to numeric directly
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

toastr_error <- function(text){
	shinytoastr::toastr_error(text, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

toastr_success <- function(text){
	shinytoastr::toastr_success(text, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

toastr_warning <- function(text){
	shinytoastr::toastr_warning(text, closeButton=TRUE, newestOnTop=TRUE, position='top-center', preventDuplicates=TRUE)
}

sendSweetAlert <- function(text='', type='error'){
	shinyWidgets::sendSweetAlert(session, title=text, type=type)
}

# check if the inputs respect the conditions
# if not it use shinyFeedback to hightlight the input and send a message
# the condition has to be FALSE
inputsTest <- function(inputs, conditions, messages){
	if(length(inputs) == 0) return(TRUE)
	for(i in 1:length(inputs)){
		feedbackDanger(inputs[i], conditions[i], messages[i])
		if(conditions[i]){
			print(paste('ERR: ', messages[i]))
			toastr_error(messages[i])
		}
	}
	return(!any(conditions))
}

condition <- function(subclass, message, call=sys.call(-1), ...){
	structure(
		class=c(subclass, "condition"),
		list(message=message, call=call),
		...
	)
}

custom_stop <- function(subclass, message, call=sys.call(-1), ...){
	c <- condition(c(subclass, "error"), message, call=call, ...)
	stop(c)
}

# catch errors and print last traces where the errors occured
withErrorTracing <- function(expr, silent=FALSE, default=NULL){
	res <- tryCatch(
		expr, 
		invalid = function(i){
			i
		}, minor_error = function(e){
			toastr_error(e$message)
			e
		}, error = function(e, silent){
			print('################## ERROR ###########################')
			print(e)
			# storing the call stack
			calls <- sys.calls()
			calls <- calls[1:length(calls)-1]
			# keeping the calls only
			trace <- limitedLabels(c(calls, attr(e, 'calls')))
			# printing the 2nd and 3rd traces that contain the line where the error occured
			print(paste0('Error occuring: ', rev(trace)[2:3]))
			# muffle any redundant output of the same message
			optionalRestart <- function(r){
				res <- findRestart(r)
				if(!is.null(res)) invokeRestart(res)
			}
			
			optionalRestart('muffleMessage')
			optionalRestart('muffleWarning')
			
			print('####################################################')
			e
		}
	)
	if(!is.null(res)){
		if("error" %in% class(res)){
			print(res)
			if(!("invalid" %in% class(res) | "minor_error" %in% class(res)) & "error" %in% class(res)){
				if(!silent) sendSweetAlert(session, title=res$message, type='error') else toastr_error(res$message)
			}	
			return(default)
		}
		else return(res)
	}
}
