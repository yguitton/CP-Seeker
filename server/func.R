`%!in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

#conversion factor to numeric directly
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# read in sql-statements and preformat them                                        
dbDisconnect <- function(db){
	suppressWarnings(RSQLite::dbDisconnect(db))
}
dbGetQuery <- function(db, query){
	suppressWarnings(RSQLite::dbGetQuery(db, query))
}

dbSendQuery <- function(db, query, ...){
	msg <- "database is locked"
	while(msg == "database is locked"){
		msg <- tryCatch({
			suppressWarnings(RSQLite::dbSendQuery(db, query, ...))
			"success"
		}, error = function(e){
			e$message
		})
	}
	if(msg != "success") stop(msg)
}

# read from a file with SQL statements
sqlFromFile <- function(file){
    require(stringr)
	sql <- readLines(file)
    sql <- unlist(str_split(paste(sql,collapse=" "),";"))
    sql <- sql[grep("^ *$", sql, invert=T)]
}

# apply query function to each element
dbSendQueries <- function(con,sql, ...){
    dummyfunction <- function(sql,con){
        dbSendQuery(con,sql, ...)
    }
    lapply(sql, dummyfunction, con)
}


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

# function provided by ProtGenerics
fileIsCentroided <- function(msFile){
	sapply(1:length(msFile@scanindex), function(i) 
		getScan(msFile, i) %>% as.data.frame %>% 
		isCentroided)
}

isCentroided <- function(spectra, k = .025, qtl = .9){
	spectra %>% filter(intensity > quantile(spectra$intensity, qtl)) %>% 
		pull(mz) %>% diff %>% quantile(.25) > k
}

deleteProjects <- function(projects=NULL){
	print('delete projects')
	print(list(projects=projects))
	if(is.null(projects)) return()
	
	deleteProject_sample(project_samples() %>% 
		filter(project %in% projects) %>% 
		pull(project_sample))
		
	query <- sprintf("delete from project where project in (%s);",
		paste(projects, collapse=', ', sep=''))
	print(query)
	dbSendQueries(db, query)
	actualize$projects <- TRUE
}

deleteProject_sample <- function(project_samples = NULL){
	print('delete project_samples')
	if(is.null(project_samples)) return()
	deleteDatas(project_samples)
	query <- sprintf('delete from project_sample where project_sample in (%s);',
		paste(project_samples, collapse=', '))
	print(query)
	dbSendQuery(db, query)
	actualize$project_samples <- TRUE
}

deleteDatas <- function(project_samples = NULL){
	print('delete datas')
	if(is.null(project_samples)) return()
	
	# get params IDs
	query <- sprintf('select param from param 
		where project_sample in (%s);', paste(project_samples, collapse=', '))
	print(query)
	params <- dbGetQuery(db, query)$param
	if(length(params) == 0){
		print('no data to delete')
		return()
	} else {
		query1 <- sprintf('delete from param where param in (%s);', 
			paste(params, collapse=', '))
		query2 <- sprintf('delete from feature where cluster in (
			select cluster from cluster where project_sample in (%s));', 
			paste(project_samples, collapse=', '))
		query3 <- sprintf('delete from cluster where project_sample in (%s);', 
			paste(project_samples, collapse=', '))
		print(query1)
		print(query2)
		print(query3)
		dbSendQueries(db, c(query1, query2, query3))
	}
}

deleteSamples <- function(samples=NULL){
	print('delete samples')
	print(list(samples=samples))
	if(is.null(samples)) return()
	
	deleteProject_sample(project_samples() %>% 
		filter(sample %in% samples) %>% 
		pull(project_sample))
		
	query <- sprintf("delete from sample where sample in (%s);",
		paste('\"', samples, '\"', collapse=', ', sep=''))
	print(query)
	dbSendQuery(db, query)
	actualize$samples <- TRUE
}

# compute the score 
calculateScore <- function(cluster, theoric, tolMz, tolI){
	if(nrow(cluster) == 0 | nrow(theoric) == 0 | !is.numeric(tolMz) | !is.numeric(tolI)) return(0)
	cluster <- cluster[which(cluster$abundance > 0), ]
	theoric <- theoric[which(theoric$abundance > 0), ]
	cluster <- cluster[order(cluster$abundance, decreasing=TRUE), ]
	# apply some weights for each feature (observed or theoretic)
	theoric <- cbind(theoric, weight=sapply(theoric$abundance, function(x) x/sum(theoric$abundance)))
	cluster <- cbind(cluster, weight=sapply(cluster$abundance, function(x) x/sum(cluster$abundance)))
	theoric <- theoric[order(theoric$abundance, decreasing=TRUE), ]
	score <- round(100*(1 - calculateScore2(cluster, theoric, tolMz, tolI)), digits=2)
	if(score < 0) return(0)
	return(score)
}
calculateScore2 <- function(cluster, theoric, tolMz, tolI){
	if(nrow(theoric) == 0 | nrow(cluster) == 0) return(sum(theoric$weight) + sum(cluster$weight))
	res <- calculateScore3(cluster, theoric[1, ], tolMz, tolI)
	cluster <- res$cluster
	score <- res$score + calculateScore2(cluster, theoric[-1, ], tolMz, tolI) # cumulative sum
}
calculateScore3 <- function(cluster, theoricFeature, tolMz, tolI){
	# first search peaks in the range tolMz of the theoric feature
	rangeMz <- c(theoricFeature$mz - tolMz, theoricFeature$mz + tolMz)
	observedFeatures <- cluster[which(cluster$mz >= rangeMz[1] & cluster$mz <= rangeMz[2]), ]
	if(nrow(observedFeatures) == 0) return(list(score=1 * theoricFeature$weight, cluster=cluster))
	# then apply a score according to their relative intensity
	deviation <- sapply(observedFeatures$abundance, function(x) abs((theoricFeature$abundance - x)/tolI))
	# return the minimal score (0: perfect match, 1: wrong match)
	if(min(deviation) >= 1) return(list(score=1 * theoricFeature$weight, cluster=cluster)) 
	observedFeature <- observedFeatures[which(deviation == min(deviation)), ]
	# remove the matched feature for avoid an other assignation with a feature
	if(nrow(observedFeatures) > 0) observedFeature <- observedFeature[1, ]
	return(list(score=min(deviation) * theoricFeature$weight, 
		cluster=cluster[which(row.names(cluster) != row.names(observedFeature)), ]))
}


arrangeEic <- function(eic, msFile){
	eic <- eic %>% as.data.frame
	colnames(eic) <- c('x', 'y')
	eic
}


arrangeEic2 <- function(eic, msFile){
	eic <- eic %>% as.data.frame
	colnames(eic) <- c('x', 'y')
	eic %>% mutate(x = msFile@scantime[x] / 60)
}