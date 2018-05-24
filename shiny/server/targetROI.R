recordTarget <- function(db, sample, xraw, molecule, data, rt, tolPpm, tolAbd, threshold){
	res <- targetMasses(xraw, data$mz, tolPpm, rt, threshold)
	AUC <- res$AUC
	if(length(AUC) < 2) return()
	else print('FIND')
	deviationPpm <- res$deviationPpm
	mzObserved <- res$mzObserved
	observed <- sapply(AUC, function(x) round((x*100)/AUC[1], digits=2))
	score <- computeScore(data$abd, observed, tolAbd)
	query <- sprintf('insert into observed (molecule, tolPpm, rtMin, rtMax, threshold, tolAbd, score, ppmDeviation, sample) values (%s, %s, %s, %s, %s, %s, %s, %s, "%s");',
		molecule, tolPpm, round(rt[1]/60, digits=2), round(rt[2]/60, digits=2), threshold, tolAbd, score, round(mean(deviationPpm), digits=2), sample)
	dbSendQuery(db, query)
	observedId <- dbGetQuery(db, 'select last_insert_rowid() from observed;')$last_insert_rowid[1]
	query <- sprintf('insert into measured (observed, mz, auc, abd) values %s;',
		paste(sapply(1:length(observed), function(x)
			sprintf('(%s, %s, %s, %s)',
				observedId, mzObserved[[x]], AUC[[x]], observed[[x]])),
		sep='', collapse=', '))
	dbSendQuery(db, query)
}

targetMasses <- function(xraw, mzs, tolPpm, rt, threshold){
	AUC <- c()
	mzList <- c()
	deviationPpm <- c()
	for(i in 1:length(mzs)){
		mz <- mzs[i]
		mzRange <- c(mz-(mz*tolPpm)/10**6, mz+(mz*tolPpm)/10**6)
		points <- extractMsData(xraw, mz=mzRange, rt=rt)[[1]]
		points <- points[which(points$i >= threshold), ]
		#compute the area
		tmpAUC <- trapz(points$rt, points$i)
		#test if the intensity decrease
		if(tmpAUC == 0) return(list(AUC=AUC, mzObserved=mzList, deviationPpm=deviationPpm))
		else if(tmpAUC > 0 & i == 1) mzObserved <- points[which(points$i == max(points$i)), 'mz']
		else if(tmpAUC >= AUC[i - 1]*1.2) return(list(AUC=AUC, mzObserved=mzList, deviationPpm=deviationPpm))
		else mzObserved <- points[which(points$i == max(points$i)), 'mz']
		if(mzObserved == -Inf) return(list(AUC=AUC, mzObserved=mzList, deviationPpm=deviationPpm))
		else{
			AUC <- c(AUC, tmpAUC)
			mzList <- c(mzList, mzObserved)
			deviationPpm <- c(deviationPpm, round(abs((mzObserved - mz) / mz * 10**6), digits=2))
		}
	}
	return(list(AUC=AUC, mzObserved=mzList, deviationPpm=deviationPpm))	
}

computeScore <- function(theoric, observed, tolAbd){
	weight <- sapply(1:length(observed), function(x) theoric[x] / sum(theoric))
	observed <- sapply(1:length(observed), function(x) if(x < 2) observed[x] else (observed[x] * 100) / observed[x-1])
	theoric <- sapply(1:length(observed), function(x) if(x < 2) theoric[x] else (theoric[x] * 100) / theoric[x-1])
	# deltaMz <- sapply(deviationPpm, function(x) x/tolPpm)
	deltaAbundance <- sapply(1:length(observed), function(x) abs((observed[x] - theoric[x]) / tolAbd))
	# deviation <- sqrt(deltaMz**2 + deltaAbundance**2)
	deviation <- deltaAbundance
	deviation[which(deviation > 1)] <- 1
	score <- 100 * (1 - sum(deviation * weight))
	return(round(score, digits=0))
}
