targetROI <- function(files, data, ppm, prefilterS, prefilterL, tolAbd){
	# first search basepeaks of chloroparaffins
	mzs <- data[which(data$abd == 100), 'mz']
	tol <- mzs * ppm / 10**6
	chroms <- chromatogram(files, mz=data.frame(mzmin=mzs-tol, mzmax=mzs+tol), missing=0)
	scans <- apply(chroms, c(1, 2), function(x) findScans(x[[1]], prefilterS, prefilterL))
	# then integrate in each file!
	index <- split(1:length(files), sapply(strsplit(names(rtime(files)), '\\.'), function(x) x[1]))
	files <- sapply(index, function(x) files[x])
	for(col in 1:ncol(scans)){
		i <- which(!sapply(scans[, col], is.null))
		if(length(i) == 0) next
		formulas <- data[which(data$mz %in% mzs[i] & data$abd == 100), 'formula']
		subData <- data[which(data$formula %in% formulas), ]
		subData <- cbind(subData, mzmin=subData$mz-rep(tol[i], each=5), mzmax=subData$mz+rep(tol[i], each=5),
			rtmin=rep(rtime(files[[col]])[sapply(scans[i, col], min)], each=5),
			rtmax=rep(rtime(files[[col]])[sapply(scans[i, col], max)], each=5))
		# in case if the max scans is equal to number of spectras + 1
		subData[which(is.na(subData$rtmax)), 'rtmax'] <- rtime(files[[col]])[length(files[[col]])]
		chroms <- chromatogram(files[[col]], mz=subData[, c('mzmin', 'mzmax')], rt=subData[, c('rtmin', 'rtmax')], missing=0)
		for(j in seq(1, nrow(chroms), by=5)){
			res <- integrateROI(files[[col]], chroms[j:(j+4), ], subData[j:(j+4), 'mz'], prefilterS, prefilterL)
			if(nrow(res) < 2) next
			res$abd <- res$AUC*100/max(res$AUC)
			ppmDeviation <- mean(res$ppmDeviation)
			score <- computeScore(subData[j:(j+4), 'abd'], res$abd, tolAbd)
			sample <- strsplit(as.character(phenoData(files[[col]])@data$sampleNames), '\\.mzXML$')[[1]][1]
			print(res)
			recordTarget(res, subData[j, 'molecule'], ppm, tolAbd, score, ppmDeviation, sample, prefilterS, prefilterL)
		}
	}
}
	
findScans <- function(chrom, prefilterS, prefilterL){
	scans <- which(chrom@intensity >= prefilterL)
	if(length(scans) == 0) return(NULL)
	# split scans into a suite
	scans <- split(scans, cumsum(c(TRUE, diff(scans) != 1)))
	maxScans <- max(listLen(scans))
	if(maxScans < prefilterS) return(NULL)
	scans <- scans[which(listLen(scans) == maxScans)]
	if(length(scans) > 1){
		sumI <- sapply(scans, function(x) sum(chrom@intensity[x]))
		scans <- scans[which(listLen(scans) == maxScans)]
	}
	scans <- scans[[1]]
	# add the last scan
	res <- if(min(scans) == 1) 1 else min(scans)-1
	res <- c(res, max(scans)+1)
	return(res)
}

integrateROI <- function(file, chroms, mzs, prefilterS, prefilterL, res=data.frame()){
	chrom <- if(is.null(nrow(chroms))) chroms else chroms[1, 1]
	scans <- findScans(chrom, prefilterS, prefilterL)
	if(is.null(scans)) return(res)
	else if(scans[2] > length(chrom@intensity)) scans[2] <- length(chrom@intensity)
	AUC <- trapz(chrom@rtime[scans[1]:scans[2]], chrom@intensity[scans[1]:scans[2]])
	if(nrow(res) > 0) if(AUC > res[nrow(res), 'AUC'] * 1.2) return(res)
	scans <- sapply(strsplit(names(chrom@rtime[scans[1]:scans[2]]), 'S'), function(x) as.numeric(x[2]))
	scans <- c(min(scans), max(scans))
	mzObserved <- unlist(mz(file[min(scans):max(scans)]))
	mzObserved <- mzObserved[which.min(abs(mzObserved - mzs[1]))]
	res <- rbind(res, data.frame(mz=mzObserved, AUC=AUC, rtmin=rtime(file)[scans[1]], rtmax=rtime(file)[scans[2]], abd=0, ppmDeviation=abs(mzObserved-mzs[1])/mzs[1]*10**6))
	if(length(mzs > 1))	res <- integrateROI(file, chroms[2:nrow(chroms), ], mzs[-1], prefilterS, prefilterL, res)
	return(res)
}


computeScore <- function(theoric, observed, tolAbd){
	weight <- sapply(1:length(observed), function(x) theoric[x] / sum(theoric))
	deltaAbundance <- sapply(1:length(observed), function(x) abs((observed[x] - theoric[x]) / tolAbd))
	deviation <- deltaAbundance
	deviation[which(deviation > 1)] <- 1
	score <- 100 * (1 - sum(deviation * weight))
	return(round(score, digits=0))
}

recordTarget <- function(res, molecule, ppm, tolAbd, score, ppmDeviation, sample, prefilterS, prefilterL){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('insert into observed (molecule, tolPpm, tolAbd, score, sample, ppmDeviation, prefilterS, prefilterL) values
		(%s, %s, %s, %s, "%s", %s, %s, %s);',
		molecule, ppm, tolAbd, score, sample, ppmDeviation, prefilterS, prefilterL)
	dbSendQuery(db, query)
	id <- dbGetQuery(db, 'select last_insert_rowid();')$last_insert_rowid[1]
	apply(res, 1, function(x) dbSendQuery(db, sprintf('insert into measured (observed, mz, auc, abd, rtmin, rtmax) values (%s, %s, %s, %s, %s, %s);',
		id, x['mz'], x['AUC'], x['abd'], x['rtmin'], x['rtmax'])))
	dbDisconnect(db)
}