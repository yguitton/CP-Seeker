targetROIs <- function(sample, adduct, file, data, rtmin, rtmax, ppm, prefilterS, prefilterL, noise, snthresh, tolAbd){
	print(data[1, 'molecule'])
	res <- data.frame(mz=c(), AUC=c(), scans=c(), deviation=c())
	for(mz in data$mz){
		row <- targetROI(file, mz, rtmin, rtmax, ppm, prefilterS, prefilterL, noise, snthresh)
		if(is.null(row)) break
		else res <- rbind(res, row)
		if(mz == data$mz[1]){
			scans <- as.numeric(strsplit(res[1, 'scans'], ', ')[[1]])
			scmin <- scans[1] - length(scans) / 2
			scmax <- scans[length(scans)] + length(scans) / 2
			if(scmin < 1) scmin <- 1
			if(scmax > max(scanIndex(file))) scmax <- max(scanIndex(file))
			rtmin <- rtime(file)[scmin]
			rtmax <- rtime(file)[scmax]
		}
	}
	if(nrow(res) < 2) return(NULL)
	res$abd <- res$AUC * 100 / res[1, "AUC"]
	weight <- res$AUC / sum(res$AUC)
	delta <- sapply(1:nrow(res), function(i) abs(res[i, 'mz']-data[i, 'mz']) / tolAbd)
	delta[which(delta > 1)] <- 1
	score <- 100 * (1 - sum(delta * weight))
	if(score == 0) return(NULL)
	print(res)
	recordROIs(sample, data, res, ppm, prefilterS, prefilterL, noise, snthresh, tolAbd, score)
	return(res)
}
	
targetROI <- function(file, mz, rtmin, rtmax, ppm, prefilterS, prefilterL, noise, snthresh){
	tolMZ <- mz * ppm / 10**6
	eic <- chromatogram(file, mz=c(mz-tolMZ, mz+tolMZ), rt=c(rtmin, rtmax), missing=0)[1, 1]
	# eic <- chromatogram(file, mz=c(mz-tolMZ, mz+tolMZ), missing=0)[1, 1]
	# plot(eic)
	# 1st search an ROI (continuous points)
	# scans don't represent the real scans, just the ids in the eic@intensity!!!
	ids_scans <- which(eic@intensity > noise)
	scans <- as.numeric(sapply(strsplit(names(ids_scans), 'F1.S'), function(x) x[2]))
	if(length(scans) == 0) return(NULL)
	# split scans into a suite
	scans <- split(scans, cumsum(c(TRUE, diff(scans) > 2)))
	ids_scans <- split(ids_scans, cumsum(c(TRUE, diff(ids_scans) > 2)))
	ids <- sapply(ids_scans, function(x) substractNoise(eic, x, noise, snthresh))
	scans <- scans[ids]
	ids_scans <- ids_scans[ids]
	if(length(scans) == 0) return(NULL)
	ids <- sapply(scans, function(x) testROI(file, x, mz, tolMZ, prefilterS, prefilterL))
	scans <- scans[ids]
	ids_scans <- ids_scans[ids]
	if(length(scans) == 0) return(NULL)
	AUC <- trapz(eic@rtime[unlist(ids_scans)], eic@intensity[unlist(ids_scans)])
	mzs <- unlist(mz(file[unlist(scans)]))
	scans <- lapply(scans, function(x) 
		c(if(min(x) != 1) min(x)-1, x, 
			if(max(x) != max(scanIndex(file)))max(x)+1))
	mzObserved <- mzs[which.min(abs(mzs - mz))]
	deviation <- abs(mzObserved - mz) / mz * 10**6
	scans <- paste(unlist(scans), collapse=', ')
	return(data.frame(mz=mzObserved, AUC=AUC, scans=scans, deviation=deviation, stringsAsFactors=FALSE))
}
	

# 2nd get hits of the mass
testROI <- function(file, scans, mz, tolMZ, prefilterS, prefilterL){
	points <- data.frame(mz=unlist(mz(file[scans])), intensity=unlist(intensity(file[scans])))
	points <- points[which(points$intensity > prefilterL), ]
	points <- points[which(points$mz >= mz-tolMZ & points$mz <= mz+tolMZ), ]
	nrow(points) > prefilterS
}

# 3rd
substractNoise <- function(eic, scans, noise, snthresh){
	# noise <- mean(eic@intensity[which(eic@intensity > 0)]) + noise
	outer <- eic@intensity[-scans]
	outer <- outer[which(outer > noise)]
	if(length(outer) < 2) return(TRUE)
	baseline <- mean(outer)
	sdnoise <- sd(outer)
	(max(eic@intensity[scans]) - baseline) / sdnoise > snthresh
}

recordROIs <- function(sample, data, res, ppm, prefilterS, prefilterL, noise, snthresh, tolAbd, score){
	db <- dbConnect(SQLite(), sqlitePath)
	query <- sprintf('insert into observed (molecule, ppm, tolAbd, score, sample, ppmDeviation, prefilterS, prefilterL, noise, snthresh) values
		(%s, %s, %s, %s, "%s", %s, %s, %s, %s, %s);',
		data[1, 'molecule'], ppm, tolAbd, score, sample, mean(res$deviation), prefilterS, prefilterL, noise, snthresh)
	dbSendQuery(db, query)
	id <- dbGetQuery(db, 'select last_insert_rowid();')$last_insert_rowid[1]
	apply(res, 1, function(x) dbSendQuery(db, sprintf('insert into measured 
		(observed, mz, auc, abd, scans) values (%s, %s, %s, %s, "%s");',
		id, x['mz'], x['AUC'], x['abd'], x['scans'])))
	dbDisconnect(db)
}