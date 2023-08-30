#' @title Get EIC & m/z vals
#' 
#' @description
#' Get EIC & all m/z values for a specific m/z range
#' For EIC will aggregate m/z values obtained by the scan number by summing all intensities
#'
#' @param xr xcmsRaw xcmsRaw object from xcms (represent a file)
#' @param mz_range vector(integer, 2) m/z min & max (borns)
#'
#' @return list of two items:
#' \itemize{
#' 		\item mzmat matrix with 3 columns:
#' 		\itemize{
#' 			\item scan integer scan number
#' 			\item mz float m/z
#' 			\item int float intensity
#'		}
#' 		\item eic matrix(numeric) with 3 columns: 
#' 		\itemize{
#' 			\item scan integer scan number
#' 			\item rt float time retention (seconds)
#' 			\item int float intensity
#' 		}
#' }
#'
#' @example
#' \dontrun{get_eic(xr, c(640.636, 640.637))}
get_mzmat_eic <- function(xr, mz_range) {
	ids <- which(xr@env$mz >= mz_range[[1]] & 
		xr@env$mz <= mz_range[[2]])
	scans <- sapply(ids, function(id) which.min(abs(xr@scanindex - id)))
	mzmat <- matrix(c(scans, xr@env$mz[ids], xr@env$intensity[ids]), 
		ncol = 3, dimnames = list(c(), c("scan", "mz", "int")))
	eic <- matrix(seq(xr@scantime), ncol = 1, dimnames = list(c(), c("scan")))
	eic <- cbind(eic, int = 0)
	if (nrow(mzmat) > 0) {
		eic <- rbind(eic, mzmat[, c("scan", "int")])
		eic <- aggregate(int ~ scan, data = eic, FUN = sum)
	}
	eic <- cbind(eic, rt = xr@scantime)
	list(
		mzmat = mzmat, 
		eic = eic
	)
}

#' @title Detect ROIs
#' 
#' @description
#' Detect ROIs. A ROI is a region with consecuting scans above the noise substracted to baseline
#' The baseline is considered as a runin median with a window width of `number of scans in file / 3`
#' It accept non-consecutive scans in a ROI depending the parameter `missing_scans`
#' 
#' @param ints vector(float) intensities
#' @param min_width integer minimum width of a ROI in scans
#' @param missing_scans integer number of scan before consider it they are not consecutive
#'
#' @return vector with 2 integer: min & max born of ROI
get_rois <- function(ints, min_width, missing_scans = 1) {
	baseline <- suppressWarnings(runmed(ints, length(ints) / 3, endrule = "constant", algorithm = "Turlach"))
	rois <- which(ints - baseline > 0)
	if (length(rois) == 0) return(NULL)
	rois <- split(rois, cumsum(c(TRUE, diff(rois) > missing_scans + 1)))
	rois <- rois[which(lengths(rois) >= min_width)]
	if (length(rois) == 0) return(NULL)
	roi <- rois[which(sapply(1:length(rois), function(x) which.max(ints) %in% rois[[x]]))]
	if (length(roi) == 0) return(NULL)
	else range(roi)
}

#' @title Overlap ROIs
#'
#' @description 
#' Merge overlapping ROIs. Simple function. Too lazy to explain it
#'
#' @param rois list with 2 integer per item: min & max born of each ROI
#'
#' @return list with 2 integer per item: min & max born of each merged ROI
overlap_rois <- function(rois) {
	if (length(rois) < 2) return(rois)
	rois_final <- rois[1]
	rois <- rois[-1]
	for (roi in rois) {
		id <- which(sapply(rois_final, function(x) 
			x[1] <= roi[2] & x[2] >= roi[1]))
		if (length(id) == 0) rois_final <- append(rois_final, list(roi))
		else {
			rois_final[[id]][1] <- min(rois_final[[id]][1], roi[1])
			rois_final[[id]][2] <- max(rois_final[[id]][2], roi[2])
		}
	}
	rois_final
}

#' @title Search the minimum of a curve
#' 
#' @description 
#' Search the minimum of a normal distribution
#' For left & right: made a sequence from the center to it; 
#' Then search were the curve augment (where the difference between two points is negative)
#'
#' @param y vector(floats) y values
#' @param center integer x value
#' @param minPts integer minimum of consecutive points to consider the curve to be at the minimum
#'
#' @return two floats: the borns left / right
descend_min <- function(y, center, minPts = 1) {
	left <- if (center != 1) {
		lefts <- split((center - 1):1, 
			cumsum(diff(y[center:1]) < 0))
		limit <- which(lengths(lefts) > minPts + 1)[1]
		if (is.na(limit)) 1 else lefts[[limit]][1]
	} else center
	right <- if (center != length(y)) {
		rights <- split((center + 1):length(y), 
			cumsum(diff(y[center:length(y)]) < 0))
		limit <- which(lengths(rights) > minPts + 1)[1]
		if (is.na(limit)) 1 else rights[[limit]][1]
	} else center
	c(left, right)	
}

#' @title Extend the x range
#'
#' @description
#' Given a x range, it will try to extend it until it found 0 values in y
#'
#' @param xrange two integer: born min / max of x
#' @param center integer center to start
#' @param y vector(floats) y values
#' @param minPts integer minimum of consecutive points to consider reaching sufficient 0 values
#' 
#' @return two integer: the new borns min / max
narrow_rt_boundaries_extend <- function(xrange, center, y, minPts = 1){
	left <- if(xrange[1] != 1){
		lefts <- (xrange[1]:1)[which(y[xrange[1]:1] <= 0)]
		limits <- split(lefts, cumsum(c(TRUE, diff(lefts) > 1)))
		limit <- which(lengths(limits) > minPts + 1)[1]
		if(is.na(limit)) 1 else limits[[limit]][1]
	} else xrange[1]
	
	right <- if(xrange[2] != length(y)){
		rights <- (xrange[2]:length(y))[which(y[xrange[2]:length(y)] <= 0)]
		limits <- split(rights, cumsum(c(TRUE, diff(rights) > 1)))
		limit <- which(lengths(limits) > minPts + 1)[1]
		if(is.na(limit)) length(y) else limits[[limit]][1]
	} else xrange[2]
	c(left, right)
}

#' @title Decrease the x range
#'
#' @description
#' Given a x range, it will try to decrease it until it found positive y values
#'
#' @param xrange two integer: born min / max of x
#' @param center integer center to start
#' @param y vector(floats) y values
#' @param minPts integer minimum of consecutive points to consider reaching sufficient positive values
#' 
#' @return two integer: the new borns min / max
narrow_rt_boundaries_reduce <- function(xrange, center, y, minPts = 1){
	left <- if(xrange[1] != center){
		lefts <- (xrange[1]:center)[which(y[xrange[1]:center] > 0)]
		limits <- split(lefts, cumsum(c(TRUE, diff(lefts) > minPts + 1)))
		limit <- which(lengths(limits) > minPts + 1)[1]
		if(is.na(limit)) xrange[1] else limits[[limit]][1] - 1
	} else xrange[1]
	
	right <- if(xrange[2] != center){
		rights <- (xrange[2]:center)[which(y[xrange[2]:center] > 0)]
		limits <- split(rights, cumsum(c(TRUE, diff(rights) > minPts + 1)))
		limit <- which(lengths(limits) > minPts + 1)[1]
		if(is.na(limit)) xrange[2] else limits[[limit]][1] + 1
	} else xrange[2]
	c(
		ifelse (left < 1, 1 , left), 
		ifelse (right > length(y), length(y), right)
	)
}

#' @title Search overlaping peaks
#'
#' @description
#' Search overlapping peaks to fusion them. The eic is needed to recompute into if needed
#'
#' @param peaks matrix with columns:
#' \itemize{
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' }
#' @param eic matrix(numeric) with two columns: 
#' \itemize{
#' 		\item rt float time retentions (seconds)
#' 		\item scan integer scan number
#' 		\item int float intensities
#' }
#' @param baseline vector(float) baseline
#' @param noise float noise
#' 
#' @return matrix with columns:
#' \itemize{
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' }
overlap_peaks <- function(peaks, eic, baseline, noise) {
	if (is.null(peaks)) return(peaks) 
	else if(nrow(peaks) < 2) return(peaks)
	peaks <- peaks[order(peaks[, "intb"], decreasing = TRUE), ]
	peaks_final <- peaks[1, ]
	peaks <- peaks[-1, ]
	for (i in seq(nrow(peaks))) {
		# search any peaks in peaks_final that overlap the peak
		ids <- which(sapply(1:nrow(peaks_final), function(j)
			peaks_final[j, 'mzmax'] >= peaks[i, 'mzmin'] & 
			peaks_final[j, 'mzmin'] <= peaks[i, 'mzmax'] & 
			peaks_final[j, 'rtmax'] >= peaks[i, 'rtmin'] & 
			peaks_final[j, 'rtmin'] <= peaks[i, 'rtmax']
		))
		if(length(ids) == 0) peaks_final <- rbind(peaks_final, peaks[i, ])
		else peaks_final[ids[1], ] <- merge_peaks(
			rbind(peaks_final[ids[1], ], peaks[i, ]), 
			eic, baseline, noise)
	}
	peaks_final
}

#' @title Merge two peaks
#' 
#' @description
#' Merge two peaks. Recompute into, intb & s/n
#' 
#' @param peaks matrix of 2 rows with columns:
#' \itemize{
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' }
#' @param eic matrix(numeric) with two columns: 
#' \itemize{
#' 		\item rt float time retentions (seconds)
#' 		\item scan integer scan number
#' 		\item int float intensities
#' }
#' @param baseline vector(float) baseline
#' @param noise float noise
#' 
#' @return matrix of 1 row with columns:
#' \itemize{
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' }
merge_peaks <- function(peaks, eic, baseline, noise){
	lm <- c(min(peaks$lmin), max(peaks$lmax))
	new_peak <- data.frame(
		mz = peaks[1, "mz"], 
		mzmin = min(peaks[, "mzmin"]), 
		mzmax = max(peaks[, "mzmax"]), 
		rt = peaks[1, "rt"], 
		rtmin = min(peaks[, "rtmin"]), 
		rtmax = max(peaks[, "rtmax"]), 
		into = pracma::trapz(eic[lm[1]:lm[2], 'rt'], eic[lm[1]:lm[2], 'int']),
		intb = pracma::trapz(eic[lm[1]:lm[2], 'rt'], eic[lm[1]:lm[2], 'int'] - baseline[lm[1]:lm[2]]), 
		maxo = max(peaks[, "maxo"]), 
		sn = if (noise == 0) intb 
		  else intb / pracma::trapz(rep(noise, diff(lm) + 1)),
		scale = peaks[1, "scale"], 
		scpos = peaks[1, "scpos"], 
		scmin = min(peaks[, "scmin"]), 
		scmax = max(peaks[, "scmax"]), 
		lmin = lm[1], 
		lmax = lm[2]
	)
}

#' @title Integrate peak(s)
#' 
#' @description
#' Integrate peak(s) on a specified ROI
#' 
#' @param eic matrix with columns:
#' \itemize{
#' 		\item scan integer scan number
#' 		\item rt float rT (in seconds)
#' 		\item int float intensity
#' }
#' @param scalerange vector(integer)[2] peakwidth range converted from seconds to scans
#' @param baseline vector(float) baseline
#' @param noise float noise
#' @param missing_scans integer number of scans to consider them not consecutive
#' @mzmat matrix with columns:
#' \itemize{
#' 		\item scan integer scan number
#' 		\item mz float m/z
#' 		\item int float intensity
#' }
#' 
#' @return matrix with columns:
#' \itemize{
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' }
integrate <- function(eic, scalerange, baseline, noise, missing_scans, mzmat) {
	# make centwaves
	scales <- seq(from = scalerange[1], to = scalerange[2], by = 2)
	wCoefs <- xcms:::MSW.cwt(eic[, "int"], scales = scales, wavelet = "mexh")
	if (is.null(wCoefs)) return(NULL)
	# smooth cause some are a little jagged
	wCoefs <- apply(wCoefs, 2, function(x) smooth.spline(x, spar = 0)$y)
	
	if (all(sapply(wCoefs, function(x) all(x - baseline < noise)))) return(NULL)
	# get ridge lines
	localMax <- xcms:::MSW.getLocalMaximumCWT(wCoefs)
	rL <- xcms:::MSW.getRidge(localMax)
	rL <- rL[which(sapply(rL, function(x) 
		any(wCoefs[x, length(x)] - baseline[x] >= noise) & 
			length(unique(x)) > 1
	))]
	if(length(rL) == 0) return(NULL)
	irange <- ceiling(scales[1] / 2)
	overlap_peaks(unique(do.call(rbind, lapply(rL, function(opp) {
		# search best scale
		inti <- sapply(opp, function(x) {
			left <- ifelse (x - irange > 1, x - irange, 1)
			right <- ifelse (x + irange < nrow(eic), x + irange, nrow(eic))
			sum(eic[left:right, "int"])
		})
		maxpi <- which(inti == max(inti))
		if (length(maxpi) > 1) {
			m <- wCoefs[opp[maxpi], maxpi]
			bestcol <- which(m == max(m), arr.ind = TRUE)[2]
			best.scale <- maxpi[bestcol]
		} else best.scale <- maxpi
		best.scale.pos <- opp[best.scale]
		# now try to find end of both side of the scale
		lwpos <- max(1, best.scale.pos - best.scale)
		rwpos <- min(nrow(eic), best.scale.pos + best.scale)
		lm <- descend_min(wCoefs[, best.scale], best.scale.pos)
		if (length(lm) < 2) return(NULL)
		else if (lm[2] - lm[1] < scales[1]) return(NULL)
		integrate2(eic, lm, baseline, noise, missing_scans, mzmat, 
			best.scale)
	}))), eic, baseline, noise)
}

#' @title Integrate peak(s)
#' 
#' @description
#' Force integration of a peak on a specified range
#' Try to extend the range until 0 intensity values, then reduce it
#' 
#' @param eic matrix with columns:
#' \itemize{
#' 		\item scan integer scan number
#' 		\item rt float rT (in seconds)
#' 		\item int float intensity
#' }
#' @param lm vector(integer)[2] scan range where to integrate
#' @param baseline vector(float) baseline
#' @param noise float noise
#' @param missing_scans integer number of scans to consider them not consecutive
#' @mzmat matrix with columns:
#' \itemize{
#' 		\item scan integer scan number
#' 		\item mz float m/z
#' 		\item int float intensity
#' }
#' @param scale integer scale used for integration with centwave, just use as an information
#' 
#' @return matrix with columns:
#' \itemize{
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' }
integrate2 <- function(eic, lm, baseline, noise, missing_scans, mzmat, scale = NA) {
	if (nrow(mzmat) == 0) return(NULL)
	center <- ceiling(sum(lm) / 2)
	lm <- narrow_rt_boundaries_extend(lm, center, 
		eic[, "int"] - baseline, missing_scans)
	lm <- narrow_rt_boundaries_reduce(lm, center, 
		eic[, "int"] - baseline, missing_scans)
	if(diff(lm) <= 0) return(NULL)
	
	mz_vals <- mzmat[which(
		mzmat[, "scan"] >= eic[lm[1], "scan"] & 
		mzmat[, "scan"] <= eic[lm[2], "scan"] & 
		mzmat[, "int"] > 0), , drop = FALSE]
	if (nrow(mz_vals) == 0) return(NULL)
	mz_range <- range(mz_vals[, "mz"])
	mz <- do.call(xcms:::mzCenter.wMean, list(
		mz = mz_vals[, "mz"], 
		intensity = mz_vals[, "int"]))
	data.frame(
		mz = mz, 
		mzmin = mz_range[1], 
		mzmax = mz_range[2], 
		rt = eic[center, 'rt'] / 60, 
		rtmin = eic[lm[1], 'rt'] / 60, 
		rtmax = eic[lm[2], 'rt'] / 60, 
		into = pracma::trapz(eic[lm[1]:lm[2], 'rt'], eic[lm[1]:lm[2], 'int']),
		intb = pracma::trapz(eic[lm[1]:lm[2], 'rt'], eic[lm[1]:lm[2], 'int'] - baseline[lm[1]:lm[2]]), 
		maxo = max(mz_vals[, "int"]), 
		sn = if (noise == 0) intb 
			else intb / pracma::trapz(rep(noise, diff(lm) + 1)),
		scale = scale, 
		scpos = eic[center, "scan"], 
		scmin = eic[lm[1], "scan"], 
		scmax = eic[lm[2], "scan"], 
		lmin = lm[1], 
		lmax = lm[2]
	)
}

#' @title Target chemicals
#' 
#' @description
#' Target chemicals
#' first it will search ROIs in the basepeak's trace
#' for all ROI founded, it will force an integration for each isotopologue (extend & reduce the integration area from the ROI)
#' 
#' @param xr xcmsRaw xcmsRaw object
#' @param theoric_patterns list of dataframes (each is a chemical isotopic pattern) with columns
#' \itemize{
#' 		\item mz float m/z
#' 		\item abundance float relative abundance tolerance
#' 		\item iso string isotopologue annotation
#' 		\item mzmin float m/z min born
#' 		\item mzmax float m/z max born
#' }
#' @param chemical_ids vector(integers) vector of IDs to differentiate chemicals integrations in the final table
#' @param scalerange vector(integers)[2] vector with 2 integer representing peakwidth but with unit in scan instead of seconds
#' @param scanrange vector(float)[2] vector with 2 float representing the scan range in seconds
#' @param missing_scans integer number of scan between two of them to consider them consecutive
#' @param pb progressbar progressbar used to print the progress like...all existing progressbar... (optional)
#' @param reintegration boolean TRUE if it's a reintegration
#' 
#' @return dataframe with columns:
#' \itemize{
#' 		\item mz float m/z
#' 		\item mzmin float born m/z min
#' 		\item mzmax float born m/z max
#' 		\item rt float rT
#' 		\item rtmin float born rT min
#' 		\item rtmax float born rT max
#' 		\item into float area integrated
#' 		\item intb float area above baseline integrated
#' 		\item maxo float max intensity
#' 		\item sn float signal/noise
#' 		\item scale integer wave used for integration
#' 		\item scpos integer center scan position
#' 		\item scmin integer born scan min
#' 		\item scmax integer born scan max
#' 		\item lmin integer to ignore
#' 		\item lmax integer to ignore
#' 		\item iso string isotopologue annotation
#' 		\item abundance float abundance
#' 		\item score float isotopic pattern score
#' 		\item deviation float m/z deviation
#' 		\item chemical_id integer id of the chemical
#' 		\item intensities float standardized intensities
#' 		\item weighted_deviation float weighted deviation
#' }
deconvolution <- function(xr, theoric_patterns, chemical_ids, scalerange, scanrange = NULL, 
		missing_scans = 1, pb = NULL, reintegration = FALSE) {
  	peaks <- NULL
	pb_max <- length(theoric_patterns)
	extend_range <- ceiling(scalerange[2] * 1.5)
	for (i in seq(theoric_patterns)) {
		time_begin <- Sys.time()
		traces <- get_mzmat_eic(xr, theoric_patterns[[i]][1, c("mzmin", "mzmax")])
		roi <- get_rois(traces$eic[, "int"], scalerange[1])
		if (length(roi) == 0) next
		traces <- append(list(traces), lapply(2:nrow(theoric_patterns[[i]]), function(j) 
			get_mzmat_eic(xr, theoric_patterns[[i]][j, c("mzmin", "mzmax")])))
		# xcms define noiserange as 1.5x peakwidth_max
		baselines <- lapply(traces, function(x) 
            suppressWarnings(runmed(x$eic[, "int"], nrow(x$eic) / 3, endrule = "constant", algorithm = "Turlach")))
		noises <- sapply(traces, function(x) sd(x$eic[-c(roi[1]:roi[2]), "int"]))
		if(reintegration){
		  roi <- range(which(traces[[1]]$eic[, "int"] > 0 & 
		    traces[[1]]$eic[, "rt"] > scanrange[1]*60 & traces[[1]]$eic[, "rt"] < scanrange[2]*60))
		  if (roi[1] == Inf) next
		  lm <- c(roi[1] - traces[[1]]$eic[roi[1], "scan"] + 1, roi[2] - traces[[1]]$eic[roi[1], "scan"] + 1)
		  basepeaks <- integrate2(traces[[1]]$eic[roi[1]:roi[2], ], lm, 
		  baselines[[1]][roi[1]:roi[2]], noises[1], missing_scans,
		  traces[[1]]$mzmat[which(traces[[1]]$mzmat[, "scan"] %in% roi[1]:roi[2]), , drop = FALSE])
		}else{
		  # extend roi for better integration
  		  roi <- range(
  		    (if (min(roi) - extend_range < 1) 1 else min(roi) - extend_range) : 
  		    (if (max(roi) + extend_range > nrow(traces[[1]]$eic)) nrow(traces[[1]]$eic) else max(roi) + extend_range)
  	      )
  	      basepeaks <- integrate(traces[[1]]$eic[roi[1]:roi[2], ], scalerange, 
  		    baselines[[1]][roi[1]:roi[2]], noises[1], missing_scans,
  		    traces[[1]]$mzmat[which(
  		      traces[[1]]$mzmat[, "scan"] %in% roi[1]:roi[2])
  		    , , drop = FALSE])
		}
		
	  if (length(basepeaks) == 0) next
	  basepeaks <- cbind(basepeaks, abundance = 100, iso = "A")
	  if(is.vector(scanrange)) basepeaks <- basepeaks[(basepeaks$rt > scanrange[1] & basepeaks$rt < scanrange[2]),]
	  if(nrow(basepeaks) == 0) next
		basepeak <- basepeaks[which.max(basepeaks$maxo),]
		peaks2 <- NULL
		scores <- c(theoric_patterns[[i]][1, "weight"])
		deviations <- c(basepeak[1, "mz"] - theoric_patterns[[i]][1, "mz"])
		weight <- c(theoric_patterns[[i]][1, "weight"])
		continue_integration <- TRUE
		k <- 2 # To avoid iso = A
		while (k < length(traces) & continue_integration) {
			eic <- traces[[k]]$eic
			mzmat <- traces[[k]]$mzmat
			peak <- integrate2(eic[roi[1]:roi[2], ], 
				unlist(basepeak[, c("lmin", "lmax")]), 
				baselines[[k]][roi[1]:roi[2]], noises[k], missing_scans, 
				mzmat[which(mzmat[, "scan"] %in% roi[1]:roi[2]), , drop = FALSE])[1, ]
			if (length(peak) > 0) {
				if (k == 2) peaks2 <- basepeak
				peak <- cbind(peak, 
					abundance = peak[1, "intb"] / basepeak[1, "intb"] * 100, 
					iso = theoric_patterns[[i]][k, "iso"])
				scores <- c(scores, (1 - 
					abs(theoric_patterns[[i]][k, "abundance"] - peak[1, "abundance"]) / 
						theoric_patterns[[i]][k, "abundance"]) * 
							theoric_patterns[[i]][k, "weight"])
				deviations <- c(deviations, 
					 peak[1, "mz"] - theoric_patterns[[i]][k, "mz"])
				weight <- c(weight, theoric_patterns[[i]][k, "weight"])
				peaks2 <- rbind(peaks2, peak)
			} else continue_integration <- FALSE
			k <- k + 1
		}
		if (length(peaks2) > 0) {
			peaks <- rbind(peaks, cbind(
					peaks2, 
					score = sum(scores) * 100, 
					deviation = mean(deviations) * 10**3, 
					chemical_ion = chemical_ids[i],
          			intensities = sum(peaks2[,"into"]),
          			weighted_deviations = sum(deviations*weight)/sum(weight)
			))
		}
		
		shinyWidgets::updateProgressBar(session, id = pb, 
			value = i * 100 / pb_max,  
			title = "")
	}
	peaks
}

deconvolution_std <- function(xr, theoric_patterns, chemical_ids = NA, scalerange, scanrange = NULL, 
		missing_scans = 1, pb = NULL, reintegration = FALSE) {
  	peaks <- NULL
	pb_max <- length(theoric_patterns)
	extend_range <- ceiling(scalerange[2] * 1.5)
	for (i in seq(theoric_patterns)) {
		time_begin <- Sys.time()
		traces <- get_mzmat_eic(xr, theoric_patterns[[i]][1, c("mzmin", "mzmax")])
		roi <- get_rois(traces$eic[, "int"], scalerange[1])
		if (length(roi) == 0){
			norois <- c(mz = NA, mzmin = NA, mzmax = NA, rt = NA, rtmin = NA, rtmax = NA, into = NA, intb = NA, maxo = NA, sn = NA, scale = NA,
				scpos = NA, scmin = NA, scmax = NA, lmin = NA, lmax = NA, abundance = NA, iso = "no ROIs", score = NA, deviation = NA, chemical_ion = chemical_ids[i], 
				intensities = NA, weighted_deviations = NA)
			peaks <- rbind(peaks, norois)
			next
		}
		traces <- append(list(traces), lapply(2:nrow(theoric_patterns[[i]]), function(j) 
			get_mzmat_eic(xr, theoric_patterns[[i]][j, c("mzmin", "mzmax")])))
		# xcms define noiserange as 1.5x peakwidth_max
		baselines <- lapply(traces, function(x) 
            suppressWarnings(runmed(x$eic[, "int"], nrow(x$eic) / 3, endrule = "constant", algorithm = "Turlach")))
		noises <- sapply(traces, function(x) sd(x$eic[-c(roi[1]:roi[2]), "int"]))
		if(reintegration){
		  roi <- range(which(traces[[1]]$eic[, "int"] > 0 & 
		    traces[[1]]$eic[, "rt"] > scanrange[1]*60 & traces[[1]]$eic[, "rt"] < scanrange[2]*60))
		  if (roi[1] == Inf) next
		  lm <- c(roi[1] - traces[[1]]$eic[roi[1], "scan"] + 1, roi[2] - traces[[1]]$eic[roi[1], "scan"] + 1)
		  basepeaks <- integrate2(traces[[1]]$eic[roi[1]:roi[2], ], lm, 
		  baselines[[1]][roi[1]:roi[2]], noises[1], missing_scans,
		  traces[[1]]$mzmat[which(
		    traces[[1]]$mzmat[, "scan"] %in% roi[1]:roi[2])
	    , , drop = FALSE])
		}
		else{
		  # extend roi for better integration
  		roi <- range(
  		  (if (min(roi) - extend_range < 1) 1 else min(roi) - extend_range) : 
  		  (if (max(roi) + extend_range > nrow(traces[[1]]$eic)) nrow(traces[[1]]$eic) else max(roi) + extend_range)
  	  )
  	  basepeaks <- integrate(traces[[1]]$eic[roi[1]:roi[2], ], scalerange, 
  		  baselines[[1]][roi[1]:roi[2]], noises[1], missing_scans,
  		  traces[[1]]$mzmat[which(
  		    traces[[1]]$mzmat[, "scan"] %in% roi[1]:roi[2])
  		  , , drop = FALSE])
		}
		
	  if (length(basepeaks) == 0) next
	  basepeaks <- cbind(basepeaks, abundance = 100, iso = "A")
	  if(is.vector(scanrange)) basepeaks <- basepeaks[(basepeaks$rt > scanrange[1] & basepeaks$rt < scanrange[2]),]
	  if(nrow(basepeaks) == 0) next
		basepeak <- basepeaks[which.max(basepeaks$maxo),]
		peaks2 <- NULL
		scores <- c(theoric_patterns[[i]][1, "weight"])
		deviations <- c(theoric_patterns[[i]][1, "mz"] - basepeak[1, "mz"])
		weight <- c(theoric_patterns[[i]][1, "weight"])
		continue_integration <- TRUE
		k <- 2
		while (k < length(traces) & continue_integration) {
			eic <- traces[[k]]$eic
			mzmat <- traces[[k]]$mzmat
			peak <- integrate2(eic[roi[1]:roi[2], ], 
				unlist(basepeak[, c("lmin", "lmax")]), 
				baselines[[k]][roi[1]:roi[2]], noises[k], missing_scans, 
				mzmat[which(mzmat[, "scan"] %in% roi[1]:roi[2]), , drop = FALSE])[1, ]
			if (length(peak) > 0) {
				if (k == 2) peaks2 <- basepeak
				peak <- cbind(peak, 
					abundance = peak[1, "intb"] / basepeak[1, "intb"] * 100, 
					iso = theoric_patterns[[i]][k, "iso"])
				scores <- c(scores, (1 - 
					abs(theoric_patterns[[i]][k, "abundance"] - peak[1, "abundance"]) / 
						theoric_patterns[[i]][k, "abundance"]) * 
							theoric_patterns[[i]][k, "weight"])
				deviations <- c(deviations, 
					peak[1, "mz"] - theoric_patterns[[i]][k, "mz"])
				weight <- c(weight, theoric_patterns[[i]][k, "weight"])
				peaks2 <- rbind(peaks2, peak)
			} else continue_integration <- FALSE
			k <- k + 1
		}
		if (length(peaks2) > 0) {
			peaks <- rbind(peaks, cbind(
					peaks2, 
					score = sum(scores) * 100, 
					deviation = mean(deviations) * 10**3, 
					chemical_ion = chemical_ids[i],
          			intensities = sum(peaks2[,"into"]),
          			weighted_deviations = sum(deviations*weight)/sum(weight)
			))
		}
		
		shinyWidgets::updateProgressBar(session, id = pb, 
			value = i * 100 / pb_max,  
			title = "")
	}
	peaks
}