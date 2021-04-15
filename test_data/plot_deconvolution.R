db_path <- "test_data/database.sqlite"
samples <- "neg 20191105-016_HGold"
project <- 1
adduct <- "M+Cl"
resolution <- list(
	instrument = "orbitrap", 
	index = NA,
	resolution = 140000, 
	mz = 200
)
ppm <- 5
mda <- 0
peakwidth <- c(30, 240)
missing_scans <- 1

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
source("server/func.R")
source("server/chem_func.R")
source("server/db_get.R")
source("server/deconvolution.R")

db <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
ion_forms <- get_chloroparaffin_ions(db, adduct)
theoric_patterns <- get_theoric(ion_forms$ion_formula, 
	ion_forms[1, "charge"], resolution)
theoric_patterns <- lapply(theoric_patterns, function(x) 
	cbind(x, get_mass_range(x[, "mz"], ppm, mda)))
xr <- load_ms_file(db, sampleID = samples[1])
scalerange <- round((peakwidth / mean(diff(xr@scantime))) /2)

#################################################################
# search for C14 Cl6
i <- which(grepl("C14Cl7", ion_forms[, "ion_formula"]))
traces <- get_mzmat_eic(xr, theoric_patterns[[i]][6, c("mzmin", "mzmax")])

#################################################################
# first trace the baseline to see where it cut
ints <- traces$eic[, "int"]
baseline <- runmed(ints, length(ints) / 3, 
	endrule = "constant", algorithm = "Turlach")
p1a <- plotly::layout(
	plotly::plot_ly(type = "scatter", mode = "lines", 
		data = data.frame(traces$eic), x = ~scan, y = ~int), 
	xaxis = list(title = "SCAN", titlefont = list(size = 18)), 
	yaxis = list(title = "INTENSITY", titlefont = list(size = 18)))
p1b <- plotly::add_trace(p1a, 
	y = baseline, color = I("red"), showlegend = FALSE, 
		line = list(width = 10))
p1b

#################################################################
# then search the rois
rois <- which(ints - baseline > 0)
rois <- split(rois, cumsum(c(TRUE, diff(rois) > missing_scans + 1)))
mz_density <- traces$mzmat
mz_density <- cbind(mz_density, 
	mzdiff = (theoric_patterns[[i]][1, "mz"] - mz_density[, "mz"]) * 10**3)
p2a <- plotly::layout(plotly::plot_ly(type = "scatter", mode = "markers", 
	data = data.frame(mz_density), x = ~scan, y = ~mzdiff), 
	xaxis = list(title = "SCAN", titlefont = list(size = 18)), 
	yaxis = list(title = "M/Z DEVIATION (mDa)", titlefont = list(size = 18)))
p2b <- p2a
for (roi in rois) {
	roi_trace <- mz_density[which(
		mz_density[, "scan"] >= roi[1] & 
			mz_density[, "scan"] <= roi[length(roi)]), "mzdiff"]
	p2b <- plotly::add_lines(p2b, 
		x = c(roi[1], roi[1], NA, roi[1], roi[length(roi)], NA, roi[length(roi)], roi[length(roi)], 
			NA, roi[length(roi)], roi[1]), 
		y = c(min(roi_trace), max(roi_trace), NA, max(roi_trace), max(roi_trace), NA, 
			max(roi_trace), min(roi_trace), NA, min(roi_trace), min(roi_trace)), 
		line = list(color = I("black"), dash = "dash"))
}
plotly::subplot(p1b, p2b, nrows = 2, shareX = TRUE)

#################################################################
# now keep only rois which are sufficient width
rois <- rois[which(lengths(rois) >= scalerange[1])]
p2b <- p2a
for (roi in rois) {
	roi_trace <- mz_density[which(
		mz_density[, "scan"] >= roi[1] & 
			mz_density[, "scan"] <= roi[length(roi)]), "mzdiff"]
	p2b <- plotly::add_lines(p2b, 
		x = c(roi[1], roi[1], NA, roi[1], roi[length(roi)], NA, roi[length(roi)], roi[length(roi)], 
			NA, roi[length(roi)], roi[1]), 
		y = c(min(roi_trace), max(roi_trace), NA, max(roi_trace), max(roi_trace), NA, 
			max(roi_trace), min(roi_trace), NA, min(roi_trace), min(roi_trace)), 
		line = list(color = I("black"), dash = "dash"))
}
plotly::subplot(p1b, p2b, nrows = 2, shareX = TRUE)

#################################################################
# compute noise
noise <- sd(traces$eic[-unlist(rois), "int"])
p1c <- plotly::add_trace(p1b, 
	y = noise, line = list(color = I("black"), width = 10))
plotly::subplot(p1c, p2b, nrows = 2, shareX = TRUE)

#################################################################
# now extend roi (xcms use 1.5x peakwidth max so we use the same)
extend_range <- ceiling(scalerange[2] * 1.5)
rois <- lapply(rois, function(roi) range(
	(if (min(roi) - extend_range < 1) 1 else min(roi) - extend_range) : 
	(if (max(roi) + extend_range > length(ints)) length(ints) else max(roi) + extend_range)
))
p2b <- p2a
for (roi in rois) {
	roi_trace <- mz_density[which(
		mz_density[, "scan"] >= roi[1] & 
			mz_density[, "scan"] <= roi[length(roi)]), "mzdiff"]
	p2b <- plotly::add_lines(p2b, 
		x = c(roi[1], roi[1], NA, roi[1], roi[length(roi)], NA, roi[length(roi)], roi[length(roi)], 
			NA, roi[length(roi)], roi[1]), 
		y = c(min(roi_trace), max(roi_trace), NA, max(roi_trace), max(roi_trace), NA, 
			max(roi_trace), min(roi_trace), NA, min(roi_trace), min(roi_trace)), 
		line = list(color = I("black"), dash = "dash"))
}
plotly::subplot(p1c, p2b, nrows = 2, shareX = TRUE)

#################################################################
# search overlapping 
rois <- overlap_rois(rois)
p2b <- p2a
for (roi in rois) {
	roi_trace <- mz_density[which(
		mz_density[, "scan"] >= roi[1] & 
			mz_density[, "scan"] <= roi[length(roi)]), "mzdiff"]
	p2b <- plotly::add_lines(p2b, 
		x = c(roi[1], roi[1], NA, roi[1], roi[length(roi)], NA, roi[length(roi)], roi[length(roi)], 
			NA, roi[length(roi)], roi[1]), 
		y = c(min(roi_trace), max(roi_trace), NA, max(roi_trace), max(roi_trace), NA, 
			max(roi_trace), min(roi_trace), NA, min(roi_trace), min(roi_trace)), 
		line = list(color = I("black"), dash = "dash"))
}
plotly::subplot(p1c, p2b, nrows = 2, shareX = TRUE)

#################################################################
# make centwave waves
roi <- rois[[1]]
scales <- seq(from = scalerange[1], to = scalerange[2], by = 2)
traces$eic <- traces$eic[roi[1]:roi[2], ]
baseline <- baseline[roi[1]:roi[2]]
traces$mzmat <- traces$mzmat[which(
	traces$mzmat[, "scan"] %in% roi[1]:roi[2]), , drop = FALSE]
wCoefs <- xcms:::MSW.cwt(traces$eic[, "int"], scales = scales, wavelet = "mexh")
# smooth cause some are a little jagged
wCoefs <- apply(wCoefs, 2, function(x) smooth.spline(x, spar = 0)$y)
p1a <- plotly::layout(
	plotly::plot_ly(type = "scatter", mode = "lines", 
		data = traces$eic, x = ~scan, y = ~int), 
	xaxis = list(title = "SCAN", titlefont = list(size = 18)), 
	yaxis = list(title = "INTENSITY", titlefont = list(size = 18)))
p1b <- p1a
for (i in seq(ncol(wCoefs))) p1b <- plotly::add_trace(p1b, 
	y = wCoefs[, i], showlegend = FALSE)
p1b

#################################################################
# get ridge lines
localMax <- xcms:::MSW.getLocalMaximumCWT(wCoefs)
rL <- xcms:::MSW.getRidge(localMax)
rL <- rL[which(sapply(rL, function(x) 
	any(wCoefs[x, length(x)] - baseline[x] >= noise) & 
		length(unique(x)) > 1
))]
p1b <- p1a
for (opp in rL) {
	opp_trace <- traces$eic[opp[1]:opp[length(opp)], "int"]
	p1b <- plotly::add_trace(
		plotly::add_lines(p1b, 
			x = c(min(opp), min(opp), NA, max(opp), max(opp)) + min(traces$eic[, "scan"]), 
			y = c(0, max(traces$eic[, "int"]), NA, 0, max(traces$eic[, "int"])),
			line = list(color = I("black"), dash = "dash")), 
		x = range(opp) + min(traces$eic[, "scan"]), 
		y = rep(max(traces$eic[, "int"]), 2), 
		fill = "tozeroy")
}
p1b

irange <- ceiling(scales[1] / 2)
opp <- rL[[1]]
#################################################################
# search best scale
inti <- sapply(opp, function(x) {
	left <- ifelse (x - irange > 1, x - irange, 1)
	right <- ifelse (x + irange < nrow(traces$eic), x + irange, nrow(traces$eic))
	sum(traces$eic[left:right, "int"])
})
maxpi <- which(inti == max(inti))
if (length(maxpi) > 1) {
	m <- wCoefs[opp[maxpi], maxpi]
	bestcol <- which(m == max(m), arr.ind = TRUE)[2]
	best.scale <- maxpi[bestcol]
} else best.scale <- maxpi
best.scale.pos <- opp[best.scale]
p1b <- plotly::add_lines(p1a, y = wCoefs[, best.scale])
p1b

#################################################################
# now try to find end of both side of the scale
lwpos <- max(1, best.scale.pos - best.scale)
rwpos <- min(nrow(traces$eic), best.scale.pos + best.scale)
lm <- descend_min(wCoefs[, best.scale], best.scale.pos)
p1c <- plotly::add_trace(p1b, 
	x = traces$eic[lm, "scan"], 
	y = rep(max(traces$eic[, "int"]), 2), 
	fill = "tozeroy")

#################################################################
center <- ceiling(sum(lm) / 2)
# now extend the integrated area
lm <- narrow_rt_boundaries_extend(lm, center, 
	traces$eic[, "int"] - baseline, missing_scans)
p1d <- plotly::add_trace(p1b, 
	x = traces$eic[lm, "scan"], 
	y = rep(max(traces$eic[, "int"]), 2), 
	fill = "tozeroy")

# then reduce it
lm <- narrow_rt_boundaries_reduce(lm, center, 
	traces$eic[, "int"] - baseline, missing_scans)
p1e <- plotly::add_trace(p1b, 
	x = traces$eic[lm, "scan"], 
	y = rep(max(traces$eic[, "int"]), 2), 
	fill = "tozeroy")
plotly::subplot(p1c, p1d, p1e, nrows = 3, shareX = TRUE)

#################################################################
# compute them all descriptors for the peak
mz_vals <- traces$mzmat[which(
	traces$mzmat[, "scan"] >= traces$eic[lm[1], "scan"] & 
	traces$mzmat[, "scan"] <= traces$eic[lm[2], "scan"] & 
	traces$mzmat[, "int"] > 0), , drop = FALSE]
mz_range <- range(mz_vals[, "mz"])
mz <- do.call(xcms:::mzCenter.wMean, list(
	mz = mz_vals[, "mz"], 
	intensity = mz_vals[, "int"]))
intb <- pracma::trapz(traces$eic[lm[1]:lm[2], 'int'] - baseline[lm[1]:lm[2]])
data.frame(
	mz = mz, 
	mzmin = mz_range[1], 
	mzmax = mz_range[2], 
	rt = traces$eic[center, 'rt'] / 60, 
	rtmin = traces$eic[lm[1], 'rt'] / 60, 
	rtmax = traces$eic[lm[2], 'rt'] / 60, 
	into = pracma::trapz(traces$eic[lm[1]:lm[2], 'int']),
	intb = intb, 
	maxo = max(mz_vals[, "int"]), 
	sn = intb / pracma::trapz(rep(noise, diff(lm) + 1)),
	scale = best.scale, 
	scpos = traces$eic[center, "scan"], 
	scmin = traces$eic[lm[1], "scan"], 
	scmax = traces$eic[lm[2], "scan"], 
	lmin = lm[1], 
	lmax = lm[2]
)