#' @title Convert ppm to Da
#'
#' @description
#' convert ppm to Da
#'
#' @param mass float mass or m/z
#' @param ppm float ppm tolerance
#'
#' @return float, tolerance in Da
ppm_to_da <- function(mass, ppm) mass * ppm * 10**-6

#' @title Convert Da to ppm
#'
#' @description
#' convert Da to ppm
#'
#' @param mass float mass or m/z
#' @param da float tolerance in da
#'
#' @return float, tolerance in ppm
da_to_ppm <- function(mass, da) da * 10**6 / mass

#' @title Get mass range
#'
#' @description
#' Get mass range with a tolerance in ppm &/or in mDa
#'
#' @param mass vector(float) mass
#' @param ppm float ppm tolerance
#' @param mda float mDa tolerance
#'
#' @return matrix(numeric) with two columns, first is the range min, second is the max
#'
#' @example
#' \dontrun{get_mass_range(c(641.64474, 605.668062258), ppm = 5, mda = 0)}
get_mass_range <- function(mass, ppm = 0, mda = 0) {
	tol_da <- ppm_to_da(mass, ppm) + mda * 10**-3
	matrix(c(mass - tol_da, mass + tol_da), ncol = 2, dimnames = list(
		c(), c("mzmin", "mzmax")))
}

#' @title Interpolation of MS measurement resolution
#' 
#' @description
#' Given a set of MS measurement resolutions (R) as a function of measurement mass (m/z), \link[enviPat]{getR} interpolates R for any given molecular mass(es) using \code{\link[stats]{smooth.spline}}.
#' 
#' @param masses vector(float) masses
#' @param resmass dataframe with columns:
#' \itemize{
#' 		\item resolution float resolution
#' 		\item mass float mass
#' }
#' @param nknots integer number of knots to use for the smoothing spline. Default = 6. See also \code{\link[stats]{smooth.spline}}
#' @param spar float smoothing parameter, (0,1]. See also \code{\link[stats]{smooth.spline}}
#' 
#' @return vector(floats) vector containing resolutions for each masses in parameters
#'
#' @example
#' \dontrun{getR(c(641.64474, 605.668062258), resmass = resolution_list[[14]])}
getR <- function(masses, resmass, nknots = 13, spar = 0.1) {
    if (length(resmass[, 1]) < 10) stop("stop: not enough data points in resmass\n")
    if (any(masses < min(resmass[, 1])) || any(masses > max(resmass[, 1]))) stop(
		"stop: some mean_mass out of range of resmass\n")
    if (nknots < 3 || nknots > length(resmass[, 1])) stop("stop: invalid nknots\n")
    if (spar <= 0 || spar >= 1) stop("stop: invalid spar; spar=(0,1]")
    options(digits = 10)
    model <- smooth.spline(resmass[, 1], resmass[, 2], cv = TRUE, 
        all.knots = FALSE, nknots = nknots, spar = spar)
    sapply(masses, function(mass) 
		predict(model, mass)$y)
}

#' @title Simulate theoretic pattern
#'
#' @description
#' Simulate theoretic pattern. Use enviPat functions + custom one to add isotope annotation
#'
#' @param formulas vector(string) formulas
#' @param charge integer charge
#' @param resolution list with items:
#' \itemize{
#' 		\item resolution float, resolution of instrument if Orbitrap
#' 		\item mz float, resolution@mz if Orbitrap
#'		\item index integer, index of the instrument in the enviPat resolution_list
#' }
#'
#' @return dataframe with columns:
#' \itemize{
#'      \item mz float m/z
#'      \item abundance float relative abundance (in %)
#'		\item weight float weight of the peak used in scoring algorithm for later
#' 		\item iso string isotope annotation in the form "A+/-"
#' }
#'
#' @example
#' \dontrun{get_theoric("C12H17Br6", -1, 78223.47)}
get_theoric <- function(formulas, charge, resolution) {
	pattern <- enviPat::isopattern(isotopes, formulas, threshold = 1, 
		charge = charge, emass = .00054858, plotit = FALSE, algo = 2, verbose = FALSE)
	masses <- sapply(pattern, function(x) x[1, 1])
	resolution_masses <- if (!is.na(resolution$index)) getR(masses, 
			resmass = resolution_list[[resolution$index]], 
			nknots = 6, spar = .2)
		else resolution$resolution * sqrt(1 / masses) / sqrt(1 / resolution$mz)
	profiles <- enviPat::envelope(pattern, ppm = FALSE, dmz = "get", frac = 1/4, 
		env = "Gaussian", resolution = resolution_masses, plotit = FALSE, verbose = FALSE)
	centroid <- enviPat::vdetect(profiles, detect = "centroid", plotit = FALSE, verbose = FALSE)
	lapply(centroid, get_isotope_annot)
}

#' @title Give isotope annotation
#'
#' @description
#' Give isotope annotation in the form "A+/-". A representing basepeak & not monoisotopeak!
#'
#' @param pattern matrix or dataframe with columns:
#' \itemize{
#' 		\item mz float m/z
#' 		\item abundance float abundance (in %)
#'		\item weight float weight of the peak used in scoring algorithm for later
#' 		\item iso string isotope annotation in the form "A+/-"
#'}
get_isotope_annot <- function(isotopic_pattern) {
	if (class(isotopic_pattern) == "character") return(data.frame(matrix(, 
		nrow = 0, ncol = 3, dimnames = list(c(), 
			c("mz", "abundance", "iso")))))
	mz_A <- isotopic_pattern[which.max(isotopic_pattern[, 2]), 1]
	weights <- isotopic_pattern[, 2] / sum(isotopic_pattern[, 2])
	isos <- round(isotopic_pattern[, 1] - mz_A)
	isos[which(isos == 0)] <- ""
	isos[which(isos > 0)] <- paste0("+", isos[which(isos > 0)])
	isos <- paste0("A", isos)
	isotopic_pattern <- data.frame(mz = isotopic_pattern[, 1], 
		abundance = isotopic_pattern[, 2], weight = weights, iso = isos, 
		stringsAsFactors = FALSE)
	isotopic_pattern[order(isotopic_pattern$abundance, decreasing = TRUE), ]
}

#' @title Get TIC
#'
#' @description
#' Get TIC data for all files in project
#' If no project or no files available return an empty dataframe
#'
#' @param db sqlite connection
#' @param project integer, project ID
#' @param project_samples vector of integers, project_sample IDs
#'
#' @return dataframe with columns: 
#' \itemize{
#'      \item rt float, retention time in min
#'      \item int float, intensity
#'      \item sample_name string, sample ID
#'}
get_tics <- function(db, project = NULL, project_samples = NULL) {
	samples <- get_samples(db, project, project_samples)
	if (nrow(samples) == 0) custom_stop("invalid", "no files in project")
	do.call(rbind, lapply(1:nrow(samples), function(i) 
		cbind(
			get_tic(db, samples[i, "sample"]), 
			sample_name = samples[i, "sample_id"]
		)
	))
}

#' @title Get TIC
#'
#' @description
#' Get TIC data for one file. If TIC data is not in metadata of file recompute it
#' Aggregate data to have 2 pts per sec -> 120 pts per min
#'
#' @param db sqlite connection
#' @param sample string, sample ID
#'
#' @return dataframe with columns: 
#' \itemize{
#'      \item rt float, retention time in min
#'      \item int float, intensity
#'}
get_tic <- function(db, sample) {
	ms_file <- load_ms_file(db, sample = sample)
	data <- if (all(ms_file@tic == 0)) data.frame(
			rt = ms_file@scantime, 
			int = xcms::rawEIC(ms_file, mzrange = range(ms_file@env$mz))
		) else data.frame(rt = ms_file@scantime, int = ms_file@tic)
	rm(ms_file)
	gc()
	# concatenate values (too many in GC-MS) (2 pts per sec)
	rt <- aggregate(data["rt"], 
		by = list(rounded = round(data$rt  * 2)), FUN = min)$rt
	int <- aggregate(data["int"], 
		by = list(rounded = round(data$rt  * 2)), FUN = max)$int
	data.frame(
		rt = rt / 60, 
		int = int
	)
}

#' @title Get EIC
#'
#' @description
#' Get EIC data for all files in project for one or multiple m/z
#' If no project or no files available return an empty dataframe
#'
#' @param db sqlite connection
#' @param project integer, project ID
#' @param project_samples vector of integers, project_sample IDs
#' @param mzs vector(float) m/z
#' @param ppm float m/z tolerance in ppm
#' @param mda float m/z tolerance in mDa
#'
#' @return dataframe with columns: 
#' \itemize{
#'      \item rt float, retention time in min
#'      \item int float, intensity
#'      \item mz float m/z asked for EIC
#' 		\item sample_name string, sample name
#'}
get_eics <- function(db, project = NULL, project_samples = NULL, 
		mzs = c(), ppm = 0, mda = 0) {
	samples <- get_samples(db, project, project_samples)
	if (nrow(samples) == 0) custom_stop("invalid", "no files in project")
	mz_ranges <- cbind(mzs, get_mass_range(mzs, ppm, mda))
	do.call(rbind, lapply(1:nrow(samples), function(i) { 
		ms_file <- load_ms_file(db, sample = samples[i, "sample"])
		data <- do.call(rbind, lapply(seq(nrow(mz_ranges)), function(j) 
			cbind(
				get_eic(db, ms_file, mz_ranges[j, c("mzmin", "mzmax")]), 
				mz = mzs[j], 
				sample_name = samples[i, "sample_id"]
			)
		))
		rm(ms_file)
		gc()
		data
	}))
}

#' @title Get EIC
#'
#' @description
#' Get EIC data for one file. 
#' Aggregate data to have 2 pts per sec -> 120 pts per min
#'
#' @param db sqlite connection
#' @param sample string, sample ID
#' @param mzrange vector of 2 floats, m/z borns
#'
#' @return dataframe with columns: 
#' \itemize{
#'      \item rt float, retention time in min
#'      \item int float, intensity
#'}
get_eic <- function(db, ms_file, mzrange) {
	ids <- which(ms_file@env$mz >= mzrange[1] & 
		ms_file@env$mz <= mzrange[2])
	scans <- sapply(ids, function(id) which.min(abs(ms_file@scanindex - id)))
	data <- data.frame(
		rt = ms_file@scantime, 
		int = 0
	)
	data[scans, "int"] <- ms_file@env$intensity[ids]
	rm(ms_file)
	gc()
	# concatenate values (too many in GC-MS) (2 pts per sec)
	rt <- aggregate(data["rt"], 
		by = list(rounded = round(data$rt  * 2)), FUN = min)$rt
	int <- aggregate(data["int"], 
		by = list(rounded = round(data$rt  * 2)), FUN = max)$int
	data.frame(
		rt = rt / 60, 
		int = int
	)
}

#' @title Get MS
#'
#' @description
#' Get MS data for all files in project at a specific time retention
#' If no project or no files available return an empty dataframe
#'
#' @param db sqlite connection
#' @param project integer, project ID
#' @param project_samples vector of integers, project_sample IDs
#' @param rt float, time retention in min
#'
#' @return dataframe with columns: 
#' \itemize{
#'      \item mz float, m/z
#'      \item int float, intensity
#'      \item sample_name string, sample ID
#'}
get_mss <- function(db, project = NULL, project_samples = NULL, rt) {
	samples <- get_samples(db, project, project_samples)
	if (nrow(samples) == 0) custom_stop("invalid", "no files in project")
	rt <- rt * 60 # convert sec to min
	do.call(rbind, lapply(1:nrow(samples), function(i) 
		cbind(
			get_ms(db, samples[i, "sample"], rt), 
			sample_name = samples[i, "sample_id"]
		)
	))
}

#' @title Get MS
#'
#' @description
#' Get MS data for one file with a time retention range
#' concatenate m/z values to 5th digit
#'
#' @param db sqlite connection
#' @param sample string, sample ID
#' @param rt float, time retention in sec
#'
#' @return dataframe with columns: 
#' \itemize{
#'      \item mz float, m/z
#'      \item int float, intensity
#'}
get_ms <- function(db, sample, rt) {
	ms_file <- load_ms_file(db, sample = sample)
	if (rt > max(ms_file@scantime) | 
		rt < min(ms_file@scantime)) return(data.frame(
			mz = NA, int = NA))
	scan <- which.min(abs(ms_file@scantime - rt))
	indexes <- c(ms_file@scanindex + 1, length(ms_file@env$mz))[scan:(scan+1)] # xcms begins always at 0, not 1
	data <- data.frame(
		mz = ms_file@env$mz[indexes[1]:indexes[2]],
		int = ms_file@env$intensity[indexes[1]:indexes[2]]
	)
	rm(ms_file)
	gc()
	# concatenate m/z values to 5th digit
	aggregate(data["int"], by = list(
		mz = round(data$mz, 5)), FUN = max)
}

#' @title Simulate all possible chloroparaffins formulas
#'
#' @description
#' Simulate all possible chloroparaffins formulas with an adduct
#' Formula is C(x)Cl(y)H(2x+2-y)
#'
#' @param adduct_names vector(string) adduct names, must be present in the adduct list of enviPat
#' @param min_C int minimum of C
#' @param max_C int maximum of C
#' @param min_Cl int minimum of Cl
#' @parama max_Cl int maximum of Cl
#'
#' @return dataframe with columns:
#' \itemize{
#' 		\item C integer number of C for the formula
#' 		\item Cl integer number of Cl for the formula
#' 		\item H integer number of H for the formula
#' 		\item formula string chemical formula of chloroparaffin
#' 		\item adduct string adduct name
#' 		\item charge integer ion charge of chloroparaffin
#' 		\item ion_formula string ion chemical formula of chloroparaffin with adduct
#' }
#' 
#' @examples
#' \dontrun{get_chloropara_form("M+Cl")}
get_chloropara_form <- function(adduct_names, min_C = 7, max_C = 36, 
		min_Cl = 3, max_Cl = 30) {
	forms <- expand.grid(min_C:max_C, min_Cl:max_Cl)
	forms <- cbind(forms, Var3 = 2 * forms[, 1] + 2 - forms[, 2])
	forms <- forms[which(forms[, 3] > 0), ]
	forms <- cbind(forms, Var4 = paste("C", forms[, 1], "Cl", forms[, 2], 
		"H", forms[, 3], sep = ""))
	forms[, "Var4"] <- enviPat::check_chemform(isotopes, forms[, "Var4"])$new_formula
	forms <- do.call(rbind, lapply(adduct_names, function(adduct_name) {
		adduct <- adducts[which(adducts$Name == adduct_name), ]
		cbind(forms, Var5 = adduct_name, Var6 = adduct$Charge, Var7 = subform(
				mergeform(
					enviPat::multiform(forms[, "Var4"], adduct$Multi), 
					adduct$Formula_add
				), adduct$Formula_ded
			)
		)
	}))
	colnames(forms) <- c("C", "Cl", "H", "formula", "adduct", "charge", "ion_formula")
	forms
}

#' @title Merge two formulas
#'
#' @description
#' Merge two formulas. Adapt from enviPat::mergeform to manage "FALSE" values
#'
#' @param forms vector(string) chemical formulas
#' @param form string chemical formula to add
#'
#' @return vector(string) chemical formulas merged
#' 
#' @examples
#' \dontrun{mergeform("C12H18Br6", "H1")}
mergeform <- function(forms, form) if (form == "FALSE") forms else enviPat::mergeform(forms, form)

#' @title Deduct a formula to others
#'
#' @description
#' Deduct a formula to others. Adapt from enviPat::mergeform to manage "FALSE" values
#'
#' @param forms vector(string) chemical formulas
#' @param form string chemical formula to deduct
#'
#' @return vector(string) chemical formulas deducted
#' 
#' @examples
#' \dontrun{subform("C12H19Br6", "H1")}
subform <- function(forms, form) {
    if (form == "FALSE") forms
    else {
        test <- enviPat::check_ded(forms, form)
        if (any(!test)) enviPat::subform(forms[which(!test)], form)
        else NULL
    }
}