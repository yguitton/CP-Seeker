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

#' @title Display TICs
#'
#' @description
#' Display TIC of all files in project
#' add two JS events:
#' \itemize{
#'      \item click: send the x coordinate at click to input$process_TIC_rt
#'      \item restyle: hide or show traces on ouptut$process_MS according 
#'                     which traces are hidden or shown on output$process_TIC
#'}
#'
#' @param db sqlite connection
#' @param input$project integer, project ID
output$process_TIC <- plotly::renderPlotly({
	params <- list(
		project = input$project
	)
	tryCatch({
		if (is.null(params$project)) custom_stop("invalid", "no project")
		else if (params$project == "") custom_stop("invalid", "no project")
		htmlwidgets::onRender(plot_TIC(db, project = params$project), 
			"function(el, x){
				el.on('plotly_click', function(eventData){
					Shiny.onInputChange('process_TIC_rt', eventData.points[0].x);
				});
				el.on('plotly_restyle', function(data){
					var gd = $('#process_MS').get(0),
						traces_hidden = el._fullData
							.filter(x => x.visible == 'legendonly')
							.map(x => x.name),
						to_hide = [],
						to_show = [],
						traces_ms = gd._fullData;
						
					if (traces_ms.length <= 1) return 0;
					for (var i = 1; i < traces_ms.length; i++) {
						if (traces_hidden.includes(traces_ms[i].name)) {
							to_hide.push(i);
						} else {
							to_show.push(i);
						}
					}
					if(to_hide.length > 0){
						Plotly.restyle(gd, 
							{visible: 'legendonly'}, to_hide);
					}
					if(to_show.length > 0){
						Plotly.restyle(gd, 
							{visible: true}, to_show);
					}
				});
			}"
		)
	}, invalid = function(i) {
		print("ERR process_TIC")
		print(i)
		plot_empty_chromato()
	}, error = function(e) {
		print("ERR process_TIC")
		print(e)
		sweet_alert_error(e$message)
		plot_empty_chromato()
	})
})

#' @title Display mass sprectras at rt
#'
#' @description
#' Display mass sprectras of all files in project  at a specific rt
#'      according the time retention selected on output$process_TIC
#'
#' @param db sqlite connection
#' @param input$project integer, project ID
#' @param input$process_TIC_rt float, time retention
output$process_MS <- plotly::renderPlotly({
	params <- list(
		project = input$project, 
		rt = input$process_TIC_rt
	)
	tryCatch({
		if (is.null(params$project)) custom_stop("invalid", "no project")
		if (is.null(params$rt)) custom_stop("invalid", "no rt selected")
		else if (params$project == "") custom_stop("invalid", "no project")
		else if (params$rt == 0) custom_stop("invalid", "no rt selected")
		
		plot_MS(db, project = params$project, rt = params$rt)
	}, invalid = function(i) {
		print("ERR process_MS")
		print(i)
		plot_empty_MS()
	}, error = function(e) {
		print("ERR process_MS")
		print(e)
		sweet_alert_error(e$message)
		plot_empty_MS()
	})
})

#' @title Launch process event
#'
#' @description
#' Launch peak picking & CAMERA on each file, then align them with XCMS
#' use blob files stored in the database
#' give a supergroup which assemble features sharing same pcgroup or aligngroup
#' 
#' @param db sqlite connection
#' @param project_samples reactive value, project_samples table
#' @param input$project integer, project ID
#' @param input$process_ppm float, Maximal tolerated m/z deviation in consecutive scans in parts per million (ppm)
#' @param input$process_snthresh float, Signal to noise ratio cutoff
#' @param input$process_peakwidth_min float, Expected approximate peak width in chromatographic space
#' @param input$process_peakwidth_max float, Expected approximate peak width in chromatographic space
#' @param input$process_prefilter_step float Mass traces are only retained if they contain at least k peaks with intensity >= I
#' @param input$process_prefilter_level float Mass traces are only retained if they contain at least k peaks with intensity >= I
#' @param input$process_mz_center_fun  string, Name of the function to calculate the m/z center of the chromatographic peak
#' @param input$process_first_baseline_check  boolean, Continuous data within regions of interest is checked to be above the first baseline
#' @param input$process_integrate boolean, Integration method. If checked the descent is done on the real data, if not peak limits are found through descent on the mexican hat filtered data.  Method 1 is very accurate but prone to noise,  while method 2 is more robust to noise but less exact
#' @param input$process_noise float, Optional argument which is useful for data that was centroided without any intensity threshold, centroids with intensity < noise are omitted from ROI detection
#' @param input$process_mzdiff float, Minimum difference in m/z for peaks with overlapping retention times, can be negative to allow overlap
#' @param input$process_rt_tol float, rT tolerance in sec
#' @param input$process_mz_tol float, m/z tolerance in mDa
#' @param input$process_center_sample integer, Sample used as sample of reference to apply the retention time correction
#' @param input$process_bw float, retention time window tolerance (s)
#' @param input$process_mzwid float, m/z window tolerance (mDa)
#' @param input$process_dist_fun string, Distance function to be used.  Allowed values are "cor"(Pearson\'s correlation), "cor_opt"(calculate only 10% diagonal band of distance matrix; better runtime), "cov"(covariance), "prd"(product) and "euc"(Euclidian distance)
#' @param input$process_response float, Defining the responsiveness of warping with response = 0 giving linear warping on start and end points and response = 100 warping using all bijective anchors.
#' @param input$process_gap_init float, Defining the penalty for gap opening
#' @param input$process_gap_extend float, Defining the penalty for gap enlargement
#' @param input$process_factor_diag float, Defining the local weight applied to diagonal moves in the alignment
#' @param input$process_factor_gap float, Defining the local weight for gap moves in the alignment
#' @param input$process_init_penalty float, Defining the penalty for initiating an alignment (for local alignmentonly)
#' @param input$process_local_alignment boolean, Whether a local alignment should be performed instead of the default global alignment
#' @param input$process_perfwhm float, Full Width at Half Maximum for grouping integrated ions
#' @param input$process_cor_eic_th float, Threshold for Extracted Ion Chromatogram correlation
#' @param input$process_graphMethod string, Method used for grouping peaks. HCS means for Highly Connected Substract and LCS for Label Propagation Community. See package CAMERA for more informations.
#' @param input$process_sigma float, Multiplier of the standard deviation
#' @param input$process_pval float, Threshold for p-value
shiny::observeEvent(input$process_launch, {
	print('############################################################')
	print('######################### PROCESS ##########################')
	print('############################################################')
	
	params <- list(
		xcms_lock = input$process_xcms_lock,
		pairing_lock = input$process_pairing_lock, 
		alignment_lock = input$process_alignment_lock,
		camera_lock = input$process_camera_lock,
		project = input$project, 
		ppm = input$process_ppm, 
		snthresh = input$process_snthresh,
		peakwidth = c(input$process_peakwidth_min, input$process_peakwidth_max), 
		prefilter = c(input$process_prefilter_step, input$process_prefilter_level), 
		mz_center_fun = input$process_mz_center_fun, 
		first_baseline_check = input$process_first_baseline_check, 
		integrate = input$process_integrate, 
		noise = input$process_noise, 
		mzdiff = input$process_mzdiff, 
		fitgauss = FALSE, # not work if at true
		verboseColumns = TRUE, 
		mz_tol = input$process_mz_tol,
		rt_tol = input$process_rt_tol, 
		center_sample = input$process_center_sample, 
		bw = input$process_bw, 
		mzwid = input$process_mzwid, 
		dist_fun = input$process_dist_fun, 
		response = input$process_response, 
		gap_init = input$process_gap_init, 
		gap_extend = input$process_gap_extend, 
		factor_diag = input$process_factor_diag, 
		factor_gap = input$process_factor_gap, 
		init_penalty = input$process_init_penalty, 
		local_alignment = input$process_local_alignment, 
		binSize = 1, # sufficient
		minFraction = 1 * 10**-9, # no annotation on files (0 doesn't work)
		minSamples = 1, # no annotation on files, 
		maxFeatures = 500, # sufficient number 
		perfwhm = input$process_perfwhm, 
		cor_eic_th = input$process_cor_eic_th, 
		cor_exp_th = 0, # we don't use other samples with CAMERA 
		graphMethod = input$process_graphMethod, 
		sigma = input$process_sigma, 
		pval = input$process_pval, 
		intval = "into", # better value than maxo and intb
		calcIso = FALSE, # i think that's better to not reject cause not find 13C annotation
		calcCiS = TRUE, # if CAMERA compute correlations inside samples, of course it is at TRUE
		calcCaS = FALSE # if CAMERA compute correlations between samples, no
	)
	print(params)
	
	tryCatch({
		if (is.null(params$project)) custom_stop("minor_error", 
			"A project with files is needed for processing")
		else if (params$project == "") custom_stop("minor_error", 
			"A project with files is needed for processing")
		params$samples <- project_samples()[which(
			project_samples()$project == params$project), "sample"]
		if (length(params$samples) == 0) custom_stop("minor_error", 
			"you need to import files in project to process them")
		params$project_samples <- project_samples()[which(
			project_samples()$project == params$project), "project_sample"]
		params$sample_ids <- project_samples()[which(
			project_samples()$project == params$project), "sample_id"]
		params$polarity <- get_project_polarity(db, params$project)
		print(params[c("samples", "project_samples", "sample_ids", "polarity")])
	
		if ((params$alignment_lock | params$camera_lock) & 
				!params$pairing_lock) {
			# check if a previous pairing was done
			test <- which(project_samples()$project == input$project & 
				is.na(project_samples()$param_pairing))
			if (length(test)) custom_stop("minor_error", 
				"No pairing data for file(s): %s. Please check the pairing step & 
					relaunch process", project_samples()[test, "sample_id"])
		} else if (params$pairing_lock & !params$xcms_lock) {
			# check if a previous peakpicking was done
			test <- which(project_samples()$project == input$project & 
				is.na(project_samples()$param_xcms))
			if (length(test)) custom_stop("minor_error", 
				"No peakpicking data for file(s): %s. Please check the peakpicking 
					step & relaunch process", project_samples()[test, "sample_id"])
		}
	
		inputs <- c()
		conditions <- c()
		msgs <- c()
		if (params$xcms_lock) {
			inputs <- c("process_ppm", "process_snthresh", 
				"process_peakwidth_min", "process_peakwidth_max", 
				"process_prefilter_step", "process_prefilter_level", 
				"process_noise", "process_mzdiff")
			conditions <- c(!is.na(params$ppm), !is.na(params$snthresh), 
				!is.na(params$peakwidth[1]), !is.na(params$peakwidth[2]), 
				!is.na(params$prefilter[1]), !is.na(params$prefilter[2]), 
				!is.na(params$noise), !is.na(params$mzdiff))
			msgs <- c("m/z tolerance (ppm) is required", "s/n is required", 
				"Peakwidth min (s) is required", "Peakwidth max (s) is required", 
				"Prefilter step is required", "Prefilter level is required", 
				"Noise is required", "m/z difference is required")
		}
		if (params$pairing_lock) {
			inputs <- c(inputs, "process_mz_tol", "process_rt_tol")
			conditions <- c(conditions, !is.na(params$mz_tol), 
				!is.na(params$rt_tol))
			msgs <- c(msgs, "m/z tolerance is required", 
				"Retention time tolerance is required")
		}
		if (params$alignment_lock) {
			inputs <- c(inputs, "process_bw", "process_mzwid", "process_gap_init", 
				"process_gap_extend", "process_factor_diag", "process_factor_gap", 
				"process_init_penalty")
			conditions <- c(conditions, !is.na(params$bw), !is.na(params$mzwid), 
				!is.na(params$gap_init), !is.na(params$gap_extend), 
				!is.na(params$factor_diag), !is.na(params$factor_gap), 
				!is.na(params$init_penalty))
			msgs <- c(msgs, "tR tolerance is required", 
				"m/z tolerance (mDa) between samples is required", 
				"Gap init is required", "Gap extend is required", 
				"Factor diagonal is required", "Factor gap is required", 
				"Initiating penalty is required")
		}
		if (params$camera_lock) {
			inputs <- c(inputs, "process_sigma", "process_pval")
			conditions <- c(conditions, !is.na(params$sigma), !is.na(params$pval))
			msgs <- c(msgs, "Mutliplier of the standard deviation is required", 
				"P-value threshold is required")
		}
		check_inputs(inputs, conditions, msgs)
		
		inputs <- c()
		conditions <- c()
		msgs <- c()
		if (params$xcms_lock) {
			inputs <- c("process_ppm", "process_snthresh", "process_peakwidth_min", "process_peakwidth_max", 
				"process_prefilter_step", "process_prefilter_level", 
				"process_noise")
			conditions <- c(params$ppm > 0, params$snthresh >= 0, 
				params$peakwidth[1] > 0, params$peakwidth[2] > 0, 
				params$prefilter[1] >= 0, params$prefilter[2] >= 0, 
				params$noise >= 0)
			msgs <- c("m/z tolerance (ppm) must be a positive number", 
				"s/n must be a positive number or 0", 
				"Peakwidth min (s) must be a positive number", 
				"Peakwidth max (s) must be a positive number", 
				"Prefilter step must be a positive number or 0", 
				"Prefilter level must be a positive number or 0", 
				"Noise must be a positive number or 0")
		}
		if (params$pairing_lock) {
			inputs <- c(inputs, "process_mz_tol", "process_rt_tol")
			conditions <- c(conditions, params$mz_tol > 0, params$rt_tol > 0)
			msgs <- c(msgs, "m/z tolerance must be a positive number", 
				"Retention time tolerance must be a positive number")
		}
		if (params$alignment_lock) {
			inputs <- c(inputs, "process_bw", "process_mzwid", "process_gap_init", 
				"process_gap_extend", "process_factor_diag", "process_factor_gap", 
				"process_init_penalty")
			conditions <- c(conditions, params$bw > 0, params$mzwid > 0, 
				params$gap_init > 0, params$gap_extend > 0, params$factor_diag > 0, 
				params$factor_gap > 0, params$init_penalty >= 0)
			msgs <- c(msgs, "tR tolerance must be a positive number", 
				"m/z tolerance (mDa) between samples must be a positive number", 
				"Gap init must be a positive number", 
				"Gap extend must be a positive number", 
				"Factor diagonal must be a positive number", 
				"Factor gap must be a positive number", 
				"Initiating penalty must be a positive number or 0")
		}
		if (params$camera_lock) {
			inputs <- c(inputs, "process_sigma", "process_pval")
			conditions <- c(conditions, params$sigma > 0, params$pval > 0)
			msgs <- c(msgs, 
				"Mutliplier of the standard deviation must be a positive number", 
				"P-value threshold must be a positive number")
		}
		check_inputs(inputs, conditions, msgs)
		
		if (params$xcms_lock) {
			inputs <- c("process_peakwidth_min", "process_peakwidth_max")
			conditions <- rep(params$peakwidth[1] < params$peakwidth[2], 2)
			msgs <- rep("peakwidth min must be lower than peakwidth max", 2)
			check_inputs(inputs, conditions, msgs)
		}
		
		peaktables <- NULL
		maxPb <- length(params$samples) * 4 + 2
		pbVal <- 0
		shinyWidgets::progressSweetAlert(session, 'pb', title = 'Initialisation',
			value = pbVal * 100 / maxPb, display_pct=TRUE)
		for (i in 1:length(params$samples)) {
			msg <- sprintf("load data of %s", params$sample_ids[i])
			print(msg)
			shinyWidgets::updateProgressBar(session, id = 'pb', 
				title = msg, value = pbVal * 100 / maxPb)	
			ms_file <- load_ms_file(db, sampleID = params$samples[i])
			pbVal <- pbVal + 1
			
			peaktable <- if (params$xcms_lock) {
				msg <- sprintf("peakpicking of %s", params$sample_ids[i])
				print(msg)
				shinyWidgets::updateProgressBar(session, id = 'pb', 
					title = msg, value = pbVal * 100 / maxPb)
				peaktable <- peakpicking(ms_file, params)
				peaktable$project_sample <- params$project_samples[i]
				peaktable$project <- params$project
				pbVal <- pbVal + 1
				peaktable
			} else { 
				peaktable <- get_peaktable(db, project_sample = params$project_samples[i])[, c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax", "iso", "abundance", "annotation", "coeff", "r_squared", "cluster", "pcgroup", "aligngroup", "project_sample", "project")]
				peaktable <- peaktable[order(peaktable$into, decreasing = TRUE), ]
				peaktable <- peaktable[which(!duplicated(peaktable[, c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax")])), ]
				peaktable[, c("rt", "rtmin", "rtmax")] <- lapply(peaktable[, c("rt", "rtmin", "rtmax")], function(x) x * 60)
			}
			
			if (params$pairing_lock) {
				msg <- sprintf("pairing of %s", params$sample_ids[i])
				print(msg)
				shinyWidgets::updateProgressBar(session, id = 'pb', 
					title = msg, value = pbVal * 100 / maxPb)
				peaktable <- peaktable[, c("mz", "mzmin", "mzmax", "rt", "rtmin", 
					"rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", 
					"h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", 
					"lmax", "project_sample", "project")]
				peaktable <- suppressWarnings(pairing(peaktable, params$mz_tol * 10**-3, 
					params$rt_tol))
				pbVal <- pbVal + 1
			}
			
			if (params$camera_lock) {
				peaktable <- peaktable[, c("mz", "mzmin", "mzmax", "rt", "rtmin", 
					"rtmax", "into", "intb", "maxo", "sn", "egauss", "mu", "sigma", 
					"h", "f", "dppm", "scale", "scpos", "scmin", "scmax", "lmin", 
					"lmax", "iso", "abundance", "annotation", "coeff", 
					"r_squared", "cluster", "project_sample", "project")]
				msg <- sprintf("adduct/fragmentation annotation of %s", params$sample_ids[i])
				print(msg)
				shinyWidgets::updateProgressBar(session, id = 'pb', 
					title = msg, value = pbVal * 100 / maxPb)
				peaktable <- camera(peaktable, ms_file, params)
				pbVal <- pbVal + 1
			} else peaktable$pcgroup <- peaktable$cluster
			
			peaktables <- if (is.null(peaktables)) peaktable
				else rbind(peaktables, peaktable)
			rm(ms_file)
			gc()
		}
		
		peaktables$pcgroup <- as.integer(as.factor(paste(
			peaktables$pcgroup, peaktables$project_sample))) # cause each pcgroup is only unique in a project_sample
		peaktables$cluster <- as.integer(as.factor(paste(
			peaktables$cluster, peaktables$project_sample))) # cause each cluster is only unique in a project_sample
		
		if (params$alignment_lock) {	
			peaktables <- peaktables[, c(
				"mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into", "intb", 
				"maxo", "sn", "egauss", "mu", "sigma", "h", "f", "dppm", "scale", 
				"scpos", "scmin", "scmax", "lmin", "lmax", "iso", "abundance", 
				"annotation", "coeff", "r_squared", "cluster", "pcgroup", 
				"project_sample", "project")]
			if (params$center_sample == "most_informative") {
				nb_peaks_per_sample <- table(peaktables$project_sample)
				params$center_sample <- names(nb_peaks_per_sample)[which.max(nb_peaks_per_sample)]
			}
			msg <- "alignment"
			print(msg)
			shinyWidgets::updateProgressBar(session, id = 'pb', 
				title = msg, value = pbVal * 100 / maxPb)
			peaktables <- alignment(peaktables, params)
			pbVal <- pbVal + 1
		} else peaktables$aligngroup <- peaktables$cluster
		
		msg <- "Create supergroups"
		print(msg)
		shinyWidgets::updateProgressBar(session, id = 'pb', 
			title = msg, value = pbVal * 100 / maxPb)
		peaktables <- cbind(peaktables, 
			supergroup = .Call("clusterize", 
				peaktables$aligngroup, peaktables$pcgroup))
		pbVal <- pbVal + 1
		gc()
		
		msg <- "record datas"
		print(msg)
		shinyWidgets::updateProgressBar(session, id = 'pb', 
			title = msg, value = pbVal * 100 / maxPb)
		delete_features(db, params$project_samples)
		record_params(db, params)
		record_peaktable(db, peaktables)
		pbVal <- pbVal + 1
				
		print('done')
		shinyWidgets::closeSweetAlert(session)
	}, invalid = function(i) NULL
	, minor_error = function(e) {
		print(e)
		toastr_error(e$message)
	}, error = function(e) {
		print(e)
		shinyWidgets::closeSweetAlert(session)
		sweet_alert_error(e$message)
	})
	
	print('############################################################')
	print('######################### END PROCESS ######################')
	print('############################################################')
})

#' @title Peakpicking
#'
#' @description
#' Peakicking process with xcms
#' remove redundant peaks (same m/z rounded to 5 digits & rT (in min) rounded to 2 digits)
#'
#' @param ms_file xcmsRaw object
#' @param params list with items:
#' \itemize{
#'      \item ppm float, Maximal tolerated m/z deviation in consecutive scans in parts per million (ppm)
#'      \item snthresh float, Signal to noise ratio cutoff
#'      \item peakwidth vector of 2 floats, Expected approximate peak width in chromatographic space
#'      \item prefilter vector of 1 integer & 1 float, Mass traces are only retained if they contain at least k peaks with intensity >= I
#'      \item mz_center_fun string, Name of the function to calculate the m/z center of the chromatographic peak
#'      \item first_baseline_check boolean, Continuous data within regions of interest is checked to be above the first baseline
#'      \item integrate boolean, Integration method. If checked the descent is done on the real data, if not peak limits are found through descent on the mexican hat filtered data.  Method 1 is very accurate but prone to noise,  while method 2 is more robust to noise but less exact
#'      \item noise float, Optional argument which is useful for data that was centroided without any intensity threshold, centroids with intensity < noise are omitted from ROI detection
#'      \item mzdiff float, Minimum difference in m/z for peaks with overlapping retention times, can be negative to allow overlap
#'      \item fitgauss boolean, fit a gaussian on the peak
#'      \item verboseColumns boolean, add some descriptors
#' }
#'
#' @return dataframe with columns:
#' \itemize{
#'      \item mz float, m/z
#'      \item mzmin float, m/z born min
#'      \item mzmax float, m/z born max
#'      \item rt float, rT
#'      \item rtmin float, rT born min
#'      \item rtmax float, rT born max
#'      \item into float, area integrated
#'      \item intb float, area - baseline integrated
#'      \item maxo float, max of intensity
#'      \item sn float, signal/noise
#'      \item egauss float, no idea
#'      \item mu float, no idea
#'      \item sigma float, no idea
#'      \item h float, no idea
#'      \item f float, no idea
#'      \item dppm float, ppm deviation recorded by xcms
#'      \item scale integer, scale of the centwave used for integration
#'      \item scpos integer, scan position of the peak
#'      \item scmin integer, scan born min of first integration
#'      \item scmax integer, scan born max of first integration
#'      \item lmin integer, scan born min of second integration
#'      \item lmax integer, scan born max of second integration
#' }
peakpicking <- function(ms_file, params) {
	peaktable <- data.frame(xcms::do_findChromPeaks_centWave(mz = ms_file@env$mz, 
		int = ms_file@env$intensity, scantime = ms_file@scantime, 
		valsPerSpect = diff(c(ms_file@scanindex, length(ms_file@env$mz))), 
		ppm = params$ppm, peakwidth = params$peakwidth, 
		snthresh = params$snthresh, prefilter = params$prefilter, 
		mzCenterFun = params$mz_center_fun, 
		integrate = if (params$integrate) 1 else 2, 
		mzdiff = params$mzdiff, fitgauss = params$fitgauss, 
		noise = params$noise, firstBaselineCheck = params$first_baseline_check, 
		verboseColumns = params$verboseColumns))
		
	# remove duplicated peaks if we round mz to 5 digits & rt to 2 digits
	peaktable <- peaktable[order(peaktable$into, decreasing = TRUE), ]
	duplicated_peaks <- which(duplicated(paste(
		round(peaktable$mz, 5), round(peaktable$mzmin, 5), 
			round(peaktable$mzmax, 5), 
		round(peaktable$rt / 60, 2), round(peaktable$rtmin / 60, 2), 
			round(peaktable$rtmax / 60, 2))))
	if (length(duplicated_peaks) > 0) peaktable[-duplicated_peaks, ]
	else peaktable
}

#' @title CAMERA process
#'
#' @description
#' Launch some functions from CAMERA to group cluster intra-file (which share same EIC shape)
#'
#' @param peaktable dataframe with columns:
#' \itemize{
#'      \item mz float, m/z
#'      \item mzmin float, m/z born min
#'      \item mzmax float, m/z born max
#'      \item rt float, rT
#'      \item rtmin float, rT born min
#'      \item rtmax float, rT born max
#'      \item into float, area integrated
#'      \item intb float, area - baseline integrated
#'      \item maxo float, max of intensity
#'      \item sn float, signal/noise
#'      \item egauss float, no idea
#'      \item mu float, no idea
#'      \item sigma float, no idea
#'      \item h float, no idea
#'      \item f float, no idea
#'      \item dppm float, ppm deviation recorded by xcms
#'      \item scale integer, scale of the centwave used for integration
#'      \item scpos integer, scan position of the peak
#'      \item scmin integer, scan born min of first integration
#'      \item scmax integer, scan born max of first integration
#'      \item lmin integer, scan born min of second integration
#'      \item lmax integer, scan born max of second integration
#'      \item iso string, isotope annotation
#'      \item abundance float, relative abundance
#'      \item annotation string, class of the cluster for filters F0, F1, F2, F2+
#'      \item coeff float, result of linear regression to see if brominated or chlorinated
#'      \item r_squared float, result of linear regression to see if brominated or chlorinated
#'      \item cluster integer, cluster ID
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID
#' }
#' @param ms_file xcmsRaw
#' @param params list with items:
#' \itemize{
#'      \item polarity string, "negative" or "positive"
#'      \item perfwhm float, Full Width at Half Maximum for grouping integrated ions
#'      \item cor_eic_th float, Threshold for Extracted Ion Chromatogram correlation
#'      \item cor_exp_th float, Threshold for Extracted Ion Chromatogram correlation between samples
#'      \item graphMethod string, , Method used for grouping peaks. HCS means for Highly Connected Substract and LCS for Label Propagation Community. See package CAMERA for more informations.
#'      \item sigma float, Multiplier of the standard deviation
#'      \item pval float, Threshold for p-value
#'      \item intval string, on which intensity value to compute pcgroups
#'      \item calcIso boolean, restrict pcgroup with iso annotation
#'      \item calcCiS boolean, compute correlations inside samples
#'      \item calcCaS boolean, compute correlations between samples
#' }
#'
#' @return dataframe with columns:
#' \itemize{
#'      \item mz float, m/z
#'      \item mzmin float, m/z born min
#'      \item mzmax float, m/z born max
#'      \item rt float, rT
#'      \item rtmin float, rT born min
#'      \item rtmax float, rT born max
#'      \item into float, area integrated
#'      \item intb float, area - baseline integrated
#'      \item maxo float, max of intensity
#'      \item sn float, signal/noise
#'      \item egauss float, no idea
#'      \item mu float, no idea
#'      \item sigma float, no idea
#'      \item h float, no idea
#'      \item f float, no idea
#'      \item dppm float, ppm deviation recorded by xcms
#'      \item scale integer, scale of the centwave used for integration
#'      \item scpos integer, scan position of the peak
#'      \item scmin integer, scan born min of first integration
#'      \item scmax integer, scan born max of first integration
#'      \item lmin integer, scan born min of second integration
#'      \item lmax integer, scan born max of second integration
#'      \item iso string, isotope annotation
#'      \item abundance float, relative abundance
#'      \item annotation string, class of the cluster for filters F0, F1, F2, F2+
#'      \item coeff float, result of linear regression to see if brominated or chlorinated
#'      \item r_squared float, result of linear regression to see if brominated or chlorinated
#'      \item cluster integer, cluster ID
#'      \item pcgroup integer, pcgroup ID
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID
#' }
camera <- function(peaktable, ms_file, params) {
	peaktable2 <- peaktable[which(peaktable$iso == "A"), ] # we keep only A
	xsa <- xs_annotate(peaktable2, params$polarity)
	xsa <- CAMERA::groupFWHM(xsa, sigma = params$sigma, 
		perfwhm = params$perfwhm / 100, intval = params$intval)
	xsa <- CAMERA::groupCorr(xsa, cor_eic_th = params$cor_eic_th, 
		pval = params$pval, graphMethod = params$graphMethod, 
		calcIso = params$calcIso, calcCiS = params$calcCiS, 
		calcCaS = params$calcCaS, xraw = ms_file, 
		cor_exp_th = params$cor_exp_th, intval = params$intval)
	
	pcgroups <- rep(0, nrow(peaktable2))
	for (i in 1:length(xsa@pspectra)) pcgroups[xsa@pspectra[[i]]] <- i
	pcgroups <- data.frame(
		pcgroup = pcgroups, 
		cluster = peaktable2$cluster
	)
	peaktable <- merge(peaktable, pcgroups, by = "cluster", all.x = TRUE)
	# give a new pcgroup id for all which don't have one
	peaktable[which(is.na(peaktable$pcgroup)), "pcgroup"] <- 
		as.integer(as.factor(peaktable[
			which(is.na(peaktable$pcgroup)), "cluster"])) + 
				max(peaktable$pcgroup, na.rm = TRUE)
	peaktable
}

#' @title Alignment process
#'
#' @description
#' Launch alignment process (retcor & then group by density of xcms)
#' for each single cluster (group id to 0), reattribute a new group id
#'
#' @param peaktable dataframe with columns:
#' \itemize{
#'      \item mz float, m/z
#'      \item mzmin float, m/z born min
#'      \item mzmax float, m/z born max
#'      \item rt float, rT
#'      \item rtmin float, rT born min
#'      \item rtmax float, rT born max
#'      \item into float, area integrated
#'      \item intb float, area - baseline integrated
#'      \item maxo float, max of intensity
#'      \item sn float, signal/noise
#'      \item egauss float, no idea
#'      \item mu float, no idea
#'      \item sigma float, no idea
#'      \item h float, no idea
#'      \item f float, no idea
#'      \item dppm float, ppm deviation recorded by xcms
#'      \item scale integer, scale of the centwave used for integration
#'      \item scpos integer, scan position of the peak
#'      \item scmin integer, scan born min of first integration
#'      \item scmax integer, scan born max of first integration
#'      \item lmin integer, scan born min of second integration
#'      \item lmax integer, scan born max of second integration
#'      \item iso string, isotope annotation
#'      \item abundance float, relative abundance
#'      \item annotation string, class of the cluster for filters F0, F1, F2, F2+
#'      \item coeff float, result of linear regression to see if brominated or chlorinated
#'      \item r_squared float, result of linear regression to see if brominated or chlorinated
#'      \item cluster integer, cluster ID
#'      \item pcgroup integer, pcgroup ID
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID
#' }
#' @param params list with items:
#' \itemize{
#'      \item project_samples vector of integers, project_sample ids
#'      \item sample_ids vecctor of strings, sample_ids from database
#'      \item center_sample integer, Sample used as sample of reference to apply the retention time correction
#'      \item bw float, retention time window tolerance (s)
#'      \item mz_wid float, m/z window tolerance (mDa)
#'      \item dist_fun string, Distance function to be used.  Allowed values are "cor"(Pearson\'s correlation), "cor_opt"(calculate only 10% diagonal band of distance matrix; better runtime), "cov"(covariance), "prd"(product) and "euc"(Euclidian distance)
#'      \item response float, Defining the responsiveness of warping with response = 0 giving linear warping on start and end points and response = 100 warping using all bijective anchors.
#'      \item gap_init float, Defining the penalty for gap opening
#'      \item gap_extend float, Defining the penalty for gap enlargement
#'      \item factor_diag float, Defining the local weight applied to diagonal moves in the alignment
#'      \item factor_gap float, Defining the local weight for gap moves in the alignment
#'      \item init_penalty float, Defining the penalty for initiating an alignment (for local alignmentonly)
#'      \item local_alignment boolean, Whether a local alignment should be performed instead of the default global alignment
#'      \item binSize float, width of m/z bins
#'      \item minFraction float, fraction of sample class to validate alignment group
#'      \item minSamples float, minimum of samples to validate alignment group
#' }
#'
#' @return dataframe with columns:
#' \itemize{
#'      \item mz float, m/z
#'      \item mzmin float, m/z born min
#'      \item mzmax float, m/z born max
#'      \item rt float, rT
#'      \item rtmin float, rT born min
#'      \item rtmax float, rT born max
#'      \item into float, area integrated
#'      \item intb float, area - baseline integrated
#'      \item maxo float, max of intensity
#'      \item sn float, signal/noise
#'      \item egauss float, no idea
#'      \item mu float, no idea
#'      \item sigma float, no idea
#'      \item h float, no idea
#'      \item f float, no idea
#'      \item dppm float, ppm deviation recorded by xcms
#'      \item scale integer, scale of the centwave used for integration
#'      \item scpos integer, scan position of the peak
#'      \item scmin integer, scan born min of first integration
#'      \item scmax integer, scan born max of first integration
#'      \item lmin integer, scan born min of second integration
#'      \item lmax integer, scan born max of second integration
#'      \item iso string, isotope annotation
#'      \item abundance float, relative abundance
#'      \item annotation string, class of the cluster for filters F0, F1, F2, F2+
#'      \item coeff float, result of linear regression to see if brominated or chlorinated
#'      \item r_squared float, result of linear regression to see if brominated or chlorinated
#'      \item cluster integer, cluster ID
#'      \item pcgroup integer, pcgroup ID
#'      \item aligngroup integer, aligngroup ID
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID
#' }
alignment <- function(peaktable, params) {
	peaktable2 <- peaktable[which(peaktable$iso %in% c("A-2", "A", "A+2")), ]
	xset <- create_xcmsSet(peaktable2)
	xset <- tryCatch(retcor_obiwarp(xset, db, params$project_samples, 
		params$sample_ids, profStep = params$binSize, 
		center = params$center_sample, response = params$response, 
		distFunc = params$dist_fun, gapInit = params$gap_init, 
		gapExtend = params$gap_extend, factorDiag = params$factor_diag, 
		factorGap = params$factor_gap, localAlignment = params$local_alignment, 
		initPenalty = params$init_penalty)
	, error = function(e) {
		print('error reject by xcms during retcor')
		print(e)
		toastr_error(e$message)
		xset
	})
	xset <- xcms::group.density(xset, bw = params$bw, mzwid = params$mzwid * 10**-3, 
		minfrac = params$minFraction, minsamp = params$minSamples)
	
	aligngroups <- rep(0, nrow(peaktable2))
	for (i in 1:length(xset@groupidx)) aligngroups[xset@groupidx[[i]]] <- i
	aligngroups <- data.frame(
		aligngroup = aligngroups, 
		cluster = peaktable2$cluster
	)
	# remove all aligngroup which are at 0
	aligngroups <- aligngroups[which(aligngroups$aligngroup != 0), ]
	aligngroups$aligngroup <- .Call("clusterize", aligngroups$aligngroup, 
		aligngroups$cluster)
	# some lines can be duplicated
	aligngroups <- unique(aligngroups)
	peaktable <- merge(peaktable, aligngroups, by = "cluster", all.x = TRUE)
	# give a new aligngroups id for all which don't have one
	peaktable[which(is.na(peaktable$aligngroup)), "aligngroup"] <- 
		as.integer(as.factor(peaktable[
			which(is.na(peaktable$aligngroup)), "cluster"])) + 
				max(peaktable$aligngroup, na.rm = TRUE)
	peaktable
}

#' @title Create xsAnnotate
#'
#' @description
#' Create xsAnnotate object for CAMERA with only a peaktable
#' Call function create_xcmsSet to create first a xcms object
#'
#' @param peaktable dataframe with columns:
#' \itemize{
#'      \item mz float, m/z
#'      \item mzmin float, m/z born min
#'      \item mzmax float, m/z born max
#'      \item rt float, rT
#'      \item rtmin float, rT born min
#'      \item rtmax float, rT born max
#'      \item into float, area integrated
#'      \item intb float, area - baseline integrated
#'      \item maxo float, max of intensity
#'      \item sn float, signal/noise
#'      \item egauss float, no idea
#'      \item mu float, no idea
#'      \item sigma float, no idea
#'      \item h float, no idea
#'      \item f float, no idea
#'      \item dppm float, ppm deviation recorded by xcms
#'      \item scale integer, scale of the centwave used for integration
#'      \item scpos integer, scan position of the peak
#'      \item scmin integer, scan born min of first integration
#'      \item scmax integer, scan born max of first integration
#'      \item lmin integer, scan born min of second integration
#'      \item lmax integer, scan born max of second integration
#' }
#' @param polarity string, "negative" & "positive"
#'
#' @return xsAnnotate object
xs_annotate <- function(peaktable, polarity) {
	xset <- create_xcmsSet(peaktable)
	
    object <- new("xsAnnotate")
	object@sample <- 1
	object@groupInfo <- xset@peaks
	object@polarity <- polarity
    object@runParallel <- list(enable = 0)
    colnames(object@annoID) <- c("id", "grpID", "ruleID", 
        "parentID")
    colnames(object@annoGrp) <- c("id", "mass", "ips", 
        "psgrp")
    colnames(object@isoID) <- c("mpeak", "isopeak", 
        "iso", "charge")
    object@xcmsSet <- xset
    object
}

#' @title Create xcmsSet
#'
#' @description
#' Create xcmsSet object for xcms with only a peaktable
#'
#' @param peaktable dataframe with columns:
#' \itemize{
#'      \item mz float, m/z
#'      \item mzmin float, m/z born min
#'      \item mzmax float, m/z born max
#'      \item rt float, rT
#'      \item rtmin float, rT born min
#'      \item rtmax float, rT born max
#'      \item into float, area integrated
#'      \item intb float, area - baseline integrated
#'      \item maxo float, max of intensity
#'      \item sn float, signal/noise
#'      \item egauss float, no idea
#'      \item mu float, no idea
#'      \item sigma float, no idea
#'      \item h float, no idea
#'      \item f float, no idea
#'      \item dppm float, ppm deviation recorded by xcms
#'      \item scale integer, scale of the centwave used for integration
#'      \item scpos integer, scan position of the peak
#'      \item scmin integer, scan born min of first integration
#'      \item scmax integer, scan born max of first integration
#'      \item lmin integer, scan born min of second integration
#'      \item lmax integer, scan born max of second integration
#'      \item project_sample integer, project_sample ID
#'      \item project integer, project ID
#' }
#'
#' @return xsAnnotate object
create_xcmsSet <- function(peaktable) {
	peaktable$sample <- as.integer(as.factor(peaktable$project_sample))
	new("xcmsSet", 
		peaks = as.matrix(peaktable[, c("mz", "mzmin", "mzmax", 
			"rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "egauss", 
			"mu", "sigma", "h", "f", "dppm", "scale", "scpos", "scmin", "scmax", 
			"lmin", "lmax", "sample")]),
		groups = matrix(nrow = 0, ncol = 0),
		groupidx = list(),
		filled = integer(0),
		phenoData = data.frame(class = unique(peaktable$sample)),
		rt = list(raw = list(), corrected = list()),
		filepaths = character(0),
		profinfo = vector('list'),
		dataCorrection = integer(0),
		polarity = character(0),
		progressInfo = list(),
		mslevel = numeric(0),
		scanrange = numeric(0),
		progressCallback = function(progress) NULL,
		.processHistory = list())
}
