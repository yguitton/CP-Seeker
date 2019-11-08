# return the theoretic pattern of formulas
# it need a vector with formulas, a charge and a resolution (can be the instrument or an integer)
# the data for instrument is give by the package enviPat
theoricClustersFunction <- function(formulas, charges, resolution){
	if(any(!is.numeric(charges)) | is.null(resolution)){
		print('charge or resolution is null')
		return(list(data.frame()))
	}
	else if(!resolution %in% names(resolution_list) & (
			!is.numeric(resolution) | resolution == "0")){
		print(paste('resolution incorrect:', resolution))
		return(list(data.frame()))
	}

	res <- check_chemform(isotopes, formulas)

	data <- lapply(1:nrow(res), function(x) data.frame(mz = 0, abundance = 0))
	if(resolution %in% names(resolution_list)){
		resmass <- resolution_list[[which(names(resolution_list) == resolution)]]
		massLimit <- range(resmass[, 'm/z'])
		
		# check if any mass is upper or below
		test <- res[, 'monoisotopic_mass'] < massLimit[1] | 
			res[, 'monoisotopic_mass'] > massLimit[2]
		data[which(test)] <- data.frame()
		data[which(!test)] <- isowrap(isotopes, checked=res[which(!test), ], 
				resmass=resmass, threshold=1, charge=charges[which(!test)]) %>% 
			map(data.frame) %>% map(setNames, c('mz', 'abundance'))
		data
	} else isowrap(isotopes, checked=res, resmass=FALSE, resolution=as.numeric(resolution),
			threshold=1, charge=charges) %>% map(data.frame %>% 
				rename(mz = `m.z`))
}

# function from the package enviPat
isowrap <- function (isotopes, checked, resmass, resolution = FALSE, nknots = 6, 
	spar = 0.2, threshold = .1, charge = 1, emass = 0.00054858, 
	algo = 2, ppm = FALSE, dmz = "get", frac = 1/4, env = "Gaussian", 
	detect = "centroid", plotit = FALSE, verbose = FALSE){
	if (any(checked[, 1])){
		stop("WARNING: checked with incorrect chemical detected!")
	}
	if (length(resmass) > 1){
		resolution <- getR(checked, resmass = resmass, nknots = nknots, 
			spar = spar, plotit = plotit)
	}
	
	pattern <- isopattern(isotopes, checked[, 2], threshold = threshold, 
		charge = charge, emass = emass, plotit = plotit, algo = algo, verbose = verbose)
	profiles <- envelope(pattern, ppm = ppm, dmz = dmz, frac = frac, 
		env = env, resolution = resolution, plotit = plotit, verbose = verbose)
	centro <- vdetect(profiles, detect = detect, plotit = plotit, verbose = verbose)
	return(centro)
}

# compute the score 
calculateScore <- function(cluster, theoric, ppm, tolI){
	if(nrow(cluster) == 0 | nrow(theoric) == 0 | 
		!is.numeric(ppm) | !is.numeric(tolI)) return(0)
	else if(ppm < 0 | tolI < 0) return(0)
	cluster <- cluster %>% arrange(desc(abundance)) %>% 
		mutate(weight = abundance / sum(abundance), id = 1:n())
	theoric <- theoric %>% arrange(desc(abundance)) %>% 
		mutate(weight = abundance / sum(abundance), id = 1:n())
	round(50 * (2 - calculateScore2(cluster, theoric, ppm, tolI)), digits=2)
}
calculateScore2 <- function(cluster, theoric, ppm, tolI){
	if(nrow(cluster) == 0 | nrow(theoric) == 0) return(
		sum(theoric$weight) + sum(cluster$weight))
	res <- calculateScore3(cluster, theoric[1, ], ppm, tolI)
	cluster <- res$cluster
	res$score + calculateScore2(cluster, theoric[-1, ], ppm, tolI) # cumulative sum
}
calculateScore3 <- function(cluster, theoricFeature, ppm, tolI){
	tolMz <- theoricFeature$mz * ppm * 10**-6
	# first search the matched observed feature
	observedFeature <- cluster %>% filter(between(mz, 
		theoricFeature$mz - tolMz, theoricFeature$mz + tolMz)) %>% 
		mutate(deviation = ((theoricFeature$abundance - abundance) %>% abs) / tolI) %>% 
		filter(deviation < 1) %>% top_n(1, abundance) %>% slice(1)
	# return the minimal score (0: perfect match, 1: wrong match)
	# remove the matched feature for avoid an other assignation with a feature
	if(nrow(observedFeature) == 0) list(
		score = theoricFeature$weight, 
		cluster = cluster
	) else list(
		score = observedFeature$deviation * (theoricFeature$weight + 
			observedFeature$weight),
		cluster = cluster %>% filter(id != observedFeature$id)
	)
}

# compute the deviation (return in mDa)
calculateDeviation <- function(cluster, theoric, ppm){
	if(nrow(cluster) == 0 | nrow(theoric) == 0) return(0)
	cluster <- cluster %>% arrange(desc(abundance)) %>% 
		mutate(id = 1:n())
	theoric <- theoric %>% arrange(desc(abundance))
	
	round(calculateDeviation2(cluster, theoric, ppm) * 10**3, digits=2)
}
calculateDeviation2 <- function(cluster, theoric, ppm){
	if(nrow(cluster) == 0 | nrow(theoric) == 0) return(
		sum(theoric$weight) + sum(cluster$weight))
	res <- calculateDeviation3(cluster, theoric[1, ], ppm)
	cluster <- res$cluster
	res$score + calculateDeviation2(cluster, theoric[-1, ], ppm) # cumulative sum
}
calculateDeviation3 <- function(cluster, theoricFeature, ppm){
	tolMz <- theoricFeature$mz * ppm * 10**-6
	# first search the matched observed feature
	observedFeature <- cluster %>% filter(between(mz, 
		theoricFeature$mz - tolMz, theoricFeature$mz + tolMz)) %>% 
		mutate(deviation = theoricFeature$mz - mz, 
			absDeviation = abs(deviation)) %>% 
		top_n(1, absDeviation) %>% slice(1)
	# remove the matched feature for avoid an other assignation with a feature
	if(nrow(observedFeature) == 0) list(
		score = 0, 
		cluster = cluster
	) else list(
		score = observedFeature$deviation,
		cluster = cluster %>% filter(id != observedFeature$id)
	)
}

arrangeEics <- function(eic, msFile) aggregate(eic$intensity, 
	by = list(round(msFile@scantime / 60, 2)), FUN = median) %>% 
	setNames(c('rt', 'intensity'))

getIonFormula <- function(formulas, adduct, charges = 0){
	check_chemform(isotopes, formulas) %>% mutate(
			row = 1:n(), 
			formula = new_formula, 
			charge = as.numeric(adduct$charge)) %>% 
		filter(!warning) %>% mutate(
			ion_formula = multiform(formula, as.numeric(adduct$multi)) %>% 
				mergeform(adduct$formula_add) %>% 
				mergeform("H0") %>% 
				subform(adduct$formula_ded)) %>% 
		filter(!grepl('not', ion_formula)) %>% 
		select(row, formula, ion_formula, charge)
}

getNeutralFormula <- function(ion_formulas, adduct){
	check_chemform(isotopes, ion_formulas) %>% dplyr::mutate(
			row = 1:n(), ion_formula = new_formula) %>% 
		filter(!warning) %>% mutate(
			formula = multiform(new_formula, 1/adduct$multi) %>% 
				mergeform(adduct$formula_ded) %>% 
				mergeform("H0") %>% 
				subform(adduct$formula_add)) %>% 
		filter(!grepl('not', formula)) %>% 
		select(row, formula, ion_formula)
}

loadMSFile <- function(db, project_sample){
	xr <- tryCatch(dbGetQuery(db, 
		sprintf("select raw from sample where sample == (
			select sample from project_sample where project_sample == %s);", 
			project_sample))$raw %>% unlist %>% decompress_fst %>% unserialize, 
		error = function(e) NULL)
	gc()
	if(is.null(xr)){
		# try to found it in mzXMLFiles repertory
		paths <- dbGetQuery(db, sprintf("select path, rawPath from sample 
			where sample == (select sample from project_sample where 
			project_sample == %s);", project_sample)) %>% 
			c %>% keep(file.exists)
		if(length(paths) == 0) return('file is not found in database')
		else {
			xr <- tryCatch(xcmsRaw(paths[1], mslevel=1, profstep=0), 
				error = function(e) NULL)
			gc()
			if(is.null(xr) & length(paths) == 2){
				xr <- tryCatch(xcmsRaw(paths[2], mslevel=1, profstep=0), 
					error = function(e) NULL)
				gc()
				if(is.null(xr)) return('file is not found in database')
			} else if(is.null(xr)) return('file is not found in database')
		}
	}
	xr
}