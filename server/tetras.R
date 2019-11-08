output$uiTetrasSample <- renderUI({
	choices <- tryCatch({
		if(is.null(input$project)) custom_stop('invalid', 'project picker is not yet initialized')
		else if(input$project == '') custom_stop('invalid', 'no project')
		else project_samples_adducts() %>% left_join(project_samples()) %>% 
			filter(project == input$project) %>% select(sampleID, project_sample)
	}, invalid = function(i){
		print(paste(i))
		data.frame(sampleID = c(), project_sample = c())
	}, error = function(e){
		print('ERR uiTetrasSamples')
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(sampleID = c(), project_sample = c())
	})
	pickerInput('tetrasSample', 'sample(s)', choices=setNames(choices$project_sample, choices$sampleID), 
		multiple=FALSE, options=list(`live-search`=TRUE))
})

output$uiTetrasAdduct <- renderUI({
	choices <- tryCatch({
		if(is.null(input$tetrasSample)) custom_stop('invalid', 'tetras sample picker is not yet initialized')
		else if(length(input$tetrasSample) == 0) custom_stop('invalid', 'no tetras sample selected')
		else project_samples_adducts() %>% filter(project_sample == input$tetrasSample) %>% 
			select(project_sample_adduct, adduct)
	}, invalid = function(i){
		print(paste(i))
		data.frame(project_sample_adduct = c(), adduct = c())
	}, error = function(e){
		print('ERR uiTetrasAdduct')
		print(paste(e))
		sendSweetAlert(paste(e$message))
		data.frame(project_sample_adduct = c(), adduct = c())
	})
	pickerInput('tetrasAdduct', 'adduct', choices=setNames(
		choices$project_sample_adduct, choices$adduct), multiple=FALSE)
})

values$tetras <- list(
	pja = NULL,
	xlsxFile = NULL, 
	vTarget = NULL, 
	triangles = NULL, 
	zCut = NULL
)

output$tetras3D <- renderPlotly({
	tryCatch(
	draw3D(values$tetras$triangles, c(minC, maxC), c(minCl, maxCl), values$tetras$zCut)
	, error = function(e){
		print('ERR tetras3D')
		print(e)
		toastr_error('Cannot draw 3D profile', paste(e$message))
		draw3D(c(minC, maxC), c(minCl, maxCl))
	})
})

draw3D <- function(triangles=NULL, xrange, yrange, zCut = 0){
	p <- plot_ly()
	if(!is.null(triangles)){
		faces <- do.call(rbind, triangles)
		edges <- purrr::reduce(triangles, function(a, b) 
			a %>% rbind(data.frame(x = NA, y = NA, z = NA)) %>% 
				rbind(b[c(1:3, 1), ]), .init=data.frame())
		p <- p %>% add_trace(data=faces, x=~x, y=~y, z=~z, i=seq(1, nrow(faces), by=3)-1,
				j=seq(2, nrow(faces), by=3)-1, k=seq(3, nrow(faces), by=3)-1, hoverinfo="text", 
				text=~paste('C:', round(x, precision), '<br />Cl:', round(y, precision), 
					'<br />intensity:', formatC(z))) %>% 
			add_trace(mode='lines', color=I('black'), line=list(width=2), data=edges, x=~x, y=~y, z=~z, hoverinfo='none')
		if(zCut > 0) p <- p %>% add_trace(data = data.frame(
				x = rep(xrange, each = 2), y = rep(yrange, times = 2), z=rep(zCut, times=4)),
				x = ~x, y = ~y, z = ~z, i = c(0,3), j = c(1,1), k = c(2,2), 
				opacity = .5, hoverinfo = "none")
	}
	p %>% layout(showlegend=FALSE, scene=list(camera=list(eye=list(x=1.25, y=-1.25, z=1.25)), 
			zaxis=list(title="Intensity", rangemode='tozero'), 
			xaxis=list(title="Number of Carbon", range=xrange), 
			yaxis=list(title="Number of Chlorine", range=yrange))) %>% 
		plotly::config(modeBarButtons=list(list('toImage', 'zoom3d', 'pan3d', 'orbitRotation', 'tableRotation',
			'resetCameraDefault3d', 'resetCameraLastSave3d')), displaylogo=FALSE)
}

output$tetras2D <- renderPlotly({
	tryCatch(
	draw2D(values$tetras$triangles, c(minC, maxC), c(minCl, maxCl), values$tetras$zCut)
	, error = function(e){
		print('ERR tetras3D')
		print(e)
		toastr_error('Cannot draw 3D profile', paste(e$message))
		draw2D(c(minC, maxC), c(minCl, maxCl))
	})
})

draw2D <- function(trianglesList = NULL, xrange, yrange, zCut){
	p <- plot_ly(type="scatter", mode="lines")
	if(!is.null(trianglesList)){
		for(i in 1:length(trianglesList)) p <- p %>% 
			add_lines(data = getEdges(trianglesList[[i]]), x=~x, y=~y, hoverinfo = "text", showlegend = FALSE, 
				text = ~paste("C:", round(x, precision), "<br />Cl:", round(y, precision)))
	}
	p %>%
		layout(xaxis=list(title="Number of Carbon", range=xrange), 
			yaxis=list(title="Number of Chlorine", range=yrange)) %>% 
		plotly::config(scrollZoom=TRUE, displaylogo=FALSE, modeBarButtons=list(
			list('zoom2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
}

observeEvent(input$tetrasCompute, {
	print('############################################################')
	print('######################### TETRAS ###########################')
	print('############################################################')

	print(list(project_sample_adduct = input$tetrasAdduct, vTarget = input$tetrasVTarget))
	tryCatch({
		args <- c('tetrasAdduct')
		conditions <- c(is.null(input$tetrasAdduct))
		messages <- c('You need to select a sample & an adduct')
		test <- inputsTest(args, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		args <- c('tetrasAdduct')
		conditions <- c(length(input$tetrasAdduct) == 0)
		messages <- c('You need to select a sample & an adduct')
		test <- inputsTest(args, conditions, messages)
		if(!test) custom_stop('invalid', 'invalid args')
		
		pb <- progressSweetAlert(session, 'pb', value = 100, striped = TRUE, title = "Triangulization")
		triangles <- initializeTetras(input$tetrasAdduct) %>% 
			profileMat(maxC, maxCl) %>% 
			triangulization
		zCut <- getZCut(triangles, input$tetrasVTarget, precision, "pb")
		triangles <- purrr::reduce(triangles, function(a, b) 
			a %>% append(splitTriangle(b, zCut)), .init=list())
			
		values$tetras <- list(
			pja = input$tetrasAdduct, 
			xlsxFile = NULL, 
			vTarget = input$tetrasVTarget, 
			triangles = triangles, 
			zCut = zCut
		)
		closeSweetAlert(session)
			
	}, invalid = function(i){
		values$tetras <- list(
			pja = NULL,
			xlsxFile = NULL, 
			vTarget = NULL, 
			triangles = NULL, 
			zCut = NULL
		)
		print(i)
	}, error = function(e){
		values$tetras <- list(
			pja = NULL,
			xlsxFile = NULL, 
			vTarget = NULL, 
			triangles = NULL, 
			zCut = NULL
		)
		print(e)
		sendSweetAlert('Cannot compute profile', paste(e$message))
	})
	
	print('############################################################')
	print('######################### END TETRAS #######################')
	print('############################################################')
})

observeEvent(input$tetrasRecord, {
	print('############################################################')
	print('######################### RECORD TETRAS ####################')
	print('############################################################')
	
	tryCatch({
		print(list(name = input$tetrasName, nbTriangles = length(values$tetras$triangles)))
		profileNames <- dbGetQuery(db, 'select name from profile;')$name
		args <- c("tetrasName", "tetrasName", "")
		conditions <- c(input$tetrasName == "", input$tetrasName %in% profileNames, 
			is.null(values$tetras$triangles))
		messages <- c("You have to specify a name for the record", 
			"A profile with this name already exists", "no profile computed")
		if(!inputsTest(args, conditions, messages)) custom_stop('invalid', 'invalid args')
		
		recordProfile(input$tetrasName, values$tetras$vTarget, values$tetras$triangles, 
			values$tetras$zCut, pja = values$tetras$pja, xlsx = values$tetras$xlsxFile)
	}, invalid = function(i) NULL
	, error = function(e){
		print(e)
		deleteProfile(input$tetrasName)
		sendSweetAlert('Cannot record this profile', paste(e$message))
	})
	actualize$profiles <- TRUE
	print('############################################################')
	print('######################### END RECORD TETRAS ################')
	print('############################################################')
})

deleteProfile <- function(name){
	query <- sprintf('delete from point where profile == "%s";', name)
	print(query)
	dbExecute(db, query)
	query <- sprintf('delete from profile where name == "%s";', name)
	print(query)
	dbExecute(db, query)
}

recordProfile <- function(name, vTarget, triangles, zCut, project_sample_adduct = NULL, xlsxFile = NULL){
	query <- if(is.null(xlsxFile)) sprintf('insert into profile (name, vTarget, zCut, project_sample_adduct) 
			values ("%s", %s, %s, %s, "%s");', name, vTarget, zCut, project_sample, adduct)
		else sprintf('insert into profile (name, vTarget, zCut, xlsxFile) 
			values ("%s", %s, %s, "%s");', name, vTarget, zCut, xlsxFile)
	print(query)
	dbExecute(db, query)
	triangles <- do.call(rbind, lapply(1:length(triangles), function(i) 
		triangles[[i]] %>% mutate(triangle = i))) %>% mutate(profile = name)
	query <- sprintf('insert into point (x, y, z, triangle, profile) values %s;', 
		paste(sprintf('(%s, %s, %s, %s, "%s")', triangles$x, triangles$y, 
			triangles$z, triangles$triangle, triangles$profile), collapse = ', '))
	dbExecute(db, query)
}

initializeTetras <- function(pja){
	query <- sprintf('select C as x, Cl as y, sum("into") as z 
		from feature left join cluster on cluster.cluster = feature.cluster 
		where project_sample_adduct == %s and score >= 70 
		group by(formula);', pja, adduct)
	print(query)
	dbGetQuery(db, query)
}

getProfile <- function(profile){
	query <- sprintf('select x, y, z, triangle from point where 
		profile == %s;', profile)
	print(query)
	triangles <- dbGetQuery(db, query)
	triangles <- split(triangles, triangles$triangle)
	query <- sprintf('select zCut from profile where profile == %s;', profile)
	print(query)
	zCut <- dbGetQuery(db, query)$zCut
	sepTri(triangles, zCut)
}

profileMat <- function(data, maxRow, maxCol){
	data2 <- matrix(0, nrow = maxRow, ncol = maxCol)
	for(row in 1:nrow(data)) data2[data[row, 'x'], data[row, 'y']] <- data[row, 'z']
	data2 / max(data2)
}

triangulization <- function(data){
	print('triangulization')
	triangles <- list()
	
	# get refs (x & y) where z > 0
	ids <- which(data != 0, arr.ind=TRUE)
	ids <- ids %>% data.frame()
	colnames(ids) <- c('x', 'y')
	# the scritp pair all values at x+1 & y+1 so need to 
	# add all x-1 & y-1
	ids <- ids %>% rbind(ids %>% dplyr::mutate(x=x-1)) %>% 
		rbind(ids %>% dplyr::mutate(y=y-1)) %>% 
		rbind(ids %>% dplyr::mutate(x=x-1, y=y-1)) %>% 
		filter(x > 0 & y > 0 & x < nrow(data)-1 & y < ncol(data)-1) %>% 
		distinct %>% arrange(x, y)
	
	for(row in 1:nrow(ids)){
		x <- ids[row, 'x']
		y <- ids[row, 'y']
		
		A <- data.frame(x=x, y=y, z=data[x, y])
		B <- data.frame(x=x, y=y+1, z=data[x, y+1])
		C <- data.frame(x=x+1, y=y, z=data[x+1, y])
		D <- data.frame(x=x+1, y=y+1, z=data[x+1, y+1])
		E <- (A+B+C+D)/4
				
		triangles <- triangles %>% append(list(
				E %>% rbind(A) %>% rbind(B),
				E %>% rbind(B) %>% rbind(D),
				E %>% rbind(C) %>% rbind(D),
				E %>% rbind(C) %>% rbind(A)) %>% 
			keep(function(x) any(x$z > 0)))
	}
	triangles
}

computeVolumeTri <- function(triangle, zVal=0){
	abs((sum(triangle$z) - 3*zVal) * 
		((triangle[2, 'x'] - triangle[1, 'x']) * (triangle[3, 'y'] - triangle[1, 'y']) -
			(triangle[2, 'y'] - triangle[1, 'y']) * (triangle[3, 'x'] - triangle[1, 'x']))/6)
}

computeVolumeTris <- function(triangles, zVal = 0){
	do.call(sum, lapply(triangles, function(triangle) 
		computeVolumeTri(triangle, zVal)))
}

newCoords <- function(a, b, z){
	if(z > 0) data.frame(
			x = ((b$x - a$x) * (z - a$z)) / (b$z - a$z) + a$x,
			y = ((b$y - a$y) * (z - a$z)) / (b$z - a$z) + a$y,
			z = z
	) else data.frame(
		x = (b$x + a$x) / 2,
		y = (b$y + a$y) / 2,
		z = 0
	)
}

# don't return triangles under zVal
cutTriangle <- function(triangle, zVal){
	test <- length(which(triangle$z < zVal))
	if(test == 0) return(list(triangle))
	else if(test == 1){
		pts <- triangle[which(triangle$z >= zVal), ]
		ptA <- pts[1, ]
		ptB <- pts[2, ]
		ptC <- triangle[which(triangle$z < zVal), ]
		ptAB <- newCoords(ptA, ptB, (ptA$z+ptB$z)/2)
		ptAC <- newCoords(ptA, ptC, zVal)
		ptBC <- newCoords(ptB, ptC, zVal)
		return(list(ptA %>% rbind(ptAB) %>% rbind(ptAC),
			ptB %>% rbind(ptAB) %>% rbind(ptBC),
			ptAB %>% rbind(ptAC) %>% rbind(ptBC)))
	} else if(test == 2){
		ptA <- triangle[which(triangle$z >= zVal), ]
		pts <- triangle[which(triangle$z < zVal), ]
		ptB <- pts[1, ]
		ptC <- pts[2, ]
		ptAB <- newCoords(ptA, ptB, zVal)
		ptAC <- newCoords(ptA, ptC, zVal)
		return(list(ptA %>% rbind(ptAB) %>% rbind(ptAC)))
	} else list()
}

# return triangles under & above zVal
splitTriangle <- function(triangle, zVal){
	test <- length(which(triangle$z < zVal))
	if(test == 0) return(list(triangle))
	else if(test == 1){
		pts <- triangle[which(triangle$z >= zVal), ]
		ptA <- pts[1, ]
		ptB <- pts[2, ]
		ptC <- triangle[which(triangle$z < zVal), ]
		ptAB <- newCoords(ptA, ptB, (ptA$z+ptB$z)/2)
		ptAC <- newCoords(ptA, ptC, zVal)
		ptBC <- newCoords(ptB, ptC, zVal)
		return(list(ptA %>% rbind(ptAB) %>% rbind(ptAC),
			ptB %>% rbind(ptAB) %>% rbind(ptBC),
			ptC %>% rbind(ptAC) %>% rbind(ptBC),
			ptAB %>% rbind(ptAC) %>% rbind(ptBC)))
	} else if(test == 2){
		ptA <- triangle[which(triangle$z >= zVal), ]
		pts <- triangle[which(triangle$z < zVal), ]
		ptB <- pts[1, ]
		ptC <- pts[2, ]
		ptAB <- newCoords(ptA, ptB, zVal)
		ptAC <- newCoords(ptA, ptC, zVal)
		ptBC <- newCoords(ptB, ptC, (ptB$z+ptC$z)/2)
		return(list(ptA %>% rbind(ptAB) %>% rbind(ptAC),
			ptB %>% rbind(ptAB) %>% rbind(ptBC),
			ptC %>% rbind(ptAC) %>% rbind(ptBC),
			ptAB %>% rbind(ptAC) %>% rbind(ptBC)))
	} else return(list(triangle))
}

sepTri <- function(triangles, zCut){
	keep(triangles, function(triangle) all(triangle$z) >= zCut) %>% 
	lapply(function(triangle) triangle %>% mutate(z = z - zCut))
}

getZCut <- function(triangles, vTarget=90, digits=2, pb = NULL){	
	vT <- computeVolumeTris(triangles)
	vCurr <- 100
	zHigh <- 1
	zDown <- 0
	while(vCurr != vTarget){
		zMed <- (zHigh + zDown) / 2
		vCurr <- do.call(sum, lapply(triangles, function(triangle) 
			triangle %>% cutTriangle(zVal = zMed) %>% computeVolumeTris))
		vCurr <- round(vCurr * 100 / vT, digits=digits)
		print(vCurr)
		if(!is.null(pb)) updateProgressBar(session, id = pb, title=paste("Reach", vCurr, "%"), value = 100)
		if(vCurr > vTarget) zDown <- zMed else zHigh <- zMed
	}
	print(sprintf('found zMed: %s', zMed)) 
	return(zMed)
}

scoreProfiles <- function(zone1, zone2, minC, maxC, minCl, maxCl){
	mat1 <- do.call(rbind, zone1) %>% distinct %>% 
		profileMat(minC, maxC, minCl, maxCl)
	mat2 <- do.call(rbind, zone2) %>% distinct %>% 
		profileMat(minC, maxC, minCl, maxCl)
	weights <- sum(mat1 + mat2)
	sum((mat1**2 + mat2**2) / weights) * 50
}

getEdges <- function(triangles){
	lapply(triangles, function(triangle) 
		list(triangle[1:2, ], triangle[2:3, ], triangle[c(1, 3), ])) %>% 
		unlist(recursive = FALSE) %>% 
		keep(function(edge) all(edge$z == 0)) %>% 
		purrr::reduce(function(a, b) a %>% 
			rbind(data.frame(x = NA, y = NA, z = NA)) %>% 
			rbind(b), .init=data.frame())
}

