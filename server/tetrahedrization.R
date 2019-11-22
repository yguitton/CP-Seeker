initializeTetras <- function(pja, minScore = 70){
	query <- sprintf('select C as x, Cl as y, sum("into") as z 
		from feature left join cluster on cluster.cluster = feature.cluster 
		where project_sample_adduct == %s and score >= %s 
		group by(formula);', pja, minScore)
	print(query)
	dbGetQuery(db, query)
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

computeVolumeTris <- function(triangles, zVal = 0){
	do.call(sum, lapply(triangles, function(triangle) 
		computeVolumeTri(triangle, zVal)))
}

computeVolumeTri <- function(triangle, zVal=0){
	abs((sum(triangle$z) - 3*zVal) * 
		((triangle[2, 'x'] - triangle[1, 'x']) * (triangle[3, 'y'] - triangle[1, 'y']) -
			(triangle[2, 'y'] - triangle[1, 'y']) * (triangle[3, 'x'] - triangle[1, 'x']))/6)
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

splitToZones <- function(triangles){
	centroids <- do.call(rbind, lapply(triangles, function(triangle)
		(triangle[1, ] + triangle[2, ] + triangle[3, ]) / 3))
	split(triangles, dbscan(dist(centroids[, -3]), .5, 1)$cluster)
}

getEdges <- function(points, zCut){
	split(points, points$triangle) %>% 
	lapply(function(triangle) 
		list(triangle[1:2, ], triangle[2:3, ], triangle[c(1, 3), ])) %>% 
	unlist(recursive = FALSE) %>% 
	keep(function(edge) all(edge$z == zCut)) %>% 
	purrr::reduce(function(a, b) a %>% 
		rbind(data.frame(x = NA, y = NA, z = NA)) %>% 
		rbind(b %>% select(x, y, z)), .init=data.frame())
}

getPoints <- function(profileName){
	query <- sprintf('select x, y, z from point where z >= (
		select zCut from profile where profile == "%s") and 
		profile == "%s";', profileName, profileName)
	print(query)
	dbGetQuery(db, query)
}

scoreProfiles <- function(points1, points2){
	points1 <- unique(points1)
	points2 <- unique(points2)
	maxRow <- max(c(points1$x, points2$x))
	maxCol <- max(c(points1$y, points2$y))
	profile1 <- profileMat(points1, maxRow, maxCol)
	profile2 <- profileMat(points2, maxRow, maxCol)
	profile1 <- profile1 / sum(profile1)
	profile2 <- profile2 / sum(profile2)
	round((2 - sum(abs(profile1 - profile2))) / 2 * 100)
}
