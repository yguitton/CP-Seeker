distMatrix <- function(data, nbRow, nbCol){
	data2 <- matrix(0, nrow=nbRow, ncol=nbCol)
	for(row in 1:nrow(data)) data2[data[row,1], data[row, 2]] <- data[row, 3]
	data2
}

triangulization <- function(data){
	triangles <- list()
	
	# get refs (x & y) where z > 0
	ids <- which(data != 0, arr.ind=TRUE)
	ids <- ids %>% data.frame()
	colnames(ids) <- c('x', 'y')
	# the scritp pair all values at x+1 & y+1 so need to 
	# add all x-1 & y-1
	ids <- ids %>% rbind(ids %>% mutate(x=x-1)) %>% 
		rbind(ids %>% mutate(y=y-1)) %>% 
		rbind(ids %>% mutate(x=x-1, y=y-1)) %>% 
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
				
		trianglesTmp <- list(
			E %>% rbind(A) %>% rbind(B),
			E %>% rbind(B) %>% rbind(D),
			E %>% rbind(C) %>% rbind(D),
			E %>% rbind(C) %>% rbind(A))
			
		trianglesTmp <- trianglesTmp %>% keep(function(x) any(x$z > 0))
		if(length(trianglesTmp) == 0) next
		triangles <- append(triangles, trianglesTmp %>%
			map(function(tri) tri %>% arrange(desc(z))))
	}
	triangles
}

computeVolume <- function(triangle, zVal=0){
	abs((sum(triangle$z) - 3*zVal) * 
		((triangle[2, 'x'] - triangle[1, 'x']) * (triangle[3, 'y'] - triangle[1, 'y']) -
			(triangle[2, 'y'] - triangle[1, 'y']) * (triangle[3, 'x'] - triangle[1, 'x']))/6)
}

newCoords <- function(a, b, z){
	data.frame(
		x = ((b$x - a$x) * (z - a$z)) / (b$z - a$z) + a$x,
		y = ((b$y - a$y) * (z - a$z)) / (b$z - a$z) + a$y,
		z = z
	)
}

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

getZCut <- function(triangles, vTarget=90, digits=2, pbVal=0){	
	vT <- reduce(triangles, function(a, b) a + 
		computeVolume(b), .init=0)
	if(vT == 0) return(0)
	vCurr <- 100
	zHigh <- reduce(triangles, function(a, b) max(c(a, b$z)), .init=0)
	zDown <- 0
	while(vCurr != vTarget){
		zMed <- (zHigh + zDown) / 2
		vCurr <- keep(triangles, function(triangle) any(triangle$z >= zMed)) %>%
			reduce(function(a, b) a %>%  
				append(cutTriangle(b, zVal=zMed)), .init=list()) %>% 
				reduce(function(a, b) a + computeVolume(b, zMed), .init=0)
		vCurr <- round(vCurr * 100 / vT, digits=digits)
		if(vCurr > vTarget) zDown <- zMed else zHigh <- zMed
		print(paste("current volume:", vCurr, '%'), value=100)
		updateProgressBar(session, id="pb", title=paste("current volume:", vCurr, '%'), value=pbVal)
	}
	return(zMed)
}

drawTri <- function(triangles=NULL, maxC, maxCl){
	p <- plot_ly()
	if(!is.null(triangles)){
		if(length(triangles) > 0){
			faces <- reduce(triangles, bind_rows)
			p <- p %>% add_trace(data=faces, x=~x, y=~y, z=~z, i=seq(1, nrow(faces), by=3)-1,
				j=seq(2, nrow(faces), by=3)-1, k=seq(3, nrow(faces), by=3)-1, hoverinfo="text", 
				text=~paste('C:', x, '<br />Cl:', y, '<br />intensity:', formatC(z)))
			edges <- reduce(triangles, function(a, b) a %>% bind_rows(data.frame(x=NA, y=NA)) %>%
				bind_rows(b[c(1, 2), ]) %>% bind_rows(data.frame(x=NA, y=NA)) %>% 
				bind_rows(b[c(1, 3), ]) %>% bind_rows(data.frame(x=NA, y=NA)) %>% 
				bind_rows(b[c(2, 3), ]), .init=data.frame())
			p <- p %>% add_trace(mode='lines', color=I('black'), line=list(width=2), data=edges, x=~x, y=~y, z=~z, hoverinfo='none')
		}
	}
	p %>% layout(showlegend=FALSE, scene=list(camera=list(eye=list(x=1.25, y=-1.25, z=1.25)), 
			zaxis=list(title="Intensity", rangemode='tozero'), 
			xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl)))) %>% 
		plotly::config(modeBarButtons=list(list('toImage', 'zoom3d', 'pan3d', 'orbitRotation', 'tableRotation',
			'resetCameraDefault3d', 'resetCameraLastSave3d')), displaylogo=FALSE)
}

drawTriCut <- function(triangles=NULL, z=NULL, maxC, maxCl){
	p <- drawTri(triangles, maxC, maxCl)
	if(!is.null(z)) p %>% add_trace(data=data.frame(x=c(0,0,maxC,maxC), y=c(0,maxCl,0,maxCl), z=rep(z, times=4)),
		x=~x, y=~y, z=~z, i=c(0,3), j=c(1,1), k=c(2,2),opacity=.5, hoverinfo="text", text=
			paste("Intensity:", rep(z, times=4)))
	else p
}

splitToZones <- function(triangles, zVal){
	if(zVal == 0) return(list(triangles))
	trianglesCut <- reduce(triangles, function(a, b) a %>% 
		append(cutTriangle(b, zVal)), .init=list())
	centroids <- reduce(trianglesCut, function(a, b)
		a %>% rbind((b[1, ] + b[2, ] + b[3, ]) / 3), .init=data.frame())
	split(trianglesCut, dbscan(dist(centroids[, -3]), 1, 1)$cluster)
}

scoreZones <- function(zone1, zone2){
	pts1 <- reduce(zone1, rbind) %>% distinct %>% filter(z > 0) %>%
		mutate(x=x*4, y=y*4, z=z/sum(z)*100)
	pts2 <- reduce(zone2, rbind) %>% distinct %>% filter(z > 0) %>%
		mutate(x=x*4, y=y*4, z=z/sum(z)*100)
	data1 <- distMatrix(pts1, max(pts1$x, pts2$x), max(pts1$y, pts2$y))
	data2 <- distMatrix(pts2, max(pts1$x, pts2$x), max(pts1$y, pts2$y))
	(200 - sum(abs(data1 - data2))) / 2 
}

contourPolyhedras <- function(triangles=NULL, samples=NULL, maxC, maxCl){
	if(is.null(triangles)) return(
		plot_ly(type="scatter", mode="lines") %>% 
		 layout(showlegend=FALSE, 
			xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl)))
	)
	p <- plot_ly(type="scatter", mode="lines")
	centroids <- data.frame()
	
	for(i in 1:length(triangles)){
		zones <- map(triangles[[i]], function(zone) reduce(zone, function(a, b) a %>% 
			append(list(b[1:2, ], b[2:3, ], b[c(1, 3), ])),
			.init=list()))
		# centroids <- reduce(zones, function(a, b) a %>% rbind(reduce(b, rbind) %>% 
			# summarise(x=sum(x)/n(), y=sum(y)/n())), .init=centroids)
		zones <- map(zones, function(zone) keep(zone, function(edge) all(edge$z == 0)))
		zones <- map(zones, function(zone) reduce(zone, function(a, b) a %>% 
			rbind(b[, c('x', 'y', 'z')]) %>% rbind(data.frame(x=NA, y=NA, z=NA)), .init=data.frame()))
		
		for(j in 1:length(zones)) p <- p %>% add_lines(data=zones[[j]], x=~x, y=~y, 
			hoverinfo="text", text=paste(samples[i], ' - Zone', j, "<br />Carbons:", 
				round(zones[[j]]$x, digits=2), "<br />Chlorines:", 
				round(zones[[j]]$y, digits=2)), 
			name=paste(samples[i], ' - Zone', j))
	}
	
	# compute scores & add a line for each couple of zone which have a score > 0
	# scores <- proxy::dist(zones, method=function(x, y) scoreZones(x, y))
	# scores <- combn(1:length(zones), 2) %>% t %>% cbind(scores %>% c)
	# scores <- scores[which(scores[, 3] > 0), ]
	# for(row in 1:nrow(scores)) p <- p %>% add_trace(mode='lines+markers', 
		# data=centroids[scores[row, 1:2], ], showlegend=FALSE, 
			# x=~x, y=~y, marker=list(symbol="x-dot", size=10), line=list(dash="dash")) %>% 
		# add_text(data=centroids[scores[row, 1:2], ] %>% summarise(x=mean(x), y=mean(y)), showlegend=FALSE, 
				# x=~x, y=~y, text=paste(round(scores[row, 3]), '%'), textfont=list(family="Balto", size=20))

	p %>%
		layout(xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl))) %>% 
		plotly::config(scrollZoom=TRUE, displaylogo=FALSE, modeBarButtons=list(
			list('zoom2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
}
