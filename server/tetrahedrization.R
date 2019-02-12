distMatrix <- function(data){
	data2 <- matrix(0, nrow=maxC, ncol=maxCl)
	for(row in 1:nrow(data)) data2[data[row,1], data[row, 2]] <- data[row, 3]
	data2
}

tetrahedrization <- function(data, zCut=0){
	tetras <- list()
	triangles <- list()
	
	for(x in 1:(nrow(data)-1)){
	for(y in 1:(ncol(data)-1)){
		
		trianglesTmp <- list(
			data.frame(x=c(x, x+1, x+1), y=c(y, y, y+1), 
				z=c(data[x, y], data[x+1, y], data[x+1, y+1])),
			data.frame(x=c(x, x, x+1), y=c(y, y+1, y+1),
				z=c(data[x, y], data[x, y+1], data[x+1, y+1])))
		trianglesTmp <- trianglesTmp %>% keep(function(x) any(x$z > 0))
		if(length(trianglesTmp) == 0) next
		
		tetrasTmp <- reduce(trianglesTmp, function(a, b) a %>% append(
			computeTetra(b)), .init=list())
		
		tetras <- append(tetras, tetrasTmp)
		triangles <- append(triangles, trianglesTmp)
	}}
	return(list(tetras=tetras, triangles=triangles))
}

computeTetra <- function(triangle, z=0){
	tetra1 <- triangle %>% rbind(data.frame(x=triangle[1, 'x'], y=triangle[1, 'y'], z))
	tetra2 <- tetra1[-1, ] %>% rbind(data.frame(x=tetra1[2, 'x'], y=tetra1[2, 'y'], z))
	tetra3 <- tetra2[-1, ] %>% rbind(data.frame(x=tetra2[2, 'x'], y=tetra2[2, 'y'], z))
	list(tetra1, tetra2, tetra3)
}

computeVolumePolyhedra <- function(tetras){
	reduce(tetras, function(a, b) a + computeVolumeTetra(b), .init=0)
}
computeVolumeTetra <- function(tetra){
	if(nrow(tetra) < 4) return(0)
	abs(
		(tetra[4, 'x'] - tetra[1, 'x']) * (
			(tetra[2, 'y'] - tetra[1, 'y']) * (tetra[3, 'z'] - tetra[1, 'z']) -
			(tetra[2, 'z'] - tetra[1, 'z']) * (tetra[3, 'y'] - tetra[1, 'y']))
		+ (tetra[4, 'y'] - tetra[1, 'y']) * (
			(tetra[2, 'z'] - tetra[1, 'z']) * (tetra[3, 'x'] - tetra[1, 'x']) - 
			(tetra[2, 'x'] - tetra[1, 'x']) * (tetra[3, 'z'] - tetra[1, 'z']))
		+ (tetra[4, 'z'] - tetra[1, 'z']) * (
			(tetra[2, 'x'] - tetra[1, 'x']) * (tetra[3, 'y'] - tetra[1, 'y']) - 
			(tetra[2, 'y'] - tetra[1, 'y']) * (tetra[3, 'x'] - tetra[1, 'x']))
	) / 6
}

splitTetra <- function(tetra, z){
	if(all(tetra$z < z)) return(list())
	else if(all(tetra$z >= z)) return(list(tetra))
	tetra[4, 'z'] <- z
	tetra <- tetra[order(tetra$z, decreasing=TRUE), ]
	nodesAboveZ <- tetra[which(tetra$z >= z), ]
	nodesBelowZ <- tetra[which(tetra$z < z), ]
	if(nrow(nodesBelowZ) == 2){
		nodes <- nodesAboveZ %>% bind_rows(newCoords(nodesAboveZ[1, ], nodesBelowZ[1, ], z)) %>% 
			bind_rows(newCoords(nodesAboveZ[1, ], nodesBelowZ[2, ], z)) %>% 
			bind_rows(newCoords(nodesAboveZ[2, ], nodesBelowZ[1, ], z)) %>% 
			bind_rows(newCoords(nodesAboveZ[2, ], nodesBelowZ[2, ], z))
		list(nodes[c(1, 2, 3, 4), ], nodes[c(2, 3, 4, 5), ], nodes[c(2, 4, 5, 6), ])
	} else if(nrow(nodesBelowZ) == 1){
		nodes <- nodesAboveZ %>% bind_rows(newCoords(nodesAboveZ[1, ], nodesBelowZ, z)) %>% 
			bind_rows(newCoords(nodesAboveZ[2, ], nodesBelowZ, z)) %>% 
			bind_rows(newCoords(nodesAboveZ[3, ], nodesBelowZ, z))
		list(nodes[c(1,2,3,6), ], nodes[c(1,2,6,5), ], nodes[c(1,5,6,4), ])
	} else list(tetra)
}

newCoords <- function(a, b, z){
	data.frame(
		x = ((b$x - a$x) * (z - a$z)) / (b$z - a$z) + a$x,
		y = ((b$y - a$y) * (z - a$z)) / (b$z - a$z) + a$y,
		z = z
	)
}

getZCut <- function(tetras, vTarget=90, digits=2){	
	vT <- computeVolumePolyhedra(tetras)
	vCurr <- 100
	zHigh <- reduce(tetras, function(a, b) max(c(a, b$z)), .init=0)
	zDown <- 0
	while(vCurr != vTarget){
		zMed <- (zHigh + zDown) / 2
		vCurr <- round(reduce(tetras, function(a, b) 
			a + computeVolumePolyhedra(splitTetra(b, zMed)), .init=0) * 100 / vT, digits=digits)
		if(vCurr > vTarget) zDown <- zMed
		else zHigh <- zMed
		print(paste("current volume:", vCurr, '%'), value=100)
		updateProgressBar(session, id="pb", title=paste("current volume:", vCurr, '%'), value=100)
	}
	return(zMed)
}

splitTri <- function(triangle){
	triangle <- triangle %>% arrange(desc(z))
	# edge to apply /4 instead /2 for z
	i <- dist(triangle[, 1:2]) %>% c %>% which.max
	nodes <- triangle %>% 
		bind_rows(triangle[2, ] + (triangle[1, ] - triangle[2, ])/2) %>% 
		bind_rows(triangle[3, ] + (triangle[1, ] - triangle[3, ])/2) %>% 
		bind_rows(triangle[3, ] + (triangle[2, ] - triangle[3, ])/2)
	nodes[3+i, 'z'] <- if(i == 1) triangle[2, 'z'] + (triangle[1, 'z'] - triangle[2, 'z'])/4 
		else if(i == 2) triangle[3, 'z'] + (triangle[1, 'z'] - triangle[3, 'z'])/4 
		else triangle[3, 'z'] + (triangle[2, 'z'] - triangle[3, 'z'])/4
	list(nodes[c(1, 4, 5), ], nodes[c(2, 4, 6), ], nodes[c(3, 5, 6), ], nodes[c(4, 5, 6), ])
}

drawTri <- function(triangles){
	p <- plot_ly()
	if(!is.null(triangles)){
		if(length(triangles) > 0){
			triangles <- reduce(triangles, function(a, b) a %>% 
				append(splitTri(b)), .init=list())
			faces <- reduce(triangles, bind_rows)
			p <- p %>% add_trace(data=faces, x=~x, y=~y, z=~z, i=seq(1, nrow(faces), by=3)-1,
				j=seq(2, nrow(faces), by=3)-1, k=seq(3, nrow(faces), by=3)-1)
			edges <- reduce(triangles, function(a, b) a %>% bind_rows(data.frame(x=NA, y=NA)) %>%
				bind_rows(b[c(1, 2), ]) %>% bind_rows(data.frame(x=NA, y=NA)) %>% 
				bind_rows(b[c(1, 3), ]) %>% bind_rows(data.frame(x=NA, y=NA)) %>% 
				bind_rows(b[c(2, 3), ]), .init=data.frame())
			p <- p %>% add_trace(mode='lines', color=I('black'), line=list(width=3), data=edges, x=~x, y=~y, z=~z)
		}
	}
	p %>% layout(showlegend=FALSE, scene=list(camera=list(eye=list(x=1.25, y=-1.25, z=1.25)), 
			zaxis=list(title="Intensity", rangemode='tozero'), 
			xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl))))
}

drawTriCut <- function(triangles, z){
	p <- drawTri(triangles)
	if(!is.null(z)) p %>% add_trace(data=data.frame(x=c(0,0,36,36), y=c(0,30,0,30), z=rep(z, times=4)),
		x=~x, y=~y, z=~z, i=c(0,3), j=c(1,1), k=c(2,2),opacity=.5, hoverinfo="text", text=
			paste("Intensity:", rep(z, times=4)))
	else p
}

contourPolyhedras <- function(triangles, zVal, centroid){
	if(is.null(triangles)) return(
		plot_ly(type="scatter", mode="lines") %>% 
		 layout(showlegend=FALSE, 
			xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl)))
	)
	if(is.null(zVal)) zVal <- 0
	# for each edge of triangle compute new coords
	trianglesCut <- list()
	for(tri in triangles){
		if(all(tri$z < zVal)) next
		else if(all(tri$z >= zVal)) trianglesCut <- append(trianglesCut, list(tri))
		else {
			if(nrow(tri[which(tri$z < zVal), ]) == 2){
				ptA <- tri[which(tri$z >= zVal), ]
				pts <- tri[which(tri$z < zVal), ]
				ptB <- pts[1, ]
				ptC <- pts[2, ]
				ptB2 <- newCoords(ptA, ptB, zVal)
				ptC2 <- newCoords(ptA, ptC, zVal)
				trianglesCut <- append(trianglesCut, list(ptA %>% bind_rows(ptB2) %>% bind_rows(ptC2)))
			} else{
				pts <- tri[which(tri$z >= zVal), ]
				ptA <- pts[1, ]
				ptB <- pts[2, ]
				ptC <- tri[which(tri$z < zVal), ]
				ptA2 <- newCoords(ptA, ptC, zVal)
				ptB2 <- newCoords(ptB, ptC, zVal)
				trianglesCut <- append(trianglesCut, list(ptA %>% bind_rows(ptA2) %>% bind_rows(ptB2),
					ptA %>% bind_rows(ptB) %>% bind_rows(ptB2)))
			}
		}
	}

	edges <- reduce(trianglesCut, function(a, b) append(a, list(
		b[1:2, ] %>% arrange(x, y), b[2:3, ] %>% arrange(x, y), 
		b[c(1,3), ] %>% arrange(x, y))), .init=list())
	edges <- keep(edges, function(edge) all(edge$z == zVal))
	edges2 <- map(edges, function(x) x[, 1:2] %>% bind_rows(data.frame(x=NA, y=NA)))
	edges2 <- reduce(edges2, bind_rows)
		
	p <- plot_ly(type="scatter", mode="lines", data=edges2, x=~x, y=~y, hoverinfo="text", 
		text=paste("Carbons:", edges2$x %>% round(digits=2), "<br />Chlorines:", edges2$y %>% round(digits=2)))
	if(!is.null(centroid)) p <- p %>% 
		add_trace(mode="markers", data=centroid, x=~x, y=~y, hoverinfo="text", 
			text=paste("Carbons:", centroid$x %>% round(digits=2), "<br />Chlorines:", centroid$y %>% round(digits=2))) %>%
		add_annotations(data=centroid, x=~x, y=~y, text="centroid", ax=20, ay=-40)
	p %>%
		 layout(showlegend=FALSE, 
			xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl))) 
}
