distMatrix <- function(data, newPoints=TRUE){
	data2 <- matrix(0, nrow=maxC, ncol=maxCl)
	for(row in 1:nrow(data)) data2[data[row,1], data[row, 2]] <- data[row, 3]

	# create new points
	if(newPoints){
		data2 <- cbind(do.call(cbind, lapply(2:ncol(data2), function(x) 
			cbind(data2[, x-1], matrix((data2[, x-1] + data2[, x]) / 2, ncol=1)))), data2[, ncol(data2)])
		data2 <- rbind(do.call(rbind, lapply(2:nrow(data2), function(x) 
			rbind(data2[x-1, ], matrix((data2[x-1, ] + data2[x, ]) / 2, nrow=1)))), data2[nrow(data2), ])
	}

	data2
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

tetrahedrization <- function(data){
	tetras <- list()
	triangles <- list()
	pbVal <- 0
	pbMax <- length(seq(2, nrow(data), 2))*length(seq(2, ncol(data), 2))
	
	for(x in seq(2, nrow(data), 2)){
	for(y in seq(2, ncol(data), 2)){
		
		tetrasTmp <- list()
		trianglesTmp <- list()
		# trace all tetras faces
		if(x > 1 & y < ncol(data)){
			tetrasTmp <- append(tetrasTmp, list(
				data.frame(x=c(x, x-1, x-1, x), y=c(y, y, y+1, y), 
					z=c(data[x, y], data[x-1, y], data[x-1, y+1], 0)),
				data.frame(x=c(x, x-1, x, x), y=c(y, y+1, y+1, y), 
					z=c(data[x, y], data[x-1, y+1], data[x, y+1], 0))
				))
			trianglesTmp <- append(trianglesTmp, list(data.frame(x=c(x, x-1, x-1), y=c(y, y, y+1), 
					z=c(data[x, y], data[x-1, y], data[x-1, y+1])),
				data.frame(x=c(x, x-1, x), y=c(y, y+1, y+1), 
					z=c(data[x, y], data[x-1, y+1], data[x, y+1]))
				))
		}
		if(x < nrow(data) & y < ncol(data)){
			tetrasTmp <- append(tetrasTmp, list(
				data.frame(x=c(x, x, x+1, x), y=c(y, y+1, y+1, y), 
					z=c(data[x, y], data[x, y+1], data[x+1, y+1], 0)),
				data.frame(x=c(x, x+1, x+1, x), y=c(y, y+1, y, y), 
					z=c(data[x, y], data[x+1, y+1], data[x+1, y], 0))
				))
			trianglesTmp <- append(trianglesTmp, list(
				data.frame(x=c(x, x, x+1), y=c(y, y+1, y+1), 
					z=c(data[x, y], data[x, y+1], data[x+1, y+1])),
				data.frame(x=c(x, x+1, x+1), y=c(y, y+1, y), 
					z=c(data[x, y], data[x+1, y+1], data[x+1, y]))
				))
		}
		if(x < nrow(data) & y > 1){
			tetrasTmp <- append(tetrasTmp, list(
				data.frame(x=c(x, x+1, x+1, x), y=c(y, y, y-1, y), 
					z=c(data[x, y], data[x+1, y], data[x+1, y-1], 0)),
				data.frame(x=c(x, x+1, x, x), y=c(y, y-1, y-1, y), 
					z=c(data[x, y], data[x+1, y-1], data[x, y-1], 0))
				))
			trianglesTmp <- append(trianglesTmp, list(
				data.frame(x=c(x, x+1, x+1), y=c(y, y, y-1), 
					z=c(data[x, y], data[x+1, y], data[x+1, y-1])),
				data.frame(x=c(x, x+1, x), y=c(y, y-1, y-1), 
					z=c(data[x, y], data[x+1, y-1], data[x, y-1]))
				))
		}
		if(x > 1 & y > 1){
			tetrasTmp <- append(tetrasTmp, list(
				data.frame(x=c(x, x, x-1, x), y=c(y, y-1, y-1, y), 
					z=c(data[x, y], data[x, y-1], data[x-1, y-1], 0)),
				data.frame(x=c(x, x-1, x-1, x), y=c(y, y-1, y, y), 
					z=c(data[x, y], data[x-1, y-1], data[x-1, y], 0))
			))
			trianglesTmp <- append(trianglesTmp, list(
				data.frame(x=c(x, x, x-1), y=c(y, y-1, y-1), 
					z=c(data[x, y], data[x, y-1], data[x-1, y-1])),
				data.frame(x=c(x, x-1, x-1), y=c(y, y-1, y), 
					z=c(data[x, y], data[x-1, y-1], data[x-1, y]))
			))
		}
		
		tetrasTmp2 <- modify_if(tetrasTmp, function(x) any(x$z > 0), function(x) rbind(x[-1, ], data.frame(x=x[2, 'x'], y=x[2, 'y'], z=0)))
		tetrasTmp3 <- modify_if(tetrasTmp2, function(x) any(x$z > 0), function(x) rbind(x[-1, ], data.frame(x=x[2, 'x'], y=x[2, 'y'], z=0)))
		
		tetrasTmp <- append(tetrasTmp, tetrasTmp2)
		tetrasTmp <- append(tetrasTmp, tetrasTmp3)
		
		tetrasTmp <- keep(tetrasTmp, function(x) any(x$z > 0))
		tetrasTmp <- modify(tetrasTmp, function(a) a %>% mutate_at(c('x', 'y'), function(.) ./2))
		trianglesTmp <- keep(trianglesTmp, function(x) any(x$z > 0))
		trianglesTmp <- modify(trianglesTmp, function(a) a %>% mutate_at(c('x', 'y'), function(.) ./2))
		
		tetras <- append(tetras, tetrasTmp)
		triangles <- append(triangles, trianglesTmp)
		
		pbVal <- pbVal + 1
		updateProgressBar(session, id="pb", title="Tetrahedrization", value=round(pbVal * 100 / pbMax, digits=2))
		
	}}
	return(list(tetras=tetras, triangles=triangles))
}

drawTri <- function(triangles){
	p <- plot_ly()
	if(!is.null(triangles)){
	if(length(triangles) > 0){
		faces <- reduce(triangles, bind_rows)
		p <- p %>% add_trace(data=faces, x=~x, y=~y, z=~z, i=seq(1, nrow(faces), by=3)-1,
			j=seq(2, nrow(faces), by=3)-1, k=seq(3, nrow(faces), by=3)-1)
		edges <- reduce(triangles, function(a, b) a %>% bind_rows(data.frame(x=NA, y=NA)) %>%
			bind_rows(b[c(1, 2), ]) %>% bind_rows(data.frame(x=NA, y=NA)) %>% 
			bind_rows(b[c(1, 3), ]) %>% bind_rows(data.frame(x=NA, y=NA)) %>% 
			bind_rows(b[c(2, 3), ]), .init=data.frame())
		p <- p %>% add_trace(mode='lines', color=I('black'), line=list(width=5), data=edges, x=~x, y=~y, z=~z)
	}}
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

newCoords <- function(a, b, z){
	data.frame(
		x = ((b$x - a$x) * (z - a$z)) / (b$z - a$z) + a$x,
		y = ((b$y - a$y) * (z - a$z)) / (b$z - a$z) + a$y,
		z = z
	)
}

splitTetra <- function(tetra, z){
	tetra <- tetra[order(tetra$z, decreasing=TRUE), ]
	nodesAboveZ <- tetra[which(tetra$z >= z), ]
	nodesBelowZ <- tetra[which(tetra$z < z), ]
	if(nrow(nodesAboveZ) == 4) return(list(tetra))
	else if(nrow(nodesAboveZ) == 3){
		nodes <- nodesAboveZ %>% bind_rows(reduce(1:3, function(a, b) 
			a %>% bind_rows(newCoords(nodesAboveZ[b, ], nodesBelowZ, z)), .init=data.frame()))
		return(list(nodes[c(1,2,3,6), ], nodes[c(1,2,6,5), ], nodes[c(1,5,6,4), ]))
	} else if(nrow(nodesAboveZ) == 2){
		nodes <- nodesAboveZ %>% bind_rows(reduce(1:2, function(a, b) 
			a %>% bind_rows(newCoords(nodesAboveZ[b, ], nodesBelowZ[1, ], z)) %>%
				bind_rows(newCoords(nodesAboveZ[b, ], nodesBelowZ[2, ], z)), .init=data.frame()))
		return(list(nodes[c(1, 2, 3, 4), ], nodes[c(2, 3, 4, 5), ], nodes[c(2, 4, 5, 6), ]))
	} else if(nrow(nodesAboveZ) == 1){
		return(list(nodesAboveZ %>% bind_rows(reduce(1:3, function(a, b) 
			a %>% bind_rows(newCoords(nodesAboveZ, nodesBelowZ[b, ], z)), .init=data.frame()))))
	} else list()
}

getZCut <- function(tetras, vTarget=90, digits=2){
	vT <- computeVolumePolyhedra(tetras)
	vTarget <- round(vT * vTarget / 100, digits=digits)
	vCurr <- 0
	zVal <- reduce(tetras, function(a, b) max(c(a, b$z)), .init=0)
	zInc <- zVal
	while(vCurr != vTarget){
		zInc <- zInc / 2
		if(vCurr > vTarget) zVal <- zVal + zInc
		else zVal <- zVal - zInc
		# tetrasTmp <- reduce(lapply(tetras, function(tetra) splitTetra(tetra, zVal)), append, .init=list())
		vCurr <- round(reduce(tetras, function(a, b) 
			a + computeVolumePolyhedra(splitTetra(b, zVal)), .init=0), digits=digits)
		updateProgressBar(session, id="pb", title=paste("current volume:", round(vCurr * 100 / vT, digits=digits), '%'), value=100)
	}
	return(zVal)
}

contourPolyhedras <- function(triangles, zVal, centroid){
	if(is.null(triangles) | is.null(zVal)) return(
		plot_ly(type="scatter", mode="lines") %>% 
		 layout(showlegend=FALSE, 
			xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl)))
	)
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
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl))) %>%
		config(edits=list(annotationTail=TRUE))
}
