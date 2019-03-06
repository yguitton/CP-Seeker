distMatrix <- function(data){
	data2 <- matrix(0, nrow=maxC, ncol=maxCl)
	for(row in 1:nrow(data)) data2[data[row,1], data[row, 2]] <- data[row, 3]
	data2
}

triangulization <- function(data){
	triangles <- list()
	
	for(x in 1:(nrow(data)-1)){
	for(y in 1:(ncol(data)-1)){
		
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
	}}
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

drawTri <- function(triangles=NULL){
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

drawTriCut <- function(triangles=NULL, z=NULL){
	p <- drawTri(triangles)
	if(!is.null(z)) p %>% add_trace(data=data.frame(x=c(0,0,36,36), y=c(0,30,0,30), z=rep(z, times=4)),
		x=~x, y=~y, z=~z, i=c(0,3), j=c(1,1), k=c(2,2),opacity=.5, hoverinfo="text", text=
			paste("Intensity:", rep(z, times=4)))
	else p
}

contourPolyhedras <- function(datas=NULL, zVals=NULL){
	if(is.null(datas)) return(
		plot_ly(type="scatter", mode="lines") %>% 
		 layout(showlegend=FALSE, 
			xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl)))
	)
	if(is.null(zVals)) zVals <- rep(0, each=length(datas))
	p <- plot_ly(type="scatter", mode="lines")
	for(i in 1:length(datas)){
		triangles <- datas[[i]]
		zVal <- zVals[i]
		
		trianglesCut <- reduce(triangles, function(a, b) a %>% 
			append(cutTriangle(b, zVal)), .init=list())
		
		edges <- reduce(trianglesCut, function(a, b) a %>% 
			append(list(b[1:2, ], b[2:3, ], b[c(1, 3), ])),
			.init=list())
		edges <- keep(edges, function(edge) all(edge$z == zVal))
		edges <- reduce(edges, function(a, b) a %>% rbind(b) %>% 
			rbind(data.frame(x=NA, y=NA, z=NA)), .init=data.frame())
		
		p <- p %>% add_lines(data=edges, x=~x, y=~y, hoverinfo="text", 
			text=paste("Carbons:", edges$x %>% round(digits=2), 
			"<br />Chlorines:", edges$y %>% round(digits=2)))
	}
	p %>%
		layout(showlegend=FALSE, 
			xaxis=list(title="Number of Carbon", range=list(0, maxC)), 
			yaxis=list(title="Number of Chlorine", range=list(0, maxCl))) %>% 
		plotly::config(scrollZoom=TRUE, displaylogo=FALSE, modeBarButtons=list(
			list('zoom2d', 'pan2d', 'autoScale2d', 'resetScale2d')))
}
