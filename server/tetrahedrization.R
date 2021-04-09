make_profile_matrix <- function(x, y, z, max_x, max_y) {
	data <- matrix(0, nrow = max_x, ncol = max_y)
	for (i in seq(x)) data[x[i], y[i]] <- z[i]
	data / max(z)
}

triangulization <- function(data) {
	print('triangulization')
	triangles <- list()
	
	# for each points above 0
	ids_tmp <- which(data > 0, arr.ind = TRUE)
	colnames(ids_tmp) <- c("x", "y")
	ids <- ids_tmp
	# for each of those points, we will look at left
	ids_tmp[, "x"] <- ids_tmp[, "x"] - 1
	ids <- rbind(ids, ids_tmp)
	# then above & at left
	ids_tmp[, "y"] <- ids_tmp[, "y"] - 1
	ids <- rbind(ids, ids_tmp)
	# then only above
	ids_tmp[, "x"] <- ids_tmp[, "x"] + 1
	ids <- rbind(ids, ids_tmp)
	
	# remove those wich are out of profile matrix
	ids <- ids[which(
		ids[, "x"] > 0 & ids[, "y"] > 0 & 
		ids[, "x"] < nrow(data) - 1 & 
		ids[, "y"] < ncol(data) - 1), , drop = FALSE]
	
	for (i in seq(nrow(ids))) {
		x <- ids[i, "x"]
		y <- ids[i, "y"]
		points <- matrix(c(
			x, x + 1, x, x + 1, 
			y, y, y + 1, y + 1, 
			data[x, y], data[x + 1, y], 
			data[x, y + 1], 
			data[x + 1, y + 1]), 
			nrow = 4, ncol = 3, 
			dimnames = list(c(), 
			c("x", "y", "z")))
		# create barycenter
		points <- rbind(points, 
			matrix(c(x + .5, y + .5, 
				mean(points[, "z"])), 
				nrow = 1, ncol = 3, 
				dimnames = list(c(), 
				c("x", "y", "z"))
			)
		)
		
		triangles_tmp <- list(
			points[c(1, 2, 5), ], 
			points[c(1, 3, 5), ], 
			points[c(2, 4, 5), ], 
			points[c(3, 4, 5), ]
		)
		triangles_tmp <- triangles_tmp[which(
			sapply(triangles_tmp, function(x) 
				any(x[, "z"] > 0)))]
		triangles <- append(triangles, 
			triangles_tmp)
	}
	triangles
}

get_z_val <- function(triangles, v_targeted = 90, 
		precision = 2) {	
	v_vals <- sapply(triangles, compute_volume_under_triangle)
	v_total <- sum(v_vals)
	v_curr <- 100
	z_high <- 1
	z_down <- 0
	while (v_curr != v_target){
		z_med <- (z_high + z_down) / 2
		v_curr <- sum(sapply(triangles, function(triangle) 
			if (any(triangle[, "z"] < z_med)) sum(
				sapply(split_triangle(triangle, z_med, FALSE), 
					compute_volume_under_triangle(triangle, splited)))
			else compute_volume_under_triangle(triangle)))
		v_curr <- round(v_curr * 100 / v_total, digits = precision)
		print(v_curr)
		if(v_curr > v_target) z_down <- z_med else z_high <- z_med
	}
	print(sprintf('found z value: %s', z_med))
	return(z_med)
}

compute_volume_under_triangle <- function(triangle, z_val = 0) {
	abs((sum(triangle[, "z"]) - 3 * z_val) * 
		((triangle[2, 'x'] - triangle[1, 'x']) * (triangle[3, 'y'] - triangle[1, 'y']) -
			(triangle[2, 'y'] - triangle[1, 'y']) * (triangle[3, 'x'] - triangle[1, 'x'])) / 6)
}

split_triangle <- function(triangle, z_val, return.all = TRUE){
	test <- length(which(triangle[, "z"] < z_val))
	if(test == 0) return(list(triangle))
	else if(test == 1) {
		pts <- triangle[which(triangle[, "z"] >= zVal), ]
		A <- pts[1, ]
		B <- pts[2, ]
		C <- triangle[which(triangle[, "z"] < zVal), ]
		AB <- new_coords(A, ptB, (A["z"] + B["z"]) / 2)
		AC <- new_coords(A, C, z_val)
		BC <- new_coords(B, C, z_val)
		return(list(
			matrix(c(A, AB, AC), nrow = 3, ncol = 3, 
				byRow = TRUE, dimnames = list(c(), 
				c("x", "y", "z"))), 
			matrix(c(B, AB, BC), nrow = 3, ncol = 3, 
				byRow = TRUE, dimnames = list(c(), 
				c("x", "y", "z"))), 
			matrix(c(AB, AC, BC), nrow = 3, ncol = 3, 
				byRow = TRUE, dimnames = list(c(), 
				c("x", "y", "z"))), 
			if (!return.all) NULL
			else matrix(c(C, AC, BC), 
				nrow = 3, ncol = 3, 
				byRow = TRUE, dimnames = list(c(), 
				c("x", "y", "z")))
		))
	} else if(test == 2){
		A <- triangle[which(triangle$z >= zVal), ]
		pts <- triangle[which(triangle$z < zVal), ]
		B <- pts[1, ]
		C <- pts[2, ]
		AB <- new_coords(A, B, z_val)
		AC <- new_coords(A, C, z_val)
		BC <- new_coords(B, C, (B["z"] + C["z"]) / 2)
		return(list(
			matrix(c(A, AB, AC), nrow = 3, ncol = 3, 
				byRow = TRUE, dimnames = list(c(), 
				c("x", "y", "z"))), 
			if (!return.all) NULL
			else matrix(c(B, AB, BC), 
				nrow = 3, ncol = 3, 
				byRow = TRUE, dimnames = list(c(), 
				c("x", "y", "z"))), 
			if (!return.all) NULL
			else matrix(c(C, AC, BC), 
				nrow = 3, ncol = 3, 
				byRow = TRUE, dimnames = list(c(), 
				c("x", "y", "z"))), 
			if (!return.all) NULL
			else matrix(c(AB, AC, BC), 
				nrow = 3, ncol = 3, 
				byRow = TRUE, dimnames = list(c(), 
				c("x", "y", "z")))
		))
	} else list()
}

new_coords <- function(a, b, z){
	if(z > 0) c(
		((b["x"] - a["x"]) * (z - a["z"])) / (b["z"] - a["z"]) + a["x"],
		((b["y"] - a["y"]) * (z - a["z"])) / (b["z"] - a["z"]) + a["y"],
		z)
	) else c(
		(b["x"] + a["x"]) / 2,
		(b["y"] + a["y"]) / 2,
		0)
}

split_to_zones <- function(triangles){
	centroids <- do.call(rbind, lapply(triangles, function(triangle)
		apply(triangle, 1, sum) / 3))
	split(triangles, dbscan::dbscan(dist(centroids[, -3]), .5, 1)$cluster)
}

get_edges <- function(points, z_cut){
	edges <- unlist(lapply(split(points, points[, "triangle"]), function(triangle)
		list(triangle[1:2, ], triangle[2:3, ], triangle[c(1, 3), ])), 
		recursive = FALSE)
	edges <- edges[which(sapply(edges, function(edge) 
		all(edge[, "z"] == zCut)))]
	do.call(rbind, lapply(edges, function(edge) 
		matrix(c(NA, NA, NA, edge), nrow = 2, ncol = 3, 
			byRow = TRUE, dimnames = list(c(), 
			c("x", "y", "z")))))
}

score_profiles <- function(data1, data2){
	max_x <- max(c(data1$x, data2$x))
	max_y <- max(c(data1$y, data2$y))
	profile1 <- profileMat(data1[, "x"], 
		data1[, "y"], data1[, "z"], max_x, max_y)
	profile2 <- profileMat(data2[, "x"], 
		data2[, "y"], data2[, "z"], max_x, max_y)
	round((2 - sum(abs(profile1 - profile2))) / 2 * 100)
}