CustomKMeans <- function(data, distance_function, average, k=2, l=1, threshold=0.70, iterate=100) {
	df <- match.fun(distance_function)
	centroids <- AssignRandomCentroids(data, k)
	i <- 0
	f <- 0
	while(i < iterate) {
		C_b <- ComputeBlocks(data, centroids, df, k, l)
		C_v1 <- AssignNewCentroids(C_b, centroids, average, k)
		d_c <- FindCentroidDifference(C_v1, centroids, distance_function, k, f, threshold)
		centroids <- C_v1
		if(d_c[1] == TRUE){
			break
		}else{
			f <- d_c[2]
		}
	}
	return(centroids)
}

AssignRandomCentroids <- function(data, k) {
	c <- list()
	for (j in 1:k){
	  random = data[sample(nrow(data), 1), ]
		if(!random %in% c){
			c <- append(c, list(random))
		} else {
			j = j-1
		}
	}
	return(c)
}

ComputeBlocks <- function(data, centroids, df, k, l) {
	B <- vector("list", k)
	for(i in 1:nrow(data)){
	  diffe <- list()
		least <- 1
		diff <- 0
		for(j in 1:k){
				diff <- df(data[i,], centroids[[j]])
				diffe[[length(diffe)+1]] <- list(diff, j)
		}
		differences <- sapply(diffe, "[[", 1)
		cen_index <- sapply(diffe, "[[", 2)
		cen_index <- cen_index[order(differences)]
		for(o in 1:l){
  		if(is.null(B[[cen_index[o]]])){
  			B[[cen_index[o]]] <- list()
  		}
  		B[[cen_index[o]]][[length(B[[cen_index[o]]])+1]] <- data[i,]
		}
	}
	return(B)
}

AssignNewCentroids <- function(C_b, centroids, average, k) {
	c_new <- vector("list", k)
	af <- match.fun(average)
	for (i in 1:k) {
		c_new[i] <- list(af(C_b[[i]], centroids[[i]]))
	}
	return(c_new)
}

FindCentroidDifference <- function(C_v1, centroids, distance_function, k, f, threshold) {
	df <- match.fun(distance_function)
	f1 <- 0
	for (i in 1:k) {
		for (j in 1:k) {
			f1 <- f+df(C_v1[[i]], centroids[[j]])
		}
	}
	diff <- f1-f
	thres <- diff/f1
	if(thres < threshold)
	  return(c(TRUE))
	else
	  return(c(FALSE, f1))
}