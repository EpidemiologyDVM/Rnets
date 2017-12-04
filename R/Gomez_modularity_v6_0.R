#' Robust Estimator of modularity (Gomez, et al 2009)
#'
#' Newman's method of estimating graphical modularity based on vertex can accomodate edge weights, but cannot incorporate signed edges, e.g. edges with both positive and negative. Gomez, et al, proposed a similar estimator of modularity estimated in two parts corresponding to positive (Q+) and negative (Q-) edges, and the latter is subtracted from the former. The 'signedModularity' function implements this method of modularity estimation, and returns a scalar.
#' @param x A graph presented in of the forms discussed below.
#' @param membership Defines vertex membership to determine if vertices are similar. May be provided as a string that matches an attribute of x or a vector of length equal to the number of vertices in the graph.
#' @param weight Edge weights. Like 'membership', this argument can be defined as a string matching an edge attribute of 'x' or a vector of length equal to the number of edges, but may also be left as NULL which will return an unweighted modularity estimate.
#' @description For flexibility, x may be provided as any of the following formats: an edgelist (data.frame), a weighted adjacency matrix (square numeric matrix), an igraph object, or an rnet.* object (e.g., rnetBasic, rnetMultiStrata, etc.).
#' @return a numeric value estimating the weighted, signed modularity of x, or a numeric vector containing respective modularity estimates if x contained multiple network.
#' @import igraph
#' @importFrom stats aggregate
#' @export
#' @examples 
#' 
#' #Signed modularity in a random graph with 10 vertices
#' 
#' x <- sample_gnp(5, 0.4)  #Creates a random graph with 10 vertices and density ~ 40%
#' x <- set_edge_attr(x, 'weight', value = runif(gsize(x), -0.5, 0.5))  #Randomly assign edge weights to edge attribute 'weight', both positive and negative
#' x <- set_vertex_attr(x, name = 'group', value = sample(c('red', 'blue'), size = 5, replace = TRUE))
#' signedModularity(x, membership = 'group', weight = 'weight')
#' signedModularity(x, membership = 'group')
#' 
#' 
#' @rdname signedModularity

signedModularity <- function(x, membership, weight = NULL){
  UseMethod("signedModularity", x)
}

#' @rdname signedModularity.matrix
signedModularity.matrix <- function(x, membership, weight = NULL) {
		if(!is.null(dimnames(x))) {
      if(!all(dimnames(x)[[1]] == dimnames(x)[[2]])) stop('Adjacency matrix row & columnn names must be symmetric.')
		  v.set <- dimnames(x)[[1]]
		} else v.set <- 1:dim(x)[[1]]
	  membership_attr <- 'Undeclared'
		if(length(membership) == 1) {
		  if(membership%in%names(attributes(x))) {
			  membership_attr <- membership
			  membership <- attr(x, membership) 
		  } else stop(membership, ' is not a valid attribute of x.') 
		} else if(length(membership)!=length(v.set)) stop("Length of 'membership' vector must match dimensions of 'x'.")

	  Q <- .sign.Q.internal(x, membership)
    
		attr(Q, 'weight') <- weight
		attr(Q, 'membership') <- membership_attr
		return(Q)
}


#' @rdname signedModularity.igraph
signedModularity.igraph <- function(x, membership, weight = NULL) {

		if(length(membership) == 1) if(membership%in%igraph::vertex_attr_names(x)) {
			membership_attr <- membership
			membership <- vertex_attr(x, membership) 
		} else stop(membership, 'is not a valid attribute of x.') 

		if(length(membership)!=length(V(x))) stop("Length of 'membership' vector must match dimensions of 'x'.")

		x.igraph <- as_adjacency_matrix(x, attr = weight, sparse = FALSE)
	
		Q <- .sign.Q.internal(x, membership)
		attr(Q, 'weight') <- weight
		attr(Q, 'membership') <- membership_attr
		return(Q)
}


#' @rdname signedModularity.rnetBasic
signedModularity.rnetBasic <- function(x, membership = NULL, weight = 'omega') signedModularity(x@R, membership, weight)


#' @rdname signedModularity.rnetBasic
signedModularity.rnetMultiStrata <- function(x, membership, weight = 'omega') {
		Q <- sapply(x@R_Strata, signedModularity, membership, weight)
		return(Q)
}


.sign.Q.internal <- function(x, membership) {
  w_pos <- apply(x, c(1, 2), max, 0)
  w_neg <- apply(x, c(1, 2), min, 0)
  
  Q_pos <- 0
  Q_neg <- 0
  
  for(i in 1:nrow(x)) {
    for(j in 1:ncol(x)) {
      if(membership[i] == membership[j] & sum(w_pos)!= 0) Q_pos <- Q_pos + (w_pos[i,j]-sum(w_pos[i,])*sum(w_pos[,j])/sum(w_pos))/sum(w_pos)
      if(membership[i] == membership[j] & sum(w_neg)!= 0) Q_neg <- Q_neg + (w_neg[i,j]-sum(w_neg[i,])*sum(w_neg[,j])/sum(w_neg))/sum(w_neg) 
    }
  }

  return(sum(w_pos)*Q_pos/(sum(w_pos) + sum(w_neg)) - sum(w_neg)*Q_neg/(sum(w_pos) + sum(w_neg)))
  
}

#.sign.Q.internal <- function(x) {
#	x$K_delta <- x$Attr_i == x$Attr_j
#	x$w_ij_pos <- sapply(x$w_ij, max, 0)
#	x$w_ij_neg <- sapply(-x$w_ij, max, 0)
#
#	w <- data.frame(i = c(x$i, x$j), w_ij_pos = x$w_ij_pos, w_ij_neg = x$w_ij_neg)
#	w_pos <- stats::aggregate(w_ij_pos ~ i, data = w, FUN = sum)
#	w_neg <- stats::aggregate(w_ij_neg ~ i, data = w, FUN = sum)
#
#	x$w_i_pos <- w_pos[match(x$i, w_pos$i),2]
#	x$w_j_pos <- w_pos[match(x$j, w_pos$i),2]
#	W_pos <- sum(w_pos$w_ij_pos)/2
#	Q_pos <- if(W_pos ==0) 0 else sum((x$w_ij_pos - (x$w_i_pos * x$w_j_pos)/(2*W_pos))*x$K_delta)/(2*W_pos)
#
#	x$w_i_neg <- w_neg[match(x$i, w_neg$i),2]
#	x$w_j_neg <- w_neg[match(x$j, w_neg$i),2]
#	W_neg <- sum(w_neg$w_ij_neg)/2
#	Q_neg <- if(W_neg ==0) 0 else sum((x$w_ij_neg - (x$w_i_neg * x$w_j_neg)/(2*W_neg))*x$K_delta)/(2*W_neg)
#
#	return(Q_pos * 2*W_pos /(2*W_pos + 2*W_neg) -  Q_neg * 2*W_neg /(2*W_pos + 2*W_neg))	
#}
