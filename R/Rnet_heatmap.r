#'Rnet_Heatmap - Function to generate a bitmap figure to represent edges in similar networks over time.
#' 
#'bitmap can be used to show how edges in a network change over time or other criteria. This function takes an object of the 'rnetStrata' class, specifically @E_aggr, and returns a numerical matrix used to visualize E_matrix. The numerical matrix can be visualized with a call to the 'image' function; The assinged colors are stored in the 'palette' attribute attached to the matrix. This matrix can then be used to plot the bitmap in which each horizontal row in the bitmap represents a unique edge found in the set of networks in rnet.multi.strata object and each vertical column is a network stratum (see the 'Rnet' method for more information). The colors represent the binned penalized partial correlation estimates in the E_aggr.
#'
#' @param x An object of class 'rnetStrata'
#' @param e_cuts A vector of numeric values used to cut the edge attribute values in x@E_aggr. Note binning of negative values is based on absolute value, making the categories symmetric around 0. 
#' @param pos_col A vector of colors corresponding to the binned positive x@E_aggr values. Defaults to 4 shades of red.
#' @param neg_col A vector of colors corresponding to the binned negative x@E_aggr values. Defaults to 4 shades of green.
#' @param zero_col A single value for coloring edges with value = 0 (typically corresponds to an absent edge).
#' @param NA_col A single value for color invalid edges. Edges will typically be found to be in valid if one or both incident vertices were missing in some strata or insufficient observations were available to estimate an edge in a stratum (see the 'n_threshold' argument in the method 'Rnet').
#' The length of pos_col and neg_col must be the same and must be of length 1 greater than e_cuts.
#' @export
#' @return A matrix of integers corresponding to positions in the containing the colors to plot on the bitmap, with an additional attribute 'palette' containing the colors to .
#' @section This function does *not* plot the heatmap. The matrix produced by Rnet_Heatmap should be called by 
#' @examples
#' #Example using EC_Rnets_byYear
#' EC_Rnets_byYear <- Rnet(x = NARMS_EC_DATA, 
#' 						L1 = 0.25, 
#' 						vert = c('AMP', 'AMC', 'AXO', 'TIO', 'NAL', 
#' 						  'CIP', 'STR', 'GEN', 'COT', 'FIS'), 
#' 						n_min = 20,
#'						subset = 'Year'
#' 						)
#'
#' EC_Heatmap <- Rnet_Heatmap(EC_Rnets_byYear, e_cuts = c(0, 0.05, 0.1, 0.2, 1))
#'
#'par(mar = c(4, 5, 1, 1)+0.1)
#'image(EC_Heatmap, col = attr(EC_Heatmap, 'palette'),
#'	axes = FALSE)

#'axis(1, 
#'	at = seq(0, 1, 1/(dim(EC_Heatmap)[1]-1)),
#'	labels = rownames(EC_Heatmap),
#'	tck = -0.02
#'	)
#'
#'axis(2,
#'	at = seq(0, 1, 1/(dim(EC_Heatmap)[2]-1)),
#'	labels = colnames(EC_Heatmap),
#'	tck = -0.015,
#'	las = 2,
#'	cex.axis = 5/6
#'	)


Rnet_Heatmap <- function(x,
			e_cuts,
			pos_col = c('#FFBBBB', '#FF8888', '#FF4444', '#FF0000'),
			neg_col = c('#BBFFBB', '#88FF88', '#44FF44', '#00FF00'),
			zero_col = '#FFFFFF',
			NA_col = '#CCCCCC'
			)
{	
	if(length(pos_col)!= length(e_cuts)-1 | length(neg_col)!= length(e_cuts)-1) stop("vectors pos_col and neg_col must be 1 shorter than e_cuts")
	names(pos_col) <- paste('pos', 1:(length(e_cuts)-1), sep = '.')
	names(neg_col) <- paste('neg', 1:(length(e_cuts)-1), sep = '.')
	names(zero_col) <- 'zero'
	names(NA_col) <- 'NA'
	palette_set <- c(pos_col, neg_col, zero_col, NA_col)

	edge_list <- reshape(as.data.frame(x@E_aggr),
		direction = 'l',
		idvar = 'Edge',
		ids = rownames(x@E_aggr),
		varying = list(colnames(x@E_aggr)),
		v.names = 'Edge_val',
		times = colnames(x@E_aggr),
		timevar = 'Stratum'
		)

	edge_list$Stratum <- gsub(paste(slot(x, 'stratify_by'), '.', sep = ''), '', edge_list$Stratum)
	edge_list$Palette_code <- unlist(sapply(edge_list$Edge_val, function (x) {
		if(is.na(x)) return ('NA')
		switch(
			sign(x) + 2,
			paste('neg', as.integer(cut(abs(x), e_cuts)), sep = '.'),	
			'zero',
			paste('pos', as.integer(cut(x, e_cuts)), sep = '.'),
			'NA'
			)
		}))

	edge_list$Palette_num <- match(edge_list$Palette_code, names(palette_set))
	color_frame <- reshape(edge_list,
		idvar = 'Edge',
		timevar = 'Stratum',
		drop = c('Edge_val', 'Palette_code'),
		direction =  'w'
		)

	color_mat <- t(as.matrix(color_frame[, -1]))
	dimnames(color_mat) <- list(
		gsub("Palette_num.", '', rownames(color_mat)),
		color_frame$Edge
		)
	attr(color_mat, 'palette') <- palette_set
	
	return(color_mat)
}