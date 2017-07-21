#'Summary - rnet.basic.
#'
#'Gives more information than 'print'.
#' @param object An rnet object
#' @rdname summary
#' @export

setMethod(f = 'summary',
	signature(object = 'rnet.basic'),
	function(object) {
		cat(	'\nBasic R-net',
			'\n',
			'\n Sample:', dim(object@Data)[1], 'isolates,', length(object@V_set), 'vertices ')
		if(length(object@V_omitted)==1) cat('(', length(object@V_omitted), ' vertex omitted)', sep = '') 		
		if(length(object@V_omitted) > 1) cat('(', length(object@V_omitted), ' vertices omitted)', sep = '') 		
		cat(	'\n',
			'\n     L1:', object@L1,
			'\n  Edges:', ecount(object@R),
			'\nDensity:', round(100*edge_density(object@R), 1),'%',
			'\n')
		print(object)
		if(length(object@V_omitted)>0) cat('\nOmitted Vertices:', paste(object@V_omitted, collapse = ', '), '\n')
		cat('\n')
	})	

setMethod(f = 'summary',
	signature(object = 'rnet.strata'),
	function(object) {
		cat(  '\nStratfied R-net (single level)',
			'\n',
			'\n Sample:', dim(object@Data)[1], 'isolates,', length(object@V_set), 'vertices ')
		if(length(object@V_omitted)==1) cat('(', length(object@V_omitted), ' vertex omitted)', sep = '') 		
		if(length(object@V_omitted) > 1) cat('(', length(object@V_omitted), ' vertices omitted)', sep = '') 		
		cat(	'\n',
			'\n     L1:', object@L1,
			'\n  Edges:', ecount(object@R),
			'\nDensity:', round(100*edge_density(object@R), 1),'%',
			'\n')
		print(object)
		if(length(object@V_omitted)>0) cat('\nOmitted Vertices:', paste(object@V_omitted, collapse = ', '), '\n')
		cat('\n')
	})
	
#'Print methods of rnet objects.
#'
#'print methods for classes 'rnet.basic', 'rnet.strata', 'rnet.multi.strata'.
#' @param x An rnet object
#' @export
setMethod(f = 'print',
	signature(x = 'rnet.basic'),
	function(x) {
		cat(  '\nVertex set:',	paste(x@V_set, collapse = ' '),
			'\n',
			'\n  Edge Set:',
			'\n')
		edge.frame <- data.frame(V1 = as_edgelist(x@R)[,1],
					V2 = as_edgelist(x@R)[,2],
					Omega = round(E(x@R)$omega, 3)	
					)
		print(edge.frame)
	})

setMethod(f = 'print',
	signature(x = 'rnet.strata'),
	function(x) {
		cat(	'\nRnet conditioned on', as.character(x@Strata_def), '\n')
		print(as(x, 'rnet.basic'))
	})

setMethod(f = 'print',
	signature(x = 'rnet.strata.multi'),
	function(x) {
		cat(	"\nRnet stratified by ", x@Stratify_by, " (", length(x@R_Strata), " Strata)\n", sep = '')
		cat(  '\nVertex set:',	paste(unique(unlist(lapply(x@R_Strata, function(x) x@V_set ))), collapse = ' '), "\n")
		cat(  '\nEdge set:\n')
		print(.Assemble_Edge_Matrix(x, 'omega'))
		
	})
	#'Plot methods for R-nets
#'
#' A plot method for R-nets, and incorporates vertex and edge metadata and layout, if assigns. Only vertex and edge metadata with names that match igraph decoration options (without 'vertex.' or 'edge.' appended to the attribute name; see plot.igraph). Layout is pulled from 'Layout_master' in the rnet object, if it exists. The layout frame can contain 3 columns, with the first column used to match the coordinates in the next two columns to graph vertices OR can contain 2 columns with the same number of vertices in the graph.
#' @param x an rnet object (currently, objects of class 'rnet.basic', 'rnet.strata', 'rnet.multi.strata')
#' @export

setMethod('plot',
	signature(x = 'rnet.basic'),
	function (x) {
		VERT.PARAMS <- c('size','size2','color','frame.color','shape','label','label.family','label.font','label.cex','label.dist','label.degree','label.color')
		EDGE.PARAMS <- c('color','width','lty','label','label.family','label.font','label.cex','label.color','label.x','label.y','curved')

		vert.attrib.lines <- character(0)
		vert.attribs <- intersect(x@V_metadata, VERT.PARAMS)
		if(length(vert.attribs) > 0) for(attrib.name in vert.attribs) vert.attrib.lines <- c(vert.attrib.lines, paste('vertex.', attrib.name, '= V(x@R)$', attrib.name, sep = ''))

		edge.attrib.lines <- character(0)
		edge.attribs <- intersect(x@E_metadata, EDGE.PARAMS)
		if(length(edge.attribs) > 0) for(attrib.name in edge.attribs) edge.attrib.lines <- c(edge.attrib.lines, paste('edge.', attrib.name, '= E(x@R)$', attrib.name, sep = ''))

		x@Layout <- .Assign_Layout_Matrix(x)
		attrib.lines <- c(vert.attrib.lines, edge.attrib.lines)

		if(length(attrib.lines)==0) plot.call <- 'plot.igraph(x@R, layout = x@Layout)' else plot.call <- paste('plot.igraph(x@R', paste(vert.attrib.lines, collapse = ','), 'layout = x@Layout)', sep = ',')
		
		eval(parse(text = plot.call))
		plot.call <- gsub('x', as.list(sys.call(1))[[2]], plot.call)
		return(plot.call)
	})

setMethod('plot',
	signature(x = 'rnet.strata'),
	function(x) {
		VERT.PARAMS <- c('size','size2','color','frame.color','shape','label','label.family','label.font','label.cex','label.dist','label.degree','label.color')
		EDGE.PARAMS <- c('color','width','lty','label','label.family','label.font','label.cex','label.color','label.x','label.y','curved')

		vert.attrib.lines <- character(0)
		vert.attribs <- intersect(x@V_metadata, VERT.PARAMS)
		if(length(vert.attribs) > 0) for(attrib.name in vert.attribs) vert.attrib.lines <- c(vert.attrib.lines, paste('vertex.', attrib.name, '= V(x@R)$', attrib.name, sep = ''))

		edge.attrib.lines <- character(0)
		edge.attribs <- intersect(x@E_metadata, EDGE.PARAMS)
		if(length(edge.attribs) > 0) for(attrib.name in edge.attribs) edge.attrib.lines <- c(edge.attrib.lines, paste('edge.', attrib.name, '= E(x@R)$', attrib.name, sep = ''))

		x@Layout <- .Assign_Layout_Matrix(x)
		attrib.lines <- c(vert.attrib.lines, edge.attrib.lines)
		
		if(length(attrib.lines)==0) plot.call <- 'plot.igraph(x@R, layout = x@Layout)' else plot.call <- paste('plot.igraph(x@R', paste(vert.attrib.lines, collapse = ','), 'layout = x@Layout)', sep = ',')
		
		eval(parse(text = plot.call))
		plot.call <- gsub('x', as.list(sys.call(1))[[2]], plot.call)
		return(plot.call)
	})

.Assign_Layout_Matrix <- function(x) {
	if(dim(x@Layout_master)[1] == 0) return(layout_with_fr(x@R))
	
	if(is.factor(x@Layout_master[[1]])) x@Layout_master[[1]] <- as.character(x@Layout_master[[1]])
	if(dim(x@Layout_master)[2] == 3) if (!all(is.character(x@Layout_master[,1]), is.numeric(x@Layout_master[,2]), is.numeric(x@Layout_master[,3]))) stop('Layout_master not valid: First column must be of type "character" and second and third columns must be of type "numeric".')
	if(dim(x@Layout_master)[2] == 3 & !all(x@V_set%in%x@Layout_master[,1])) stop('Layout_master invalid: Vertex names are missing from the first column.')
	if(dim(x@Layout_master)[2] == 2 & !is.numeric(x@Layout_master)) stop('Layout_master not valid: Two column layout matrix must be numeric')
	if(dim(x@Layout_master)[2] == 2 & length(x@V_set)!= dim(x@Layout_master)[1]) stop('Layout_master not valid: Two column layout matrix must match vertex length, or provide a matching column')
	
	if(dim(x@Layout_master)[2] == 2) return(as.matrix(x@Layout_master))
	
	seq.vec <- match(V(x@R)$name,x@Layout_master[[1]])
	if(dim(x@Layout_master)[2] == 3) return(as.matrix(x@Layout_master[seq.vec, 2:3]))
	stop('Layout_master invalid')
	}