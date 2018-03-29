#'Summary - rnetBasic
#'
#'Gives more information than 'print'.
#' @param object an rnet object of class 'rnetBasic'
#' @param ... Additional arguments passed to 'summary' method
#' @rdname summary-rnetBasic
#' @importFrom stringr str_pad
#' @aliases summary
#' @export

setMethod(f = 'summary',
          signature(object = 'rnetBasic'),
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



#'Summary - rnetMultiStrata
#'
#'Gives more information than 'print'.
#' @param object an rnet object of class 'rnetMultiStrata'
#' @rdname summary-rnetMultiStrata

setMethod(f = 'summary',
          signature(object = 'rnetMultiStrata'),
          function(object) {
           Edges <- ifelse(object@E_matrix == 0, '', stringr::str_pad(abs(object@E_matrix), width = 5, side = 'right', pad = '0'))
           Edges <- ifelse(object@E_matrix < 0, 
                           stringr::str_pad(Edges, width = 6, side = 'left', pad = '-'),
                           stringr::str_pad(Edges, width = 6, side = 'left', pad = ' ')
                            )
            colnames(Edges) <- paste(' ', gsub(paste(object@Stratify_by, '.', sep = ''), '', colnames(Edges)), sep = '')
            summary_table <- rbind(
                  sapply(object@R_Strata, function(x) dim(x@Data)[1]),
                  sapply(object@R_Strata, function(x) vcount(x@R)),
                  sapply(object@R_Strata, function(x) ecount(x@R))
                  )
            dimnames(summary_table) <- list(c('Total n', 'Vertices', 'Edges'), colnames(Edges))

            cat( '\n   Stratfied R-net',
                 '\n',
                 '\n Stratified by:', object@Stratify_by,
                 '\n    L1 Penalty:', object@R_Strata[[1]]@L1,
                 '\n',
                 '\nStrata Summary:\n'
                 )
            print(summary_table)
            cat('\n\nEdges:\n')
            print(Edges, quote = F)
            cat('\n\nNote: The "Total n" row in the summary refers to the size of dataset.',
              '\n  The number observations used to estimate partial correlations may vary by edge within each stratum.',
              '\n  Stratum-sepcific details can be displayed by calling: summary(foo@R_Stratum[[1]])\n')
          })



#'Summary - rnetStrata
#'
#'Gives more information than 'print'.
#' @param object an rnet object of class 'rnetStrata'

#' @rdname summary-rnetStrata

setMethod(f = 'summary',
          signature(object = 'rnetStrata'),
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