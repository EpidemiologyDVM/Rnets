#' Rnet Classes
#'
#' The Class definitions for Rnets package.
#' @export


rnet.input <- setClass(Class = "rnet.input",
	slots = c(
		RawData = 'data.frame',
		cor_method = 'character',
		cor_pairing = 'character',
		n_threshold = 'numeric',
		L1_orig = 'numeric',
		V_set_orig = 'character',
		Forced_zeros = 'matrix',
		Layout_master = 'data.frame'
		)
	)

rnet.basic <- setClass(Class = "rnet.basic",
	slots = c(
		Data = 'data.frame',
		L1 = 'numeric',
		V_set = 'character',
		n = 'matrix',
		V_omitted = 'character',
		V_metadata = 'character',
		E_metadata = 'character',
		Zeros = 'list',
		Sigma = 'matrix',
		Theta = 'matrix',
		Omega = 'matrix',
		A = 'matrix',
		loglik = 'numeric',
		R = 'ANY',
		Layout = 'matrix'
		),
	contains = 'rnet.input'
	)

rnet.strata <- setClass(Class = "rnet.strata",
	slots = c(Strata_def = 'expression'),
	contains = 'rnet.basic'
	)

rnet.strata.multi <- setClass(Class = "rnet.strata.multi",
	slots = c(
		Stratify_by = 'character',
		E_matrix = 'matrix',
		R_Strata = 'list'
		),
	contains = 'rnet.input'
	)
	
rnet.L1.set <- setClass(Class = 'rnet.L1.set',
	slots = c(
		Data = 'data.frame',
		L1_set = 'numeric',
		B = 'numeric',
		B_method = 'character',
		B_sets = 'matrix',
		Data_b = 'array',
		pr_b = 'numeric',
		n_b = 'numeric',
		W_aggr = 'array',
		E_aggr = 'data.frame',
		M = 'data.frame',
		Edge_stability = 'data.frame',
		StARS_D = 'numeric'
		)
	)