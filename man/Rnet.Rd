% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rnet.R
\docType{methods}
\name{Rnet}
\alias{Rnet}
\alias{Rnet,ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,expression-method}
\alias{Rnet,ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,character-method}
\title{R-net Methods}
\usage{
Rnet(Data, L1, V_set = NULL, n_threshold = 0, cor_method = "s",
  cor_pairing = "pair", Forced_zeros = NULL, Plot_layout = NULL,
  Stratify = NULL)

\S4method{Rnet}{ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,expression}(Data, L1,
  V_set = NULL, n_threshold = 0, cor_method = "s", cor_pairing = "pair",
  Forced_zeros = NULL, Plot_layout = NULL, Stratify = NULL)

\S4method{Rnet}{ANY,ANY,ANY,ANY,ANY,ANY,ANY,ANY,character}(Data, L1,
  V_set = NULL, n_threshold = 0, cor_method = "s", cor_pairing = "pair",
  Forced_zeros = NULL, Plot_layout = NULL, Stratify = NULL)
}
\arguments{
\item{Data}{The dataset used to estimate the structure of the Rnet.}

\item{L1}{The L1 penalty used by the graphical LASSO to create a sparse precision matrix, also referred to as 'rho'. Must be non-negative.}

\item{V_set}{A character vector corresponding to the names of the antibiotics to include in the Rnet. Defaults to an empty list, in which case one vertex will be included for each column in 'Data'. If declared, only variables declared in 'V_set' will be represented with vertices in the MRF.}

\item{n_threshold}{The minimum number of observations required for an an estimated correlation to be valid. Defaults to 0, in which case any number of observations will be sufficient to estimate a valid correlation. If a vertex/variable has fewer valid observations than n_threshold, the vertex will be fully omitted from the network.}

\item{cor_method}{The method used to estimate the correlation matrix. Must be 'pearson', 'spearman', or 'kendall'. Partial matches allowed. Defaults to 'spearman'.}

\item{cor_pairing}{The method used to determine how NAs are handled when determining which pairs are used to estimate correlations. See 'cor' function documentation for additional information.}

\item{Forced_zeros}{The set of edges to be omitted from the Rnet. These partial correlations are forced to zero. Additional edges and vertices may be set to zero if n_threshold is employed.}

\item{Plot_layout}{A dataframe of two or three columns. See plot methods for more information.}

\item{Stratify}{Either a character variable of length one or expression. If a character value is supplied, it must match a column name in 'Data' and an object of type 'rnetMultiStrata' with a network for each level of the declared variable. If an expression is supplied, an object of 'rnetStrata' will be returned with the network estimated from a subset of 'Data' defined by the expression. If no value is supplied, an object of 'rnetBasic' will be returned with the network estimated from all observations in 'Data'.}
}
\value{
An rnet object containing the graphical LASSO results. The specific type of object is determined by the 'Stratify' argument.
}
\description{
This method takes an dataset (typically containing MICs values, AMR phenotypes, and presence abscence of genes) and returns an rnet object. The specific object class that is returned varies by what is provided to the L1 and Stratify arguments. The networks are Markov random fields (MRFs), a type of undirected graph. The network structure/topology is estimated using the graphical least absolute shrinkage and selection operator (glasso) as implemented in the R package of the same name developed by Friedman, Hastie, & Tibshirani (maintained by the latter)
}
\examples{
#Create a single R-net for all E. coli isolates in the provided dataset. 
#Vertices to be used defined by 'ABX_LIST' below.
#Edges require at least 20 observations to be valid.

ABX_LIST <- c('AMP', 'AMC', 'AXO', 'TIO', 'NAL', 'CIP', 'STR', 'GEN', 'COT', 'FIS')

EC_Rnet_ALL <- Rnet(Data = NARMS_EC_DATA, 
						L1 = 0.3, 
						V_set = ABX_LIST, 
						n_threshold = 20
						)
class(EC_Rnet_ALL)[1]	#EC_Rnet_ALL is a 'rnetBasic' object
print(EC_Rnet_ALL)	#Basic Rnet information
summary(EC_Rnet_ALL) 	#More detailed information

#Create a single R-net for only E. coli isolates collected during 2008
EC_Rnet_2008 <- Rnet(Data = NARMS_EC_DATA, 
						L1 = 0.3, 
						V_set = ABX_LIST, 
						n_threshold = 20,
					Stratify = expression(Year == 2008)
						)
class(EC_Rnet_2008)[1]	#EC_Rnet_ALL is an 'rnet.stratum' object

#Create a set of R-nets, one for each year of E.coli isolates.
EC_Rnet_byYear <- Rnet(Data = NARMS_EC_DATA, 
						L1 = 0.3, 
						V_set = ABX_LIST, 
						n_threshold = 20,
					Stratify = 'Year'
						)
class(EC_Rnet_byYear)[1]	#EC_Rnet_ALL is an 'rnetMultiStrata' object
}
