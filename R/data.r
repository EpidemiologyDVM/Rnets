#'E. coli Isolate information from Nat'l Antimicrobial Resistance Monitoring System.
#'
#'A dataset of isolate resistance and metadata for 14,418 E. coli isolates from commercial chicken breasts and chicken carcasses rinsates. Resistance data stored log2 transformed minimum inhibitory concentration (MIC). Columns beyond SAMPLE_ID, Year, and SOURCE are log2(MICs). See source for key for antimicrobials.
#'
#'@docType data
#'
#'@usage data(NARMS_EC_DATA)
#'
#'@format A dataframe from with 14,418 rows and 28 columns
#' \describe{
#'	\item{SAMPLE_ID}{NARMS-assigned isolate code}
#'	\item{Year}{Year collected}
#'	\item{SOURCE}{Source of isolate: Retail (chicken breasts) or Slaughter (carcass rinsates)}
#'	\item{AMC - TIO}{log2(MIC) of respective antibiotics (see source for drug codes)}
#'
#' }
#'@source \url{https://www.fda.gov/AnimalVeterinary/SafetyHealth/AntimicrobialResistance/NationalAntimicrobialResistanceMonitoringSystem/ucm416741.htm}
"NARMS_EC_DATA"

#'Sample Vertex Attributes for NARMS_EC_DATA
#'
#'A small data frame that contains vertex metadata for variables in the NARMS_EC_DATA set.  See also documentation for "Assign_Vmetadata"
#'
#'@docType data
#'
#'@usage data(V_ATTRS)
#'
#'@format A dataframe from with 61 rows and 5 columns
#' \describe{
#'	\item{Code}{A 3 letter code identifying the drug resistance}
#'	\item{Drug}{Full name of the drug}
#'	\item{Class}{A 5 character code identifying the class of drug resistance (e.g., B-LAC for beta-lactam resistance)}
#'	\item{color}{Assigned vertex color for resistance}
#'	\item{label.color}{Assigned vertex label color for resistance}
#'
#' }
"V_ATTRS"

#'Sample Edge Attributes
#'
#'A small data frame that contains metadata for decorating graph edges using "Assign_Emetadata"
#'
#'@docType data
#'
#'@usage data(E_ATTRS)
#'
#'@format A dataframe from with 4 rows and 2 columns
#' \describe{
#'	\item{lty}{The line type to use for the 4 edges categories}
#'	\item{Drug}{The line width to use for the 4 edge categories}
#'
#' }
"E_ATTRS"		