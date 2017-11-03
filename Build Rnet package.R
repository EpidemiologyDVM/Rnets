library(devtools)
#devtools::install_github('klutometis/roxygen')
#Not sure if need to use previous line every time.
library(roxygen2)
library(rmarkdown)

load(file = 'data\\Narms_data.rdata')
load(file = 'data\\Attributes.rdata')
load(file = 'data\\EC_coords.rdata')

NARMS_EC_DATA <- NARMS_EC_DATA[,c(1, 6,8, 10:34)]

use_data(NARMS_EC_DATA, V_ATTRS, E_ATTRS, EC_COORDS, overwrite = T)

devtools::use_build_ignore("Build Rnet package.R")
devtools::use_build_ignore("check.txt")
devtools::use_build_ignore(".*.Rproj")


document()
build(path = '.')
sink(file = 'check.txt')
check()
sink()
