library(devtools)
#devtools::install_github('klutometis/roxygen')
#Not sure if need to use previous line every time.
library(roxygen2)
library(rmarkdown)

load(file = 'Source\\data\\Narms_data.rdata')
load(file = 'Source\\data\\Attributes.rdata')
load(file = 'Source\\data\\EC_coords.rdata')

NARMS_EC_DATA <- NARMS_EC_DATA[,c(1, 6,8, 10:34)]

use_data(NARMS_EC_DATA, V_ATTRS, E_ATTRS, EC_COORDS, overwrite = T)

document()
build()
check()


