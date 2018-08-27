library(devtools)
devtools::install(build_vignettes = F)
#devtools::install_github('klutometis/roxygen')
#devtools::install_github('gustavdelius/roxygen') #8.24.2018 - klutometis version of roxygen currently broken. Use gustavdelius' fix.
#Not sure if need to use previous line every time.
library(roxygen2)
library(rmarkdown)

for(file.name in list.files('data')) load(file = file.path('data', file.name))
#load(file = 'data\\Narms_data.rdata')
#load(file = 'data\\Attributes.rdata')
#load(file = 'data\\EC_coords.rdata')

#NARMS_EC_DATA <- NARMS_EC_DATA[,c(1, 6, 8, 10:34)]

use_data(NARMS_EC_DATA, V_ATTRS, E_ATTRS, EC_COORDS, overwrite = T)

devtools::use_build_ignore("Build Rnet package.R")
devtools::use_build_ignore("check.txt")
devtools::use_build_ignore(".*.Rproj")
devtools::use_build_ignore(".test.signedModularity.R")
devtools::use_build_ignore(".install_newRnetVersion.R")
devtools::use_build_ignore("Archive")

devtools::use_build_ignore("vignettes\\Rnets-vignette.html") #TEMPORARILY IGNORE INCOMPLETE VIGNETTES!!!
devtools::use_build_ignore("vignettes\\Rnets-vignette.rmd") #TEMPORARILY IGNORE INCOMPLETE VIGNETTES!!!


document() #IGNORE warning about no defined signature for igraph?
build(path = '.')
#sink(file = 'check.txt')
check()
#sink()
