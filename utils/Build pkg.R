source('utils/Install from Rproj.R')

for(file.name in list.files('data')) load(file = file.path('data', file.name))

use_data(NARMS_EC_DATA, V_ATTRS, E_ATTRS, EC_COORDS, overwrite = T)


devtools::use_build_ignore(".*.Rproj")
devtools::use_build_ignore("Archive")
devtools::use_build_ignore("utils")
devtools::use_build_ignore("tests_examples")

devtools::use_build_ignore("vignettes\\Rnets-vignette.html") #TEMPORARILY IGNORE INCOMPLETE VIGNETTES!!!
devtools::use_build_ignore("vignettes\\Rnets-vignette.rmd") #TEMPORARILY IGNORE INCOMPLETE VIGNETTES!!!


document() #IGNORE warning about no defined signature for igraph
build(path = '.')
#sink(file = 'utils/check.txt')
check()
#sink()
