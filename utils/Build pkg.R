source('utils/Install from Rproj.R')

for(file.name in list.files('data')) load(file = file.path('data', file.name))

use_data(NARMS_EC_DATA, V_ATTRS, E_ATTRS, EC_COORDS, overwrite = T)

devtools::use_build_ignore("Rnets.Rproj")
devtools::use_build_ignore("Archive")
devtools::use_build_ignore("utils")
devtools::use_build_ignore(".gitignore")

#ver_num <- '1.0.1.9001'
inc_major <- F
inc_minor <- F
source('./utils/.v_update.R')

devtools::build_vignettes()
document() #IGNORE warning about no defined signature for igraph
build(path = '.')
#sink(file = 'utils/check.txt')
check()
#sink()
