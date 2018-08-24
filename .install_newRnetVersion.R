latest <- '0.9.10.9001'

remove.packages('Rnets')
detach('package:Rnets')

install.packages(
  paste('Rnets_', latest, '.tar.gz', sep = ''), 
  repos = NULL, type = 'source'
  )
library(Rnets)
