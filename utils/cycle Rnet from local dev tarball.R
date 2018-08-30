latest <- '1.0.0.9003'

remove.packages('Rnets')
detach('package:Rnets')

install.packages(
  paste('Rnets_', latest, '.tar.gz', sep = ''), 
  repos = NULL, type = 'source'
  )
library(Rnets)
