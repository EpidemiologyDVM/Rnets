ABX_LIST <- c(
  'AMP',
  'AMC',
  'AXO',
  'FOX',
  'TIO',
  'STR',
  'GEN',
  'KAN',
  'NAL',
  'CIP',
  'FIS',
  'COT',
  'AZI',
  'CHL',
  'TET'
)

EC08_Rnet <- Rnet(
  NARMS_EC_DATA,
  L1 = 0.25,
  Stratify = NARMS_EC_DATA$Year == 2008,
  V_set = ABX_LIST
  )

Assign_Vmetadata(EC08_Rnet, V_ATTRS)

#DATA FRAME
EC08_edgeFrame <- cbind(as.data.frame(as_edgelist(EC08_Rnet@R)), omega = E(EC08_Rnet@R)$omega)

EC08_edgeFrame$Class1 <- V_ATTRS$Class[match(EC08_edgeFrame$V1, V_ATTRS$Code)]
EC08_edgeFrame$Class2 <- V_ATTRS$Class[match(EC08_edgeFrame$V2, V_ATTRS$Code)]

signedModularity(EC08_edgeFrame, 'Class')
signedModularity(EC08_edgeFrame, 'Class', weight = 'omega')

EC08_edgeFrame['omega']

signedModularity(EC08_edgeFrame)