R_EC_08 <- Rnet(x = NARMS_EC_DATA,
                L1 = 0.15,
                vertices = c('AMP', 'AMC','FOX', 'TIO', 'AXO', 'CIP', 'NAL', 'TET', 'COT', 'FIS'),
                subset = expression(Year == 2008)
                )

Assign_Emetadata(R_EC_08, E_ATTRS, 'omega', e_cuts = c(0, 0.05, 0.10, 0.20, 1))
Assign_Vmetadata(R_EC_08, V_ATTRS, 'Code')

plot.call <- plot(R_EC_08, vertex.frame.color = NA)
plot.call

R_EC_08@R

x <- R_EC_08
x
eval(parse(text = plot.call))

is.character(edge_attr(R_EC_08@R, 'color'))

