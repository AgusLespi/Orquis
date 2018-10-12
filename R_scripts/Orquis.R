####################### Orquis
####################### Script para fourth corner & RQL
####################### Tutorial por Dray 2013
####

library(ade4)

############# Fourth corner

four <- fourthcorner(aviurba$mil,aviurba$fau,aviurba$traits,nrepet=99) summary(four.comb.default) plot(four.comb.default, stat = "G")
































