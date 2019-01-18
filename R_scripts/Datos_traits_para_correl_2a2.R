################################################
######
###### 14/01/2019 - 
######
####################################################

# Contents

# 1. Arreglo de datos
## 1.a. Arreglo inicial
### 1.a.1. Datos crudos de luz: promediar (mluz) y varianzas (vluz) OK
### 1.a.2. Datos crudos de rasgos: promediar (mtrait) y obtener OK varianzas (vtrait)
### 1.a.3. Fusionar las dos tablas (habrá diferencias de filas) OK

## 1.b. Rasgos promediados por sp y estrato OK
### 1.b.1. Tabla de medias			OK
### 1.b.1. Tabla de varianzas intra-sample	FALTA

# 2. Análisis de medias (LM)
## 2.a. Explorar correlaciones con script de carrascal
## 2.b. Analizar correlaciones seleccionadas 

# 3. Análisis de variazas (LM)
## 3.a. Explorar correlaciones con script de carrascal
## 4.b. Analizar correlaciones seleccionadas 

####################################################

# 1.a. Arreglo inicial

#setwd("E:/MS_in_prep/Orquis/Data")
setwd("F:/MS_in_prep/Orquis/Data")

luz <- read.csv("par.csv", sep=",", head=T)
traits <- read.csv("traits.csv", sep=";", head=T)

library(reshape)
library(dplyr)

### 1.a.1. Datos crudos de luz: promediar (mluz) y varianzas (vluz)

data <- luz

#### Crear variable de código combinado arbol x estrato (sample)
     data$sampl = paste(data$Tree, data$Elev, sep = "_")

#### replace NA with 0
data[is.na(data)] <- 0

#### Obtain means
mdata <- by(data, data$sampl, function(x){
  means <- colMeans(x[,5:17])
})
	# coerce by to df
	mluz <- data.frame(do.call("rbind", mdata))

	# add code vars
	mluz$sampl <- row.names(mluz)
	
#### Obtain var (lo dejo pendiente de encontrar una solución)


### 1.a.2. Datos crudos de rasgos: promediar (mtraits) y varianzas (vtraits)

data <- traits

#### Crear variable de código combinado arbol x estrato (sample)
     data$sampl = as.factor(paste(data$tree, data$stratum, sep = "_"))
     data$NFU <- as.numeric(data$NFU)

#### replace NA with 0
	data[is.na(data)] <- 0

#### Obtain means
mdata <- by(data, data$sampl, function(x){
  means <- colMeans(x[,12:25])
})
	# coerce by to df
	mtraits <- data.frame(do.call("rbind", mdata))

	# add code vars
	mtraits$sampl <- row.names(mtraits)
	
#### Obtain var (lo dejo pendiente de encontrar una solución)


### 1.a.3. Fusionar dos df

datos <- merge(mluz, mtraits, by="sampl") # excelente

###################
# mluz contiene los datos promediados de luz, mtraits idem para los traits, 
# y datos contiene ambos, más el código sampl. No he guardado estos datos, 
#así que hay que volver a correr el script para pasar a la fase 2 (análisis).
###################


# 2. Análisis de medias (LM)

## 2.a. Explorar correlaciones con script de carrascal (p.70 ppt 2018)

# Quitar 0's
dato <- datos
dato[dato == 0] <- NA

## VISUALIZACIÓN DE LAS RELACIONES ENTRE TODAS LAS VARIABLES

### AMBIENTALES
	## del modo: ~ respuesta+predictoras
	relaciones0 <- ~ GF+Op+CI1+CI2+DSF+ISF+TSF+DirB+DifB+TotB

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)

### CI2
	relaciones0 <- ~ CI2+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)


### ISF
	relaciones0 <- ~ ISF+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)


### DSF
	relaciones0 <- ~ DSF+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)

### op
	relaciones0 <- ~ Op+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)


## 2.b. Analizar correlaciones seleccionadas 

LT ~ ISF
NFU ~ DSF
Chl_a ~ DSF
Chl_b ~ DSF
LT ~ DSF
SLA ~ DSF

# Tests

eqt <- as.formula(log(LT) ~ ISF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(log(NFU) ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(log(Chl_a) ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(Chl_b ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # R2: 0.127, p=0.0069

eqt <- as.formula(log(LT) ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(log(SLA) ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.


eqt <- as.formula(Chl_b ~ DSF)
plot(dato$Chl_b, dato$DSF)


####################################################################
### Esto está hecho para todo el conjunto de datos. habría que separarlo por sp. y quizás por pobl.
###################################################################

##### Por localidad

lh <- datos[1:33,] # LH
ry <- datos[34:nrow(datos),] # RY

#################### LA HONDONADA

# dat <- lh
dat <- ry

# 2. Análisis de medias (LM)

## 2.a. Explorar correlaciones con script de carrascal (p.70 ppt 2018)

# Quitar 0's
dato <- dat
dato[dato == 0] <- NA

## VISUALIZACIÓN DE LAS RELACIONES ENTRE TODAS LAS VARIABLES

### AMBIENTALES
	## del modo: ~ respuesta+predictoras
	relaciones0 <- ~ GF+Op+CI1+CI2+DSF+ISF+TSF+DirB+DifB+TotB

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)

### CI2
	relaciones0 <- ~ CI2+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)


### ISF
	relaciones0 <- ~ ISF+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)


### DSF
	relaciones0 <- ~ DSF+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)

### op
	relaciones0 <- ~ Op+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)

## Entre Rasgos

	relaciones0 <- ~ NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)

## 2.b. Analizar correlaciones seleccionadas 

# LH
Chl_b ~ CI2
NFU ~ ISF
LT ~ ISF
LS ~ ISF
NFU ~ DSF
hmax ~ DSF
ratio_ab ~ DSF

# Tests

eqt <- as.formula(Chl_b ~ CI2)
modelo <- lm(eqt, dato)
summary(modelo) # R2=0.2, p=0.041 (+)

eqt <- as.formula(NFU ~ ISF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(LT ~ ISF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(LS ~ ISF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(NFU ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(Chl_b ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(hmax ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(ratio_ab ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.


eqt <- as.formula(Chl_b ~ CI2)
plot(dato$Chl_b, dato$CI2)

# RY

Chl_b ~ CI2
NFU ~ ISF
LT ~ ISF
LS ~ ISF
NFU ~ DSF
hmax ~ DSF
ratio_ab ~ DSF

# Tests

eqt <- as.formula(SLA ~ ISF)
modelo <- lm(eqt, dato)
summary(modelo) # R2=0.15, P=0.0169

eqt <- as.formula(LDM ~ ISF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(LA ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(ratio_ab ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(Chl_b ~ DSF)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(SLA ~ Op)
modelo <- lm(eqt, dato)
summary(modelo) # R2=0.1422, p=0.0234

eqt <- as.formula(LDM ~ Op)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.

eqt <- as.formula(succulence ~ Op)
modelo <- lm(eqt, dato)
summary(modelo) # n.s.



eqt <- as.formula(SLA ~ ISF)
plot(dato$Chl_b, dato$CI2)

eqt <- as.formula(SLA ~ Op)
plot(dato$Chl_b, dato$CI2)



#######################################################################
#######################################################################
#### POR SP



str(traits)

## 1. Ver qué spp. tienen más de 10 individuos
counts<-data.frame(table(traits$spp))
counts

	counts2 <- subset(counts, Freq>9) # dejar sólo las que tienen 10 o más.
	spp <- as.character(counts2$Var1)

# Datasets: mluz, traits

## 2. Crear variable de código combinado arbol x estrato (sample)
     traits$sampl = paste(traits$tree, traits$stratum, sep = "_")
	
	#### replace NA with 0
	traits[is.na(traits)] <- 0

## 3. Subset by sp

# especie0 <- subset(traits, spp=="Campylocentrum micranthum")
# especie0 <- subset(traits, spp=="Cryptocentrum latifolium")
# especie0 <- subset(traits, spp=="Dichaea hystricina")
# especie0 <- subset(traits, spp=="Lankesteriana cuspidata")
# especie0 <- subset(traits, spp=="Lepanthes sp.2")
# especie0 <- subset(traits, spp=="Maxillaria ochracea")
# especie0 <- subset(traits, spp=="Oncidium pictum")
# especie0 <- subset(traits, spp=="Pleurothallis hystrix")
# especie0 <- subset(traits, spp=="Restrepia antennifera")
# especie0 <- subset(traits, spp=="Stelis argentata")
# especie0 <- subset(traits, spp=="Stelis popayanensis")
# especie0 <- subset(traits, spp=="Stelis spathulata")
# especie0 <- subset(traits, spp=="Stelis vulcanica")

data <- especie0

## 4. Obtain means
especie <- by(data, data$sampl, function(x){
  means <- colMeans(x[,12:25])
}) # calculate means
	especie <- data.frame(do.call("rbind", especie)) # coerce by to df
	especie$sampl <- row.names(especie) # add code vars

## 5. Unir con luz promediada

	datos <- merge(mluz, especie, by="sampl") # excelente

## 6. Nombrar la tabla

# Cmyc <- datos # 18 
# Clat <- datos # 14
# Dhys <- datos # 5
# Lcus <- datos # 5
# Lep2 <- datos # 12
# Moch <- datos # 7
# Opic <- datos # 10
# Phys <- datos # 13
# Rant <- datos # 25
# Sarg <- datos # 22
# Spop <- datos # 6
# Sspa <- datos # 41
# Svul <- datos # 10

dfs <- list(Cmyc, Clat, Lep2, Opic, Phys, Rant, Sarg, Sspa, Svul)
#########################################################################

########## Análisis por sp.

########## Cmyc

# 2. Análisis de medias (LM)

## 2.a. Explorar correlaciones con script de carrascal (p.70 ppt 2018)

# Quitar 0's
dato <- Cmyc
dato[dato == 0] <- NA

## VISUALIZACIÓN DE LAS RELACIONES ENTRE TODAS LAS VARIABLES

### CI2
	relaciones0 <- ~ CI2+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)


### ISF
	relaciones0 <- ~ ISF+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)


### DSF
	relaciones0 <- ~ DSF+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)

### op
	relaciones0 <- ~ Op+NFU+hmax+Chl_a+Chl_b+ratio_ab+Chl_total+LT+LA+LDM+LFM+SLA+LS+succulence+LWC

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r <- abs(cor(x, y, use="complete.obs"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 1)
}
pairs(relaciones0, data=dato, cex.labels=2, pch="o", lower.panel = panel.cor)

# Me quedé acá 16/01/2019. Toca hacer un PCA. Ver detalled de resultados exploratorios (carrascal) en la libreta del loro.

 























