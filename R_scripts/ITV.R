####################################################################

# 		ITV - Orquis

# Descripcion: el objetivo de este script es hacer análisis de varianza intrasp de rasgos a lo largo del gradiente de luz del tronco. La parte exploratoria viene de otro script. 

# contenido
	# promediar datos de luz
	# exploración de correlaciones 2 a 2.
	# pca: en desarrollo.

# comentarios: de momento sólo esta hecho parcialmente en C. micranthum. 
# más abajo hay una lista con las spp lo suficientemente abundantes como para hacer este tipo de análisis.
####################################################################



setwd("F:/MS_in_prep/Orquis/Data")

luz <- read.csv("par.csv", sep=",", head=T)
traits <- read.csv("traits.csv", sep=";", head=T)

library(reshape)
library(dplyr)


### 1.a.1. Datos crudos de luz: promediar (mluz)

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
	
## Traits

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

# save list
ITV <- tempfile("dfs", fileext = ".rds") # no se donde lo está guardando, pero por el momento me vale.
saveRDS(dfs, ITV)

# readRDS(dfs) # para abrir el archivo
#########################################################################

########## Análisis por sp.

########## Cmyc

# 2. Análisis de medias (LM)

## 2.a. Explorar correlaciones con script de carrascal (p.70 ppt 2018)

# Quitar 0's
datos <- Cmyc
datos[datos == 0] <- NA

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

# Ver detalled de resultados exploratorios (carrascal) en la libreta del loro.

dat <- datos[,15:ncol(datos)]
cmic.pca <- prcomp(dat[,2:ncol(dat)], center = TRUE,scale. = TRUE)
summary(cmic.pca)

str(cmic.pca)

# library(devtools)
# install_github("vqv/ggbiplot")
# library(ggbiplot)

ggbiplot(cmic.pca)

## El código para pca fue sacado de aquí: https://www.datacamp.com/community/tutorials/pca-analysis-r

########################



























