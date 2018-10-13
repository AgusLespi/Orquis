###### ORQUIS
###### ARREGLO DE DATOS PRIMARIOS

# CONTENIDOS
###### Arreglo de L-matrix
###### Arreglo de R-matrix
###### Arreglo de Q-matrix
###### Igualar filas L y R.
###### Juntarlas en una lista
###### Arreglo de Q matrix por localidad
##########################################################################

###### Arreglo de L-matrix

##### Importar datos de excel para sumar el número de presencias de cada sp. por estrato y forófito.

# arreglo_L-spp_matrix.xls (hoja 3)
dat <- read.excel() # from clipboard
datos <- dat

# variable de código combinado forofito-estrato
datos$tstrat <- paste0(datos$ID.Forófito, sep="_", datos$Estrato_Orq)

# pasar de integer a numérico
datos[6:39] <- lapply(datos[6:39], as.numeric)

# suma por código combinado
sumas <- by(datos[,6:39], datos$tstrat, colSums)
sumas1 <- data.frame(do.call("rbind", sumas))

# reemplazar los valores > 1 por un 1 (matríz de presencias)
dato <- sumas1
dato1 <- as.data.frame(apply(dato, 2, function(x) ifelse(x>=1,1,0)))

# recuperar el código de forófito y estrato
code <- as.data.frame(rownames(dato1))
	names(code) <- "code"
	library(stringr)
	code2 <- as.data.frame(str_split_fixed(code$code, "_", 2))
	names(code2) <- c("tree","stratum") 

# pegar código a tabla
l.matrix0 <- cbind(code2, dato1)

# exportar la tabla

write.table(l.matrix0, "L-matrix.txt")

################## LISTO

###### Arreglo de R-matrix

##### Importar datos de excel para sumar el número de presencias de cada sp. por estrato y forófito.

dat <- read.csv("par.csv", head=TRUE, sep=",") # 
datos <- dat

# variable de código combinado forofito-estrato
datos$tstrat <- paste0(datos$Tree, sep="_", datos$Elev)

# medias por código combinado
sumas <- by(datos[,5:17], datos$tstrat, colMeans)
sumas1 <- data.frame(do.call("rbind", sumas))
dato1 <- sumas1

# recuperar el código de forófito y estrato
code <- as.data.frame(rownames(dato1))
	names(code) <- "code"
	library(stringr)
	code2 <- as.data.frame(str_split_fixed(code$code, "_", 2))
	names(code2) <- c("tree","stratum") 

# pegar código a tabla
r.matrix0 <- cbind(code2, dato1)

# seleccionar las columnas utiles
r.matrix1 <- r.matrix0[,c(1:2,4,7:9,13:15)] 

# exportar la tabla

write.table(r.matrix1, "R-matrix.txt")

################## LISTO


###### Arreglo de Q-matrix

##### Importar datos de excel para sumar el número de presencias de cada sp. por estrato y forófito.

dat <- read.csv("traits.csv", head=TRUE, sep=";") # 
datos <- dat

# pasar de integer a numérico
datos[8:12] <- lapply(datos[8:12], as.numeric)

# reemplazar NA por ceros

datos[is.na(datos)] <- 0

# medias por código combinado
sumas <- by(datos[,8:25], datos$spp, colMeans)
sumas1 <- data.frame(do.call("rbind", sumas))
dato1 <- sumas1

# recuperar el código de spp
code <- as.data.frame(rownames(dato1))
	names(code) <- "spp"
	
# pegar código a tabla
q.matrix0 <- cbind(code, dato1)

# exportar la tabla

write.table(q.matrix0, "Q-matrix.txt")

################## LISTO

##################

###### Igualar filas L y R (en excel)

lmatrix <- read.excel()
write.table(lmatrix, "l-mat.txt")

rmatrix <- read.table("R-matrix.txt", head=TRUE, sep="	") # sep=tab
lmatrix1 <- read.table("L-mat.txt", head=TRUE, sep=" ")

site <- as.data.frame(rownames(rmatrix))
names(site) <- "site"

rmat <- cbind(site,rmatrix[,4:10])
lmat <- cbind(site,lmatrix1[,3:36])
qmat <- q.matrix0

write.table(rmat, "rmat.txt")

write.table(lmat, "lmat.txt")

write.table(qmat, "qmat.txt")

###### Juntar las matrices en una lista
rmat <- read.table("rmat.txt", head=TRUE, sep=" ")
lmat <- read.table("lmat.txt", head=TRUE, sep=" ")

qmat <- read.table("qmat.txt", head=TRUE, sep=" ")

matrices <- list(rmat, lmat, qmat)
names(matrices) <- c("rmat", "lmat", "qmat")


####################################################################
####################################################################
####################################################################

################## Arreglo Q matrix por localidad

dat <- read.csv("traits.csv", head=TRUE, sep=";") # 
datoss <- dat

#datos <- subset(datoss, site =="LH")
datos <- subset(datoss, site =="RY")

# pasar de integer a numérico
datos[8:12] <- lapply(datos[8:12], as.numeric)

# reemplazar NA por ceros

datos[is.na(datos)] <- 0

# medias por sp
sumas <- by(datos[,8:25], datos$spp, colMeans)
sumas1 <- data.frame(do.call("rbind", sumas))
dato1 <- sumas1

# recuperar el código de spp
code <- as.data.frame(rownames(dato1))
	names(code) <- "spp"
	
# pegar código a tabla
# q.matrixLH <- cbind(code, dato1)
q.matrixRY <- cbind(code, dato1)

# exportar la tabla

# write.table(q.matrixLH, "Q-matrix-LH.txt")
write.table(q.matrixRY, "Q-matrix-RY.txt")

################## LISTO

qmat3 <- read.table("Q-matrix-LH.txt", head=TRUE, sep="	")
qmat4 <- read.table("Q-matrix-RY.txt", head=TRUE, sep="	")

###### Eliminar spp que sobran de cada hoja
 
datos[, colSums(SelectVar != 0) > 0]

###### Igualar filas L y R (en excel)

#LH
 # q-mat 24 spp. x 18 traits

#RY
 # q-mat 20 spp. x 18 traits




###### ################ Por localidades

###### Abrir matrices 
rmat <- read.table("rmat.txt", head=TRUE, sep=" ")
lmat <- read.table("lmat.txt", head=TRUE, sep=" ")
qmat <- read.table("qmat.txt", head=TRUE, sep=" ")

## abrir rasgos cualitativos 

qual <- read.csv("qualitativetraits.csv", head=TRUE)
###### Quitar col codigo
lmat2 <- lmat[,2:35]
rmat2 <- rmat[,2:8]
qmat2 <- qmat[,2:19]

	# LH
	rmat3 <- rmat2[1:43,] 
	lmat3 <- lmat2[1:43,]
	# write.table(lmat3, "lmat3LH.txt",sep=",")
	l3 <- read.table("lmat3LH.txt", head=TRUE, sep="")
	l3 <- l3[,2:25] # este es el bueno. Solo las spp. de la localidad
	
	qmat3 <- read.table("Q-matrix-LH.txt", head=TRUE, sep="	")
	q3 <- qmat3[,2:19]
	q3 <- cbind(q3, qual[,2:3])

	# RY
	rmat4 <- rmat2[44:87,]
	lmat4 <- lmat2[44:87,]
	# write.table(lmat4, "lmat4RY.txt",sep=",")
	l4 <- read.table("lmat4RY.txt", head=TRUE, sep="")
	l4 <- l4[,2:21] # este es el bueno. Solo las spp. de la localidad
	
	qmat4 <- read.table("Q-matrix-RY.txt", head=TRUE, sep="	")
	q4 <- qmat4[,2:19]
	qual2 <- qual[1:20,5:6]
	q4 <- cbind(q4, qual2)


## orq para cada localidad
#LH
orq <- list(rmat3, l3, q3)
names(orq) <- c("env", "spe", "traits")









