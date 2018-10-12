###### ORQUIS
###### ARREGLO DE DATOS PRIMARIOS

# CONTENIDOS
###### Arreglo de L-matrix
###### Arreglo de R-matrix
###### Arreglo de Q-matrix
###### Igualar filas L y R.
###### Juntarlas en una lista

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



matrices <- list(rmat, lmat, qmat)
names(matrices) <- c("rmat", "lmat", "qmat")















