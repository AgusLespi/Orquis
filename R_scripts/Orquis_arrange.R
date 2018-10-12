###### ORQUIS
###### ARREGLO DE DATOS PRIMARIOS

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




















