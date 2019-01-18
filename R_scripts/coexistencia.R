####################################################################
#
#	ANALISIS DE COEXISTENCIA
#
# CONTENIDOS
#
#	1. arreglo de datos
#	2. análisis de matrices pequeñas
#	3. análisis de matrices grandes
#
####################################################################

### 1. arreglo de datos

setwd("F:/MS_in_prep/Orquis/Data(Coexistence_Analysis")

# mydata <- read.csv("LH_E1.csv", sep=",", head=T)
# mydata <- read.csv("LH_E2.csv", sep=",", head=T)
# mydata <- read.csv("LH_E3.csv", sep=",", head=T)
# mydata <- read.csv("LH_E4.csv", sep=",", head=T)
# mydata <- read.csv("LH_E5.csv", sep=",", head=T)
# mydata <- read.csv("RY_E1.csv", sep=",", head=T)
# mydata <- read.csv("RY_E2.csv", sep=",", head=T)
# mydata <- read.csv("RY_E3.csv", sep=",", head=T)
# mydata <- read.csv("RY_E4.csv", sep=",", head=T)
# mydata <- read.csv("RY_E5.csv", sep=",", head=T)
 

# delete cols with all 0 values

	datos <- mydata[,2:35]
	datos[sapply(datos, is.integer)] <- lapply(datos[sapply(datos, is.numeric)], as.numeric) # turn integer into numeric
	
	RIGHT = function(x,n){  
  	substring(x,nchar(x)-n+1)
	} # function for extracting the last number of tree code, so that i keep track of the actual tree (instead of numbering trees from 1 to n)
	code <- as.character(mydata[,1])
	tree <- as.data.frame(as.numeric(RIGHT(code,1)))
	names(tree) <- "" # delete col name

	dato <- as.data.frame(c(tree, datos)) # bind in a df
	colnames(dato)[1] <- ""
                       
	x <- as.matrix(dato[,2:35]) # turn into matrix
	y <- x[,!(colSums(abs(x)) == 0)] # delete 0 columns 
	z <- as.data.frame(as.numeric(which(!(rowSums(abs(x)) == 0)))) # identify cols with presences for coding purposes
		names(z) <- ""
	y2 <- as.data.frame(y[!(rowSums(abs(y)) == 0),]) # delete 0 rows
	
	final <- cbind(z, y2)
		colnames(final)[1] <- ""

############

# lh1 <- final
# lh2 <- final
# lh3 <- final
# lh4 <- final
# lh5 <- final
# ry1 <- final
# ry2 <- final
# ry3 <- final
# ry4 <- final
# ry5 <- final

#### Matrices grandes

# mydata <- read.csv("LH.csv", sep=",", head=T)
# mydata <- read.csv("RY.csv", sep=",", head=T)
# mydata <- read.csv("Matriz_2loc.csv", sep=",", head=T)

# delete cols with all 0 values

	datos <- mydata[,2:35]
	datos[sapply(datos, is.integer)] <- lapply(datos[sapply(datos, is.numeric)], as.numeric) # turn integer into numeric

	code <- as.data.frame(as.character(mydata[,1]))
	dato <- as.data.frame(c(code, datos)) # bind in a df
	colnames(dato)[1] <- ""
                       
	x <- as.matrix(dato[,2:35]) # turn into matrix
	y <- x[,!(colSums(abs(x)) == 0)] # select non-0 columns 
	z <- which(!(rowSums(abs(x)) == 0)) # identify rows to be retained
	code2 <- as.data.frame(as.character((code[z,]))) # select code labels using z
		names(code2) <- "sample_label"
	y2 <- as.data.frame(y[!(rowSums(abs(y)) == 0),]) # delete 0 rows
	z2 <- as.data.frame(as.numeric(z))
		names(z2) <- ""
	final <- cbind(code2, z2, y2)
		colnames(final)[2] <- ""

# lh <- final
# ry <- final
# todo <- final

#### Guardar todas las matrices en una lista.

coex_mat <- list(lh1, lh2, lh3, lh4, lh5, ry1, ry2, ry3, ry4, ry5, lh, ry, todo)  

# save .txt

write.table(lh1, "lh1.txt")
write.table(lh2, "lh2.txt")
write.table(lh3, "lh3.txt")
write.table(lh4, "lh4.txt")
write.table(lh5, "lh5.txt")
write.table(ry1, "ry1.txt")
write.table(ry2, "ry2.txt")
write.table(ry3, "ry3.txt")
write.table(ry4, "ry4.txt")
write.table(ry5, "ry5.txt")
write.table(lh, "lh.txt")
write.table(ry, "ry.txt")
write.table(todo, "todo.txt")

# save list
coex_dat <- tempfile("coex_mat", fileext = ".rds") # no se donde lo está guardando, pero por el momento me vale.
saveRDS(coex_mat, coex_dat)

# readRDS(coex_dat) # para abrir el archivo

####################################################################
####################################################################
####################################################################
####################################################################
####################################################################


### 2. análisis de matrices pequeñas

library(metacom)
library(vegan)

# fichero <- read.table("lh1.txt") # no se puede simular (2x5)
# fichero <- read.table("lh2.txt") # sims = 1000 (6x11)
# fichero <- read.table("lh3.txt") # sims = 1000 (9x17)
# fichero <- read.table("lh4.txt") # sims = 1000 (9x15)
# fichero <- read.table("lh5.txt") # sims = 1000 (8x14)

# fichero <- read.table("ry1.txt") # sims = 1000 (5x8)
# fichero <- read.table("ry2.txt") # sims = 1000 (9x11)
# fichero <- read.table("ry3.txt") # sims = 1000 (6x12) 
# fichero <- read.table("ry4.txt") # sims = 1000 (8x12) 
# fichero <- read.table("ry5.txt") # sims = 1000 (8x16) 

### Las dos matrices grandes tienen un código compuesto que indica el estrato por forófito. Para el análisis, no se puede mantener como factor. Solucion: guardar la matriz en .txt con 2 cols de código: una en forma de factor y otra en forma del códifo de fila que se retuvo en la eliminación de filas compuestas de 0's. Más adelante, si se requiere, se puede usar el factor para etiquetar figuras.

 fichero <- read.table("lh.txt") # sims = 1000 (34x25)
	sample_labs_lh <- fichero[,1]
	fichero <- fichero[,-1]

# fichero <- read.table("ry.txt") #  sims = 1000 (36x21)
#	sample_labs_ry <- fichero[,1]
#	fichero <- fichero[,-1]

# fichero <- read.table("todo.txt") #  sims = 1000 (36x21)
#	sample_labs_todo <- fichero[,1]
#	fichero <- fichero[,-1]

	## Tunear matriz

	fichero[sapply(fichero, is.integer)] <- lapply(fichero[sapply(fichero, is.numeric)], as.numeric)
	colnames(fichero)[1] <- ""

	## Tests - Jani Heino recomienda r1 y quasiswap.

	# Eje 1
	eje1.r1 <- Metacommunity(comm=fichero, scores=1, method="r1", order=T, sims=1000, allowEmpty=F)
	eje1.r1

	eje1.qs <- Metacommunity(comm=fichero, scores=1, method="quasiswap", order=T, sims=1000, allowEmpty=F)
	eje1.qs 

	# Eje 2
	eje2.r1 <- Metacommunity(comm=fichero, scores=2, method="r1", order=T, sims=1000, allowEmpty=F)
	eje2.r1

	eje2.qs <- Metacommunity(comm=fichero, scores=2, method="quasiswap", order=T, sims=1000, allowEmpty=F)
	eje2.qs 


## 

#################################################################
newdata <- fichero
newdata[35,] <- colSums(fichero) # 

newdata2 <- t(as.matrix(newdata))
newdata3 <- as.data.frame(newdata2)
colnames(newdata3)[35] <- "var"
newdata4 <- newdata3[order(newdata3$var),]
newdata5 <- t(as.matrix(newdata4))
newdata6 <- as.data.frame(newdata5[-35,])

Imagine(newdata6, col=c(0,1), order=TRUE, scores=1, fill=TRUE, xlab='Species', ylab='Site', yline=2, xline=2, sitenames=rownames(fichero), speciesnames=colnames(newdata6), binary=TRUE)

# no lo hace bien
#################################################################
























