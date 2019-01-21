################################################################
############ Ver trait analysis checklist en doc MS_Orch.

setwd("F:/MS_in_prep/Orquis/Data")
traits <- read.csv("traits.csv", sep=";", head=T)

# data <- subset(traits, site == "LH")
# data <- subset(traits, site == "RY")
data <- traits

library(ggplot2) # http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization

###### NEWTRAIT #######################################################



### LA Línea 17. No mover.

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(data$LA) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
ceros <- which(datos$trait == 0)
ceros
	# datos <- datos[-ceros,]

# lh <- datos
# ry <- datos

################## A. density plots

######## LA 

	# 1. calculate the mean of each group
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 100)) + ylim(c(0, 0.2)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
la <- as.data.frame(matriz)
colnames(la) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(la, "discreteLA.txt")

# end of trait
###################################################################




###### NEWTRAIT #######################################################

str(data)

### LT

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(data$LT) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
ceros <- which(datos$trait == 0)
ceros
datos <- datos[-ceros,]

# lh <- datos
# ry <- datos

################## A. density plots


######## LT ## Distribución bastante rara: bimodal. En LH está más clara: valores por debajo de algo menos de 0.5 y valores por encima de 1.2 aprox. entre medias no hay nada.  En RY idem, pero los valores son algo más altos y tiene valores altos extremos.

	# 1. calculate the mean of each group
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 5)) + ylim(c(0, 0.2)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
lt <- as.data.frame(matriz)
colnames(lt) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(lt, "discreteLT.txt")

# end of trait
###################################################################


###### NEWTRAIT #######################################################

str(data)

### LDM 

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(data$LDM) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## LDM ### Peor que LT. Pendiente.

	# 1. calculate the mean of each group
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 1.2)) + ylim(c(0, 0.2)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
ldm <- as.data.frame(matriz)
colnames(ldm) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(ldm, "discreteLDM.txt")


# end of trait
###################################################################


###### NEWTRAIT #######################################################

str(data)

### LFM 

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(data$LFM) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
ceros <- which(datos$trait == 0)
ceros
datos <- datos[-ceros,]

################## A. density plots
######## LFM 

	# 1. calculate the mean of each group
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 20)) + ylim(c(0, 0.2)) 
# Add mean lines
p + geom_vline(data=mu1, aes(xintercept=grp.mean, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
lfm <- as.data.frame(matriz)
colnames(lfm) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(lfm, "discreteLFM.txt")

# end of trait
###################################################################


###### NEWTRAIT #######################################################

str(data)

### SLA 

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(data$SLA) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## SLA

	# 1. calculate the mean of each group
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 460)) + ylim(c(0, 0.02)) 
# Add mean lines
p + geom_vline(data=mu1, aes(xintercept=grp.mean, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu1, median=FALSE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
sla <- as.data.frame(matriz)
colnames(sla) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(sla, "discreteSLA.txt")

# end of trait
###################################################################





###### NEWTRAIT #######################################################

str(data)

### hmax 

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(data$hmax) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## SLA

	# 1. calculate the mean of each group
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 160)) + ylim(c(0, 0.02)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
hmax <- as.data.frame(matriz)
colnames(hmax) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(hmax, "discretehmax.txt")

# end of trait
###################################################################




###### NEWTRAIT #######################################################

str(data)

### NFU 

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(as.numeric(data$NFU)) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
##Go through each row and determine if a value is zero
#row_sub = apply(datos, 1, function(row) all(row !=0 ))
##Subset as usual
#datos <- datos[row_sub,] 

ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## NFU

	# 1. calculate the mean of each group
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 50)) + ylim(c(0, 0.02)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
NFU <- as.data.frame(matriz)
colnames(NFU) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(NFU, "discreteNFU.txt")

# end of trait
###################################################################



###### NEWTRAIT #######################################################

str(data)

### LS 

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(as.numeric(data$LS)) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
##Go through each row and determine if a value is zero
#row_sub = apply(datos, 1, function(row) all(row !=0 ))
##Subset as usual
#datos <- datos[row_sub,] 

ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## LS

	# 1. calculate the mean of each group
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 50)) + ylim(c(0, 0.02)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
ls <- as.data.frame(matriz)
colnames(ls) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(ls, "discreteLS.txt")

# end of trait
###################################################################



###### NEWTRAIT #######################################################

str(data)

### Chl_b

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(as.numeric(data$Chl_b)) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
##Go through each row and determine if a value is zero
#row_sub = apply(datos, 1, function(row) all(row !=0 ))
##Subset as usual
#datos <- datos[row_sub,] 

ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## Chl_b

	# 1. calculate the mean of each group
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 50)) + ylim(c(0, 0.02)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
Chl_b <- as.data.frame(matriz)
colnames(Chl_b) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(Chl_b, "discreteChl_b.txt")

# end of trait
###################################################################



###### NEWTRAIT #######################################################

str(data)

### Chl_a

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(as.numeric(data$Chl_a)) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

# delete zero rows
##Go through each row and determine if a value is zero
#row_sub = apply(datos, 1, function(row) all(row !=0 ))
##Subset as usual
#datos <- datos[row_sub,] 

ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## Chl_a

	# 1. calculate the mean of each group
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)
	mu1$upper <- mu1$grp.mean + mu1$sd
	mu1$lower <- mu1$grp.mean - mu1$sd

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 50)) + ylim(c(0, 0.02)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)
# p + geom_vline(data=mu1, aes(xintercept=lower, color=site),
#           size=1)
# p + geom_vline(data=mu1, aes(xintercept=upper, color=site),
#           size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
Chl_a <- as.data.frame(matriz)
colnames(Chl_a) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(Chl_a, "discreteChl_a.txt")

# end of trait
###################################################################



###### NEWTRAIT #######################################################

str(data)

### ratio_ab 

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(as.numeric(data$ratio_ab)) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## ratio_ab

	# 1. calculate the mean of each group
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)
	mu1$upper <- mu1$grp.mean + mu1$sd
	mu1$lower <- mu1$grp.mean - mu1$sd

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 190)) + ylim(c(0, 0.02)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)
# p + geom_vline(data=mu1, aes(xintercept=lower, color=site),
#           size=1)
# p + geom_vline(data=mu1, aes(xintercept=upper, color=site),
#           size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
ratio_ab <- as.data.frame(matriz)
colnames(ratio_ab) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(ratio_ab, "discreteratio_ab.txt")

# end of trait
###################################################################



###### NEWTRAIT #######################################################

str(data)

### Chl_total  

# isolate trait (avoids issues with zero values due to NA replacement
code <- data[,1:5]
trait <- as.data.frame(as.numeric(data$Chl_total)) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

ceros <- which(datos$trait == 0)
datos <- datos[-ceros,]

################## A. density plots
######## ratio_ab

	# 1. calculate the mean of each group
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)
	mu1$upper <- mu1$grp.mean + mu1$sd
	mu1$lower <- mu1$grp.mean - mu1$sd

	#2. exploratory plot
p <- ggplot(datos, aes(x = trait, fill = site)) + geom_density(alpha = 0.5)
p + xlim(c(0, 190)) + ylim(c(0, 0.02)) 
# Add mean lines
p + geom_vline(data=mu2, aes(xintercept=grp.median, color=site),
             linetype="dashed", size=1)
# p + geom_vline(data=mu1, aes(xintercept=lower, color=site),
#           size=1)
# p + geom_vline(data=mu1, aes(xintercept=upper, color=site),
#           size=1)

# end of plot
###################################################################

################## B. Identificar spp. raras.
# in prep.
# end of section
###################################################################


################## C. Categorizar la variable.

	# Tareas
		# 1. Crear dos variables: LA<md y LA>md. Función discretizar()
		# 2. Meter en una matriz donde haya al menos las cols <- c("loc", "tree", "height", "sample", "id", "spp")
		# 4. Este código se ha de mantener, porque luego hay que fustionar todas las matrices por variable.

	# 1. calculate the mean of each group (este paso repite un cálculo que se hace más arriba en el script. Es para tenerlo a mano por si acaso)

	##### DATOS
	# datos. Vienen de aplicar el código de la línea 20 a 32 a los datos de traits.

	##### OBTENER mu
	# library(plyr)
	mu1 <- ddply(datos, "site", summarise, grp.mean=mean(trait))
	mu2 <- ddply(datos, "site", summarise, grp.median=median(trait))
	sds <- ddply(datos, "site", summarise, grp.sd=sd(trait))
	mu1$sd <- sds[,2]
	# head(mu)

# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+5)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")

# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

result <- discretizar(data=datos, mu=mu2, median=TRUE)
matriz[,7:10] <- result[,2:5]

# 4. rename and save
Chl_total <- as.data.frame(matriz)
colnames(Chl_total) <- c("site", "tree", "height", "id", "sp", "sample", "trait.value", "lower", "upper", "middle", "V11")
write.table(Chl_total, "discreteChl_total.txt")

# end of trait
###################################################################



############# 	qualitative traits (habit1, habit2)

###### NEWTRAIT #######################################################

str(data)

### habit1

datos0 <- data
datos0[is.na(datos0)] <- 0

# isolate trait (avoids issues with zero values due to NA replacement
code <- datos0[,1:5]
trait <- as.data.frame(as.numeric(datos0$habit1)) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

ceros <- which(datos$trait == 0)
# datos <- datos[-ceros,]


################## C. Separar la variable.
# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+6)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")


# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

split_discrete <- function(data=data, mu=mu, median=TRUE) {

	# create matrix to store the variables in
	mat <- matrix(nrow=nrow(datos), ncol=7)
	mat[,1] <- datos$site # LH = 1, RY = 2
	mat[,2] <- datos$trait
	
  for(i in 1:nrow(data)){
		
		mat[i,3] <- ifelse(mat[i,2] == 5, 1, 0) #
		mat[i,4] <- ifelse(mat[i,2] == 4, 1, 0) #
		mat[i,5] <- ifelse(mat[i,2] == 3, 1, 0) #
		mat[i,6] <- ifelse(mat[i,2] == 2, 1, 0) #
		mat[i,7] <- ifelse(mat[i,2] == 1, 1, 0) #
		} 
return(mat)
}

result <- split_discrete(data=datos)
matriz[,7:12] <- result[,2:ncol(result)]

# 4. rename and save
habit1 <- as.data.frame(matriz)
habit1$habit1 <- traits$habit1
colnames(habit1) <- c("site", "tree", "height", "id", "sp", "sample", "code_habit1","Ramicaule","Pseudobulb","Monopodial","LeafyStem","empty", "habit1")

write.table(habit1, "discretehabit1.txt")

# end of trait
###################################################################


###### NEWTRAIT #######################################################

str(data)

### habit2

datos0 <- data
datos0[is.na(datos0)] <- 0

# isolate trait (avoids issues with zero values due to NA replacement
code <- datos0[,1:5]
trait <- as.data.frame(as.numeric(datos0$habit2)) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

#### replace NA with 0
	datos[is.na(datos)] <- 0

ceros <- which(datos$trait == 0)
# datos <- datos[-ceros,]


################## C. Separar la variable.
# 2. matríz con códigos
matriz <- matrix(nrow=nrow(datos), ncol=ncol(datos)+6)
matriz[,1:5] <- as.matrix(datos[,1:5])  
matriz[,6] <- paste(matriz[,2], matriz[,3], sep="_")


# 3. matríz con trait categorizado y clasificado en presencia-ausencia 

split_discrete2 <- function(data=data, mu=mu, median=TRUE) {

	# create matrix to store the variables in
	mat <- matrix(nrow=nrow(datos), ncol=7)
	mat[,1] <- datos$site # LH = 1, RY = 2
	mat[,2] <- datos$trait
	
  for(i in 1:nrow(data)){
		
		mat[i,3] <- ifelse(mat[i,2] == 4, 1, 0) #
		mat[i,4] <- ifelse(mat[i,2] == 3, 1, 0) #
		mat[i,5] <- ifelse(mat[i,2] == 2, 1, 0) #
		mat[i,6] <- ifelse(mat[i,2] == 1, 1, 0) #
		} 
return(mat)
}

result <- split_discrete2(data=datos)
matriz[,7:12] <- result[,2:ncol(result)]

# 4. rename and save
habit2 <- as.data.frame(matriz)
habit2$habit2 <- traits$habit2
colnames(habit2) <- c("site", "tree", "height", "id", "sp", "sample", "code_habit1","Scandent","Prostrate","Erect","empty","empty", "na")

write.table(habit2, "discretehabit2.txt")

# end of trait
###################################################################



