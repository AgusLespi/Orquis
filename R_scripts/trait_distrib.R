################################################################

############


setwd("F:/MS_in_prep/Orquis/Data")

# luz <- read.csv("par.csv", sep=",", head=T)
traits <- read.csv("traits.csv", sep=";", head=T)

data <- traits
#### replace NA with 0
data[is.na(data)] <- 0

library(ggplot2) # http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization

### LA

# isolate trait (avoids issues with zero values due to NA replacement
code <- traits[,1:5]
trait <- as.data.frame(traits$LA) # modificar aquí
datos <- cbind(code, trait)
names(datos) [6] <- "trait"

# delete zero rows
##Go through each row and determine if a value is zero
row_sub = apply(datos, 1, function(row) all(row !=0 ))
##Subset as usual
datos <- datos[row_sub,] 

# density plots
p <- ggplot(datos, aes(x=trait)) + 
  geom_density() # Basic density
p
p+ geom_vline(aes(xintercept=median(trait)),
            color="blue", linetype="dashed", size=1) # Add median line


# p+ geom_vline(aes(xintercept=mean(trait)),
#             color="red", linetype="dashed", size=1) # Add mean line













