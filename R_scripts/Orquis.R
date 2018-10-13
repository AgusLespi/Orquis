####################### Orquis
####################### Script para fourth corner & RQL
####################### Tutorial por Dray 2013
####

library(ade4)

#########################################################################
#########################################################################
############# CODE FOR ANALYSES

##### 1. RQL. 
# A preliminary step of RLQ analysis is to perform the separate analyses of each table.
	# lw: sitios, cw: spp.

## Spp. table: correspondence anaysis
afcL.aravo <- dudi.coa(aravo$spe, scannf = FALSE)

## Traits: 
	# if all quantitative, pca.
	 acpQ.aravo <- dudi.pca(aravo$traits, row.w = afcL.aravo$cw,scannf = FALSE)
	
	# if mixed, hillsmith
# acpQ.aravo <- dudi.hillsmith(aravo$traits, row.w = afcL.aravo$cw, scannf = FALSE)

## Environment. All quantitative
	# mixed:
	acpR.aravo <- dudi.hillsmith(aravo$env, row.w = afcL.aravo$lw,scannf = FALSE)
	
	# quant:
	# acpR.aravo <- dudi.pca(aravo$env, row.w = afcL.aravo$lw,scannf = FALSE)

## RQL
rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo,
scannf = FALSE)

# Obtain RQL output

	# Layout
	plot(rlq.aravo)

	# Seaprate
	par(mfrow = c(1, 3))
	s.arrow(rlq.aravo$l1)
	s.arrow(rlq.aravo$c1)
	s.label(rlq.aravo$lQ, boxes = FALSE)

# As RLQ analysis maximizes the covariance between the traits and the environmental variables mediated by the species abundances, it is important to see how the individual parts (i.e. formula, see tutorial) of the compromise are considered.. To this end, one can compare the RLQ analysis to the separate analyses which maximize independently the structure of the trait (principal component analysis of the traits), the structure of the environment (Hill-Smith analysis of the environmental variables) and the correlation (correspondence analysis of the sites-species table). These comparisons are provided by the summary function.

summary(rlq.aravo)


##### 2. Fourth Corner.

# nrepet <- 49999
nrepet <- 999

four.comb.aravo <- fourthcorner(aravo$env, aravo$spe,
aravo$traits, modeltype = 6, p.adjust.method.G = "none",
p.adjust.method.D = "none", nrepet = nrepet)

# associations between (categorical) traits and (quantitative) environmental variables can be measured in three different ways:

	# stat="D2": the association is measured between the quantitative variable and each category separately. A correlation coeffcient is used to indicate the strength of the association between the given category and the small or large values of the quantitative variable.

	# stat="G": the association between the quantitative variable and the whole categorical variable is measured by a global statistic (F).

	# stat="D": the association is estimated between the quantitative variable and each category separately by a measure of the within-group homogeneity. The strength of the association is indicated by the dispersion of the values of the quantitative variable for a given category.

plot(four.comb.aravo, alpha = 0.05, stat = "D2")

# adjusted p-values for multiple comparisons (here we used the fdr method using the p.adjust.4thcorner function).

four.comb.aravo.adj <- p.adjust.4thcorner(four.comb.aravo,
p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")

	# adjusted p-values can also be obtained directly using the 		fourthcorner function:

	# fourthcorner(aravo$env, aravo$spe, aravo$traits, modeltype = 6, 		p.adjust.method.G = "fdr", p.adjust.method.D = "fdr", nrepet = 		nrepet)

# Plot associations with adjusted p-values
plot(four.comb.aravo.adj, alpha = 0.05, stat = "D2")

##### 3. Combining both approaches.

# Apply a multivariate test to evaluate the global significance of the traits-environment relationships. This test is based on the total inertia of the RLQ analysis:

testrlq.aravo <- randtest(rlq.aravo, modeltype = 6, nrepet = nrepet)
testrlq.aravo
	#The results are (highly) significant. Me sale diferente porque usé menos iteraciones.

plot(testrlq.aravo)
# The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and Legendre (2008). This statistic is returned by the fourthcorner2 function:

Srlq <- fourthcorner2(aravo$env, aravo$spe, aravo$traits,
modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)

Srlq$trRLQ

# Both approaches can be combined if RLQ scores are used to represent traits and environmental variables on a biplot. Then, significant associations revealed by the fourthcorner approach can be represented using segments (blue lines for negative associations, red lines for positive associations, see the argument col).
#Only traits and environmental variables that have at least one significant association are represented. Here, we apply this method using adjusted pvalues for multiple comparisons and a significant level alfa = 0:05.

plot(four.comb.aravo.adj, x.rlq = rlq.aravo, alpha = 0.05,
stat = "D2", type = "biplot")

### Another approach is provided by the fourthcorner.rlq function and consists in testing directly the links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").

testQaxes.comb.aravo <- fourthcorner.rlq(rlq.aravo, modeltype = 6,
typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
p.adjust.method.D = "fdr")

testRaxes.comb.aravo <- fourthcorner.rlq(rlq.aravo, modeltype = 6,
typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
p.adjust.method.D = "fdr")

print(testQaxes.comb.aravo, stat = "D")

print(testRaxes.comb.aravo, stat = "D")

# Results can be represented using a table with colors indicating significance :

par(mfrow = c(1, 2))
plot(testQaxes.comb.aravo, alpha = 0.05, type = "table",
stat = "D2")
plot(testRaxes.comb.aravo, alpha = 0.05, type = "table",
stat = "D2")

# Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant associations with the first axis are represented in blue, with the second axis in orange, with both axes in green (variables with no significant association are in black):

par(mfrow = c(1, 2))
plot(testQaxes.comb.aravo, alpha = 0.05, type = "biplot",
stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.aravo, alpha = 0.05, type = "biplot",
stat = "D2", col = c("black", "blue", "orange", "green"))

#########################################################################
#########################################################################
#########################################################################

################# ANALISIS ORQUIS

###### Abrir matrices 
rmat <- read.table("rmat.txt", head=TRUE, sep=" ")
lmat <- read.table("lmat.txt", head=TRUE, sep=" ")
qmat <- read.table("qmat.txt", head=TRUE, sep=" ")

###### Juntarlas en una lista
lmat2 <- lmat[,2:35]
rmat2 <- rmat[,2:8]
qmat2 <- qmat[,2:19]

orq <- list(rmat2, lmat2, qmat2)
names(orq) <- c("env", "spe", "traits")

#####
##### 1. RQL. 
# A preliminary step of RLQ analysis is to perform the separate analyses of each table.
	# lw: sitios, cw: spp.

## Spp. table: correspondence anaysis
	afcL.orq <- dudi.coa(orq$spe, scannf = FALSE)

## Traits: 
	# if all quantitative, pca.
	#acpQ.orq <- dudi.pca(orq$traits, row.w = afcL.orq$cw,scannf = FALSE)
	
	# if mixed, hillsmith
	acpQ.orq <- dudi.hillsmith(orq$traits, row.w = afcL.orq$cw, scannf = FALSE)

## Environment. All quantitative
	# mixed:
	#acpR.orq <- dudi.hillsmith(orq$env, row.w = afcL.orq$lw, scannf = FALSE)
	
	# quant:
	acpR.orq <- dudi.pca(orq$env, row.w = afcL.orq$lw, scannf = FALSE)

## RQL
rlq.orq <- rlq(acpR.orq, afcL.orq, acpQ.orq,
scannf = FALSE)

# Obtain RQL output

	# Layout
	plot(rlq.orq)

	# Seaprate
	par(mfrow = c(1, 3))
	s.arrow(rlq.orq$l1)
	s.arrow(rlq.orq$c1)
	s.label(rlq.orq$lQ, boxes = FALSE)

# As RLQ analysis maximizes the covariance between the traits and the environmental variables mediated by the species abundances, it is important to see how the individual parts (i.e. formula, see tutorial) of the compromise are considered.. To this end, one can compare the RLQ analysis to the separate analyses which maximize independently the structure of the trait (principal component analysis of the traits), the structure of the environment (Hill-Smith analysis of the environmental variables) and the correlation (correspondence analysis of the sites-species table). These comparisons are provided by the summary function.

summary(rlq.orq)


##### 2. Fourth Corner.

 nrepet <- 49999
# nrepet <- 999

four.comb.orq <- fourthcorner(orq$env, orq$spe,
orq$traits, modeltype = 6, p.adjust.method.G = "none",
p.adjust.method.D = "none", nrepet = nrepet)

# associations between (categorical) traits and (quantitative) environmental variables can be measured in three different ways:

	# stat="D2": the association is measured between the quantitative variable and each category separately. A correlation coeffcient is used to indicate the strength of the association between the given category and the small or large values of the quantitative variable.

	# stat="G": the association between the quantitative variable and the whole categorical variable is measured by a global statistic (F).

	# stat="D": the association is estimated between the quantitative variable and each category separately by a measure of the within-group homogeneity. The strength of the association is indicated by the dispersion of the values of the quantitative variable for a given category.

plot(four.comb.orq, alpha = 0.05, stat = "D2")

# adjusted p-values for multiple comparisons (here we used the fdr method using the p.adjust.4thcorner function).

four.comb.orq.adj <- p.adjust.4thcorner(four.comb.orq,
p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")

	# adjusted p-values can also be obtained directly using the 		fourthcorner function:

	# fourthcorner(orq$env, orq$spe, orq$traits, modeltype = 6, 		p.adjust.method.G = "fdr", p.adjust.method.D = "fdr", nrepet = 		nrepet)

# Plot associations with adjusted p-values
plot(four.comb.orq.adj, alpha = 0.05, stat = "D2")

##### 3. Combining both approaches.

# Apply a multivariate test to evaluate the global significance of the traits-environment relationships. This test is based on the total inertia of the RLQ analysis:

testrlq.orq <- randtest(rlq.orq, modeltype = 6, nrepet = nrepet)
testrlq.orq
	#The results are (highly) significant. Me sale diferente porque usé menos iteraciones.

plot(testrlq.orq)
# The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and Legendre (2008). This statistic is returned by the fourthcorner2 function:

Srlq <- fourthcorner2(orq$env, orq$spe, orq$traits,
modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)

Srlq$trRLQ

# Both approaches can be combined if RLQ scores are used to represent traits and environmental variables on a biplot. Then, significant associations revealed by the fourthcorner approach can be represented using segments (blue lines for negative associations, red lines for positive associations, see the argument col).
#Only traits and environmental variables that have at least one significant association are represented. Here, we apply this method using adjusted pvalues for multiple comparisons and a significant level alfa = 0:05.

plot(four.comb.orq.adj, x.rlq = rlq.orq, alpha = 0.05,
stat = "D2", type = "biplot") # NADA

### Another approach is provided by the fourthcorner.rlq function and consists in testing directly the links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").

testQaxes.comb.orq <- fourthcorner.rlq(rlq.orq, modeltype = 6,
typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
p.adjust.method.D = "fdr")

testRaxes.comb.orq <- fourthcorner.rlq(rlq.orq, modeltype = 6,
typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
p.adjust.method.D = "fdr")

print(testQaxes.comb.orq, stat = "D")

print(testRaxes.comb.orq, stat = "D")

# Results can be represented using a table with colors indicating significance :

par(mfrow = c(1, 2))
plot(testQaxes.comb.orq, alpha = 0.05, type = "table",
stat = "D2")
plot(testRaxes.comb.orq, alpha = 0.05, type = "table",
stat = "D2")

# Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant associations with the first axis are represented in blue, with the second axis in orange, with both axes in green (variables with no significant association are in black):

par(mfrow = c(1, 2))
plot(testQaxes.comb.orq, alpha = 0.05, type = "biplot",
stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.orq, alpha = 0.05, type = "biplot",
stat = "D2", col = c("black", "blue", "orange", "green"))

######################################################################
######################################################################
######################################################################

################ POR localidades
#####
##### 1. RQL. 
# A preliminary step of RLQ analysis is to perform the separate analyses of each table.
	# lw: sitios, cw: spp.

## Spp. table: correspondence anaysis
	afcL.orq <- dudi.coa(orq$spe, scannf = FALSE)

## Traits: 
	# if all quantitative, pca.
	#acpQ.orq <- dudi.pca(orq$traits, row.w = afcL.orq$cw,scannf = FALSE)
	
	# if mixed, hillsmith
	acpQ.orq <- dudi.hillsmith(orq$traits, row.w = afcL.orq$cw, scannf = FALSE)

## Environment. All quantitative
	# mixed:
	#acpR.orq <- dudi.hillsmith(orq$env, row.w = afcL.orq$lw, scannf = FALSE)
	
	# quant:
	acpR.orq <- dudi.pca(orq$env, row.w = afcL.orq$lw, scannf = FALSE)

## RQL
rlq.orq <- rlq(acpR.orq, afcL.orq, acpQ.orq,
scannf = FALSE)

# Obtain RQL output

	# Layout
	plot(rlq.orq)

	# Seaprate
	par(mfrow = c(1, 3))
	s.arrow(rlq.orq$l1)
	s.arrow(rlq.orq$c1)
	s.label(rlq.orq$lQ, boxes = FALSE)

# As RLQ analysis maximizes the covariance between the traits and the environmental variables mediated by the species abundances, it is important to see how the individual parts (i.e. formula, see tutorial) of the compromise are considered.. To this end, one can compare the RLQ analysis to the separate analyses which maximize independently the structure of the trait (principal component analysis of the traits), the structure of the environment (Hill-Smith analysis of the environmental variables) and the correlation (correspondence analysis of the sites-species table). These comparisons are provided by the summary function.

summary(rlq.orq)


##### 2. Fourth Corner.

 nrepet <- 49999
# nrepet <- 999

four.comb.orq <- fourthcorner(orq$env, orq$spe,
orq$traits, modeltype = 6, p.adjust.method.G = "none",
p.adjust.method.D = "none", nrepet = nrepet)

# associations between (categorical) traits and (quantitative) environmental variables can be measured in three different ways:

	# stat="D2": the association is measured between the quantitative variable and each category separately. A correlation coeffcient is used to indicate the strength of the association between the given category and the small or large values of the quantitative variable.

	# stat="G": the association between the quantitative variable and the whole categorical variable is measured by a global statistic (F).

	# stat="D": the association is estimated between the quantitative variable and each category separately by a measure of the within-group homogeneity. The strength of the association is indicated by the dispersion of the values of the quantitative variable for a given category.

plot(four.comb.orq, alpha = 0.05, stat = "D2")

# adjusted p-values for multiple comparisons (here we used the fdr method using the p.adjust.4thcorner function).

four.comb.orq.adj <- p.adjust.4thcorner(four.comb.orq,
p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")

	# adjusted p-values can also be obtained directly using the 		fourthcorner function:

	# fourthcorner(orq$env, orq$spe, orq$traits, modeltype = 6, 		p.adjust.method.G = "fdr", p.adjust.method.D = "fdr", nrepet = 		nrepet)

# Plot associations with adjusted p-values
plot(four.comb.orq.adj, alpha = 0.05, stat = "D2")

##### 3. Combining both approaches.

# Apply a multivariate test to evaluate the global significance of the traits-environment relationships. This test is based on the total inertia of the RLQ analysis:

testrlq.orq <- randtest(rlq.orq, modeltype = 6, nrepet = nrepet)
testrlq.orq
	#The results are (highly) significant. Me sale diferente porque usé menos iteraciones.

plot(testrlq.orq)
# The total inertia of RLQ analysis is equal to the SRLQ multivariate statistic defined in Dray and Legendre (2008). This statistic is returned by the fourthcorner2 function:

Srlq <- fourthcorner2(orq$env, orq$spe, orq$traits,
modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepet)

Srlq$trRLQ

# Both approaches can be combined if RLQ scores are used to represent traits and environmental variables on a biplot. Then, significant associations revealed by the fourthcorner approach can be represented using segments (blue lines for negative associations, red lines for positive associations, see the argument col).
#Only traits and environmental variables that have at least one significant association are represented. Here, we apply this method using adjusted pvalues for multiple comparisons and a significant level alfa = 0:05.

plot(four.comb.orq.adj, x.rlq = rlq.orq, alpha = 0.05,
stat = "D2", type = "biplot") # NADA

### Another approach is provided by the fourthcorner.rlq function and consists in testing directly the links between RLQ axes and traits (typetest="Q.axes") or environmental variables (typetest="R.axes").

testQaxes.comb.orq <- fourthcorner.rlq(rlq.orq, modeltype = 6,
typetest = "Q.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
p.adjust.method.D = "fdr")

testRaxes.comb.orq <- fourthcorner.rlq(rlq.orq, modeltype = 6,
typetest = "R.axes", nrepet = nrepet, p.adjust.method.G = "fdr",
p.adjust.method.D = "fdr")

print(testQaxes.comb.orq, stat = "D")

print(testRaxes.comb.orq, stat = "D")

# Results can be represented using a table with colors indicating significance :

par(mfrow = c(1, 2))
plot(testQaxes.comb.orq, alpha = 0.05, type = "table",
stat = "D2")
plot(testRaxes.comb.orq, alpha = 0.05, type = "table",
stat = "D2")

# Significance with axes can also be reported on the factorial map of RLQ analysis. Here, significant associations with the first axis are represented in blue, with the second axis in orange, with both axes in green (variables with no significant association are in black):

par(mfrow = c(1, 2))
plot(testQaxes.comb.orq, alpha = 0.05, type = "biplot",
stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb.orq, alpha = 0.05, type = "biplot",
stat = "D2", col = c("black", "blue", "orange", "green"))
























