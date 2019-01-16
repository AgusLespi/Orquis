
# Lista de especies según la     

mat <- matrix(nrow=34,ncol=4)

spp1 <- c("Campylocentrum micranthum", "Cryptocentrum latifolium", "Dichaea hystricina", "Dichaea pendula", "Encyclia prostechea", "Epidendrum LH", "Epidendrum porpax", "Epidendrum RY", "Epidendrum sp1", "Lankesteriana cuspidata", "Lepanthes sp", "Lepanthes sp1", "Lepanthes sp2", "Lepanthes sp3", "Lepanthopsis floripecten", "Maxillaria", "Maxillaria fritzii", "Maxillaria ochracea", "Mormolyca rufescens", "Myoxanthus affinis", "Oncidium adelaide", "Oncidium chrysomorphum", "Oncidium pictum", "Platystele consobrina", "Pleurothallis hystrix", "Pleurothallis tripterantha", "Restrepia antennifera", "Stelis argentata", "Stelis popayanensis", "Stelis sp1", "Stelis sp2", "Stelis spathulata", "Stelis vulcanica", "Sigmatostalix sergii")

spp2 <- c("Campylocentrum micranthum (Lindl.) Rolfe", "Cryptocentrum latifolium Schltr.", "Dichaea hystricina Rchb. f.", "Dichaea pendula  (AubL.) Cogn.", "Encyclia prostechea", "Epidendrum LH", "Epidendrum porpax Rchb. f.", "Epidendrum RY", "Epidendrum sp1", "Lankesteriana cuspidata", "Lepanthes sp", "Lepanthes sp1", "Lepanthes sp2", "Lepanthes sp3", "Lepanthopsis floripecten (Rchb. f.) Ames", "Maxillaria", "Maxillaria fritzii (Ojeda & Carnevali) Christenson", "Ornithidium pendulum (Poepp. & Endl.) Cogn.", "Mormolyca rufescens  (Lindl.) M.A. Blanco", "Myoxanthus affinis (Lindl.) Luer", "Oncidium adelaide", "Oncidium chrysomorphum Lindl.", "Oncidium pictum Kunth", "Platystele consobrina Luer", "Myoxanthus hystrix (Rchb. f.) Luer", "Pabstiella tripterantha (Rchb. f.) F. Barros", "Restrepia antennifera Kunth", "Stelis argentata Lindl.", "Stelis popayanensis F. Lehm. & Kraenzl.", "Stelis sp1", "Stelis sp2", "Stelis spathulata Poepp. & Endl.", "Stelis nanegalensis Lindl.", "Oncidium sergii (P. Ortiz) M.W. Chase & N.H. Williams")

ref1 <- c("Arévalo 621 (COL)", "Arboleda s.n. (CR)", "Betancur 4620 (COL)", "Betancur 1916 (COL)", "", "", "Idrobo 5343(COL)", "", "", "", "", "", "", "", "Cuatrecasas 18762 (AMES)","","Kapuler 238 (COL)", "Chaparro de Barrera 1251 (COL)", "Cuatrecasas s.n. (COL)", "Córdoba 4290 (COL)", "", "Königer k193f (JAUM)","Bonpland 1599 (P)", "Escobar 2708 (SEL)", "Killip 20302 (AMES)", "Cuatrecasas 8063 (COL)", "Bonpland 2121 (P)", "Arévalo 971 (COL)", "Galindo-Tarazona 165 (COL)", "","", "Duque 2282 (JAUM)", "Cogollo 2827 (JAUM)[nota: S. nan]", "Hugh-Jones 76 (COL)")

ref2 <-  c("J.  de  Bot.  3:  273.  1889", "Feddes  Repert.  Beih.  19:  247.  1923", "Flora 48: 279. 1865", "Urban Symbol. Antill. 4: 182. 1903", "", "", "Bonpl. 3: 220. 1855", "", "", "", "", "", "", "", "Bot. Mus. Leafl. 1: 11. 1933","","Christenson, E. A. 2010b. Maxillaria fritzii. Australian Orchid Review", "Caldasia 10: 235. 1968","", "Selbyana 7: 35: 1982", "","Nov. Gen. et Sp. Pl. 1: 346. 1816", "Icon. Pleuroth. 7: 32. 1990","", "Selbyana 7: 43. 1982", "", "Nov. Gen. et Sp. Pl. 1: 367. 1816", "Bot. Reg. 28: Misc. p. 64. 1842", "Lindleyana 16(4): 265. 2001", "","", "Nov. Gen. ac Sp. Pl. 1: 46. 1836 ", "Feddes Repert. 14: 125. 1915 [nota: S. vul]", "(P. Ortiz) M.W. Chase & N.H. Williams [Nota: Sigmatostalix]")

# "Maxillaria orchracea" = "Ornithidium pendulum (Poepp. & Endl.) Cogn."
# "Pleurothallis hystrix" = "Myoxanthus hystrix"
# "Pleurothallis tripterantha" = "Pabstiella tripterantha"
# "Stelis vulcanica" = "S. nanegalensis"
# "Sigmatostalix sergii" = "Oncidium sergii"

spp1 <- as.data.frame(spp1)
spp2 <- as.data.frame(spp2)
ref1 <- as.data.frame(ref1)
ref2 <- as.data.frame(ref2)

df <- cbind(spp1,spp2,ref1,ref2)










