CONTENIDOS

################# A. ARREGLO PRIMARIO DE DATOS
######## A.1. Arreglo de traits
######## A.2. Arreglo de PAR

######## A.1. Arreglo de traits

#### 03/10/2018
# Al calcular la suculencia de Mantovani aparecieron valores negativos debidos a que LFM < LDM.
Se deben a 2 cosas: 
1) LFM medido en varias hojas estaba dividido 2 veces. Está corregido en el mismo traits.xlsx.
2) los valores de LFM interpolados son demasiado bajos (menores que LDM). Se repite la interpolación en sigmaplot para cada spp. 
El proceso está registrado en el excel. borrador_arreglo-datos-traits.xlsx en las hojas "Interpol_LFM" y "Interpol2"

<<<<< Stelis spathulata

N regresión = 112
N missing = 7
Code missing ids {36,37,39,57,86,87,88}
Comments: El problema estaba en el valor del peso seco de esos 7 ids, que es muchísimo más alto que la media, mientras que su área está dentro de la media. 
	Acciones:
	1) Regresión LA-LDM para interpolar en la curva el LDW de los 7 ids
	2) Regresión LDM-LFM para interpolar en la curva el LFM que falta.

<<<<< Restrepia antennifera

N regresión = 50
N missing = 10
Code missing ids {53,54,89,90,91,96,97,101,102,105}
Comments: idem S. spa. Mismo procedimiento.

<<<<< Campylocentrum mycranthum

N regresión = 30
N missing = 5
Code missing ids {49,55,56,104,106}
Comments: idem S. spa. Mismo procedimiento.

<<<<< Lepanthes sp.1

N regresión = 6
N missing = 1
Code missing ids {64}
Comments: Demasiada dispersión. No regresión. El id 64 se pierde para LDM y LFM

<<<<< Lepanthes sp.2

N regresión = 16
N missing = 8
Code missing ids {41,42,43,50,51,92,93,95}
Comments: idem S. spa. Mismo procedimiento.

<<<<< Lepanthes sp.3

N regresión = 6
N missing = 1
Code missing ids {40}
Comments: idem S. spa. Mismo procedimiento.

<<<<< Lepanthopsis floripecten

N regresión = 5
N missing = 1
Code missing ids {333}
Comments: Demasiada dispersión. No regresión. El id 333 se pierde para  LFM

<<<<< Platystele consobrina

N total = 2
N missing = 2
Comments: Se pierden para LFM y medidas de suculencia.

#### end of day

#### 04/10/2018

## Registro de cambios: Eliminación de filas por columna.

# Sp_Orq_NHOC
Ids sin etiqueta en esta col se eliminan: NEW_ID_CODE {117,121,139,168,170,172,176,179,190,191,203,211,448}

# Listo. El resto queda por el momento.

#################################################################################

######## A.2. Arreglo de PAR
# 11/10/2018. El fichero par.csv es identico a 180706_PAR_DHTF.csv de la carpeta OngoingProjects/Orchids.../Data.













