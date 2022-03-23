##########################################################################
############--------------Universidad ORT Uruguay---------------##########
############-----Obligatorio Machine Learning NO Supervisado----##########
############----------------Prof. Damian Coltzau----------------##########
##########################################################################

#=========================================================================
#**********
# Preámbulo 
#**********

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
setwd("C:/Users/Cecilia Machado/Desktop/Machine")

# Cargamos librerias a utilizar
library(factoextra) 
library(class)

# Cargamos el archivo "Pandemia.csv"
p = read.csv('pandemia.csv')

# Visualizamos los datos
View(p)
head(p)

# Resumen de datos más importantes de todas las dimensiones
summary(p)

# Verificamos si los datos están escalados 

colMeans(p)
apply(p, 2, var)

# Hay dimensiones sin escalar. Se normalizan

pscaled = scale(p)
pscaled

# Confirmamos normalización 
colMeans(pscaled)
apply(pscaled, 2, var) 

# Se confirma normalización ya que todas las desviaciones son = 1 

# Fin del preambulo
#====================================================================

# *************
# Introducción
# *************

# Se aplicarán métodos de selección de variables para depurar la base de datos de aquellas dimensiones que puedan
# ocasionar distorsiones en la futura clusterización. 


# **********************
# Selección de variables
# **********************

# Visualizamos los datos para observar si tienen algún tipo de estructura a simple vista

pairs(pscaled, col = "brown3", main = "Estructura de Pandemia.csv", pch = 20)

# Se observa que hay estructura en algunas dimensiones
# pero es necesario eliminar aquellas que presentan una estructura
# Uniforme y pueden ocasionar clusters distorsionados

##################################
# Aplicación de Modelo de Hopkins#
##################################

# Verificamos el coeficiente con todas las dimensiones

n = 50 # cantidad total de dimensiones de la base de datos

Hop_Total = get_clust_tendency(pscaled, n, graph = FALSE)
Hop_Total

# Verificamos el modelo sacando dimensiones de a una, de esta manera vemos si el coeficiente mejora

v = integer(6)

for(i in 1:6) {
  ni = -1*i
  qi = get_clust_tendency(pscaled[,ni ], n, graph = FALSE)
  v[i]=qi
}
v

# Sancando la dimensión 4 (ldl) se obtiene mejor coeficiente que Hop_Total

Hop_sin_4 = get_clust_tendency(pscaled[,-4], n, graph = FALSE)
Hop_sin_4


# Verificamos el modelo sacando la dimension 4 y otra más

v_4 = integer(6)
for(i in 1:6){ 
  if(i != 4) { 
    ni = -1*i
    q_4 = get_clust_tendency(pscaled[,c(-4,ni)], n, graph = FALSE)
    v_4[i]=q_4 
  }
}
v_4

# Creamos tabla comparando resultados

tabla1= cbind(v, v_4)
tabla1

# Sancando la dimensión 4 (ldl) y 1 (hemoglobina) se obtiene mejor coeficiente que Hop_sin_4

Hop_sin_4_1 = get_clust_tendency(pscaled[,c(-4,-1)], n, graph = FALSE)
Hop_sin_4_1

# Verificamos el modelo sacando la dimension 4, 1 y otra más

v_4_1 = integer(6)
for(i in 1:6){ 
  ni = -1*i
  if( (i != 1) & (i != 4)) { 
    q_4_1 = get_clust_tendency(pscaled[,c(-4,-1,ni)], n, graph = FALSE)
    v_4_1[i]=q_4_1 
  }
}
v_4_1

# Creamos tabla comparando resultados

tabla2= cbind(v, v_4, v_4_1)
tabla2

# Sancando la dimensión 4 (ldl), 1 (hemoglobina) y 2 (glucidos) se obtiene mejor coeficiente que Hop_sin_4_1

Hop_sin_4_1_2 = get_clust_tendency(pscaled[,c(-4,-1,-2)], n, graph = FALSE)
Hop_sin_4_1_2

# Verificamos el modelo sacando la dimension 4, 1, 2 y otra más

v_4_1_2 = integer(6)
for(i in 1:6){ 
  ni = -1*i
  if( (i != 1) & (i != 4) & (i != 2)) { # no es la columna 1 ni la 2 ni la 4 porque las estoy sacando
    q_4_1_2= get_clust_tendency(pscaled[,c(-4,-1, -2,ni)], n, graph = FALSE)
    v_4_1_2[i]=q_4_1_2
  }
}
v_4_1_2

# Creamos tabla comparando resultados

tabla3= cbind(v, v_4, v_4_1, v_4_1_2)
tabla3

hist()

# Sancando la dimensión 4 (ldl), 1 (hemoglobina), 2 (glucidos) y 3 (temperatura) se obtiene mejor coeficiente que Hop_sin_4_1_2

Hop_sin_4_1_2_3 = get_clust_tendency(pscaled[,c(-4,-1,-2,-3)], n, graph = FALSE)
Hop_sin_4_1_2_3

# Visualizamos el aumento del estadístico Hopkins a medida que fuimos depurando la base de datos 

tabla4 = c(Hop_Total$hopkins_stat, Hop_sin_4$hopkins_stat, Hop_sin_4_1$hopkins_stat, 
               Hop_sin_4_1_2$hopkins_stat, Hop_sin_4_1_2_3$hopkins_stat)
tabla4

plot(tabla4, xlab = "N° de estimaciones" , ylab = "Coeficiente de Hopkins", 
     main = "Evolución de estadístico de Hopkins", type = "b", col="deepskyblue4")

# ************************
# Validación de clustering
# ************************

# Base de datos depurada

pdepurada = pscaled[,c(-4,-1,-2,-3)]

#Visualizamos que haya quedado correctamente depurada
head(pdepurada)

# Buscamos cantidad de clusters óptima por método de Codo
validCodo = fviz_nbclust(pdepurada, FUNcluster = kmeans, method = 'wss', k.max = 1e1, nboot = 5e1)
validCodo 

# El método del codo parece mostrarnos que el número óptimo de clusters es 3, lo cual resulta contraintuitivo 
# con el hecho de que únicamente buscamos infectados y no infectados (2 clusters)

# Aplicación de Silueta para validar la cantidad óptima de K 
silueta = fviz_nbclust(pdepurada, FUNcluster = kmeans, method = 'silhouette', k.max = 1e1)
silueta

# El método Silueta nos muestra que el número óptimo de clusters es 2, lo cual coincide con el conocimiento
# específico mencionado anteriormente

# Aplicación de estadístico de GAP

gap = fviz_nbclust(pdepurada, FUNcluster = kmeans, method = 'gap', nboot = 1e1)
gap

# Misma situación anterior: GAP statistic nos indica que K* es 5, sin embargo esto
# no conicide con el conocimiento específico

#****************************
#Aplicación de Modelo K-Means
#****************************

# Plantamos semilla para fijar resultados

n = 100
k = 2 
w = integer(n)


for (i in 1:n) {
  set.seed(i)
  q <- kmeans(pdepurada, 2)
  w[i] <- q$tot.withinss
  
}

#Visualizamos los W de cada uno de los Guess iniciales 

valores_w = w
valores_w

#Graficamos dichos Ws

barplot(w, main = "Valor de W" , ylim=c(0,3000), col = "brown3")

#Buscamos la ubicación y valor del menor W

ubicacion_menor_w = which.min(w)
ubicacion_menor_w

valor_menor_w = min(w)
valor_menor_w

# Nos quedamos la semilla que crea un Guess inicial que minimiza W
# Con este dato aplicamos K-Means

set.seed(3)
q <- kmeans(pdepurada, 2)
q
q$tot.withinss

# Calculamos la proporcion del error bueno sobre T (cuadrados totales)

q$betweenss/q$tot.withinss

# Resultados 

clasificacion_clusters = q$cluster
clasificacion_clusters

ubicacion_centros=q$centers
ubicacion_centros

tamaño_clusters = q$size
tamaño_clusters

# El primer cluster muestra observaciones con ondas y defensas cercanas a la media
# mientras que el cluster número 2, muestra observaciones con ondas por encima de la media
# al igual que las defensas, lo cual podría indicar personas infectadas con el virus

#************************
#Aplicación de Modelo KNN
#************************

# Trabajaremos con otra base de datos que contiene nuevos individuos. 
# Intentaremos evaluar su estado(infectado/no infectados) comparandolos con los individuos ya 
# estudiados

# Cargamos el archivo "nuevos.csv"
nuevos = read.csv("nuevos.csv")

# Verificamos si los datos de "nuevos.csv" se encuentran escalados

colMeans(nuevos)
apply(nuevos, 2, var)

# Normalizamos

nscaled = scale(nuevos[,5:6])
nscaled

colMeans(nscaled)
apply(nscaled, 2, var)


# Utilizamos el modelo de KNN para integrar la base "nuevos.csv"

nuevos_casos = knn(q$centers, nscaled, 1:k, k = 1)
nuevos_casos

# Identificamos las observaciones que presentan características similares
# a las de los clusters 1 y 2 de la base "pandemia.csv"

# Visualizamos los datos con la clusterización obtenida

pairs(pdepurada, col = q$cluster, pch = 20, main= "Clusterización Ondas vs Defensas")

pairs(nscaled, col = nuevos_casos, pch = 20, main= "Clusterización Ondas vs Defensas")

# Identificamos las observaciones reclasificadas en no infectados (1) e infectados (2) 
# datos
ncasos = matrix(t(nuevos_casos))

summary(nuevos_casos)

#Fin del análisis
#===========================================================================s