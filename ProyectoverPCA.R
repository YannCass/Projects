rm(list = ls(all.names = T))

library(corrplot) 
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(mice)
library("FactoMineR")
library("factoextra")

# Cargamos los datos

datos <- read.csv("Base_diab2.csv")
attach(datos)

# Estructura de la base
str(datos)

# Cambiar el tipo de dato de sex como factor
datos$sex = as.factor(datos$sex)

# Variables ####

# fastgb.- Variable de tipo entero representa a la glucosa en ayuno.
# p2gb.- Variable de tipo entero representa la post carga de 2hrs de glucosa.
# tcholb.- Variable de tipo entero representa el colesterol total.
# tgb.- Tricgliceridos totales es una variable de tipo entero.
# hdl.- Lipoproteinas de alta densidad, variable de tipo entero.
# peso.- El peso del individuo/observacion es de tipo numerico.
# talla.- Altura del individuo es de tipo numerico.
# bm1.- Indice de masa corporal es de tipo numerico.
# sex.- El sexo del individuo es de tipo factor.1 para hombre y 2 mujer.

# Datos Faltantes ####

plot_missing(datos)
# Podemos observar que las variables talla, peso, bm1 y hdl tienen menos del 3%
# de datos faltantes mientras que la variable de la post carga de 2hrs de la
# glucosa tiene un 8.3% de datos faltantes.

# Como la variable p2gb tiene mas del 3% de datos faltantes vamos a usar la
# paqueteria mice para imputar dichos valores.

apply(X = is.na(datos), MARGIN = 2, FUN = sum)
# Podemos observar que 3 indivudos tienen valores faltantes en hdl, 2 tienen 
# valores faltantes en bm1 y un individuo tiene un NA en talla y otro tiene un 
# NA en peso.

aux <- mice(datos)
datosC <- complete(aux)

plot_missing(datosC)
# Podemos observar que ya no tenemos ningun valor faltante, para las variables
# que tenian menos del 3% de NA's mice tambien les imputo datos, sin embargo no
# hay mucha diferencia si las hubieramos quitado y luego aplicamos mice dado que
# solo eran 3 observaciones con al menos un dato faltante.

# Analisis Descriptivo ####

summary(datosC)
# Podemos observar que nuestra base ya no contiene datos faltantes, para la 
# variable fastgb podemos observar que el promedio de la glucosa en ayuno es de
# 97.61, el menor numero de glusoca en ayuno que tienen nuestros individuos es 
# de 41 mientras que la glucosa en ayuno mas alta es de 518 vemos que este esta
# por arriba del doble de la media. Pasa algo similar con la post carga de 2hrs 
# de glucosa pues su promedio es de 133, su minimo de 30 y el valor maximo es de
# 697. Para el colesterol total tenemos que en promedio nuestros individuos tienen
# un colesterol total de 191.3, con un colesterol total maximo de 450 y un minimo
# de 87 vemos que en esta variable tenemos valores muy grandes por arriba del
# segundo cuantil.
# En promedio tenemos que las lipoproteinas de alta densidad es de 32.22, su valor
# mas pequeno es de 12 y la lipoproteina de alta densidad que se tiene es de 79.
# Para el peso vemos que en promedio nuestros individuos tienen un peso de 68.76,
# vemos que el peso mas bajo es de 34 y el maximo de 128, en promedio la talla 
# es de 156.4, la talla mas pequena es de 131.3 y la mas grande de 185 tiene 
# sentido dado que el peso debe estar relacionado con la talla que tengamos.
# bm1 es el indide de masa corporal, que se obtiene de dividir el peso entre la
# talla en metros cuadrados. Finalmente vemos que tenemos 827 individuos hombres
# y 1172 individuos que son mujeres. Mas o menos tenemos equidad en nuestros 
# datos.

#  Analisis de las variables de forma individual

ggplot(data = datosC, aes(x = "", y = fastgb)) + 
  geom_boxplot(fill = "darkslategray3", outlier.colour = "black",alpha = 0.9) + 
  ylab("fastgb")
# Vemos que tenemos muchos valores atipicos por arriba de nuestro tercer cuantil
# y dos valores atipicos por debajo del primer cuantil, parece ser que los valores
# que van del primer cuantil a la media y de la media al tercer cuantil son
# simetricos.

ggplot(data = datosC, aes(x = "", y = p2gb)) + 
  geom_boxplot(fill = "lightpink1", outlier.colour = "black",alpha = 0.9) + 
  ylab("p2gb")
# La post carga de glucosa de 2hrs solo presenta valores atipicos por arriba del
# tercer cuantil y podemos observar que muchos de ellos estan muy juntos sin
# embargo hay dos que le alejan de los otros. 

ggplot(data = datosC, aes(x = "", y = tcholb)) + 
  geom_boxplot(fill = "greenyellow", outlier.colour = "black",alpha = 0.9) + 
  ylab("tcholb")
# Para el colesterol total tenemos valores atipicos arriba del tercer cuantil y
# podemos ver que hay muchos que estan muy juntos pero pasa lo mismo que con la
# variable anterior hay dos datos que son mucho mas altos que los demas, en este
# caso parece ser que tenemos mas valores por encima del promedio.

ggplot(data = datosC, aes(x = "", y = tgb)) + 
  geom_boxplot(fill = "salmon1", outlier.colour = "black",alpha = 0.9) + 
  ylab("tgb")
# Los trigliceridos totales tambien muestra valores atipicos por arriba del 
# tercer cuantil, sin mebargo estan un poco mas dispersos que las ultimas dos 
# variables anteriores. Se ve que hay un valor mucho mas grande y parece ser que
# tenemos mas valores por arriba del promedio.

ggplot(data = datosC, aes(x = "", y = hdl)) + 
  geom_boxplot(fill = "orangered", outlier.colour = "black",alpha = 0.9) + 
  ylab("hdl")
# Las lipoproteinas de alta densidad igual presentan valores atipicos pero mucho
# menos que las variables anteriores estos igual estan por arriba del promedio.
# Estos valores igual estan mas dispersos y tenemos un valor que se aleja mas.

ggplot(data = datosC, aes(x = "", y = peso)) + 
  geom_boxplot(fill = "red", outlier.colour = "black",alpha = 0.9) + 
  ylab("peso")
# Vemos que para el peso tnemos valores atipicos tanto por arriba como por
# debajo del tercer y primer cuantil. Hay un valor que se va hasta 128 y otro
# por debajo de 35.

ggplot(data = datosC, aes(x = "", y = talla)) + 
  geom_boxplot(fill = "deeppink1", outlier.colour = "black",alpha = 0.9) + 
  ylab("talla")
# En el caso de la talla solo tenemos un valor atipico que esta en 185, parece 
# ser que entre el tercer cuantil a la media como de la media al primer cuantil
# se tiene la misma cantidad de datos.

levels(datosC$sex) <- c("Hombre","Mujer")
Freq <- table(datosC$sex)/length(datosC$sex)

barplot(Freq, main = "Proporcion de Hombres y Mujeres ",
        xlab = "Sexo", ylab = "Frecuencia",
        col = c("#B0E2FF", "plum1"))
# Notamos que de nuestra poblacion poco mas del 50% son mujeres y el 40% son
# hombres.

# Analisis de las variables en conjunto

corrplot(cor(datosC[,-9:-10]))
# Podemos observar que no tenemos correlaciones tan fuertes entre las variables, 
# para p2gb y fastgb tenemos una correlacion positiva alta dado que son 
# variables que miden la glucosa, otro ejemplo es el peso con el indice de masa
# corporal y esto es mas que evidente dado que el bm1 se calcula usando el peso
# y la talla. Otras variables que presentan una correlacion positiva media alta
# es el peso y talla esto tambien es evidente porque tu peso debe estar acorde
# a la estaura que tienes.

ggplot(data = datosC, aes(x = sex, y = fastgb)) + 
  geom_boxplot(fill = c("#00BFFF","#EE6AA7"), outlier.colour = "black",alpha = 0.9) + 
  ylab("fastgb")
# Parece ser que si separamos la glucosa en ayuno por sexo no hay mucha 
# diferencia, vemos que el promedio de ambos esta en el mismo rango de valores
# aunque parece ser que hay mas valores entre el primer cuantil y el segundo 
# cuantil y del segundo cuantil al tercer cuantil en el caso de las mujeres. Otra
# cosa es que hay valores atipicos muy altos para los hombres y tiene valores 
# atipicos por debajo de su primer cuantil, sin embargo las mujeres solo tienen 
# valores atipicos por arriba de su tercer cuantil y se ven mas juntos que el de
# los hombres.

ggplot(data = datosC, aes(x = sex, y = p2gb)) + 
  geom_boxplot(fill = c("#EEDC82","#FFAEB9"), outlier.colour = "black",alpha = 0.9) + 
  ylab("p2gb")
# Parece que en promedio las mujeres tienen una post carga de glucosa mas alta 
# que la de los hombres y tiene valores atipicos muy agrupados. Mientras que los 
# hombres vuelven a tener dos valores atipicos muy marcados.

ggplot(data = datosC, aes(x = sex, y = tcholb)) + 
  geom_boxplot(fill = c("#CDCDB4","#E0FFFF"), outlier.colour = "black",alpha = 0.9) + 
  ylab("tcholb")
# Igual parece que el promedio de colesterol total es el mismo para ambos sexos, 
# algo importante a notar esque parece que hay mas valores entre la media y el
# tercer cuantil en el caso de las mujeres. Ambos sexos tienen valores atipicos 
# pero las mujeres muestran mas valores atipicos y tienen dos que sobresalen de 
# los demas.

ggplot(data = datosC, aes(x = sex, y = tgb)) + 
  geom_boxplot(fill = c("#2E8B57","#8B5F65"), outlier.colour = "black",alpha = 0.9) + 
  ylab("tgb")
# Aqui si podemos ver que en promedio los tricgliceridos totales de los hombres 
# son mas altos que el de las mujeres. Ademas, los hombres muestran valores
# atipicos mas dispersos y mas altos en comparacion con las mujeres.

ggplot(data = datosC, aes(x = sex, y = hdl)) + 
  geom_boxplot(fill = c("mediumaquamarine","#FFE1FF"), outlier.colour = "black",alpha = 0.9) + 
  ylab("hdl")
# Para este caso en promedio las lipoproteinas de alta densidad son mas altas en 
# las mujeres que en los hombres. Ademas, las mujeres presentan mayor numero de
# valores atipicos que los hombres e incluso son mas dispersos.

ggplot(data = datosC, aes(x = sex, y = peso)) + 
  geom_boxplot(fill = c("sandybrown","powderblue"), outlier.colour = "black",alpha = 0.9) + 
  ylab("peso")
# Podemos observar que el peso promedio de los hombres es mas grande que el de 
# las mujeres, ademas comparando los pesos maximos y minimos el de los hombres 
# es mayor que el de las mujeres, esto pasa ya que por lo general los
# hombres tienden hacer mas robustos que las mujeres. 
# Se observa que los hombres tienen mas valores atipicos por arriba del tercer
# cuantil que las mujeres, sin embargo estas tienen un valor por debajo de su 
# primer cuantil.

ggplot(data = datosC, aes(x = sex, y = talla)) + 
  geom_boxplot(fill = c("aliceblue","beige"), outlier.colour = "black",alpha = 0.9) + 
  ylab("talla")
# Pasa lo mismo que con el peso, las alturas de los hombres son mayores que el 
# de las mujeres. Ademas vemos que el promedio de la talla de los hombres es 
# mayor que el promedio de tallas de las mujeres. Se observa que en las tallas 
# de los hombres tenemos valores atipicos por arriba del primer y tercer cuantil,
# esto se ve igual en las mujeres. Aunque tenemos un poco mas de valores atipicos
# en los hombres. 

# Matriz de covarianzas y de correlaciones 
var(datosC[,-9:-10])
cor(datosC[,-9:-10])

# Ahora vamos a dividir a la poblacion en hombres y muejres para analizarlos por
# separado.

# Creamos las bases de datos separando por sexo.
mujeres <- subset(datosC, sex == "Mujer")
hombres <- subset(datosC, sex == "Hombre")

# Medias
apply(datosC[,-9:-10], 2, mean)

apply(hombres[,-9:-10], 2, mean)

apply(mujeres[,-9:-10], 2, mean)
# Vemos que si obtenemos la media por separado de hombres y mujeres, las muejeres
# tienen mayor media en la glucosa en ayuno, masa corporal, post carga de 2hrs 
# de glucosa y lipoproteinas de alta densidad. Sin embargo los hombres en promedio
# tienen mayor colesterol total, trigliceridos totales, peso y talla.

# Varianzas 
apply(datosC[,-9:-10], 2, var)

apply(hombres[,-9:-10], 2, var)

apply(mujeres[,-9:-10], 2, var)
# Dado que nuestraas variables no estan escaladas nuestras varianzas son muy 
# grandes, ahora los hombres tienen mayor varianza en la post carga de 2hrs de
# glucosa y las mujeres tienen mayor varianza en los trigliceridos totales.

# Dado el analisis descriptivo decidimos quitar las variables peso y talla ya que
# estan se pueden obtener con el indice de masa corporal. 
# Otra cosa importante que nos muestra el analisis es que aunque cruzamos todas
# las variables con el sexo solo el peso y talla mostraron diferencias 
# significativas, sin embargo las medidas bioquimicas no mostraron mucha diferencia
# Asi que realizaremos un analisis de componentes principales con toda la muestra
# quitando unicamente las variables peso y talla.

# Analisis de Componentes Principales ####

datosC = datosC[,-6:-7]

pca <- prcomp(datosC[,-7:-8], scale = TRUE)

pca$center
pca$scale
# "center" y "scale" corresponden a las medias y desviaciones estándar de las
# variables que se utilizaron para escalar antes de implementar PCA.

# Podemos graficar a los primeros dos componentes principales:
biplot(pca, scale = 0)

summary(pca)$importance
# Notamos que con 5 componentes podemos explicar el 98% de la varianza total 
# de los datos totales.

# Podemos graficar la proporción de varianza explicada por cada componente, 
# así como la proporción de varianza acumulada, de la siguiente manera:

pcve <- (pca$sdev^2)/sum(pca$sdev^2)*100

par(mfrow = c(1, 2))
plot (pcve , xlab = " Componente Principal ", 
      ylab = " Proporción de Varianza Explicada ", 
      ylim = c(0, 100), type = "b")
plot(cumsum(pcve), xlab = " Componente Principal ", 
     ylab = " Proporción de Varianza Acumulada ", 
     ylim = c(0, 100), type = "b")
par(mfrow = c(1, 1))

#Relación entre variables:

fviz_pca_var(pca, col.var = "red")
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
cor(redes_1)

