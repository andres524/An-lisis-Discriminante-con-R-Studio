#ANÁLISIS DISCRIMINANTE APLICADO A LA CLASIFICACIÓN DE AUTOS POR MARCA 
#PRESENTADO POR: ANDRÉS GUILLERMO ANGARITA MONROY - LUZ MARINA DELGADO MONROY

#¿QUÉ ES ANÁLISIS DISCRIMINANTE?

#El Análisis Discriminante Lineal o Linear Discrimiant Analysis (LDA) es un método de clasificación supervisado de variables cualitativas en el que dos o más grupos
#son conocidos a priori y nuevas observaciones se clasifican en uno de ellos en función de sus características. Haciendo uso del teorema de Bayes, LDA estima la 
#probabilidad de que una observación, dado un determinado valor de los predictores, pertenezca a cada una de las clases de la variable cualitativa,P(Y=k | X=x).
#Por otro lado, e crea un modelo predictivo para la pertenencia al grupo. El modelo está compuesto por una función discriminante (o, para más de dos grupos, un 
#conjunto de funciones discriminantes) basada en combinaciones lineales de las variables predictoras que proporcionan la mejor discriminación posible entre los 
#grupos. Las funciones se generan a partir de una muestra de casos para los que se conoce el grupo de pertenencia; posteriormente, las funciones pueden ser aplicadas
#a nuevos casos que dispongan de mediciones para las variables predictoras pero de los que se desconozca el grupo de pertenencia.

#CONSIDERACIÓN DE LOS DATOS SOBRE EL ANÁLISIS DISCRIMINANTE

#DATOS
#La variable de agrupación debe tener un número limitado de categorías distintas, codificadas como números enteros. Las variables independientes que sean nominales 
#deben ser recodificadas a variables auxiliares o de contraste.

#SUPUESTOS
#Los casos deben ser independientes. Las variables predictoras deben tener una distribución normal multivariada y las matrices de varianzas-covarianzas intra-grupos 
#deben ser iguales en todos los grupos. Se asume que la pertenencia al grupo es mutuamente exclusiva (es decir, ningún caso pertenece a más de un grupo) y exhaustiva 
#de modo colectivo (es decir, todos los casos son miembros de un grupo). El procedimiento es más efectivo cuando la pertenencia al grupo es una variable verdaderamente 
#categórica; si la pertenencia al grupo se basa en los valores de una variable continua (por ejemplo, un cociente de inteligencia alto respecto a uno bajo), considere
#el uso de la regresión lineal para aprovechar la información más rica ofrecida por la propia variable continua.



#Paso 1. Instalar las librerías
library(ggplot2)
library(ggpubr)
library(MASS)
library(reshape2)
library(knitr)
library(dplyr)
library(MVN)
library(biotools)
library(klaR)
library(readr)
library(splitstackshape)


#Paso 2. Cargara la data
data <- read.csv("C:\\Users\\ANDRES ANGARITA\\Desktop\\TRABAJO FINAL ANALISIS MULTIVARIANTE\\ANALISIS DISCRIMINANTE\\Base de datos_clasificacion_autos.csv", sep = ";")
View(data)

#Paso 3. Análisis preliminar y comprobacion de supuestos

#Exploración gráfica de los datos

plot1 <- ggplot(data = data, aes(x = consumo.millas.por.galon.)) +
  geom_density(aes(colour = marca)) + theme_bw()
plot2 <- ggplot(data = data, aes(x = cilindradje)) +
  geom_density(aes(colour = marca)) + theme_bw()
plot3 <- ggplot(data = data, aes(x = HP)) +
  geom_density(aes(colour = marca)) + theme_bw()
plot4 <- ggplot(data = data, aes(x = peso)) +
  geom_density(aes(colour = marca)) + theme_bw()
plot5 <- ggplot(data = data, aes(x = aceleracion)) +
  geom_density(aes(colour = marca)) + theme_bw()
plot6 <- ggplot(data = data, aes(x = modelo)) +
  geom_density(aes(colour = marca)) + theme_bw()

# la función grid.arrange del paquete grid.extra permite ordenar
# graficos de ggplot2
ggarrange(plot1, plot2, plot3, plot4,plot5,plot6, common.legend = TRUE, legend = "bottom")

pairs(x = data[, -7], col = c("firebrick", "green3", "blue")[data$marca],
      pch = 20)

#Normalidad univariante, normalidad multivariante y homogeneidad de varianza

#Distribución de los predictores de forma individual:
##Representación mediante histograma de cada variable para cada marca

par(mfcol = c(3, 4))
for (k in 1:6) {
  j0 <- names(data)[k]
  x0 <- seq(min(data[, k]), max(data[, k]), le = 50)
  for (i in 1:4) {
    i0 <- levels(data$marca)[i]
    x <- data[data$marca == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("marca", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}

# (1) Normalidad Univariante y Multivariante

#Contraste de normalidad Shapiro-Wilk para cada variable en cada marca
ShapiroWilks <- melt(data, value.name = "valor")
aggregate(formula = valor ~ marca + variable, data = ShapiroWilks,
          FUN = function(x){shapiro.test(x)$p.value})


#Pruebas de normalidad univariante y multivariante
#realizados a través de los tres test de hipótesis (Henze-Zirkler, Royston y Mardia)

hz_test <- mvn(data = data[,-7], mvnTest = "hz")
hz_test$univariateNormality
hz_test$multivariateNormality


royston_test <- mvn(data = data[,-7], mvnTest = "royston", multivariatePlot = "qq")
royston_test$univariateNormality
royston_test$multivariateNormality


outliers <- mvn(data = data[,-7], mvnTest = "mardia", multivariateOutlierMethod = "quan")
outliers$univariateNormality
outliers$multivariateNormality


#(2) Homogeneidad en la matriz de covarianza
boxM(data = data[, -7], grouping = data[, 7])


#Una vez comprobado los supuestos de Normalidad multivariante;homogeneidad de matrices de varianza- covarianza, 
#linealidad y ausencia de multicolinealidad, se procede a dividir la data en dos conjuntos: Entrenamiento y Prueba

#Paso 4. División de la data ingresada en un conjunto de entrenamiento y eun conjunto de prueba

estratificacion <- stratified(indt = data,group = "marca",size = 0.3,bothSets = T)

#Data de validación
test_data <- data.frame(estratificacion[[1]])
View(test_data)

#Data de entrenamiento
entrenamiento_data <- data.frame(estratificacion[[2]])
View(entrenamiento_data)


#Paso 5. Calculo de la función discriminante

modelo_lda <- lda(marca ~ consumo.millas.por.galon. + cilindradje + HP +
                    peso + aceleracion + modelo, data = entrenamiento_data)
modelo_lda

#Primera funcion discriminante
modelo_lda$scaling[,1]

#segunda funcion discriminante
modelo_lda$scaling[,2]


#tercera funcion discriminante
modelo_lda$scaling[,3]


#Con la obtención de las funciones discriminantes, se procede a aplicarlas en la data 
#que se encuentran en el grupo de testeo.


#Paso 6. Predicción con el conjunto de datos de testeo

Prediccion <- predict(object = modelo_lda, newdata = test_data[,-7], method = "predictive")
Prediccion

#Prediccion de cada individuo en las clases
Prediccion$class



#Paso 7. Formulación de la matriz de confusion

Matriz_confusion <- table(test_data$marca, Prediccion$class,
                          dnn = c("Clase real", "Clase predicha"))
Matriz_confusion



#Paso 8. Evaluación de los errores de clasificación

trainig_error <- mean(test_data$marca != Prediccion$class) * 100
trainig_error



#Paso 9. Visualización de las clasificaciones
partimat(marca ~ consumo.millas.por.galon. + cilindradje + HP +
           peso + aceleracion + modelo,
         data = entrenamiento_data, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2","green"),
         col.mean = "firebrick")

