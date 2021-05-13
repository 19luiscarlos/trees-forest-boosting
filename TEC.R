###############################################
###############################################
### Árboles de decisión en machine learning ###
###############################################
###############################################

library(rpart) # construir árboles
library(rpart.plot) # graficar árboles

cali <- read.csv('cali.csv')
summary(cali)

#### Árboles de decisión ####

# Sumas de cuadrados
vals.unicos <- sort(unique(cali[,"MedInc"])) # Valores únicos de MedInc, ordenados
puntos.corte <- (vals.unicos[-1] + vals.unicos[-length(vals.unicos)])/2 # Sus puntos medios
cuadrados <- rep(NA, length(puntos.corte))

for(i in 1:(length(puntos.corte))) {
  spl <- puntos.corte[i]
  izq  <- cali[,"MedInc"] < spl # las que están a la izquierda del punto de corte
  der <- !izq # las de la derecha
  cuadrados[i] <-
    sum((cali[izq, "MedianHouseValue"]  - mean(cali[izq, "MedianHouseValue"]))^2) +
    sum((cali[der, "MedianHouseValue"] - mean(cali[der, "MedianHouseValue"]))^2)
}

plot(puntos.corte, cuadrados, pch = 16, cex = 0.2, col = '#00b3b3',
     xlab = "Puntos de corte de MedInc", ylab = "Sumas de cuadrados")
puntos.corte[cuadrados==min(cuadrados)] # 5.04
# Ubicación del punto mínimo:
points(puntos.corte[cuadrados==min(cuadrados)], min(cuadrados),
       col = "#e84712", pch = 16, cex = 0.8)

# Construcción y gráficas de árboles
# minbucket indica el número mínimo de observaciones en los nodos finales
# Un árbol poco profundo tiene más observaciones en los nodos finales
arbol1 <- rpart(MedianHouseValue ~ ., data = cali, method = "anova",
                minbucket = 4000, cp = 0)
rpart.plot(arbol1, compress = TRUE, type = 5, fallen.leaves = FALSE, 
           box.palette = "Greens")

# Un árbol más profundo tiene menos observaciones en los nodos finales
arbol2 <- rpart(MedianHouseValue ~ ., data = cali, method = "anova", 
                minbucket = 2000, cp = 0) # 800
rpart.plot(arbol2, compress = TRUE, type = 5, fallen.leaves = FALSE, 
             box.palette = "Greens", cex = 0.6)

# Varianza y sesgo
# Probaremos diferentes profundidades; entre más profundo, más complejo es el árbol
n <- nrow(cali)
(min_sizes <- c(seq(from = 100, to = 10, by = -10), 5, 1))

# Tablitas para registrar los errores 
error.train <- matrix(nrow = 100, ncol = length(min_sizes))
error.test <- error.train

# Seeds para reproducir resultados 
set.seed(19); seeds <- sample(999999, size = 100, replace = FALSE)

# Se obtendrán 100 estimaciones de errores, los cuales se promediarán
t <- proc.time()
for (j in 1:100) {
  # Se usará repeated training-test, 
  # dividiendo en 2/3 para entrenar y 1/3 para evaluar
  set.seed(seeds[j]); test <- sample(n, size = n/3, replace = FALSE)
  
  for (i in 1:length(min_sizes)) {
    # Se ajusta el árbol con datos de entrenamiento para predecir MedianHouseValue
    arbol <- rpart(MedianHouseValue ~ ., data = cali[-test, ], method = "anova",
                   minbucket = min_sizes[i], cp = 0)
    
    # Se calculan predicciones para esa variable...
    preds.train <- predict(arbol, newdata = cali[-test, ])
    preds.test <- predict(arbol, newdata = cali[test, ])
    
    # ...y son comparadas con los valores reales por medio del ECM
    error.train[j, i] <- mean((preds.train - cali[-test, 'MedianHouseValue'])^2)
    error.test[j, i] <- mean((preds.test - cali[test, 'MedianHouseValue'])^2)
  }
}
(proc.time() - t) #16 min. con 39 s.

# Calculamos el promedio de ambos tipos de error
media.train <- colMeans(error.train)
media.test <- colMeans(error.test)

# Parámetros para graficar:
plot.min <- min(cbind(error.test, error.train))
plot.max <- max(cbind(error.test, error.train))

# Gráfica del error a través de la complejidad
# una complejidad baja implica árboles poco profundos
# una alta implica más profundos
# Primero, las medias
plot(media.train, type = "l", col = "#44aef4", lwd = 2.2,
     ylim = c(plot.min, plot.max), xlim = c(1.5,11.5), 
     ylab = "Error", xlab = "", xaxt = "n")
lines(media.test, type = "l", col = "#e54485", lwd = 2.2)
# Después, las 100 repeticiones
for (i in 1:100) {
  lines(error.train[i,], col = "#44aef4", lwd = .06)
  lines(error.test[i,], col = "#e54485", lwd = .06)
}
# Leyendas:
text(x = c(3, 10),  par("usr")[3], 
     labels = c("Complejidad baja (árboles menos profundos)",
                "Complejidad alta (árboles más profundos)"), pos = 1, xpd = TRUE)
legend("bottomleft", legend = c("Test", "Train"), col = c("#e54485", "#44aef4"),
       pch=19, cex=1, bty = "n")
legend(x = 0.5, y = 0.6, legend = "Sesgo alto, varianza baja",
       bty = "n")
legend(x = 8, y = 0.6, legend = "Sesgo bajo, varianza alta",
       bty = "n")


# save.image("TEC.RData")


### Toy example ####
toy <- data.frame("Horas" = c(6, 25, 15, 10, 20, 10, 2, 30),
                  "Edad" = c(22, 23, 50, 18, 25, 48, 40, 28),
                  "Luismi" = c(0, 1, 2, 1, 0, 1, 2, 1),
                  "Rupaul" = c(151, 1.5, 53, 62, 140, 2, 2.5, 141))
toy[,"Luismi"] <- as.factor(toy[,"Luismi"])

arbol3 <- rpart(Rupaul ~ ., data = toy[c(7, 1, 7, 1, 3, 8, 5, 3),], 
                method = "anova", minbucket = 1, cp = 0)
rpart.plot(arbol3, compress = TRUE, type = 5, fallen.leaves = FALSE, 
           box.palette = "Greens")

res1 <- data.frame("Horas" = c(6, 25, 15, 10, 20, 10, 2, 30),
                  "Edad" = c(22, 23, 50, 18, 25, 48, 40, 28),
                  "Luismi" = c(0, 1, 2, 1, 0, 1, 2, 1),
                  "Rupaul" = c(151, 1.5, 53, 62, 140, 2, 2.5, 141) - 
                    mean(c(151, 1.5, 53, 62, 140, 2, 2.5, 141)))
res1[,"Luismi"] <- as.factor(res1[,"Luismi"])

arbol3 <- rpart(Rupaul ~ ., data = res1, 
                method = "anova", minbucket = 2, cp = 0)
rpart.plot(arbol3, compress = TRUE, type = 5, fallen.leaves = FALSE, 
           box.palette = "Greens")
