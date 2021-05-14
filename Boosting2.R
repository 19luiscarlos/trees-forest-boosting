library(h2o)
#library(plot3Drgl) # para trellis de dependencia parcial

h2o.init(ip='localhost', nthreads=-1,
         min_mem_size='7G', max_mem_size='8G')
h2o.removeAll() 

# Cargar y adaptar datos:
cali = h2o.importFile('cali.csv',
                      sep = ',', header = T)
y = "MedianHouseValue"
x = setdiff(names(cali), y)

seeds = c(652, 13, 27, 450, 199)
a = c(0.1, 0.8, 0.3, 0.8, 0.5, 0.9, 0.9, 0.2)
J = c(4, 8, 3, 7, 3, 6, 4, 6)
t = c(2250, 1750, 2250, 1750, 2250, 1750, 2250, 1750)

# gbm_hub_fits = rep(list(rep(list(0), 7)), 5)

t1 = proc.time()
for (i in 5) { # 1 2 3 4
  for (j in 8) { # 
    gbm_hub_fits[[i]][[j]] <- h2o.gbm(x = x, y = y,
                                      training_frame = cali,
                                      nfolds = 5, 
                                      ntrees = t[j],
                                      distribution = 'huber',
                                      huber_alpha = a[j],
                                      max_depth = J[j],
                                      learn_rate = 0.099,
                                      stopping_rounds = 5,
                                      stopping_tolerance = 1e-3,
                                      stopping_metric = "MSE",
                                      seed = seeds[i])
  }
}
(proc.time() - t1) #14:56
#El octavo 38, 37, 35, 37, 36.5
#El séptimo 14.3, 11.63, 10.85
#El sexto 23 min, 23.7, 22.5, 29
#El quinto 9 min, 8 min, 8.45, 9
#El cuarto 33 min, 34, 36, 37
#El tercero 21 min, 9.3, 8, 9
#El segundo 52 min, 1hr 23, 1hr 18, 1hr 19
#El primero 38 min, 13min, 16min

# Tabla resumen
hub_table <- matrix(nrow = 8, ncol = 6)
colnames(hub_table) = c('J', 'alpha', 'Arbol', 'Train', 'Test', 'Test sd')

for (j in 1:8) {
  hub_table[j, 1] = gbm_hub_fits[[1]][[j]]@allparameters$max_depth
  hub_table[j, 2] = gbm_hub_fits[[1]][[j]]@allparameters$huber_alpha
  hub_table[j, 3] = gbm_hub_fits[[1]][[j]]@allparameters$ntrees
  class(hub_table) = "numeric"
}

# Promedio de las iteraciones
# Train error, test error, sd error
hub_trn <- matrix(nrow = 8, ncol = 5)
hub_tst <- matrix(nrow = 8, ncol = 5)
hub_sd <- matrix(nrow = 8, ncol = 5)
for (i in 1:8) {
  for (j in 1:5) {
    hub_trn[i, j] <- gbm_hub_fits[[j]][[i]]@model$training_metrics@metrics$MSE
    hub_tst[i, j] <- gbm_hub_fits[[j]][[i]]@model$cross_validation_metrics_summary$mean[3]
    hub_sd[i, j] <- gbm_hub_fits[[j]][[i]]@model$cross_validation_metrics_summary$sd[3]
  }
}
class(hub_trn) = "numeric"
hub_table[, 4] = round(rowMeans(hub_trn), 4)
class(hub_tst) = "numeric"
hub_table[, 5] = round(rowMeans(hub_tst), 4)
class(hub_sd) = "numeric"
hub_table[, 6] = round(rowMeans(hub_sd), 4)

(hub_table = hub_table[order(hub_table[,1], hub_table[,2], decreasing = T),])
write.csv(hub_table, 
          file = "/Users/luiscarlos/Documents/Tesis/Ejemplos/California/Tables/Appen1_gbm_hub.csv",
          row.names = T)

matplot(1:8, hub_table[,5:4],
        col = "#e6efff22", pch=1, type="b", lty = 3, cex = .1,
        ylab = "Error", xlab = "", xaxt = "n")
matlines(1:8, hub_table[,5:4], lty = 1,
         col = c("#00b3b3", "#e60073"))
matpoints(1:4, hub_table[1:4, 5:4], pch = 19, cex = 1.2,
          col = c("#00b3b3", "#e60073"))
matpoints(5:8, hub_table[5:8, 5:4], pch = 17, cex = 1.2,
          col = c("#00b3b3", "#e60073"))
legend("bottomright", legend = c("Prueba, J > 5", "Prueba, J < 5", 
                                 "Entrenamiento, J > 5", "Entrenamiento, J < 5"), 
       col = c("#00b3b3", "#00b3b3", "#e60073", "#e60073"), 
       pch = c(19, 17, 19, 17), cex = 1.05, bty = "n")


h2o.varimp_plot(hub_fits1[[1]])
h2o.partialPlot(hub_fits1[[1]], data = cali, cols = "MedInc")
h2o.partialPlot(hub_fits1[[1]], data = cali, cols = "Longitude")
h2o.partialPlot(hub_fits1[[1]], data = cali, cols = "Latitude")
h2o.partialPlot(hub_fits1[[1]], data = cali, cols = "HouseAge")
h2o.partialPlot(hub_fits1[[1]], data = cali, cols = "Population")
h2o.partialPlot(hub_fits1[[1]], data = cali, cols = "AveBedrms")
h2o.partialPlot(hub_fits1[[1]], data = cali, cols = "AveRooms")

t1 = proc.time()
h2o.partialPlot(hub_fits1[[1]], data = cali, 
                col_pairs_2dpdp = list(c("Longitude", "Latitude")))
(proc.time() - t1)



t1 <- proc.time()
gbm_hub_fits_pred <- h2o.gbm(x = x, y = y,
                                  training_frame = cali,
                                  nfolds = 5, 
                                  ntrees = t[6],
                                  distribution = 'huber',
                                  huber_alpha = a[6],
                                  max_depth = J[6],
                                  learn_rate = 0.099,
                                  stopping_rounds = 5,
                                  stopping_tolerance = 1e-3,
                                  stopping_metric = "MSE",
                                  keep_cross_validation_predictions = TRUE,
                                  seed = seeds[1])
(proc.time() - t1)
#21:08
gbm_preds <- h2o.cross_validation_predictions(gbm_hub_fits_pred)
# 
# plot(as.data.frame(gbm_preds[[1]]), 
#      type = 'l', col = '#00b3b3',
#      ylab = 'Error de clasificación', xlab = 'Número de árboles')
# lines(rf1.1@model$scoring_history$validation_classification_error[1:985],
#       col = '#e60073')
# legend("topright", legend = c('Boosting', 'Random Forest'), 
#        pch=19, cex=1, bty = "n", col=c('#00b3b3', '#e60073'))


predicciones <- matrix(nrow = dim(cali)[1], ncol = length(gbm_preds) + 1)
for (i in 1:length(gbm_preds)) {
  predicciones[,i] <- as.vector(gbm_preds[[i]])
}
for (i in 1:dim(cali)[1]) {
  predicciones[i,6] <- sum(predicciones[i,1:5])
}
plot(predicciones[,6], type = 'l')
write.csv(head(predicciones,5), file = "predicciones_cali.csv")

t1 <- proc.time()
rf_pred <- h2o.randomForest(x = x, y = y,
                          training_frame = cali,
                          mtries = 3,
                          ntrees = 1000,
                          nfolds = 5,
                          keep_cross_validation_predictions = TRUE,
                          seed = 19)
(proc.time() - t1)
# 12:07

rf_preds <- h2o.cross_validation_predictions(rf_pred)

predicciones_rf <- matrix(nrow = dim(cali)[1], ncol = length(rf_preds) + 1)
for (i in 1:length(rf_preds)) {
  predicciones_rf[,i] <- as.vector(rf_preds[[i]])
}
for (i in 1:dim(cali)[1]) {
  predicciones_rf[i,6] <- sum(predicciones_rf[i,1:5])
}
plot(predicciones_rf[,6], type = 'l')

comparacion <- cbind('original' = as.vector(cali[y]), 
                          'boosting' = predicciones[,6], 
                          'forest' = predicciones_rf[,6])

comparacion <- comparacion[order(comparacion[,'original']), ]

plot(comparacion[,2], type = 'p', col = '#0097fc', cex = 0.007, pch = '.',
     xlab = '', ylab = 'Mediana del valor de las casas/10000')
points(comparacion[,3], col = '#ff8d1a', cex = 0.04)
lines(comparacion[,1], col = '#424a4a', lwd = 1.6)
legend("topleft", legend = c('Boosting', 'Random Forest'), 
       pch=19, cex=1, bty = "n", col=c('#0097fc', '#ff8d1a'))

sd(comparacion[,3])
sd(comparacion[,2])

# save.image("3Huber.RData")
# h2o.shutdown()
# y

