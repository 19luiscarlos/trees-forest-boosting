library(h2o)
# library(plot3Drgl) # para trellis de dependencia parcial
h2o.init(ip='localhost', nthreads=-1,
         min_mem_size='7G', max_mem_size='8G')
h2o.removeAll() 

# Cargar y adaptar datos:
setwd("~/OneDrive/Tesis/Codes/California")
cali = h2o.importFile('/Users/luiscarlos/Documents/Tesis/Codes/California/cali.csv',
                      sep = ',', header = T)
y = "MedianHouseValue"
x = setdiff(names(cali), y)

# Por si se usa método de train/test:
# splits = h2o.splitFrame(data = cali, ratios = 0.8, seed = 19)
# train = splits[[1]]; test = splits[[2]]
# dim(train); dim(test)

promedio = function(df){
  df[, ncol(df) + 1] = rowMeans(df[,-1])
  colnames(df)[ncol(df)] = 'Mean'
  return(df)
}

#### Bagging/RF ####
# Para random forest solo se ajustará el parámetro de mtry
# Para bagging no se modificarán los parámetros
# Se usa la profundidad por default, i.e. 20 (probar diferentes profundidades es oneroso)
# mtry estará entre 2 y 5, i.e. alrededor del recomendado que es sqrt(p)
# El error de prueba se estimará con muestras OOB cinco veces (el promedio)

# rf_train = data.frame("mtries" = c(2:5, 8))
# rf_test = data.frame("mtries" = c(2:5, 8))
# rf_oob = data.frame("mtries" = c(2:5, 8))

# Se acaba la memoria si se hacen los grids juntos
# Tendrán que hacerse los ajustes uno por uno, mantener el for es por estilo
# Con lapply h2o no reconocía el dataframe

# rf = rep(list(rep(list(0), 5)), 5)
seeds = c(19, 78, 356, 444, 963) # i
mtry = c(2:5, 8) # j

t1 = proc.time()
for (i in 0) { # 1:5
  for (j in 0) { # 1:5
    rf[[i]][[j]] = h2o.randomForest(x = x, y = y,
                                    training_frame = cali,
                                    mtries = mtry[j],
                                    ntrees = 1000,
                                    nfolds = 5,
                                    seed = seeds[i],
                                    model_id = paste("seed", i, "_", 
                                               "mtry", mtry[j], sep = ''))
    
    rf_test[j, i+1] = rf[[i]][[j]]@model$cross_validation_metrics@metrics$MSE
    rf_oob[j, i+1] = rf[[i]][[j]]@model$training_metrics@metrics$MSE
    rf_train[j, i+1] = h2o.performance(rf[[i]][[j]], newdata = cali[-1,])@metrics$MSE
  }
}
(proc.time() - t1) 
# tiempo: mtry=2 ~550s, ... ,mtry=8 ~980s
rf_test = promedio(rf_test)
rf_oob = promedio(rf_oob)
rf_train = promedio(rf_train)

rf_final = data.frame("mtries" = rf_train[,'mtries'], "train" = rf_train[,'Mean'],
                      "test" = rf_test[,'Mean'], "oob" = rf_oob[,'Mean'])
round(rf_final, 4)

# rf_train
# mtries     i=1        i=2       i=3       i=4        i=5
# 2      0.03965084 0.03976417 0.03977912 0.03981011 0.03951564
# 3      0.03433298 0.03423237 0.03412585 0.03416792 0.03410325
# 4      0.03366079 0.03356624 0.03377936 0.03357153 0.03364759
# 5      0.03375083 0.03375188 0.03395482 0.03357274 0.03378075
# 8      0.03506404 0.03496764 0.03510043 0.03509394 0.03501973

# rf_oob
# mtries    i=1        i=2       i=3       i=4        i=5
# 2       0.2301991 0.2311104 0.2307588 0.2315398 0.2304934
# 3       0.2234278 0.2231830 0.2227213 0.2228342 0.2229460
# 4       0.2254639 0.2252473 0.2261151 0.2252689 0.2258868
# 5       0.2292832 0.2296332 0.2305396 0.2280760 0.2297585
# 8       0.2441033 0.2436470 0.2440326 0.2441802 0.2442241

# rf_test
# mtries    i=1        i=2       i=3       i=4        i=5
# 2      0.2358444 0.2362749 0.2378237 0.2364137 0.2371410
# 3      0.2293863 0.2291323 0.2298320 0.2290856 0.2305746
# 4      0.2323346 0.2322101 0.2337946 0.2331015 0.2346572
# 5      0.2364411 0.2371476 0.2383650 0.2360971 0.2393143
# 8      0.2517762 0.2531499 0.2525685 0.2513436 0.2546324

mean(rf_test[,7] - rf_oob[,7]) #0.007350757

# Comparación entre error OOB y 5-CV
matplot(mtry, cbind(rf_test[,2:6], rf_oob[,2:6]), 
        col = c(rep("#00b3b322", 5), rep("#e6007322", 5)), 
        pch = 1, type="b", ylab = "Error", xlab = "Variables permitas por nodo (mtry)",
        lty = 1, cex = 1)
matlines(mtry, cbind(rf_test[,7], rf_oob[,7]), col = c("#00b3b3", "#e60073"),
         pch=19, type="b", ylab = "ECM", lty = 1)
legend("topleft", legend=c("Validación cruzada", "Out-of-bag"), pch=19,
       cex=1,col=c("#00b3b3","#e60073"), bty = "n")

#### GBM ECM ####
# best strategy appears to be to set ν to be very small (ν < 0.1) 
# and then choose M by early stopping.
seeds = c(162, 161, 399, 359, 19)

gbm_grid = list() 
gbm_fits = list() 
gbm_tables = list() 
for (i in 1:5) gbm_tables[[i]] = matrix(nrow = 20, ncol = 6)

hyper_params = list(max_depth = 2:8, 
                    learn_rate = seq(0.001, 0.1, 0.001))
search_criteria = list(strategy = "RandomDiscrete",
                       max_models = 20,
                       seed = 5693,
                       stopping_rounds = 5,
                       stopping_metric = "MSE",
                       stopping_tolerance = 1e-4)
t1 = proc.time()
for (i in 5) { #1 2 3 4
  gbm_grid[[i]] = h2o.grid("gbm", x = x, y = y,
                          training_frame = cali,
                          hyper_params = hyper_params,
                          search_criteria = search_criteria,
                          ntrees = 10000,
                          distribution = 'gaussian',
                          nfolds = 5,
                          stopping_rounds = 5,
                          stopping_tolerance = 1e-3,
                          stopping_metric = "MSE",
                          seed = seeds[i],
                          parallelism = 0)
  gbm_fits[[i]] = lapply(gbm_grid[[i]]@model_ids, h2o.getModel) #1 2
} 
(proc.time() - t1) #12:08
#1h43m..... 1h41m.... paralelizado: 1hr, 55min, 1hr 1 min

# for (i in 2) gbm_fits[[i]] = lapply(gbm_grid[[i]]@model_ids, h2o.getModel) #1 2

# extraer info: profundidad, aprendizaje, número de árboles, mse train, mse cv, cv sd
for (i in 5) { # 1 2 3 4
  for (j in 1:20) {
    gbm_tables[[i]][j, 1] = gbm_fits[[i]][[j]]@allparameters$max_depth
    gbm_tables[[i]][j, 2] = gbm_fits[[i]][[j]]@allparameters$learn_rate
    gbm_tables[[i]][j, 3] = gbm_fits[[i]][[j]]@allparameters$ntrees
    gbm_tables[[i]][j, 4] = gbm_fits[[i]][[j]]@model$training_metrics@metrics$MSE
    gbm_tables[[i]][j, 5] = gbm_fits[[i]][[j]]@model$cross_validation_metrics_summary$mean[3]
    gbm_tables[[i]][j, 6] = gbm_fits[[i]][[j]]@model$cross_validation_metrics_summary$sd[3]
    class(gbm_tables[[i]]) = "numeric"
  }
}

# Ordenar para preparar la tabla resumen final
for (i in 1:5) {
  gbm_tables[[i]] = cbind(gbm_tables[[i]][,1] + gbm_tables[[i]][,2],
                          gbm_tables[[i]])
  gbm_tables[[i]] = gbm_tables[[i]][order(gbm_tables[[i]][,1],decreasing=TRUE),]
}

#desv est del test error
desv.est = cbind(gbm_tables[[1]][,6], gbm_tables[[2]][,6], 
                 gbm_tables[[3]][,6], gbm_tables[[4]][,6],
                 gbm_tables[[5]][,6], rep(NA, 20))

for (i in 1:20) {
  desv.est[i, 6] = sd(desv.est[i,1:5])
}

#tabla final
gbm_final = matrix(nrow = 20, ncol = 6)
gbm_final[,1] = gbm_tables[[1]][,2]
gbm_final[,2] = gbm_tables[[1]][,3]
gbm_final[,6] = desv.est[,6]

for (i in 4:6) {
  gbm_final[,i-1] = rowMeans(cbind(gbm_tables[[1]][,i], gbm_tables[[2]][,i], 
                                   gbm_tables[[3]][,i], gbm_tables[[4]][,i],
                                   gbm_tables[[5]][,i]))
}
colnames(gbm_final) = c('J', 'v', 'n', 'trn', 'tst', 'sd tst')

#Gráfica de los modelos
matplot(1:20, gbm_final[,5:4], 
        col = c("#00b3b3", "#e60073"), 
        pch=19, type="b", ylab = "Error", xlab = "",
        lty = 1, cex = 1, xaxt = "n")
legend("bottomright", legend = c("Prueba", "Entrenamiento"), 
       col = c("#00b3b3", "#e60073"), pch=19, cex=1, bty = "n",)

cols = c("#530994", "#1751cc", "#2989e1", "#2996cc", "#2bd9e7", "#1fe3ca", "#adede5")
matplot(1:20, gbm_final[,5], 
        col = "grey", pch=1, type="b", lty = 3, cex = .1, xaxt = "n",
        ylab = "Error", xlab = "")
for (i in 8:2) {
  matlines(c(which(gbm_final[,1]==i)),
           gbm_final[which(gbm_final[,1]==i),5], col = cols[i-1])
  matpoints(c(which(gbm_final[,1]==i)),
            gbm_final[which(gbm_final[,1]==i),5], col = cols[i-1], pch = 19)
}
matpoints(4, gbm_final[4,5], col="#1fe3ca", pch = 15, cex = 1.4)
legend("topleft", title = "Profundidad",
       legend=c(8:2), pch=19, cex=1, bty = "n",
       col=rev(cols))

gbm_final[which(gbm_final[,5]<0.1932),]
# J     v      n    trn    tst sd tst
# 7 0.059 2218.8 0.0479 0.1931  9e-04
# 7 0.029 2774.4 0.0562 0.1931  5e-04
# 6 0.027 3963.4 0.0642 0.1931  7e-04

# El elegido es el de J=7 v=0.29
# 0.1936755 0.1928225 0.1936978 0.1930005 0.1924770[6]

#### GBM Huber ####
# best strategy appears to be to set ν to be very small (ν < 0.1) 
# and then choose M by early stopping.
seeds = c(652, 13, 27, 450, 199)

hub_grid = list() #gbm_grid[[seed]]
hub_fits = list() #gbm_fits[[seed]]
hub_tables = list() #gbm_fits[[seed]]
for (i in 1:5) hub_tables[[i]] = matrix(nrow = 25, ncol = 7)

hyper_params = list(max_depth = 2:8, 
                    learn_rate = seq(0.001, 0.1, 0.001),
                    huber_alpha = seq(0.1, 0.9, 0.1))
search_criteria = list(strategy = "RandomDiscrete",
                       max_models = 20,
                       seed = 22,
                       stopping_rounds = 5,
                       stopping_metric = "MSE",
                       stopping_tolerance = 1e-4)
t1 = proc.time()
for (i in 1) { #
  hub_grid[[i]] = h2o.grid("gbm", x = x, y = y,
                           training_frame = cali,
                           hyper_params = hyper_params,
                           search_criteria = search_criteria,
                           ntrees = 8000,
                           distribution = 'huber',
                           nfolds = 5,
                           stopping_rounds = 5,
                           stopping_tolerance = 1e-3,
                           stopping_metric = "MSE",
                           seed = seeds[i],
                           parallelism = 0)
  #hub_fits[[i]] = lapply(hub_grid[[i]]@model_ids, h2o.getModel) 
} 
(proc.time() - t1) #15:37

# extraer info: profundidad, alpha, aprendizaje, número de árboles, 
#mse train, mse cv, cv sd
for (i in 1) { #
  for (j in 1:20) {
    hub_tables[[i]][j, 1] = hub_fits[[i]][[j]]@allparameters$max_depth
    hub_tables[[i]][j, 2] = hub_fits[[i]][[j]]@allparameters$huber_alpha
    hub_tables[[i]][j, 3] = hub_fits[[i]][[j]]@allparameters$learn_rate
    hub_tables[[i]][j, 4] = hub_fits[[i]][[j]]@allparameters$ntrees
    hub_tables[[i]][j, 5] = hub_fits[[i]][[j]]@model$training_metrics@metrics$MSE
    #hub_tables[[i]][j, 6] = hub_fits[[i]][[j]]@model$cross_validation_metrics_summary$mean[3]
    #hub_tables[[i]][j, 7] = hub_fits[[i]][[j]]@model$cross_validation_metrics_summary$sd[3]
    class(hub_tables[[i]]) = "numeric"
  }
}
hub_tables[[1]]

save.image(file = '2Cali.Rdata')
h2o.shutdown()
y
