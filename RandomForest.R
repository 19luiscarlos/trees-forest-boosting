library(randomForest)
cali = read.csv('cali.csv',
                sep = ',', header = T)

# Matrices para registrar errores test (oob) y train de cinco repeticiones
#rf.oob = matrix(nrow = 5, ncol = 7)
#rf.trn = matrix(nrow = 5, ncol = 7)
colnames(rf.oob) = c('mtry', "1o", "2o", "3o", "4o", "5o", "Media")
colnames(rf.trn) = c('mtry', "1o", "2o", "3o", "4o", "5o", "Media")

# Se probará el parámetro mtry con 2, 3, 4, 5 y 8
rf.oob[,1] = c(2:5, 8)
rf.trn[,1] = c(2:5, 8)

# Lista para almacenar los bosques
# rf = rep(list(rep(list(0), 5)), 5)

seeds = c(19, 431, 133, 572, 93) # para repeticiones
mtries = c(2:5, 8) # diferentes parámetros

t1 = proc.time()
for (i in 1:5) { # seeds: 1, 
  for (j in 1:5) { # mtries
    set.seed(seeds[i])
    rf[[i]][[j]] = randomForest(MedianHouseValue ~ ., 
                                data = cali, 
                                mtry = mtries[j], 
                                ntree = 1000,
                                importance = F)
    rf.oob[j, i + 1] = rf[[i]][[j]]$mse[1000]
    rf.trn[j, i + 1] = mean((predict(rf[[i]][[j]], newdata = cali) - cali$MedianHouseValue)^2)
  }
}
(proc.time() - t1)

# Promedio de las cinco repeticiones
rf.oob[,ncol(rf.oob)] = rowMeans(rf.oob[,2:6])
rf.trn[,ncol(rf.trn)] = rowMeans(rf.trn[,2:6])

# Gráfica del promedio de ECM
matplot(mtries, rf.oob[,7], col = "#00b3b3",
        pch=19, type="b", lty = 1, ylim = c(0.228, 0.243),
        xlab = "Variables permitidas por nodo (mtry)", ylab = "Error")
matlines(mtries, rf.oob[,-7], col = "#00b3b322",
         pch=1, type="b", lty = 1, cex = .1)

# ECM de prueba a través de 1000 árboles para mtry=3
rf.scoring = matrix(nrow = 1000, ncol = 6)

for (i in 1:5) rf.scoring[,i] = rf[[i]][[2]]$mse
rf.scoring[,6] = rowMeans(rf.scoring[,-6])

# Gráfica
plot(rf.scoring[,6], type = 'l', ylim = c(0.22, 0.3), 
     xlab = 'Número de árboles', ylab = 'Error', 
     col = '#00b3b3')

# rf.oob
# mtry     1o     2o     3o     4o     5o   Media
# 2      0.2324 0.2315 0.2313 0.2312 0.2320 0.2317
# 3      0.2280 0.2289 0.2286 0.2289 0.2289 0.2287
# 4      0.2323 0.2330 0.2333 0.2324 0.2327 0.2327
# 5      0.2365 0.2365 0.2368 0.2359 0.2360 0.2363
# 8      0.2430 0.2427 0.2427 0.2426 0.2424 0.2427

# > rf.trn
# mtry     1o     2o     3o     4o     5o   Media
# 2      0.0470 0.0470 0.0469 0.0469 0.0471 0.0470
# 3      0.0433 0.0436 0.0435 0.0434 0.0435 0.0435
# 4      0.0426 0.0428 0.0429 0.0426 0.0427 0.0427
# 5      0.0425 0.0425 0.0425 0.0424 0.0424 0.0425
# 8      0.0422 0.0421 0.0421 0.0421 0.0421 0.0421


# Obtener el número promedio de veces que las obs están oob
oob.times = matrix(nrow = 6, ncol = 6)
for (i in 1:5) {
  for (j in 1:5) {
    oob.times[j,i] = mean(rf[[i]][[j]]$oob.times)/1000
  }
}
oob.times[,6] = rowMeans(oob.times[,-6])
oob.times[6,] = colMeans(oob.times[-6,])

# # profundidad de los árboles con mtry=3
# tabla_rf = matrix(nrow = 1000, ncol = 5)
# for (i in 1:1000) {
#   for(j in 1:5){
#     tabla_rf[i,j] <- length(which(getTree(rf[[j]][[2]], k = i)[,5] == -1))
#   }
# }
# mean(tabla_rf); sd(tabla_rf); min(tabla_rf); max(tabla_rf)

save.image(file = 'Cali_rf.Rdata')
