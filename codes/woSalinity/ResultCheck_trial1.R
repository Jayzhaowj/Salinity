GW <- seq(8000000, 12000000, 500000)
XpI <- seq(25000, 75000, 5000)
XpF <- seq(50000, 150000, 10000)
#Cgw <- seq(500, 5000, 500)

# final <- CreateGrid(list(GW, XpF))
# 
# mat_k_min <- rep(list(NA), 10)
# 
# for(i in 1:9){
#   tmp <- apply(final[k_min$Pessim[, i], ], 1, toString)
#   mat_k_min[[i]] <- matrix(tmp, nrow = 99, ncol = 10)
# }
# 
# i <- 10
# 
# final <- CreateGrid(list(10000000, XpF))
# tmp <- apply(final[k_min$Pessim[, i], ], 1, toString)
# mat_k_min[[i]] <- matrix(tmp, nrow = 99, ncol = 10)



############################
final <- CreateGrid(list(GW, XpF))

k_min_GW <- rep(list(NA), 10)

for(i in 1:9){
  tmp <- final[k_min[,i], 1]
  k_min_GW[[i]] <- matrix(tmp, nrow = 99, ncol = 1)
}

i <- 10

final <- CreateGrid(list(10000000, XpF))
tmp <- final[k_min[,i], 1]
k_min_GW[[i]] <- matrix(tmp, nrow = 99, ncol = 1)

#k_min_GW_MAF <- lapply(k_min_GW, function(x) x/1000000)


############################=
k_min_Xp <- rep(list(NA), 10)

final <- CreateGrid(list(GW, XpF))
for(i in 1:9){
  tmp <- final[k_min[, i], 2]
  k_min_Xp[[i]] <- matrix(tmp, nrow = 99, ncol = 1)
}

i <- 10

final <- CreateGrid(list(10000000, XpF))
tmp <- final[k_min[, i], 2]
k_min_Xp[[i]] <- matrix(tmp, nrow = 99, ncol = 1)


############################
ReturnFinal <- function(GW_tminus1, X_p_tminus1, hyper_par, t){
  
  I <- which(GW == GW_tminus1)
  J <- which(XpI == X_p_tminus1)
  #K <- which(Cgw == Cgw_tminus1)
  K <- 1
  
  index_final <- (J-1)*length(GW)+I
  GW_t <- k_min_GW[[t]][index_final, K]
  X_p_t <- k_min_Xp[[t]][index_final, K]
  
  A <- which(GW == GW_t)
  B <- which(XpF == X_p_t)
  
  index_x <- (K-1)*length(GW)*length(XpF)*length(GW)*length(XpI)+(J-1)*length(GW)*length(XpF)*length(GW)+(I-1)*length(GW)*length(XpF)+(B-1)*length(GW)+A
 # browser()
  if(length(index_x) == 0){
    browser()
  }
  X <- OptimalX_backup[[index_x]]
  Profit <- Pt_profit(GW_tminus1 = GW_tminus1, X_p_tminus1 = X_p_tminus1, GW_t = GW_t, X_p_t = X_p_t, t = t, hyper_par = hyper_par, x = X)
  #Cgw_t <- FinalSalinity(OptimalX = X, GW_tminus1 = GW_tminus1, Cgw_tminus1 = Cgw_tminus1, GW_t = GW_t, X_p_t = X_p_t, hyper_par = hyper_par)
  #index_Cgw <- MatchCgw(Cgw_t = Cgw_t, Cgw = Cgw, type = "Pessimistic")
  #Cgw_t_rounded <- Cgw[index_Cgw]
  return(list(GW_t=GW_t, X_p_t = X_p_t, index_x = index_x, Profit = Profit))
  #return(c(GW_t/1000000, X_p_t, Cgw_t_rounded, Profit))
  
}

InitialTable <- CreateGrid(list(GW, XpF, GW, XpI))
result <- rep(list(NA), 10)
result[[1]] <- ReturnFinal(GW_tminus1 = 10500000, X_p_tminus1 = 50000, hyper_par = hyper_par, t = 1)

for(i in 2:10){
  initial_value <- as.numeric(InitialTable[result[[i-1]]$index_x, ])
  result[[i]] <- ReturnFinal(GW_tminus1 = initial_value[1], X_p_tminus1 = initial_value[2]/2, hyper_par = hyper_par, t = i)
}

profit <- 0
for(i in 1:10){
  profit <- profit + result[[i]]$Profit
}
