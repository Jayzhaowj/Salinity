#### load priliminary results
load("D:/PhD_at_UCD/phd research/20_spring/fastercode/woSalinityBase.RData")

#### check the dimention of the profit_all#######
# it should be a 99 (9 GW levels * 11 incoming perennial crops) by 11 (10 stages, plus one column with no value) matrix
dim(profit_all)

# the first column is the total profit for all 10 stages
TotalProfit <- matrix(profit_all[, 1]/(-1000000), nrow = 9, ncol = 11)
(round(TotalProfit, digits = 0))

#### load the function
setwd("D:/PhD_at_UCD/phd research/20_summer/Salinity")
source(paste0(getwd(), "/codes/woSalinity/NoSalinity_funs.R"))



#### create initials and finals
GW <- seq(8000000, 12000000, 500000)
XpI <- seq(25000, 75000, 5000)
XpF <- seq(50000, 150000, 10000)

final1 <- CreateGrid(list(GW, XpF))
final2 <- CreateGrid(list(10000000, XpF))
InitialTable <- CreateGrid(list(GW, XpF, GW, XpI))

#### get the best decision for each initial

N <- 10

# GW

k_min_GW <- rep(list(NA), N)

for(i in 1:(N-1)){
  
  k_min_GW[[i]] <- final1[k_min[, i], 1]

}

k_min_GW[[N]] <- final2[k_min[, N], 1]

# XpF

k_min_Xp <- rep(list(NA), N)

for(i in 1:(N-1)){

  k_min_Xp[[i]] <- final1[k_min[, i], 2]
}

k_min_Xp[[N]] <- final2[k_min[, N], 2]



####### move forward

# function
#type <- "Pesimmistic"
hyper_par <- list(x0 = numeric(15), P = c(0.2, 0.2, 0.2, 0.2, 0.2), xLB = numeric(15), R = 0.035, INI_P = 12000)

ReturnFinal <- function(GW_tminus1, X_p_tminus1, Cgw_tminus1, hyper_par, t){
  
  I <- which(GW == GW_tminus1)
  J <- which(XpI == X_p_tminus1)
  #K <- which(Cgw == Cgw_tminus1)
  
  index_final <- (J-1)*length(GW)+I
  GW_t <- k_min_GW[[t]][index_final]
  X_p_t <- k_min_Xp[[t]][index_final]
  
  A <- which(GW == GW_t)
  B <- which(XpF == X_p_t)
  
  #index_x <- (K-1)*length(GW)*length(XpF)*length(GW)*length(XpI)+(J-1)*length(GW)*length(XpF)*length(GW)+(I-1)*length(GW)*length(XpF)+(B-1)*length(GW)+A
  index_x <- (J-1)*length(GW)*length(XpF)*length(GW)+(I-1)*length(GW)*length(XpF)+(B-1)*length(GW)+A
  #browser()
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

# store the best decision from 1st stage to last stage
result <- rep(list(NA), N)
result[[1]] <- ReturnFinal(GW_tminus1 = 11000000, X_p_tminus1 = 50000, hyper_par = hyper_par, t = 1)

for(i in 2:N){
  initial_value <- as.numeric(InitialTable[result[[i-1]]$index_x, ])
  result[[i]] <- ReturnFinal(GW_tminus1 = initial_value[1], X_p_tminus1 = initial_value[2]/2, hyper_par = hyper_par, t = i)
}

profit <- 0
for(i in 1:N){
  profit <- profit + result[[i]]$Profit
}
profit 

