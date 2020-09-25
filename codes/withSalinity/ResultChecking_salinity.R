#### check the dimention of the profit_all#######
# it should be 99 (9 GW * 11 XpI) by 10 (10 Salinities)
dim(profit_all$Pessim[[1]])

# incoming perennial crops = 50000 acres
profit_all$Pessim[[1]][46:54, ]/(-1000000)
round(profit_all$Pessim[[1]][46:54, ]/(-1000000), digits = 0)

#### load the function
setwd("D:/PhD_at_UCD/phd research/20_summer/Salinity")
source(paste0(getwd(), "/codes/withSalinity/Salinity_funs_new.R"))


#### create initials and finals
GW <- seq(8000000, 12000000, 500000)
XpI <- seq(25000, 75000, 5000)
XpF <- seq(50000, 150000, 10000)
#Cgw <- seq(500, 5000, 500)
Cgw <- seq(500, 5000, 250)

final1 <- CreateGrid(list(GW, XpF))
final2 <- CreateGrid(list(10000000, XpF))
InitialTable <- CreateGrid(list(GW, XpF, GW, XpI, Cgw))

#### get the best decision for each initial: Pessimistic

N <- 10

# GW

k_min_GW <- rep(list(NA), N)

for(i in 1:(N-1)){
  tmp <- final1[k_min$Pessim[, i], 1]
  k_min_GW[[i]] <- matrix(tmp, nrow = length(GW)*length(XpI), ncol = length(Cgw))
}

i <- N

tmp <- final2[k_min$Pessim[, i], 1]
k_min_GW[[i]] <- matrix(tmp, nrow = length(GW)*length(XpI), ncol = length(Cgw))

# XpF

k_min_Xp <- rep(list(NA), N)

for(i in 1:(N-1)){
  tmp <- final1[k_min$Pessim[, i], 2]
  k_min_Xp[[i]] <- matrix(tmp, nrow = length(GW)*length(XpI), ncol = length(Cgw))
}

i <- N

tmp <- final2[k_min$Pessim[, i], 2]
k_min_Xp[[i]] <- matrix(tmp, nrow = length(GW)*length(XpI), ncol = length(Cgw))


####### move forward

# function
type <- "Pesimmistic"
hyper_par <- list(x0 = numeric(15), P = c(0.2,0.2,0.2,0.2,0.2), 
                  xLB = numeric(15), R = 0.035, type = type, Cgw = Cgw)

ReturnFinal <- function(GW_tminus1, X_p_tminus1, Cgw_tminus1, hyper_par, t){
  
  I <- which(GW == GW_tminus1)
  J <- which(XpI == X_p_tminus1)
  K <- which(Cgw == Cgw_tminus1)
  
  index_final <- (J-1)*length(GW)+I
  GW_t <- k_min_GW[[t]][index_final, K]
  X_p_t <- k_min_Xp[[t]][index_final, K]
  
  A <- which(GW == GW_t)
  B <- which(XpF == X_p_t)
  
  index_x <- (K-1)*length(GW)*length(XpF)*length(GW)*length(XpI)+(J-1)*length(GW)*length(XpF)*length(GW)+(I-1)*length(GW)*length(XpF)+(B-1)*length(GW)+A
  #browser()
  if(length(index_x) == 0){
    browser()
  }
  X <- OptimalX_backup[[index_x]]
  Profit <- Pt_profit(GW_tminus1 = GW_tminus1, X_p_tminus1 = X_p_tminus1, Cgw_tminus1 = Cgw_tminus1, GW_t = GW_t, X_p_t = X_p_t, t = t, hyper_par = hyper_par, x = X)
  Cgw_t <- FinalSalinity(OptimalX = X, GW_tminus1 = GW_tminus1, Cgw_tminus1 = Cgw_tminus1, GW_t = GW_t, X_p_t = X_p_t, hyper_par = hyper_par)
  index_Cgw <- MatchCgw(Cgw_t = Cgw_t, Cgw = Cgw, type = "Pessimistic")
  Cgw_t_rounded <- Cgw[index_Cgw]
  return(list(GW_t=GW_t, X_p_t = X_p_t, index_x = index_x, Salinity = Cgw_t_rounded, Profit = Profit))
  #return(c(GW_t/1000000, X_p_t, Cgw_t_rounded, Profit))
  
}


# store the best decision from 1st stage to last stage
result <- rep(list(NA), N)
result[[1]] <- ReturnFinal(GW_tminus1 = 9000000, X_p_tminus1 = 50000, Cgw_tminus1 = 3000, hyper_par = hyper_par, t = 1)

for(i in 2:N){
  initial_value <- as.numeric(InitialTable[result[[i-1]]$index_x, ])
  result[[i]] <- ReturnFinal(GW_tminus1 = initial_value[1], X_p_tminus1 = initial_value[2]/2, Cgw_tminus1 = result[[i-1]]$Salinity, hyper_par = hyper_par, t = i)
}

profit <- 0
for(i in 1:N){
  profit <- profit + result[[i]]$Profit
}
profit 

# # a bit more aggresive in planting perennial crops
# P_Tyear2 <- function(GW_tminus1, X_p_tminus1, Cgw_tminus1, GW_t, X_p_t, t, hyper_par, xUB){
#   
#   AW_p <- 4.07
#   Interval <- 10
#   
#   mu <- 625000
#   sigma <- 400000
#   var <- sigma^2
#   logmu <- log((mu^2)/sqrt(var + mu^2))
#   logsigma <- sqrt(log(var/mu^2+1))
#   
#   z <- seq(0,4,1)
#   SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
#   
#   x0 <- hyper_par$x0
#   P <- hyper_par$P
#   xLB <- hyper_par$xLB
#   R <- hyper_par$R
#   
#   #SWneeded <- (GW_t-GW_tminus1)+AW_p*X_p_t*Interval
#   #SWavailable <- Interval*sum(P*SW)
#   
#   #browser()
#   #if(SWneeded>SWavailable){
#     
#     
#     #OptimalX <- NA
#   #}else{
#     
#   OBJ <- function(x){
#       
#       Interval <- 10
#       
#       FtoP <- (1-(1+R)^(-Interval))/(R*(1+R)^((t-1)*Interval))
#       BP_Ty <- BP_Tyear(x, Cgw_tminus1, X_p_t, P, t, R)
#       INIP_Ty <- INIP_Tyear(X_p_tminus1, X_p_t, t, R)
#       PA_1y <- PA_1year(x, Cgw_tminus1, P)
#       C_1y <- C_1year(x, GW_tminus1, GW_t, P)
#       
#       obj <- -FtoP*(PA_1y-C_1y)-BP_Ty+INIP_Ty
#       
#       return(obj)
#   }
#     
#   eqCON <- function(x){
#       
#       Interval <- 10
#       AW_p <- 4.07
#       AW_a <- 4.84
#       phi <- 0.15
#       cap <- 15
#       
#       X_a_t <- x[1:5]
#       X_r_t <- x[6:10]
#       W_p_t <- x[11:15]
#       
#       GW_use <- -phi*Interval*AW_p*X_p_t
#       GW_use <- GW_use + Interval*sum(P*(W_p_t-cap*X_r_t-phi*AW_a*X_a_t))
#       
#       return(GW_tminus1-GW_t-GW_use)
#       
#   }
#     
#   ineqCON <- function(x){
#       
#       L <- 500000
#       cap <- 15
#       AW_p <- 4.07
#       AW_a <- 4.84
#       
#       mu <- 625000
#       sigma <- 400000
#       var <- sigma^2
#       logmu <- log((mu^2)/sqrt(var + mu^2))
#       logsigma <- sqrt(log(var/mu^2+1))
#       
#       z <- seq(0,4,1)
#       SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
#       
#       X_a_t <- x[1:5]
#       X_r_t <- x[6:10]
#       W_p_t <- x[11:15]
#       
#       remaining_land <- L - X_p_t - X_a_t - X_r_t
#       remaining_water <- SW + W_p_t -cap*X_r_t - AW_p*X_p_t - AW_a*X_a_t
#       
#       return(c(remaining_land,remaining_water))
#       
#   }
#     
#   sol <- slsqp(x0, fn = OBJ, lower = xLB, upper = xUB,
#                  hin = ineqCON, heq = eqCON, control = list(xtol_rel = 1e-8))
#     #browser()
#     #fun <- sol$value
#   OptimalX <- sol$par
#     
#   
#   return(list(OptimalX, sol$value))
#   #return(list(fun=fun, OptimalX=OptimalX))
#   
# }
# 
# 
# 
# 
# 
# 

#### get the best decision for each initial: Optimistic

N <- 10

# GW

k_min_GW_O <- rep(list(NA), N)

for(i in 1:(N-1)){
  tmp <- final1[k_min$Optim[, i], 1]
  k_min_GW_O[[i]] <- matrix(tmp, nrow = length(GW)*length(XpI), ncol = length(Cgw))
}

i <- N

tmp <- final2[k_min$Optim[, i], 1]
k_min_GW_O[[i]] <- matrix(tmp, nrow = length(GW)*length(XpI), ncol = length(Cgw))

# XpF

k_min_Xp_O <- rep(list(NA), N)

for(i in 1:(N-1)){
  tmp <- final1[k_min$Optim[, i], 2]
  k_min_Xp_O[[i]] <- matrix(tmp, nrow = length(GW)*length(XpI), ncol = length(Cgw))
}

i <- N

tmp <- final2[k_min$Optim[, i], 2]
k_min_Xp_O[[i]] <- matrix(tmp, nrow = length(GW)*length(XpI), ncol = length(Cgw))


####### move forward

# function
type2 <- "Optimistic"
hyper_par_O <- list(x0 = numeric(15), P = rep(0.2, 5), 
                  xLB = numeric(15), R = 0.035, type = type2, Cgw = Cgw)

ReturnFinal_O <- function(GW_tminus1, X_p_tminus1, Cgw_tminus1, hyper_par, t){
  
  I <- which(GW == GW_tminus1)
  J <- which(XpI == X_p_tminus1)
  K <- which(Cgw == Cgw_tminus1)
  
  index_final <- (J-1)*length(GW)+I
  GW_t <- k_min_GW_O[[t]][index_final, K]
  X_p_t <- k_min_Xp_O[[t]][index_final, K]
  
  A <- which(GW == GW_t)
  B <- which(XpF == X_p_t)
  
  index_x <- (K-1)*length(GW)*length(XpF)*length(GW)*length(XpI)+(J-1)*length(GW)*length(XpF)*length(GW)+(I-1)*length(GW)*length(XpF)+(B-1)*length(GW)+A
  #browser()
  if(length(index_x) == 0){
    browser()
  }
  X <- OptimalX_backup[[index_x]]
  Profit <- Pt_profit(GW_tminus1 = GW_tminus1, X_p_tminus1 = X_p_tminus1, Cgw_tminus1 = Cgw_tminus1, GW_t = GW_t, X_p_t = X_p_t, t = t, hyper_par = hyper_par_O, x = X)
  Cgw_t <- FinalSalinity(OptimalX = X, GW_tminus1 = GW_tminus1, Cgw_tminus1 = Cgw_tminus1, GW_t = GW_t, X_p_t = X_p_t, hyper_par = hyper_par_O)
  index_Cgw <- MatchCgw(Cgw_t = Cgw_t, Cgw = Cgw, type = "Optimistic")
  Cgw_t_rounded <- Cgw[index_Cgw]
  return(list(GW_t=GW_t, X_p_t = X_p_t, index_x = index_x, Salinity = Cgw_t_rounded, Profit = Profit))
  #return(c(GW_t/1000000, X_p_t, Cgw_t_rounded, Profit))
  
}


# store the best decision from 1st stage to last stage
result <- rep(list(NA), N)
result[[1]] <- ReturnFinal_O(GW_tminus1 = 8000000, X_p_tminus1 = 50000, Cgw_tminus1 = 500, hyper_par = hyper_par, t = 1)

for(i in 2:N){
  initial_value <- as.numeric(InitialTable[result[[i-1]]$index_x, ])
  result[[i]] <- ReturnFinal_O(GW_tminus1 = initial_value[1], X_p_tminus1 = initial_value[2]/2, Cgw_tminus1 = result[[i-1]]$Salinity, hyper_par = hyper_par, t = i)
}

profit <- 0
for(i in 1:N){
  profit <- profit + result[[i]]$Profit
}
profit 

