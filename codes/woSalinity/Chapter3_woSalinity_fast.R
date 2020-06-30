library(snowfall)
library(nloptr)

########constants##########

N <- 10

xLB <- numeric(15)
x0 <- numeric(15)

##############################
########## functions #########
##############################

BP_1year <- function(X_p_t){
  
  V_p <- 4226.68
  yld_p <- 1
  alpha_p <- 1502.85
  gamma_p <- 0.00795
  
  BP <- (V_p*yld_p)*X_p_t-(alpha_p+0.5*gamma_p*X_p_t)*X_p_t
  
  return(BP)
  
}

INIP_Tyear <- function(X_p_tminus1, X_p_t, t, R, INI_P){
  
  Interval <- 10
  
  INIP <- max(X_p_t-X_p_tminus1, 0)*INI_P/((1+R)^((t-1)*Interval+1))
  return(INIP)
  
}

PA_1year <- function(x, P){
  
  V_a <- 157.28
  yld_a <- 8
  alpha_a <- 636
  gamma_a <- 0.002941
  AW_a <- 4.84
  
  X_a_t <- x[1:5]
  
  BA <- sum(P*((V_a*yld_a)*X_a_t-(alpha_a+0.5*gamma_a*X_a_t)*X_a_t))
  return(BA)
  
}

C_1year <- function(x, GW_tminus1, GW_t, P){
  
  AFtoM3 <- 1233.48184
  g <- 9.81
  fttom <- 0.3048
  
  L <- 500000
  cap <- 15
  phi <- 0.15
  SW_class1 <- 500000
  Sy <- 0.1
  eta <- 0.7
  rho <- 1000
  B <- 400
  
  C_re <- 300
  C_class1 <- 42
  C_class2 <- 30
  C_energy <- 0.189
  
  mu <- 625000
  sigma <- 400000
  var <- sigma^2
  logmu <- log((mu^2)/sqrt(var + mu^2))
  logsigma <- sqrt(log(var/mu^2+1))
  
  z <- seq(0,4,1)
  SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
  
  X_r_t <- x[6:10]
  W_p_t <- x[11:15]
  
  H_avg <- B-0.5*(GW_tminus1+GW_t)/(L*Sy)
  PAR <- C_energy*AFtoM3*H_avg*fttom*rho*g/(eta*3.6e6)
  CP <- PAR*W_p_t
  CR <- C_re*X_r_t
  CSW <- C_class1*ifelse(SW<SW_class1,SW,SW_class1)+C_class2*ifelse(SW>SW_class1,SW-SW_class1,0)
  EXPC <- sum(P*(CP+CR+CSW))
  
  return(EXPC)
  
}

# P_Tyear <- function(GW_tminus1, X_p_tminus1, GW_t, X_p_t, t, hyper_par, xUB){
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
#   INI_P <- hyper_par$INI_P
#   
#   SWneeded <- (GW_t-GW_tminus1)+AW_p*X_p_t*Interval
#   SWavailable <- Interval*sum(P*SW)
#   
#   #browser()
#   if(SWneeded>SWavailable){
#     
#     OptimalX <- NA
#     
#   }else{
#     
#     OBJ <- function(x){
#       
#       Interval <- 10
#       
#       FtoP <- (1-(1+R)^(-Interval))/(R*(1+R)^((t-1)*Interval))
#       BP_1y <- BP_1year(X_p_t)
#       INIP_Ty <- INIP_Tyear(X_p_tminus1, X_p_t, t, R, INI_P)
#       PA_1y <- PA_1year(x, P)
#       C_1y <- C_1year(x, GW_tminus1, GW_t, P)
#       
#       obj <- -FtoP*(PA_1y-C_1y+BP_1y)+INIP_Ty
#       
#       return(obj)
#     }
#     
#     eqCON <- function(x){
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
#     }
#     
#     ineqCON <- function(x){
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
#     }
#     
#     sol <- slsqp(x0, fn = OBJ, lower = xLB, upper = xUB,
#                  hin = ineqCON, heq = eqCON, control = list(xtol_rel = 1e-8))
#     #browser()
#     #fun <- sol$value
#     OptimalX <- sol$par
#     
#   }
#   return(OptimalX)
#   #return(list(fun=fun, OptimalX=OptimalX))
#   
# }

P_Tyear <- function(GW_tminus1, X_p_tminus1, GW_t, X_p_t, t, hyper_par, xUB){
  
  # AW_p <- 4.07
  # Interval <- 10
  # 
  # mu <- 625000
  # sigma <- 400000
  # var <- sigma^2
  # logmu <- log((mu^2)/sqrt(var + mu^2))
  # logsigma <- sqrt(log(var/mu^2+1))
  # 
  # z <- seq(0,4,1)
  # SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
  
  x0 <- hyper_par$x0
  P <- hyper_par$P
  xLB <- hyper_par$xLB
  R <- hyper_par$R
  INI_P <- hyper_par$INI_P
  
  # SWneeded <- (GW_t-GW_tminus1)+AW_p*X_p_t*Interval
  # SWavailable <- Interval*sum(P*SW)
  # 
  # #browser()
  # if(SWneeded>SWavailable){
  #   
  #   OptimalX <- NA
  #   
  # }else{
    
  OBJ <- function(x){
      
      Interval <- 10
      
      FtoP <- (1-(1+R)^(-Interval))/(R*(1+R)^((t-1)*Interval))
      BP_1y <- BP_1year(X_p_t)
      INIP_Ty <- INIP_Tyear(X_p_tminus1, X_p_t, t, R, INI_P)
      PA_1y <- PA_1year(x, P)
      C_1y <- C_1year(x, GW_tminus1, GW_t, P)
      
      obj <- -FtoP*(PA_1y-C_1y+BP_1y)+INIP_Ty
      
      return(obj)
  }
    
  eqCON <- function(x){
      
      Interval <- 10
      AW_p <- 4.07
      AW_a <- 4.84
      phi <- 0.15
      cap <- 15
      
      X_a_t <- x[1:5]
      X_r_t <- x[6:10]
      W_p_t <- x[11:15]
      
      GW_use <- -phi*Interval*AW_p*X_p_t
      GW_use <- GW_use + Interval*sum(P*(W_p_t-cap*X_r_t-phi*AW_a*X_a_t))
      
      return(GW_tminus1-GW_t-GW_use)
      
  }
    
  ineqCON <- function(x){
      
      L <- 500000
      cap <- 15
      AW_p <- 4.07
      AW_a <- 4.84
      
      mu <- 625000
      sigma <- 400000
      var <- sigma^2
      logmu <- log((mu^2)/sqrt(var + mu^2))
      logsigma <- sqrt(log(var/mu^2+1))
      
      z <- seq(0,4,1)
      SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
      
      X_a_t <- x[1:5]
      X_r_t <- x[6:10]
      W_p_t <- x[11:15]
      
      remaining_land <- L - X_p_t - X_a_t - X_r_t
      remaining_water <- SW + W_p_t -cap*X_r_t - AW_p*X_p_t - AW_a*X_a_t
      
      return(c(remaining_land,remaining_water))
      
  }
    
  sol <- slsqp(x0, fn = OBJ, lower = xLB, upper = xUB, 
               hin = ineqCON, heq = eqCON, control = list(xtol_rel = 1e-8))
    #browser()
    #fun <- sol$value
  OptimalX <- sol$par
  
  if (abs(eqCON(OptimalX))>10){
    
    OptimalX <- NA
    
  }
  
  return(OptimalX)
  #return(list(fun=fun, OptimalX=OptimalX))
  
}


findX <- function(init_fin, t, hyper_par){
  OptimalX <- rep(list(rep(NA, 15)), dim(init_fin)[1])
  L <- 500000
  
  for(i in 1:dim(init_fin)[1]){
    GW_tminus1 <- init_fin[i,3]
    xUB <- c(rep(L,10),rep(GW_tminus1,5))
    X_p_tminus1 <- init_fin[i,4]
    GW_t <- init_fin[i,1]
    X_p_t <- init_fin[i,2]
    #browser()
    OptimalX[[i]] <- P_Tyear(GW_tminus1 = GW_tminus1, X_p_tminus1 = X_p_tminus1, 
                             xUB = xUB, 
                             GW_t = GW_t, X_p_t = X_p_t, t = t, hyper_par = hyper_par)
    
    
  }
  return(OptimalX)
}


Pt_profit <- function(GW_tminus1, X_p_tminus1, 
                      GW_t , X_p_t, t, hyper_par, x){
  # AW_p <- 4.07
  # Interval <- 10
  # 
  # mu <- 625000
  # sigma <- 400000
  # var <- sigma^2
  # logmu <- log((mu^2)/sqrt(var + mu^2))
  # logsigma <- sqrt(log(var/mu^2+1))
  # 
  # z <- seq(0,4,1)
  # SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
  
  x0 <- hyper_par$x0
  P <- hyper_par$P
  xLB <- hyper_par$xLB
  R <- hyper_par$R
  INI_P <- hyper_par$INI_P
  
  # SWneeded <- (GW_t-GW_tminus1)+AW_p*X_p_t*Interval
  # SWavailable <- Interval*sum(P*SW)
  
  #browser()
  if(sum(is.na(x))>0){
    
    fun <- Inf
    
  }else{
    
    Interval <- 10
    
    FtoP <- (1-(1+R)^(-Interval))/(R*(1+R)^((t-1)*Interval))
    BP_1y <- BP_1year(X_p_t)
    INIP_Ty <- INIP_Tyear(X_p_tminus1, X_p_t, t, R, INI_P)
    PA_1y <- PA_1year(x, P)
    C_1y <- C_1year(x, GW_tminus1, GW_t, P)
    
    fun <- -FtoP*(PA_1y-C_1y+BP_1y)+INIP_Ty
    
  }
  return(fun)
}

# FinalSalinity <- function(OptimalX, GW_tminus1, Cgw_tminus1, GW_t, X_p_t, hyper_par){
#   
#   X_a_t <- OptimalX[1:5]
#   X_r_t <- OptimalX[6:10]
#   W_p_t <- OptimalX[11:15]
#   P <- hyper_par$P
#   Interval <- 10
#   
#   AW_p <- 4.07
#   AW_a <- 4.84
#   
#   cap <- 15
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
#   Csw <- 400
#   
#   IRR_actual <- AW_p*X_p_t + AW_a*X_a_t
#   IrrSW <- SW-cap*X_r_t
#   IrrT = IrrSW+W_p_t
#   Cirr = (Csw*IrrSW+Cgw_tminus1*W_p_t)/IrrT
#   SALTirr = sum(P*Cirr*IRR_actual)*Interval
#   SALTar = sum(P*X_r_t)*Interval*cap*Csw
#   SALTgw = Cgw_tminus1*(GW_tminus1-Interval*sum(P*W_p_t))
#   Cgw_t = (SALTirr+SALTar+SALTgw)/GW_t
#   
#   return(Cgw_t)
#   
# }
# 
# MatchCgw <- function(Cgw_t, Cgw, type){
#   
#   if(Cgw_t<Cgw[1]){
#     
#     return(1)
#     
#   }else if(Cgw_t>max(Cgw)){
#     
#     return(length(Cgw))
#     
#   }else if(type=="Optimistic"){
#     
#     return(max(which(Cgw_t>=Cgw)))
#     
#   }else if(type=="Pessimistic"){
#     
#     return(min(which(Cgw_t<=Cgw)))
#     
#   }else{
#     
#     stop("typo")
#     
#   }
# }


wrapper <- function(initial, final, profit_tplus1, hyper_par, t, OptimalX, index_div){
  
  # scenarios: row = iteration, column = GW, XpI, Cgw
  iter_initial <- nrow(initial)
  iter_final <- nrow(final)
  profit_all <- rep(0, iter_initial)
  k_min <- rep(0, iter_initial)
  L <- 500000
  #Cgw <- hyper_par$Cgw
  #type <- hyper_par$type
  index_div <- (index_div-1)*iter_initial
  
  for (a in 1:iter_initial){
    index_div <- index_div + 1
    GW_tminus1 <- initial[a,1]
    xUB <- c(rep(L,10),rep(GW_tminus1,5))
    
    X_p_tminus1 <- initial[a,2]
    #Cgw_tminus1 <- initial[a,3]
    
    profit_temp <- rep(0,iter_final)
    index_Optim <- (index_div-1)*iter_final
    # one fixed i,j w/ all combinations of k&l
    for (b in 1:iter_final){
      index_Optim <- index_Optim + 1
      GW_t <- final[b,1]
      X_p_t <- final[b,2]
      #browser()
      
      #browser()
      if(sum(is.na(OptimalX[[index_Optim]]))!=0){
        
        profit_temp[b] <- Inf
        
      }else{
        
        profit_value <- Pt_profit(GW_tminus1 = GW_tminus1, 
                                  GW_t = GW_t, X_p_t = X_p_t, hyper_par = hyper_par, 
                                  X_p_tminus1 = X_p_tminus1, t = t, 
                                  x = OptimalX[[index_Optim]])
        # Cgw_t <- FinalSalinity(OptimalX = OptimalX[[index_Optim]], 
        #                        GW_tminus1 = GW_tminus1, Cgw_tminus1 = Cgw_tminus1, 
        #                        GW_t = GW_t, X_p_t = X_p_t, hyper_par = hyper_par)
        # index <- MatchCgw(Cgw_t = Cgw_t, Cgw = Cgw, type = type)
        profit_temp[b] <- profit_value + profit_tplus1[b]
      }
    }
    #browser()
    if(sum(is.infinite(profit_temp)) == iter_final){
      k_min[a] <- NA
    }else{
      k_min[a] <- which.min(profit_temp)
    }
    
    profit_all[a] <- min(profit_temp)
    
  }
  
  
  return(list("profit_all"=profit_all, "k_min"=k_min))
  
}

CreateGrid <- function(X){
  
  return(as.matrix(expand.grid(X)))
  
}

DivideWork <- function(Grid, NofCore){
  
  n <- nrow(Grid)
  if(n%%NofCore!=0) warning("please assgin different number of cores!")
  m <- n/NofCore
  index <- (1:NofCore-1)*m + 1
  return(lapply(index, function(x) Grid[x:(x+m-1),]))
  
}


########## DP #############
# GW <- seq(8000000, 10000000, length.out = 2)
# XpI <- seq(50000, 75000, length.out = 2)
# XpF <- seq(100000, 150000, length.out = 2)
# Cgw <- seq(500, 5000, length.out = 2)

GW <- seq(8000000, 12000000, 500000)
XpI <- seq(25000, 75000, 5000)
XpF <- seq(50000, 150000, 10000)
#Cgw <- seq(500, 5000, 500)


#GW <- seq(9000000, 11000000, 1000000)
#XpI <- seq(35000, 65000, 15000)
#XpF <- seq(70000, 130000, 30000)
#Cgw <- seq(2000, 3000, 1000)
#type <- "Optimistic"

NofCore <- 11
hyper_par <- list(x0 = numeric(15), P = c(0.2, 0.2, 0.2, 0.2, 0.2), xLB = numeric(15), R = 0.035, INI_P = 12000)

initial <- CreateGrid(list(GW, XpI))
init_fin <- CreateGrid(list(GW, XpF, GW, XpI))
#final <- CreateGrid(GW, XpF)

Initial_Divide <- DivideWork(initial, NofCore)
Init_Fin_Divde <- DivideWork(init_fin, NofCore)
profit_all <- matrix(0, nrow  = nrow(initial), ncol = N+1)
k_min <- matrix(0, nrow = nrow(initial), ncol = N)

sfInit(parallel = TRUE, cpus = NofCore, type = "SOCK")
sfLibrary(nloptr)


tmp_time <- proc.time()
sfExportAll()
temp_OptimalX <- sfLapply(x = Init_Fin_Divde, fun = function(W) findX(init_fin = W, 
                                                                      t = 3, hyper_par = hyper_par))
#OptimalX <- unlist(temp_OptimalX, recursive = FALSE)
OptimalX_backup <- unlist(temp_OptimalX, recursive = FALSE)
sfExportAll()

for (t in N:1){
  
  if (t == N){
    
    final <- CreateGrid(list(10000000, XpF))
    init_fin_index <- init_fin[, 1] == 10000000
    OptimalX <- OptimalX_backup[init_fin_index]
    
  }else{
    
    final <- CreateGrid(list(GW, XpF))
    OptimalX <- OptimalX_backup
  }
  #sfExport("t", "final", "profit_all", "OptimalX")
  sfExportAll()
  temp_result <- sfLapply(x = 1:length(Initial_Divide), 
                          fun = function(W) wrapper(initial = Initial_Divide[[W]], 
                                                    final = final, profit_tplus1 = profit_all[,t+1],
                                                    hyper_par = hyper_par, t = t, OptimalX = OptimalX, 
                                                    index_div = W))
  #temp_result <- lapply(X = Initial_Divide, FUN = function(W) wrapper(initial = W, final = final, profit_tplus1 = profit_all[[t+1]],
  #                                                                  hyper_par = hyper_par, t = t))
  
  #temp_result <- wrapper(initial = initial, final = final, 
  #                       profit_tplus1 = profit_all[[t+1]],
  #                       hyper_par = hyper_par, t = t, OptimalX = OptimalX)
  #temp_result <-  wrapper(initial = Initial_Divide[[1]], final = final, profit_tplus1 = profit_all[,t+1], hyper_par = hyper_par, t = t)
  
  #temp_profit <- temp_result$profit_all
  profit_all[,t] <- unlist(lapply(temp_result, FUN = function(x) x$profit_all))
  k_min[,t] <- unlist(lapply(temp_result, FUN = function(x) x$k_min))
  #k_min[,t] <- temp_result$k_min
  cat("t=", t, "/", N, "\n")
  
}

sfStop()
save(profit_all, k_min, OptimalX_backup,
     file = "/soe/wjzhao/project/Salinity/results/woSalinity/woSalinity_result_base.RData")

