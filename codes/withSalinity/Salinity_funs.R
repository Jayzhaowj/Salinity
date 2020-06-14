library(snowfall)
library(nloptr)


##############################
########## functions #########
##############################

BP_Tyear <- function(x, Cgw_tminus1, X_p_t, P, t, R){
  
  X_r_t <- x[6:10]
  W_p_t <- x[11:15]
  
  Interval <- 10
  
  V_p <- 4226.68
  yld_p <- 1
  alpha_p <- 1502.85
  gamma_p <- 0.00795
  
  cap <- 15
  
  mu <- 625000
  sigma <- 400000
  var <- sigma^2
  logmu <- log((mu^2)/sqrt(var + mu^2))
  logsigma <- sqrt(log(var/mu^2+1))
  
  z <- seq(0,4,1)
  SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
  
  Csw <- 400
  ECtoClow <- 640
  ECtoChigh <- 800
  
  Ky_p <- 1.2
  ECet_p <- 1.5
  b_p <- 0.19
  
  PTMoY <- matrix(c(0.5,0.5,0,0,0, rep(P,4)), nrow = 5, ncol = 5, byrow = TRUE)
  PMoY <- matrix(P, nrow = 1, ncol = 5, byrow = TRUE)
  
  for (i in 1:(Interval-1)) {
    
    PMoY <- rbind(PMoY, PMoY[i,]%*% PTMoY)
    
  }
  
  IrrSW <- SW-cap*X_r_t
  IrrT <- IrrSW+W_p_t
  Cirr <- (Csw*IrrSW+Cgw_tminus1*W_p_t)/IrrT
  ECirr <- ifelse(Cirr<3200, Cirr/ECtoClow, ifelse(Cirr<4000,5,Cirr/ECtoChigh))
  Ksalt_p <- ifelse(ECirr<ECet_p, 1, 1-b_p/Ky_p*(ECirr-ECet_p))
  Yp <- (1-Ky_p*(1-Ksalt_p))*yld_p
  
  BP <- 0
  EXP <- (V_p*Yp)*X_p_t-(alpha_p+0.5*gamma_p*X_p_t)*X_p_t
  
  for (k in 1:Interval){
    
    BPPsum <- sum(PMoY[k,]*EXP)
    BP <- BP + (1/(1+R)**((t-1)*Interval+k))*BPPsum
    
  }
  
  return(BP)
  
}

INIP_Tyear <- function(X_p_tminus1, X_p_t, t, R){
  
  INI_p <- 12000
  Interval <- 10
  
  INIP <- max(X_p_t-X_p_tminus1, 0)*INI_p/((1+R)^((t-1)*Interval+1))
  return(INIP)
  
}

PA_1year <- function(x, Cgw_tminus1, P){
  
  V_a <- 157.28
  yld_a <- 8
  alpha_a <- 636
  gamma_a <- 0.002941
  AW_a <- 4.84
  
  X_a_t <- x[1:5]
  X_r_t <- x[6:10]
  W_p_t <- x[11:15]
  
  cap <- 15
  
  mu <- 625000
  sigma <- 400000
  var <- sigma^2
  logmu <- log((mu^2)/sqrt(var + mu^2))
  logsigma <- sqrt(log(var/mu^2+1))
  
  z <- seq(0,4,1)
  SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
  
  Csw <- 400
  ECtoClow <- 640
  ECtoChigh <- 800
  
  Ky_a <- 1.1
  ECet_a <- 2
  b_a <- 0.073
  
  IrrSW <- SW-cap*X_r_t
  IrrT <- IrrSW+W_p_t
  Cirr <- (Csw*IrrSW+Cgw_tminus1*W_p_t)/IrrT
  ECirr <- ifelse(Cirr<3200, Cirr/ECtoClow, ifelse(Cirr<4000,5,Cirr/ECtoChigh))
  Ksalt_a <- ifelse(ECirr<ECet_a, 1, 1-b_a/Ky_a*(ECirr-ECet_a))
  Ya <- (1-Ky_a*(1-Ksalt_a))*yld_a
  
  BA <- sum(P*((V_a*Ya)*X_a_t-(alpha_a+0.5*gamma_a*X_a_t)*X_a_t))
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

P_Tyear <- function(GW_tminus1, X_p_tminus1, Cgw_tminus1, GW_t, X_p_t, t, hyper_par, xUB){
  
  AW_p <- 4.07
  Interval <- 10
  
  mu <- 625000
  sigma <- 400000
  var <- sigma^2
  logmu <- log((mu^2)/sqrt(var + mu^2))
  logsigma <- sqrt(log(var/mu^2+1))
  
  z <- seq(0,4,1)
  SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
  
  x0 <- hyper_par$x0
  P <- hyper_par$P
  xLB <- hyper_par$xLB
  R <- hyper_par$R
  
  SWneeded <- (GW_t-GW_tminus1)+AW_p*X_p_t*Interval
  SWavailable <- Interval*sum(P*SW)
  
  #browser()
  if(SWneeded>SWavailable){
    
    
    OptimalX <- NA
  }else{
    
    OBJ <- function(x){
      
      Interval <- 10
      
      FtoP <- (1-(1+R)^(-Interval))/(R*(1+R)^((t-1)*Interval))
      BP_Ty <- BP_Tyear(x, Cgw_tminus1, X_p_t, P, t, R)
      INIP_Ty <- INIP_Tyear(X_p_tminus1, X_p_t, t, R)
      PA_1y <- PA_1year(x, Cgw_tminus1, P)
      C_1y <- C_1year(x, GW_tminus1, GW_t, P)
      
      obj <- -FtoP*(PA_1y-C_1y)-BP_Ty+INIP_Ty
      
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
    Cgw_tminus1 <- init_fin[i,5]
    GW_t <- init_fin[i,1]
    X_p_t <- init_fin[i,2]
    #browser()
    OptimalX[[i]] <- P_Tyear(GW_tminus1 = GW_tminus1, X_p_tminus1 = X_p_tminus1, 
                             Cgw_tminus1 = Cgw_tminus1, xUB = xUB, 
                             GW_t = GW_t, X_p_t = X_p_t, t = t, hyper_par = hyper_par)
    
    
  }
  return(OptimalX)
}


Pt_profit <- function(GW_tminus1, X_p_tminus1, 
                      Cgw_tminus1, 
                      GW_t , X_p_t, t, hyper_par, x){
  AW_p <- 4.07
  Interval <- 10
  
  mu <- 625000
  sigma <- 400000
  var <- sigma^2
  logmu <- log((mu^2)/sqrt(var + mu^2))
  logsigma <- sqrt(log(var/mu^2+1))
  
  z <- seq(0,4,1)
  SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
  
  x0 <- hyper_par$x0
  P <- hyper_par$P
  xLB <- hyper_par$xLB
  R <- hyper_par$R
  
  SWneeded <- (GW_t-GW_tminus1)+AW_p*X_p_t*Interval
  SWavailable <- Interval*sum(P*SW)
  
  #browser()
  if(SWneeded>SWavailable){
    
    fun <- Inf
    
  }else{
    
    Interval <- 10
    FtoP <- (1-(1+R)^(-Interval))/(R*(1+R)^((t-1)*Interval))
    BP_Ty <- BP_Tyear(x, Cgw_tminus1, X_p_t, P, t, R)
    INIP_Ty <- INIP_Tyear(X_p_tminus1, X_p_t, t, R)
    PA_1y <- PA_1year(x, Cgw_tminus1, P)
    C_1y <- C_1year(x, GW_tminus1, GW_t, P)
    
    fun <- -FtoP*(PA_1y-C_1y)-BP_Ty+INIP_Ty
    
    
  }
  return(fun)
}

FinalSalinity <- function(OptimalX, GW_tminus1, Cgw_tminus1, GW_t, X_p_t, hyper_par){
  
  X_a_t <- OptimalX[1:5]
  X_r_t <- OptimalX[6:10]
  W_p_t <- OptimalX[11:15]
  P <- hyper_par$P
  Interval <- 10
  
  AW_p <- 4.07
  AW_a <- 4.84
  
  cap <- 15
  
  mu <- 625000
  sigma <- 400000
  var <- sigma^2
  logmu <- log((mu^2)/sqrt(var + mu^2))
  logsigma <- sqrt(log(var/mu^2+1))
  
  z <- seq(0,4,1)
  SW <- exp(qnorm(0.1+z*0.2)*logsigma+logmu)
  
  Csw <- 400
  
  IRR_actual <- AW_p*X_p_t + AW_a*X_a_t
  IrrSW <- SW-cap*X_r_t
  IrrT = IrrSW+W_p_t
  Cirr = (Csw*IrrSW+Cgw_tminus1*W_p_t)/IrrT
  SALTirr = sum(P*Cirr*IRR_actual)*Interval
  SALTar = sum(P*X_r_t)*Interval*cap*Csw
  SALTgw = Cgw_tminus1*(GW_tminus1-Interval*sum(P*W_p_t))
  Cgw_t = (SALTirr+SALTar+SALTgw)/GW_t
  
  return(Cgw_t)
  
}

MatchCgw <- function(Cgw_t, Cgw, type){
  
  if(Cgw_t<Cgw[1]){
    
    return(1)
    
  }else if(Cgw_t>max(Cgw)){
    
    return(length(Cgw))
    
  }else if(type=="Optimistic"){
    
    return(max(which(Cgw_t>=Cgw)))
    
  }else if(type=="Pessimistic"){
    
    return(min(which(Cgw_t<=Cgw)))
    
  }else{
    
    stop("typo")
    
  }
}


wrapper <- function(initial, final, profit_tplus1, hyper_par, t, OptimalX, index_div){
  
  # scenarios: row = iteration, column = GW, XpI, Cgw
  iter_initial <- nrow(initial)
  iter_final <- nrow(final)
  profit_all <- rep(0, iter_initial)
  k_min <- rep(0, iter_initial)
  L <- 500000
  Cgw <- hyper_par$Cgw
  type <- hyper_par$type
  index_div <- (index_div-1)*iter_initial
  for (a in 1:iter_initial){
    index_div <- index_div + 1
    GW_tminus1 <- initial[a,1]
    xUB <- c(rep(L,10),rep(GW_tminus1,5))
    
    X_p_tminus1 <- initial[a,2]
    Cgw_tminus1 <- initial[a,3]
    
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
        profit_value <- Pt_profit(GW_tminus1 = GW_tminus1, Cgw_tminus1 = Cgw_tminus1, 
                                  GW_t = GW_t, X_p_t = X_p_t, hyper_par = hyper_par, 
                                  X_p_tminus1 = X_p_tminus1, t = t, 
                                  x = OptimalX[[index_Optim]])
        Cgw_t <- FinalSalinity(OptimalX = OptimalX[[index_Optim]], 
                               GW_tminus1 = GW_tminus1, Cgw_tminus1 = Cgw_tminus1, 
                               GW_t = GW_t, X_p_t = X_p_t, hyper_par = hyper_par)
        index <- MatchCgw(Cgw_t = Cgw_t, Cgw = Cgw, type = type)
        profit_temp[b] <- profit_value + profit_tplus1[b, index]
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
