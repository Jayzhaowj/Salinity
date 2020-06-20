library(snowfall)
library(nloptr)
dir <- "/Users/johnn/Documents/Research/Salinity/"
setwd(dir)
source(paste0(getwd(), "/codes/withSalinity/Salinity_funs.R"))

######## load OptimalX ########
load(paste0(getwd(), "/results/withSalinity/OptimalX2.RData"))
########constants##########

N <- 10


###########################################
### run dynamic programming with optimistic way
###########################################


########## DP #############
# GW <- seq(8000000, 10000000, length.out = 2)
# XpI <- seq(50000, 75000, length.out = 2)
# XpF <- seq(100000, 150000, length.out = 2)
# Cgw <- seq(500, 5000, length.out = 2)

GW <- seq(8000000, 12000000, 500000)
XpI <- seq(25000, 75000, 5000)
XpF <- seq(50000, 150000, 10000)
Cgw <- seq(500, 5000, 500)



##############################################

#### set up type and hyper_par
type <- "Optimistic"
hyper_par <- list(x0 = numeric(15), P = c(.25, .25, .2, .2, .1), 
                  xLB = numeric(15), R = 0.035, type = type, Cgw = Cgw)


#### set up number of cores
NofCore <- 6


#### set up initial and final table
initial <- CreateGrid(list(GW, XpI, Cgw))
init_fin <- CreateGrid(list(GW, XpF, GW, XpI, Cgw))
#final <- CreateGrid(GW, XpF)


#### start parallel
sfInit(parallel = TRUE, cpus = NofCore, type = "SOCK")
sfLibrary(nloptr)

###########################################

#### dynamic programming
Initial_Divide <- DivideWork(initial, NofCore)

profit_all_Optim <- rep(list(matrix(0, nrow  = length(GW)*length(XpI), ncol = length(Cgw))), N+1)
k_min_Optim <- matrix(0, nrow = nrow(initial), ncol = N)


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
                                                    final = final, profit_tplus1 = profit_all_Optim[[t+1]],
                                                    hyper_par = hyper_par, t = t, OptimalX = OptimalX, 
                                                    index_div = W))
  #temp_result <- lapply(X = Initial_Divide, FUN = function(W) wrapper(initial = W, final = final, profit_tplus1 = profit_all[[t+1]],
  #                                                                  hyper_par = hyper_par, t = t))
  
  #temp_result <- wrapper(initial = initial, final = final, 
  #                       profit_tplus1 = profit_all[[t+1]],
  #                       hyper_par = hyper_par, t = t, OptimalX = OptimalX)
  #temp_result <-  wrapper(initial = Initial_Divide[[1]], final = final, profit_tplus1 = profit_all[,t+1], hyper_par = hyper_par, t = t)
  
  temp_profit <- unlist(lapply(temp_result, FUN = function(x) x$profit_all))
  #temp_profit <- temp_result$profit_all
  profit_all_Optim[[t]] <- matrix(temp_profit, ncol = length(Cgw))
  k_min_Optim[,t] <- unlist(lapply(temp_result, FUN = function(x) x$k_min))
  #k_min[,t] <- temp_result$k_min
  cat("t=", t, "/", N, "\n")
  
}


###########################################
### run dynamic programming with pessimistic way
###########################################
type <- "Pessimistic"
hyper_par <- list(x0 = numeric(15), P = c(.25, .25, .2, .2, .1), 
                  xLB = numeric(15), R = 0.035, type = type, Cgw = Cgw)

Initial_Divide <- DivideWork(initial, NofCore)

profit_all_Pessim <- rep(list(matrix(0, nrow  = length(GW)*length(XpI), ncol = length(Cgw))), N+1)
k_min_Pessim <- matrix(0, nrow = nrow(initial), ncol = N)

#OptimalX <- unlist(temp_OptimalX, recursive = FALSE)

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
                                                    final = final, profit_tplus1 = profit_all_Pessim[[t+1]],
                                                    hyper_par = hyper_par, t = t, OptimalX = OptimalX, 
                                                    index_div = W))
  #temp_result <- lapply(X = Initial_Divide, FUN = function(W) wrapper(initial = W, final = final, profit_tplus1 = profit_all[[t+1]],
  #                                                                  hyper_par = hyper_par, t = t))
  
  #temp_result <- wrapper(initial = initial, final = final, 
  #                       profit_tplus1 = profit_all[[t+1]],
  #                       hyper_par = hyper_par, t = t, OptimalX = OptimalX)
  #temp_result <-  wrapper(initial = Initial_Divide[[1]], final = final, profit_tplus1 = profit_all[,t+1], hyper_par = hyper_par, t = t)
  
  temp_profit <- unlist(lapply(temp_result, FUN = function(x) x$profit_all))
  #temp_profit <- temp_result$profit_all
  profit_all_Pessim[[t]] <- matrix(temp_profit, ncol = length(Cgw))
  k_min_Pessim[,t] <- unlist(lapply(temp_result, FUN = function(x) x$k_min))
  #k_min[,t] <- temp_result$k_min
  cat("t=", t, "/", N, "\n")
  
}
time <- proc.time() - tmp_time
sfStop()
profit_all <- list("Pessim" = profit_all_Pessim, "Optim" = profit_all_Optim)
k_min <- list("Pessim" = k_min_Pessim, "Optim" = k_min_Optim)
#save(profit_all, k_min, time, OptimalX_backup,
#     file = "/soe/wjzhao/project/SalinityServer/result4.RData")
save(profit_all, k_min, time, OptimalX_backup, file = paste0(getwd(), "/results/withSalinity/result_drier.RData"))
