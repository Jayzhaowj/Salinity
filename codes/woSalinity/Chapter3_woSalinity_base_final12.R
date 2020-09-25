library(snowfall)
library(nloptr)

########constants##########

N <- 10

##############################
########## functions #########
##############################

dir <- "/soe/wjzhao/project/Salinity/"
setwd(dir)
source(paste0(getwd(), "/codes/woSalinity/NoSalinity_funs.R"))


########Load OptimalX#########

load(paste0(getwd(), "/results/woSalinity/OptimalX_woSalinity_base.RData"))


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

sfInit(parallel = TRUE, cpus = NofCore, type = "SOCK")
sfLibrary(nloptr)


######Start DP##########

Initial_Divide <- DivideWork(initial, NofCore)
#Init_Fin_Divde <- DivideWork(init_fin, NofCore)
profit_all <- matrix(0, nrow  = nrow(initial), ncol = N+1)
k_min <- matrix(0, nrow = nrow(initial), ncol = N)

sfExportAll()
#temp_OptimalX <- sfLapply(x = Init_Fin_Divde, fun = function(W) findX(init_fin = W, 
#                                                                      t = 3, hyper_par = hyper_par))
#OptimalX <- unlist(temp_OptimalX, recursive = FALSE)
#OptimalX_backup <- unlist(temp_OptimalX, recursive = FALSE)
#sfExportAll()

for (t in N:1){
  
  if (t == N){
    
    final <- CreateGrid(list(12000000, XpF))
    init_fin_index <- init_fin[, 1] == 12000000
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
     file = "/soe/wjzhao/project/Salinity/results/woSalinity/woSalinity_result_base_final12.RData")

