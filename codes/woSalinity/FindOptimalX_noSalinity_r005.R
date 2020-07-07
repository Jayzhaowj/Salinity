library(snowfall)
library(nloptr)

##############################
########## functions #########
##############################

dir <- "/soe/wjzhao/project/Salinity/"
setwd(dir)
source(paste0(getwd(), "/codes/woSalinity/NoSalinity_funs.R"))

########## Find OptimalX #############

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
hyper_par <- list(x0 = numeric(15), P = c(0.2, 0.2, 0.2, 0.2, 0.2), xLB = numeric(15), R = 0.05, INI_P = 12000)

initial <- CreateGrid(list(GW, XpI))
init_fin <- CreateGrid(list(GW, XpF, GW, XpI))
#final <- CreateGrid(GW, XpF)


# start parallel

sfInit(parallel = TRUE, cpus = NofCore, type = "SOCK")
sfLibrary(nloptr)

#Initial_Divide <- DivideWork(initial, NofCore)
Init_Fin_Divde <- DivideWork(init_fin, NofCore)
#profit_all <- matrix(0, nrow  = nrow(initial), ncol = N+1)
#k_min <- matrix(0, nrow = nrow(initial), ncol = N)


sfExportAll()
temp_OptimalX <- sfLapply(x = Init_Fin_Divde, fun = function(W) findX(init_fin = W, 
                                                                      t = 3, hyper_par = hyper_par))
#OptimalX <- unlist(temp_OptimalX, recursive = FALSE)
OptimalX_backup <- unlist(temp_OptimalX, recursive = FALSE)
sfStop()
save(OptimalX_backup, file = paste0(getwd(), "/results/woSalinity/OptimalX_woSalinity_r005.RData"))
