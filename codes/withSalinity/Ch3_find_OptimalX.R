library(snowfall)
library(nloptr)
#dir <- "/Users/johnn/Documents/Research/Salinity/"
dir <- "/soe/wjzhao/project/Salinity/"
setwd(dir)
source(paste0(getwd(), "/codes/withSalinity/Salinity_funs.R"))

########## Find optimal X ###############
GW <- seq(8000000, 12000000, 500000)
XpI <- seq(25000, 75000, 5000)
XpF <- seq(50000, 150000, 10000)
Cgw <- seq(500, 5000, 500)

type <- NULL
hyper_par <- list(x0 = numeric(15), P = c(.25, .25, .2, .2, .1), 
                  xLB = numeric(15), R = 0.035, type = type, Cgw = Cgw)


#### set up number of cores
NofCore <- 11


#### set up initial and final table
initial <- CreateGrid(list(GW, XpI, Cgw))
init_fin <- CreateGrid(list(GW, XpF, GW, XpI, Cgw))
#final <- CreateGrid(GW, XpF)


#### start parallel
sfInit(parallel = TRUE, cpus = NofCore, type = "SOCK")
sfLibrary(nloptr)

#### find solutions
Init_Fin_Divde <- DivideWork(init_fin, NofCore)
tmp_time <- proc.time()
sfExportAll()
temp_OptimalX <- sfLapply(x = Init_Fin_Divde, fun = function(W) findX(init_fin = W, 
                                                                      t = 3, hyper_par = hyper_par))

OptimalX_backup <- unlist(temp_OptimalX, recursive = FALSE)
sfStop()
save(OptimalX_backup, file = paste0(getwd(), "/results/withSalinity/OptimalX2.RData"))