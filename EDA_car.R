rm(list = ls())
gc()
setwd("D:/rProg/Rank_consistency")
read.csv(file = "./data/racing_data.csv", header = F)

library(MASS)
source('D:/JJJ/ranking problem/Jeon-2014-03-05 order consistency/experiment/library/car_lib.r')
source('D:/JJJ/ranking problem/Jeon-2014-03-05 order consistency/experiment/library/lib_rank.r')
require('glmnet')
source('C:/Users/uos_stat/Dropbox/A rank-consistency/prog/library/sim.R')