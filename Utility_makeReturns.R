# calculate compound mean returns for return series provided by Susie. 
# 4/19/2016
# Yimeng Yin

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library(xlsx)
library("btools")

source("Functions.R")

######################################################

create_returns <- function(r.mean, r.sd, period){
  i.r <- unlist(mapply(rnorm, period, r.mean, r.sd))
}




#RS3. 
r.mean <- c(0.0572, 0.0722, 0.0792)
r.sd   <- c(0.12, 0.12, 0.12)
period <- c(10, 5, 15)

#set.seed(1234)
replicate(50000, create_returns(r.mean, r.sd, period) %>% get_geoReturn) %>% mean

(1.05^10*1.065^5*1.075^15)^(1/30)


# stochastic: 6.54-6.56%
# Deterministic: 6.49%






