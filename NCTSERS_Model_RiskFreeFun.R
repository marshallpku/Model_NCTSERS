

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
library(zoo)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library(xlsx)
library("btools")
library("scales")

source("Functions.R")



load("./Results/Outputs_t1_RS1_riskFree.RData")

df_riskFreeALNC <- outputs_list$results %>% 
  filter(sim == 0) %>% 
  ungroup() %>% 
  select(year, AL_rf = AL, NC_rf = NC) 
  
save(df_riskFreeALNC, file = "./Results/df_riskFreeALNC.RData")

# df_riskFreeNC %>% head
# 
# amort_cd(100, 0.072, 12)
# amort_cd(100, 0.072, 12)
# amort_cd(100, 0.0306, 12)
# amort_cd(142814312015-64246523614, 0.0306, 12) 
# 
# 

df_riskFreeALNC 






