# Risk measures for MISERS

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



#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder       <- "Results/"
Outputs_folder  <- "Results/Graphs_report/"


#*****************************************************
##  Loading data  ####
#*****************************************************

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    
    # if("results.t7" %in% names(outputs_list)){
    #   df_out <- bind_rows(outputs_list$results,
    #                       outputs_list$results.t7,
    #                       outputs_list$results.xt7)
    #   return(df_out)
    # } else {
    #   return(outputs_list$results)
    # }
    
    return(outputs_list$results)
    
  }
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}



results_all <- get_results(IO_folder, "results_") %>% select(runname, sim, year, everything())


## Loading revenue projection data
load("GenFund_proj.RData")

results_all %<>% left_join(df_revenue %>% select(year, GenFund = GenFund.proj)) %>% 
  mutate(ERC.final_GF = 100 * ERC.final/GenFund, 
         AL_GF  = 100 * AL/GenFund)


#results_all %>% head


#**********************************************************************************************
##  Defining color and theme for publication format of Rockefeller Institute of Government ####
#**********************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}






#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

rn_policy <- c("SR0EL0",
               "SR1EL1",
               "SR0EL1",
               "SR1EL0",
               "SR2EL1",
               "SR1EL2")

# Runs used in the report 
runs_RS1 <- paste0("RS1_", rn_policy)
runs_RS2 <- paste0("RS2_", rn_policy)
runs_RS3 <- paste0("RS3_", rn_policy)


runs_RS1_labels <- c("Assumption Achieved: Baseline",
                     "Assumption Achieved: Current Policy",
                     "Assumption Achieved: ERC limit Only",
                     "Assumption Achieved: Risk-sharing Only",
                     "Assumption Achieved: Risk-sharing 5% max",
                     "Assumption Achieved: No ERC floor")

runs_RS2_labels <- c("15 Years of Low Returns : Baseline",
                     "15 Years of Low Returns : Current Policy",
                     "15 Years of Low Returns : ERC limit Only",
                     "15 Years of Low Returns : Risk-sharing Only",
                     "15 Years of Low Returns : Risk-sharing 5% max",
                     "15 Years of Low Returns: No ERC floor")

runs_RS3_labels <- c("High Volatility: Baseline",
                     "High Volatility: Current Policy",
                     "High Volatility: ERC limit Only",
                     "High Volatility: Risk-sharing Only",
                     "High Volatility: Risk-sharing 5% max",
                     "High Volatility: No ERC floor")




# lab_s1 <- "Scenario 1 \nAssumption Achieved: \nClosed Plan"
# lab_s2 <- "Scenario 2 \nAssumption Achieved: \nOpen Plan"
# lab_s3 <- "Scenario 3 \n15 Years of Low Returns"
# lab_s4 <- "Scenario 4 \nHigh Volatility"
# lab_s5 <- "Scenario 5 \nCurrent Return Assumption"
# lab_s6 <- "Scenario 6 \nLower Return Assumption"


runs_all <- c(runs_RS1, runs_RS2, runs_RS3)
runs_all_labels <- c(runs_RS1_labels, runs_RS2_labels, runs_RS3_labels)


df_all.stch <- results_all  %>% 
  filter(runname %in% runs_all, sim > 0, year %in% 2015:2045)


df_all.stch %<>%   
  select(runname, sim, year, FR_MA, AL, MA, ERC, EEC, PR, ERC_PR, ERC.final_PR, ERC.final_GF) %>%
  group_by(runname, sim) %>% 
  mutate(
         #FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more = FR_MA >= 100,
         ERC_high  = cumany(ERC.final_PR >= 50), 
         ERC_hike     = cumany(na2zero(ERC.final_PR - lag(ERC.final_PR, 5) >= 10)),
         ERC_GF_hike  = cumany(na2zero(ERC.final_GF - lag(ERC.final_GF, 5) >= 5)),
         EEC_PR       = 100 * EEC / PR
         ) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            ERC_GF_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC.final_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC.final_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC.final_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC.final_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC.final_PR, 0.9, na.rm = T),
            
            EEC_PR.q10 = quantile(EEC_PR, 0.1, na.rm = T),
            EEC_PR.q25 = quantile(EEC_PR, 0.25, na.rm = T),
            EEC_PR.q50 = quantile(EEC_PR, 0.5, na.rm = T),
            EEC_PR.q75 = quantile(EEC_PR, 0.75, na.rm = T),
            EEC_PR.q90 = quantile(EEC_PR, 0.9, na.rm = T),
            
            ERC_GF.q10 = quantile(ERC.final_GF, 0.1, na.rm = T),
            ERC_GF.q25 = quantile(ERC.final_GF, 0.25, na.rm = T),
            ERC_GF.q50 = quantile(ERC.final_GF, 0.5, na.rm = T),
            ERC_GF.q75 = quantile(ERC.final_GF, 0.75, na.rm = T),
            ERC_GF.q90 = quantile(ERC.final_GF, 0.9, na.rm = T)
            
  ) %>% 
  ungroup() %>%
  mutate(runname.lab = factor(runname, 
                              levels = runs_all, 
                              labels = runs_all_labels))


  
df_all.stch %>% filter(runname == "RS1_SR1EL1")
df_all.stch %>% filter(runname == "RS1_SR0EL1")
df_all.stch %>% filter(runname == "RS1_SR1EL0")
df_all.stch %>% filter(runname == "RS1_SR0EL0")
df_all.stch %>% filter(runname == "RS1_SR2EL1")
df_all.stch %>% filter(runname == "RS1_SR1EL2")

df_all.stch %>% filter(runname == "RS2_SR1EL1")
df_all.stch %>% filter(runname == "RS2_SR0EL1")
df_all.stch %>% filter(runname == "RS2_SR1EL0")
df_all.stch %>% filter(runname == "RS2_SR0EL0")
df_all.stch %>% filter(runname == "RS2_SR2EL1")
df_all.stch %>% filter(runname == "RS2_SR1EL2")

df_all.stch %>% filter(runname == "RS3_SR1EL1")
df_all.stch %>% filter(runname == "RS3_SR0EL1")
df_all.stch %>% filter(runname == "RS3_SR1EL0")
df_all.stch %>% filter(runname == "RS3_SR0EL0")
df_all.stch %>% filter(runname == "RS3_SR2EL1")
df_all.stch %>% filter(runname == "RS3_SR1EL2")



df_all.stch %>% filter(runname == "RS2_SR1EL1")
df_all.stch %>% filter(runname == "RS2_SR1EL0")

df_all.stch %>% filter(runname == "RS3_SR1EL1")
df_all.stch %>% filter(runname == "RS3_SR1EL0")


results_all %>% filter(runname == "RS1_SR1EL1", sim == 54 ) %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, EEC_PR, ERC_PR, ERC.final_PR, i.r)
results_all %>% filter(runname == "RS1_SR0EL0", sim == 1 ) %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, EEC_PR, ERC_PR, ERC.final_PR, i.r)


results_all %>% filter(runname == "RS2_SR1EL1", sim == 1 ) %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, EEC_PR, ERC_PR, ERC.final_PR, i.r)
results_all %>% filter(runname == "RS2_SR0EL0", sim == 1 ) %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, EEC_PR, ERC_PR, ERC.final_PR, i.r)


results_all %>% 
  group_by(runname, sim) %>%
  summarise(ERC_PR = max(ERC_PR - lag(ERC_PR), na.rm = TRUE)) %>% 
  group_by(runname) %>% 
  summarise(ERC_PR = max(ERC_PR - lag(ERC_PR), na.rm = TRUE))

results_all %>% filter(runname != "Dev.allTiers") %>% 
  group_by(runname, sim) %>%
  summarise(ERC_PR = max(ERC_PR - lag(ERC.final_PR), na.rm = TRUE)) %>% 
  filter(ERC_PR >= 4.5)
  
results_all %>% filter(runname != "Dev.allTiers") %>% 
  group_by(runname, sim) %>%
  mutate(ERC_PR = ERC_PR - lag(ERC.final_PR)) %>% 
  filter(ERC_PR >= 4.5) %>% 
  select(runname, sim, year, ERC_PR, ERC.final_PR)



results_all %>% filter(runname == "RS1_SR1EL1", sim == 1) %>% 
  group_by(runname, sim) %>%
  mutate(PR.growth = PR/lag(PR) - 1 ) %>% 
  select(sim, year, PR, PR.growth)
  
  










# results_all %>% filter(runname == "RS1.closed", sim == 0, year %in% c(2015, 2016, 2030:2045) ) %>% select(runname, Tier, year, AL, AL.DC, PR, NC, NC.DC,B,SC, EEC,ERC, GenFund, ERC, nactives)
# 
# results_all %>% filter(runname == "RS1.closed", sim == 13 ) %>% select(runname,sim, Tier, year, AL, PR, NC,SC, EEC,ERC, GenFund, ERC, ERC_GF, i.r, UAAL, FR_MA) 
# results_all %>% filter(runname == "RS1.open",   sim == 13 ) %>% select(runname,sim, Tier, year, AL, PR, NC,SC, EEC,ERC, GenFund, ERC, ERC_GF, i.r, UAAL) 
# 
# results_all %>% filter(runname == "RS4.closed", sim == 0, year %in% c(2016:2020) ) %>% select(runname, Tier, year, AL, PR, SC, ERC, GenFund, ERC_GF) %>% mutate(AL_PR = AL/PR)
# results_all %>% filter(runname == "RS5.closed", sim == 0, year %in% c(2016:2020) ) %>% select(runname, Tier, year, AL, PR, SC, ERC, GenFund, ERC_GF) %>% mutate(AL_PR = AL/PR)




#*****************************************************
## 10-year and 30 year compound return  ####
#*****************************************************


results_all %>% filter(runname == "RS1_SR1EL1", sim > 0) %>%
  group_by(sim) %>%
  summarise(geoReturn30y = get_geoReturn(i.r),
            geoReturn10y = get_geoReturn(i.r[year<=2024])) %>%
  summarise(negReturn30y = sum(geoReturn30y <= 0)/n(),
            negReturn10y = sum(geoReturn10y <= 0)/n())



#*****************************************************
## Deterministic run  ####
#*****************************************************

fig_projGenFund <- 
  results_all %>% filter(runname == "RS1_SR1EL1", sim == 0) %>% 
  ggplot(aes(x = year, y = GenFund/1e6)) + 
  geom_bar(stat = "identity", fill = "skyblue2", color = "grey50", width = 0.5) + 
  theme_bw() + 
  RIG.theme() + 
  scale_x_continuous(breaks = c(2015, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 100000, 5000), labels = comma(seq(0, 100000, 5000))) + 
  labs(title = "Projected General fund of the State of Pennsylvania",
       y = "$Million",
       x = NULL)
fig_projGenFund


#*****************************************************
##   Distribution of 30-year compound returns ####
#*****************************************************

# Distribution of 30-year compound returns

fig_distReturn <- results_all %>% 
  filter(runname == "RS1_SR1EL1", sim > 0) %>% 
  group_by(sim) %>% 
  summarize(geoReturn = get_geoReturn(i.r)) %>% 
  ggplot(aes(100*geoReturn)) + theme_bw() + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 0.5, boundary = 0) + 
  geom_vline(xintercept = 0.075 * 100, color = RIG.red) + 
  scale_x_continuous(breaks = seq(0,20,1))+
  labs(title = "Distribution of 30-year compound annual return over 2,000 simulations",
       x = "%",
       y = "Simulatoin count") + 
  RIG.theme()

fig_distReturn



#*****************************************************
## 1 Assumption achieved; Different policies   ####
#*****************************************************

rn_RS1 <- paste0("RS1_", 
                 c("SR1EL1",
                   "SR1EL0",
                   "SR0EL1",
                   "SR0EL0"))

# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7.5%"
fig_stchDet.FRdist <- df_all.stch %>% filter(runname %in% rn_RS1 ) %>% 
  left_join(results_all  %>% 
              filter(runname  %in% rn_RS1, sim == 0) %>% 
              select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75, FR_det) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  facet_grid(.~runname) +
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,200)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()

fig_stchDet.FRdist



# Distribution of ERC as % Payroll
fig.title <- "Distribution of employer contribution as a percentage of general fund across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7.5%"
fig_stchDet.ERCdist <- df_all.stch %>% filter(runname %in% rn_RS1) %>% 
  left_join(results_all  %>%
              filter(runname  %in% rn_RS1, sim == 0) %>%
              select(runname, year, ERC_det = ERC.final_PR)) %>%
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
             shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
  facet_grid(. ~ runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,50)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of payroll") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_stchDet.ERCdist


# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7.5%"
fig_stchDet.FR40less <- df_all.stch %>% filter(runname %in% rn_RS1) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, FR40less) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less)) + 
             # color = runname, shape = runname)) + 
  theme_bw() + 
  facet_grid(. ~ runname) + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,35)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black",RIG.red),  name = "") + 
  scale_shape_manual(values = c(17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_stchDet.FR40less
fig_stchDet.FR40less$data %>% filter(year == 2045)


# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_stchDet.ERChike <- df_all.stch %>% filter(runname %in% c(rn_RS1, "RS1_SR2EL1") ) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,25)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2015, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_stchDet.ERChike
fig_stchDet.ERChike$data %>% filter(year == 2045)


# Risk of high increase in ERC/GF
fig.title <- "Probability of employer contribution rising above 10% of general fund \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_stchDet.ERChigh <- df_all.stch %>% filter(runname %in% rn_RS1) %>% 
  select(runname, year, ERC_high) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_stchDet.ERChigh



#*****************************************************
## 2 15 years of low returns; Different policies   ####
#*****************************************************

rn_RS2 <- paste0("RS2_", 
                 c("SR1EL1",
                   "SR1EL0",
                   "SR0EL1",
                   "SR0EL0"))

# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7.5%"
fig_RS2.FRdist <- df_all.stch %>% filter(runname %in% rn_RS2 ) %>% 
  left_join(results_all  %>% 
              filter(runname  %in% rn_RS2, sim == 0) %>% 
              select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75, FR_det) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  facet_grid(.~runname) +
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,200)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()

fig_RS2.FRdist



# Distribution of ERC as % Payroll
fig.title <- "Distribution of employer contribution as a percentage of general fund across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7.5%"
fig_RS2.ERCdist <- df_all.stch %>% filter(runname %in% rn_RS2) %>% 
  left_join(results_all  %>%
              filter(runname  %in% rn_RS2, sim == 0) %>%
              select(runname, year, ERC_det = ERC.final_PR)) %>%
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
             shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
  facet_grid(. ~ runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,60)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of payroll") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_RS2.ERCdist


# Distribution of EEC as % Payroll
fig.title <- "Distribution of employee contribution as a percentage of general fund across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7.5%"
fig_RS2.EECdist <- df_all.stch %>% filter(runname %in% paste0("RS2_", c("SR0EL1", "SR1EL1", "SR2EL1"))) %>% 
  left_join(results_all  %>%
              filter(runname  %in% rn_RS2, sim == 0) %>%
              select(runname, year, EEC_det = EEC_PR)) %>%
  select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")),
             shape = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
  facet_grid(. ~ runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,15)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 2)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of payroll") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_RS2.EECdist



# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7.5%"
fig_RS2.FR40less <- df_all.stch %>% filter(runname %in% rn_RS2) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, FR40less) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less)) + 
  # color = runname, shape = runname)) + 
  theme_bw() + 
  facet_grid(. ~ runname) + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black",RIG.red),  name = "") + 
  scale_shape_manual(values = c(17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_RS2.FR40less
fig_RS2.FR40less$data %>% filter(year == 2045)


# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_RS2.ERChike <- df_all.stch %>% filter(runname %in% c(rn_RS2, "RS2_SR2EL1") ) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,40)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2015, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_RS2.ERChike
fig_RS2.ERChike$data %>% filter(year == 2045)


# Risk of high increase in ERC/GF
fig.title <- "Probability of employer contribution rising above 10% of general fund \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_RS2.ERChigh <- df_all.stch %>% filter(runname %in%  c(rn_RS2, "RS2_SR2EL1")) %>% 
  select(runname, year, ERC_high) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_RS2.ERChigh
fig_RS2.ERChigh$data %>% filter(year == 2045)




#*****************************************************
## 3 High Volatility; Different policies   ####
#*****************************************************

rn_RS3 <- paste0("RS3_", 
                 c("SR1EL1",
                   "SR1EL0",
                   "SR0EL1",
                   "SR0EL0"))

# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7.5%"
fig_RS3.FRdist <- df_all.stch %>% filter(runname %in% rn_RS3 ) %>% 
  left_join(results_all  %>% 
              filter(runname  %in% rn_RS3, sim == 0) %>% 
              select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75, FR_det) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  facet_grid(.~runname) +
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,220)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()

fig_RS3.FRdist



# Distribution of ERC as % Payroll
fig.title <- "Distribution of employer contribution as a percentage of general fund across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7.5%"
fig_RS3.ERCdist <- df_all.stch %>% filter(runname %in% rn_RS3) %>% 
  left_join(results_all  %>%
              filter(runname  %in% rn_RS3, sim == 0) %>%
              select(runname, year, ERC_det = ERC.final_PR)) %>%
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
             shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
  facet_grid(. ~ runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,60)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of payroll") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_RS3.ERCdist
fig_RS2.ERCdist


# Distribution of EEC as % Payroll
fig.title <- "Distribution of employee contribution as a percentage of general fund across simulations"
fig.subtitle <- "Assumption achieved: expected compound return = 7.5%"
fig_RS3.EECdist <- df_all.stch %>% filter(runname %in% paste0("RS3_", c("SR0EL1", "SR1EL1", "SR2EL1"))) %>% 
  left_join(results_all  %>%
              filter(runname  %in% rn_RS3, sim == 0) %>%
              select(runname, year, EEC_det = EEC_PR)) %>%
  select(runname, year, EEC_PR.q25, EEC_PR.q50, EEC_PR.q75) %>% 
  gather(type, value, -runname, -year) %>% 
  # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")),
             shape = factor(type, levels = c("EEC_PR.q75", "EEC_PR.q50", "EEC_PR.q25")))) + 
  facet_grid(. ~ runname) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,15)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 2)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of payroll") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_RS3.EECdist




# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 7.5%"
fig_RS3.FR40less <- df_all.stch %>% filter(runname %in% rn_RS3) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, FR40less) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = FR40less)) + 
  # color = runname, shape = runname)) + 
  theme_bw() + 
  facet_grid(. ~ runname) + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black",RIG.red),  name = "") + 
  scale_shape_manual(values = c(17,16),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_RS3.FR40less
fig_RS3.FR40less$data %>% filter(year == 2045)


# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_RS3.ERChike <- df_all.stch %>% filter(runname %in% c(rn_RS3, "RS3_SR2EL1") ) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,40)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2015, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_RS3.ERChike
fig_RS3.ERChike$data %>% filter(year == 2045)


# Risk of high increase in ERC/PR
fig.title <- "Probability of employer contribution rising above 10% of general fund \nat any time prior to and including the given year"
fig.subtitle <- "Assumption achieved; expected compound return = 8%"
fig_RS3.ERChigh <- df_all.stch %>% filter(runname %in%  c(rn_RS3, "RS3_SR2EL1")) %>% 
  select(runname, year, ERC_high) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,50)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  RIG.theme()
fig_RS3.ERChigh
fig_RS3.ERChigh$data %>% filter(year == 2045)






#*************************************************************************
##                     Summary tables ####
#*************************************************************************


# 
# runs_report <- c("RS1_SR0EL0","RS1_SR1EL1", "RS1_SR1EL0", "RS1_SR0EL1" , "RS1_SR2EL1",
#                  "RS2_SR0EL0","RS2_SR1EL1", "RS2_SR1EL0", "RS2_SR0EL1" , "RS2_SR2EL1",
#                  "RS3_SR0EL0","RS3_SR1EL1", "RS3_SR1EL0", "RS3_SR0EL1" , "RS3_SR2EL1")


runs_report <- c("RS1_SR0EL0", "RS1_SR0EL1", "RS1_SR1EL0", "RS1_SR2EL1",
                 "RS2_SR0EL0", "RS2_SR0EL1", "RS2_SR1EL0", "RS2_SR2EL1",
                 "RS3_SR0EL0", "RS3_SR0EL1", "RS3_SR1EL0", "RS3_SR2EL1")

lvl_measures  <- c("FR40less", "ERC_hike")

# Summary tables for the three major risk measures
tab_summary <- 
  df_all.stch %>% filter(runname %in% c(runs_report ), year == 2045) %>% 
  select(runname, FR40less, ERC_hike) %>% 
  gather(Measure, value, -runname) %>% 
  mutate(runname = factor(runname, levels = runs_report),
         Measure = factor(Measure, levels = lvl_measures)) %>% 
  spread(runname, value)

tab_summary

write.xlsx2(tab_summary1, paste0(Outputs_folder, "tables.xlsx"), sheetName = "summary", append = TRUE)





#*************************************************************************
##                        Saving results                              ####
#*************************************************************************

g.height.1col <- 7*0.8
g.width.1col  <- 10*0.8

g.height.2col <- 6*0.8
g.width.2col  <- 13*0.8

g.height.3col <- 5*0.8
g.width.3col  <- 15*0.8



ggsave(file = paste0(Outputs_folder, "distReturn.png"),   fig_distReturn, height = 7*0.8, width = 10*0.8)




# closed and open plan

ggsave(file = paste0(Outputs_folder, "AL.png"),  fig_AL,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "stchDet.FRdist.png"),  fig_stchDet.FRdist,  height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERCdist.png"), fig_stchDet.ERCdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERC.USDdist.png"), fig_stchDet.ERC.USDdist, height = g.height.2col, width = g.width.2col)


ggsave(file = paste0(Outputs_folder, "stchDet.ERC_Med.png"), fig_stchDet.ERC_Med, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERC_GF_Med.png"), fig_stchDet.ERC_GF_Med, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "stchDet.FR40less.png"), fig_stchDet.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERChike.png"),  fig_stchDet.ERChike, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "stchDet.ERChigh.png"),  fig_stchDet.ERChigh, height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "min5.FR40less.png"), fig_min5.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "min5.ERChike.png"),  fig_min5.ERChike, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "min5.FRdist.png"),  fig_min5.FRdist,  height = g.height.2col, width = g.width.2col)



# low returns and high volatility
ggsave(file = paste0(Outputs_folder, "RS.FRdist.png"),   fig_RS.FRdist,  height = g.height.3col, width = g.width.3col)
ggsave(file = paste0(Outputs_folder, "RS.ERCdist.png"),  fig_RS.ERCdist, height = g.height.3col, width = g.width.3col)

ggsave(file = paste0(Outputs_folder, "RS.FR40less.png"), fig_RS.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "RS.ERChike.png"),  fig_RS.ERChike,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "RS.ERChigh.png"),  fig_RS.ERChigh,  height = g.height.1col, width = g.width.1col)


# Lowering discount rate

ggsave(file = paste0(Outputs_folder, "DC.DetERC.png"), fig_DC.DetERC,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "DC.DetFR.png"),  fig_DC.DetFR, height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "DC.FRdist.png"),   fig_DC.FRdist,  height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "DC.ERCdist.png"),  fig_DC.ERCdist, height = g.height.2col, width = g.width.2col)

ggsave(file = paste0(Outputs_folder, "DC.FR40less.png"), fig_DC.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "DC.ERChike.png"),  fig_DC.ERChike,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "DC.ERChigh.png"),  fig_DC.ERChigh,  height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "projGenFun.png"),  fig_projGenFund,  height = g.height.1col, width = g.width.1col)



ggsave(file = paste0(Outputs_folder, "fig1_AL.pdf"),  fig_AL,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig2_stchDet.FR40less.pdf"), fig_stchDet.FR40less, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig3_stchDet.ERC.USDdist.pdf"), fig_stchDet.ERC.USDdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "fig4_stchDet.ERCdist.pdf"), fig_stchDet.ERCdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "fig5_stchDet.ERChike.pdf"),  fig_stchDet.ERChike, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig6_RS.FR40less.pdf"), fig_RS.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig7_RS.ERChike.pdf"),  fig_RS.ERChike,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig8_DC.DetERC.pdf"), fig_DC.DetERC,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig9_DC.FR40less.pdf"), fig_DC.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig10_DC.ERChike.pdf"),  fig_DC.ERChike,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig11_projGenFun.pdf"),  fig_projGenFund,  height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "fig1_AL.png"),  fig_AL,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig2_stchDet.FR40less.png"), fig_stchDet.FR40less, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig3_stchDet.ERC.USDdist.png"), fig_stchDet.ERC.USDdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "fig4_stchDet.ERCdist.png"), fig_stchDet.ERCdist, height = g.height.2col, width = g.width.2col)
ggsave(file = paste0(Outputs_folder, "fig5_stchDet.ERChike.png"),  fig_stchDet.ERChike, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig6_RS.FR40less.png"), fig_RS.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig7_RS.ERChike.png"),  fig_RS.ERChike,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig8_DC.DetERC.png"), fig_DC.DetERC,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig9_DC.FR40less.png"), fig_DC.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig10_DC.ERChike.png"),  fig_DC.ERChike,  height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig11_projGenFun.png"),  fig_projGenFund,  height = g.height.1col, width = g.width.1col)


ggsave(file = paste0(Outputs_folder, "fig12_min5.FR40less.pdf"), fig_min5.FR40less, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig13_min5.ERChike.pdf"),  fig_min5.ERChike, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig14_min5.FRdist.pdf"),  fig_min5.FRdist,  height = g.height.2col, width = g.width.2col)














