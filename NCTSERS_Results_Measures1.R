# Risk measures for NCTERS

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

results_all <- get_results(IO_folder) %>% select(runname, sim, year, everything())


## Loading revenue projection data
load("GenFund_proj.RData")

results_all %<>% left_join(df_revenue %>% select(year, GenFund = GenFund.proj)) %>% 
  mutate(ERCstate = 0.25 * ERC,
         ERCstate_GF = 100 * ERCstate/(1e6 * GenFund),
         ERC_GF = 100 * ERC/(1e6 * GenFund), 
         AL_GF  = 100 * AL/(1e6 * GenFund))

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

runs_RS <-   c("RS1",
               "RS2",
               "RS3")


runs_ECRSP <-   c("RS1_ECRSP",
                  "RS2_ECRSP",
                  "RS3_ECRSP")

runs_open <-   c( "RS1_open12",
                  "RS1_open24", 
                  "RS1_10y",
                  "RS1_open24_10y")


# runs_alt <- c("RS1_SR1EL1.open", "RS1_SR1EL1.PR", "RS1_SR1EL1.s5",
#             "RS1_SR1allEL1", "RS2_SR1allEL1", "RS3_SR1allEL1")




runs_RS_labels <- c("Assumption Achieved: Baseline",
                     "15 Years of Low Returns",
                     "High Volatility")

runs_ECRSP_labels <- c("Assumption Achieved: ECRSP",
                    "15 Years of Low Returns: ECRSP",
                    "High Volatility: ECRSP")

runs_open_labels <- c("open 12-year amort",
                      "open 24-year amort",
                      "10-year smoothing",
                      "open 24-year amort 10-year smoothing")


# runs_alt_labels    <- c("open amortization", 
#                         "lower payroll growth assumption for amort",
#                         "5-year asset smoothing",
#                         "Shared-risk EEC for all; RS1",
#                         "Shared-risk EEC for all; RS2",
#                         "Shared-risk EEC for all; RS3")


# lab_s1 <- "Scenario 1 \nAssumption Achieved: \nClosed Plan"
# lab_s2 <- "Scenario 2 \nAssumption Achieved: \nOpen Plan"
# lab_s3 <- "Scenario 3 \n15 Years of Low Returns"
# lab_s4 <- "Scenario 4 \nHigh Volatility"
# lab_s5 <- "Scenario 5 \nCurrent Return Assumption"
# lab_s6 <- "Scenario 6 \nLower Return Assumption"


runs_all <- c(runs_RS, runs_ECRSP, runs_open)
runs_all_labels <- c(runs_RS_labels, runs_ECRSP_labels, runs_open_labels)



# Calculate total final ERC rate for runs with DC reform (include ERC to DC in ERC.final_PR)


# in the past 5 years ERC rate
#ERC_rate_5y <- data.frame(year = 2012:2016, ERC_rate_5y = c(0.0869,0.0915, 0.0915 ,0.0998, 0.1078))
ERC_rate_5y <- c(8.69,9.15, 9.15 ,9.98, 10.78)

df_all.stch <- results_all  %>% 
  filter(runname %in% runs_all, sim > 0, year %in% 2017:2046)

df_all.stch %<>%   
  select(runname, sim, year, FR_MA, AL, MA, ERC, EEC, PR, ERC_PR, ERC_GF, ERCstate_GF) %>%
  group_by(runname, sim) %>% 
  mutate(rowNumber = row_number(),
         FR40less  = cumany(FR_MA <= 40),
         FR60less  = cumany(FR_MA <= 60),
         FR75less  = cumany(FR_MA <= 75),
         FR100more = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 64), 
         ERC_hike     = cumany(na2zero(c(ERC_rate_5y, ERC_PR) - lag(c(ERC_rate_5y, ERC_PR), 5) >= 10))[-(1:5)],  # NA for 2016 value: excludes impact of new amort payment in 2017 
         ERCstate_GF_hike  = cumany(na2zero(ERCstate_GF - lag(ERCstate_GF, 5) >= 5))
         ) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less  = 100 * sum(FR40less, na.rm = T)/n(),
            FR60less  = 100 * sum(FR60less, na.rm = T)/n(),
            FR75less  = 100 * sum(FR75less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            ERC_high  = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike  = 100 * sum(ERC_hike, na.rm = T)/n(),
          #  ERC_GF_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm  = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm  = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm  = T),
            
            ERCstate_GF.q10 = quantile(ERCstate_GF, 0.1, na.rm = T),
            ERCstate_GF.q25 = quantile(ERCstate_GF, 0.25, na.rm = T),
            ERCstate_GF.q50 = quantile(ERCstate_GF, 0.5, na.rm = T),
            ERCstate_GF.q75 = quantile(ERCstate_GF, 0.75, na.rm = T),
            ERCstate_GF.q90 = quantile(ERCstate_GF, 0.9, na.rm = T)
            
  ) %>% 
  ungroup() %>%
  mutate(runname.lab = factor(runname, 
                              levels = runs_all, 
                              labels = runs_all_labels))





  
df_all.stch %>% filter(runname == "RS1")
df_all.stch %>% filter(runname == "RS2")
df_all.stch %>% filter(runname == "RS3")

df_all.stch %>% filter(runname == "RS1_open12")
df_all.stch %>% filter(runname == "RS1_open24")
df_all.stch %>% filter(runname == "RS1_open24_10y")
df_all.stch %>% filter(runname == "RS1_10y")




results_all %>% filter(runname == "RS1", sim == 0)  %>% select(runname, year, FR_MA, AL,MA, AA, PR, NC,B,SC, ERC_PR, i, ERCrate_max, ERCrate_min, ERC, ERC_original, Amort_basis, i.r, NC_PR, SC_PR)
results_all %>% filter(runname == "RS2", sim == 0 )  %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, ERC_PR, i)
results_all %>% filter(runname == "RS3", sim == 0 )  %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, ERC_PR, i)

results_all %>% filter(runname == "RS1", sim == 0 )  %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, ERC_PR, i, ERCrate_max, ERCrate_min, i.r)
results_all %>% filter(runname == "RS2_ECRSP", sim == 0 )  %>% select(runname, year, FR_MA, AL, PR, NC,B,SC, ERC_PR, i, ERCrate_max, ERCrate_min, ECRSP_effective)

results_all %>% filter(year <=2020) %>% 
  group_by(runname, year) %>% 
  summarize(binding = sum(ERC!= ERC_original))


results_all %>% filter(runname == "RS1", sim == 0)  %>% 
  mutate(interest = (AL * 0.0725 + (NC - B) * ((1 + 0.0725)^0.5 - 1))/1e6,
         AL_BOP   = AL/1e6,
         NC       = NC/1e6,
         B        = B/1e6,
         AL_EOP   = AL_BOP + NC - B + interest,
         PR  = PR/1e6) %>% 
  select(runname, year, AL_BOP, NC,interest, B,  AL_EOP, PR) %>%
  filter(year %in% c(2017, 2020,2025, 2035))





#*****************************************************
## Misc calculations in the report  ####
#*****************************************************

# results_all %>% filter(runname != "Dev.allTiers", year %in% 2017:2046, runname == "RS1_SR1EL1.s5") %>% 
#   group_by(runname, sim) %>%
#   mutate(ERC_PR = ERC_PR - lag(ERC.final_PR)) %>% 
#   filter(ERC_PR >= 4.5) %>% 
#   select(runname, sim, year, ERC_PR, ERC.final_PR)
# 
# 
# results_all %>% select(runname, sim, year, ERC.final.0 = ERC.final) %>% filter(sim > 0, runname == "RS1_SR0EL1") %>%
#   left_join(results_all %>% filter(sim > 0, runname == "RS1_SR1EL1")%>% select(sim, year, ERC.final.1 = ERC.final)) %>% 
#   group_by(sim) %>% 
#   summarise(PV.ERC.0 = sum(ERC.final.0 / 1e6*(1 + 0.075)^(row_number() - 1)),
#             PV.ERC.1 = sum(ERC.final.1 / 1e6*(1 + 0.075)^(row_number() - 1))) %>%
#   mutate(diff.PV.ERC = 1 - PV.ERC.1 / PV.ERC.0 ) %>% 
#   ungroup() %>% 
#   summarise(diff.PV.ERC.q10   = quantile(diff.PV.ERC, 0.1,na.rm = T),
#             diff.PV.ERC.q25   = quantile(diff.PV.ERC, 0.25, na.rm = T),
#             diff.PV.ERC.q50   = quantile(diff.PV.ERC, 0.5, na.rm = T),
#             diff.PV.ERC.q75   = quantile(diff.PV.ERC, 0.75, na.rm = T),
#             diff.PV.ERC.q90   = quantile(diff.PV.ERC, 0.9, na.rm = T))
# 


#*****************************************************
## 10-year and 30 year compound return  ####
#*****************************************************


results_all %>% filter(runname == "RS1", sim > 0) %>%
  group_by(sim) %>%
  summarise(geoReturn30y = get_geoReturn(i.r),
            geoReturn10y = get_geoReturn(i.r[year<=2024])) %>%
  summarise(negReturn30y = sum(geoReturn30y <= 0)/n(),
            negReturn10y = sum(geoReturn10y <= 0)/n())


#*****************************************************
## NC general fund revenue  ####
#*****************************************************

fig_projGenFund <- 
  results_all %>% filter(runname == "RS1", sim == 0) %>% 
  ggplot(aes(x = year, y = GenFund)) + 
  geom_bar(stat = "identity", fill = "skyblue2", color = "grey50", width = 0.5) + 
  theme_bw() + 
  RIG.theme() + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 100000, 10000), labels = comma(seq(0, 100000, 10000))) + 
  labs(#title = "Projected General fund of the state of Pennsylvania",
       y = "$Million",
       x = NULL)
fig_projGenFund


#*****************************************************
##   Distribution of 30-year compound returns ####
#*****************************************************

# Distribution of 30-year compound returns

fig_distReturn <- results_all %>% 
  filter(runname == "RS1", sim > 0) %>% 
  group_by(sim) %>% 
  summarize(geoReturn = get_geoReturn(i.r)) %>% 
  ggplot(aes(100*geoReturn)) + theme_bw() + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 0.5, boundary = 0) + 
  geom_vline(xintercept = 0.072 * 100, color = RIG.red) + 
  scale_x_continuous(breaks = seq(0,20,1))+
  labs(title = "Distribution of 30-year compound annual return over 2,000 simulations",
       x = "%",
       y = "Simulatoin count") + 
  RIG.theme()

fig_distReturn



#**********************************************************
## 1. Current policy: 3 investment return scenarios    ####
#**********************************************************


## 1.1 Current policy: Assumption Achieved    ####

# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Current NC-TSERS funding policy; Scenario 1: Assumption Achieved" 
fig_CP.RS1.FRdist <- df_all.stch %>% filter(runname %in% "RS1") %>% 
  left_join(results_all  %>% 
              filter(runname  %in% "RS1", sim == 0) %>% 
              select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75, FR_det) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(40,200)) + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5),2046)) + 
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
fig_CP.RS1.FRdist
fig_CP.RS1.FRdist$data




# Distribution of ERC as % Payroll
fig.title    <- "Distribution of employer contribution as a percentage of payroll across simulations"
fig.subtitle <- "Current NCTSERS funding policy; Scenario 1: Assumption Achieved"
fig_CP.RS1.ERCdist <- df_all.stch %>% filter(runname %in% "RS1") %>% 
  left_join(results_all  %>%
              filter(runname  %in% "RS1", sim == 0) %>%
              select(runname, year, ERC_det = ERC_PR)) %>%
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_det) %>% 
  gather(type, value, -runname, -year) %>% 
  # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25", "ERC_det")),
             shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25", "ERC_det")))) + 
  theme_bw() + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,25)) + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of payroll") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_CP.RS1.ERCdist


# Risk of low funded ratio
fig.title <- "Probabilities of funded ratio below 75%, 60%, and 40% in any year up to the given year"
fig.subtitle <- "Current NCTSERS funding policy; Scenario 1: Assumption Achieved"
fig_CP.RS1.FR40less <- df_all.stch %>% filter(runname %in% "RS1", year >= 2016) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, FR75less, FR60less, FR40less) %>%
  gather(type, value, -runname, -year) %>% 
  mutate(type = factor(type, levels = c("FR75less", "FR60less", "FR40less"), labels = c("75%","60%", "40%" ))) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = value, color = type, shape = type)) + 
             # color = runname, shape = runname)) + 
  theme_bw() + 
  geom_point(size = 2) + 
  geom_line() + 
  coord_cartesian(ylim = c(0,80)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "Probability of \nfunded ratio below:") + 
  scale_shape_manual(values = c(17,16, 15),  name = "Probability of \nfunded ratio below:") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_CP.RS1.FR40less
fig_CP.RS1.FR40less$data %>% filter(year == 2046)




# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Current NCTSERS funding policy; Scenario 1: Assumption Achieved"
fig_CP.RS1.ERChike <- df_all.stch %>% filter(runname %in% "RS1" , year >= 2016) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(runname, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike)) + theme_bw() + 
  geom_point(size = 2, color = RIG.blue) + geom_line(color = RIG.blue) + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_CP.RS1.ERChike
fig_CP.RS1.ERChike$data %>% filter(year == 2046)



## 1.2 Current policy: Assumption Achieved    ####

lab.RS1 <- "Scenario 1: \nAssumption Achieved: \nBase Case"
lab.RS2 <- "Scenario 2: \n15 Years of Low Returns"
lab.RS3 <- "Scenario 3: \nHigh Volatility"


# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Current NCTSERS funding policy"
fig_CP.RScompare.FRless <- df_all.stch %>% filter(runname %in% c("RS1","RS2", "RS3" ), year >= 2016  ) %>% 
  mutate(runname = factor(runname, labels = c(lab.RS1, lab.RS2, lab.RS3))) %>%  
  select(runname, year, FR75less, FR60less, FR40less) %>% 
  gather(type, value, -runname, -year) %>% 
  mutate(type = factor(type, levels = c("FR75less", "FR60less", "FR40less"), labels = c("75%","60%", "40%" ))) %>% 
  ggplot(aes(x = year, y = value, color = type, shape = type)) + 
  theme_bw() + 
  facet_grid(.~runname) +
  geom_point(size = 2) + 
  geom_line() + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "Probability of \nfunded ratio below:") + 
  scale_shape_manual(values = c(17,16, 15),  name = "Probability of \nfunded ratio below:") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_CP.RScompare.FRless
fig_CP.RScompare.FRless$data %>% filter(year == 2046)



# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Current NCTSERS funding policy"
fig_CP.RScompare.ERChike <- df_all.stch %>% filter(runname %in% c("RS1", "RS2", "RS3" ), year >= 2016) %>% 
  mutate(runname = factor(runname, labels = c(lab.RS1, lab.RS2, lab.RS3))) %>%  
  select(runname, year, ERC_hike) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,100)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_CP.RScompare.ERChike
fig_CP.RScompare.ERChike$data %>% filter(year == 2046)







#*************************************************************************
##                        3. Fiscal analysis                              ####
#*************************************************************************
lab.RS1 <- "Scenario 1: \nAssumption Achieved: \nBase Case"
lab.RS2 <- "Scenario 2: \n15 Years of Low Returns"
lab.RS3 <- "Scenario 3: \nHigh Volatility"


# Deterministic 
fig.title <- "State employer contribution as a percentage of \nNorth Carolina state general fund revenue"
fig.subtitle <- "Current NC-TERS funding policy; Deterministic runs"
fig_fiscal.det <- results_all %>% filter(runname %in% c("RS1","RS2"), sim == 0, year %in% 2017:2046) %>% 
  mutate(runname = factor(runname, levels = c("RS1", "RS2"), labels = c("Scenario 1: Assumption Achieved: \nDeterministic \nAnnual return = 7.2%",
                                                                          "Scenario 2: 15 Years of Low Returns: \nDeterministic \nAnnual return = 6.4%"))) %>%  
  select(runname, year, ERCstate_GF) %>% 
  #mutate(ERChike.det = 0) %>% 
  # gather(variable, value, -year, -returnScn) %>% 
  ggplot(aes(x = year, y = ERCstate_GF, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,4)) + 
  scale_y_continuous(breaks = seq(0,200, 0.5)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of general fund revenue (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_fiscal.det
fig_fiscal.det$data #%>% filter(year == 2045)


# Risk of sharp increase in ERC/PR
fig.title <- "Distribution of state employer contribution as a percentage of North Carolina state general fund revenue \nunder different return scenarios"
fig.subtitle <- "Current NCTSERS funding policy"
fig_fiscal.stch <- df_all.stch %>% filter(runname %in% c("RS1","RS2", "RS3"), year %in% 2016:2046) %>%
  # mutate(ERC_GF.xSchool.q25 = 0.5 * ERC_GF.q25,
  #        ERC_GF.xSchool.q50 = 0.5 * ERC_GF.q50,
  #        ERC_GF.xSchool.q75 = 0.5 * ERC_GF.q75) %>% # about 50% is paid by school districts
  select(runname, year, ERCstate_GF.q25, 
                        ERCstate_GF.q50, 
                        ERCstate_GF.q75) %>% 
  gather(var, value, -year, -runname) %>% 
  mutate(var       = factor(var, levels = c("ERCstate_GF.q75", "ERCstate_GF.q50", "ERCstate_GF.q25"),
                                 labels = c("75th percentile", "50th percentile", "25th percentile")),
         returnScn = factor(runname, levels = c("RS1", "RS2", "RS3"), 
                                       labels = c(lab.RS1, lab.RS2, lab.RS3))) %>%  
  # gather(variable, value, -year, -returnScn) %>% 
  ggplot(aes(x = year, y = value, color = var, shape = var)) + theme_bw() + 
  facet_grid(. ~ returnScn) + 
  geom_point(size = 1.5) + geom_line() + 
  coord_cartesian(ylim = c(0,4)) + 
  scale_y_continuous(breaks = seq(0,200, 0.5)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of general fund revenue (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_fiscal.stch
fig_fiscal.stch$data %>% filter(year == 2017)
fig_fiscal.stch$data %>% filter(year == 2027)
fig_fiscal.stch$data %>% filter(year == 2046)

fig_fiscal.stch$data

#*************************************************************************
##                        Summary table                              ####
#*************************************************************************

runs_report <- c("RS1", "RS2", "RS3")

lvl_measures  <- c("FR40less","FR60less","FR75less", "ERC_hike")

# Summary tables for the three major risk measures
tab_summary <-
  df_all.stch %>% filter(runname %in% c(runs_report ), year == 2046) %>%
  select(runname, FR40less,FR60less, FR75less, ERC_hike) %>%
  gather(Measure, value, -runname) %>%
  mutate(runname = factor(runname, levels = runs_report),
         Measure = factor(Measure, levels = lvl_measures)) %>%
  spread(runname, value)

tab_summary

write.xlsx2(tab_summary, paste0(Outputs_folder, "tables.xlsx"), sheetName = "summary")





#*************************************************************************
##                        Saving results                              ####
#*************************************************************************

g.height.1col <- 7*0.8
g.width.1col  <- 10*0.8

g.height.2col <- 6*0.8
g.width.2col  <- 13*0.8

g.height.3col <- 5*0.8
g.width.3col  <- 15*0.8



ggsave(file = paste0(Outputs_folder, "distReturn.png"),   fig_distReturn, height = g.height.1col, width = g.width.1col)

# 1. Current policy: assumption achieved
ggsave(file = paste0(Outputs_folder, "fig.CP.RS1.FRdist.png"),   fig_CP.RS1.FRdist,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig.CP.RS1.FR40less.png"), fig_CP.RS1.FR40less,height = g.height.1col, width = g.width.1col*1.1)
ggsave(file = paste0(Outputs_folder, "fig.CP.RS1.ERCdist.png"),  fig_CP.RS1.ERCdist, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig.CP.RS1.ERChike.png"),  fig_CP.RS1.ERChike, height = g.height.1col, width = g.width.1col*0.8)

ggsave(file = paste0(Outputs_folder, "fig.CP.RS1.FRdist.pdf"),   fig_CP.RS1.FRdist,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig.CP.RS1.FR40less.pdf"), fig_CP.RS1.FR40less,height = g.height.1col, width = g.width.1col*1.1)
ggsave(file = paste0(Outputs_folder, "fig.CP.RS1.ERCdist.pdf"),  fig_CP.RS1.ERCdist, height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig.CP.RS1.ERChike.pdf"),  fig_CP.RS1.ERChike, height = g.height.1col, width = g.width.1col*0.8)



# 2. Current policy: alt return scn
ggsave(file = paste0(Outputs_folder, "fig.CP.RScompare.FRless.png"), fig_CP.RScompare.FRless, height = g.height.1col*0.8, width = g.width.1col*1.35)
ggsave(file = paste0(Outputs_folder, "fig.CP.RScompare.ERChike.png"),fig_CP.RScompare.ERChike, height = g.height.1col, width = g.width.1col)

ggsave(file = paste0(Outputs_folder, "fig.CP.RScompare.FRless.pdf"), fig_CP.RScompare.FRless,height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig.CP.RScompare.ERChike.pdf"),fig_CP.RScompare.ERChike, height = g.height.1col, width = g.width.1col)


# 3. Fiscal
ggsave(file = paste0(Outputs_folder, "fig8.fiscal.det.png"),  fig_fiscal.det, height = g.height.1col, width = g.width.1col*1.1)
ggsave(file = paste0(Outputs_folder, "fig9.fiscal.stch.png"), fig_fiscal.stch, height = g.height.3col*0.9*1.15, width = g.width.3col*0.9)

ggsave(file = paste0(Outputs_folder, "fig8.fiscal.det.pdf"),  fig_fiscal.det, height = g.height.1col, width = g.width.1col*1.1)
ggsave(file = paste0(Outputs_folder, "fig9.fiscal.stch.pdf"), fig_fiscal.stch, height = g.height.3col*0.9*1.15, width = g.width.3col*0.9)


ggsave(file = paste0(Outputs_folder, "fig11.projGenFun.png"),  fig_projGenFund,  height = g.height.1col, width = g.width.1col)
ggsave(file = paste0(Outputs_folder, "fig11.projGenFun.pdf"),  fig_projGenFund,  height = g.height.1col, width = g.width.1col)




#fig_fiscal.stch$data

# 
# # Risk of high increase in ERC/GF
# fig.title <- "Probability of employer contribution rising above 10% of general fund \nat any time prior to and including the given year"
# fig.subtitle <- "Assumption achieved; expected compound return = 8%"
# fig_stchDet.ERChigh <- df_all.stch %>% filter(runname %in% rn_RS1) %>% 
#   select(runname, year, ERC_high) %>% 
#   #mutate(ERChike.det = 0) %>% 
#   #gather(variable, value, - year) %>% 
#   ggplot(aes(x = year, y = ERC_high, color = runname, shape = runname)) + theme_bw() + 
#   geom_point(size = 2) + geom_line() + 
#   coord_cartesian(ylim = c(0,50)) + 
#   scale_y_continuous(breaks = seq(0,200, 5)) + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_color_manual(values = c("black", RIG.red, RIG.blue, RIG.green, RIG.purple),  name = "") + 
#   scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
#   labs(title = fig.title,
#        subtitle = fig.subtitle,
#        x = NULL, y = "Probability (%)") + 
#   RIG.theme()
# fig_stchDet.ERChigh
# 
# 


# results_all %>% filter(runname == "RS1_SR1EL1", sim == 1 ) %>% 
#   select(runname, year, UAAL, EUAAL, LG, SC, Amort_basis) %>% 
#   mutate(basis1 = UAAL - (lag(UAAL - SC)) * 1.0725,
#          diff = basis1 - Amort_basis) 




# Reconciliation with Pew

results_all %>% filter(runname == "RS1_SR1EL1", sim == -1) %>% 
  mutate(FY     = year + 1,
         AL_EOP = lead(AL)
         #I.r = (AL + NC - B) * 0.0725 
         ) %>% 
  select(FY, AL_BOP = AL, NC, I.r, B, AL_EOP) %>% 
  filter(FY %in% c(2017, 2021, 2026, 2036)) %>% 
  mutate_at(vars(-FY), funs(./1e6)) %>% 
  kable(digits = 0)





#*************************************************************************
##     Comparision with GASB reporting projections                    ####
#*************************************************************************

read_ExcelRange(file = "./Data_inputs/NCTSERS_PlanInfo_AV2016.xlsx", sheet = "GASBcashflow")











