# Fiscal analysis of LAFPP

# Risk measures for LAFPP

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


# demo.shape5 <- c(16, 16, 16, 15, 17) # 16-average, 15-mature, 17-immature 


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}



#**********************************************************************************************
##  Load revenue data and extend the projection into 2044  ####
#**********************************************************************************************

# source: City of LA Revenue Outlook, FY 2016-17
df_revenue <- read_ExcelRange("Data_inputs/PSERS_PlanInfo_AV2015.xlsx", sheet = "Fiscal")

# extend the projection into 2044 using the projected growth rate of 2.9% in 2020 
rev.growth <- 0.033

df_revenue %<>% 
  mutate(GenFund.proj = ifelse(year <= 2021, GenFund.original, GenFund.original[year == 2021] * (1 + rev.growth)^(year - 2021))) 

df_revenue


save(df_revenue, file = "GenFund_proj.RData")






# 
# #**********************************************************************************************
# ##  Fiscal analysis:  ####
# #**********************************************************************************************
# 
# # Assumption about the non-pension ERC in the LAFPP system (mainly health subsidy):
# # Assume non-pension ERC is 25% of total LAFPP system ERC
# share_LAFPP.health <- 0.25
# 
# # Assumption about the total contributions of LACERS:
# # Assume total LACERS contribution is 85% of total LAFPP system ERC (pension + health)
# factor_LACERS <- 0.85 
# 
# df_estimates <- results_all %>% 
#   filter(run.returnScn %in% paste0("RS", 1:5),run.policyScn == "noCap") %>%
#   select(run.returnScn, sim, year, ERC) %>% 
#   left_join(df_revenue) %>% 
#   mutate(ERC.LAFPP.pension = ERC,
#          ERC.LAFPP = ERC / (1-share_LAFPP.health),
#          ERC.LAFPP.health = ERC.LAFPP - ERC.LAFPP.pension, 
#          ERC.LACERS = ERC.LAFPP * factor_LACERS,
#          #ERC.tot    = ERC.LAFPP + ERC.LACERS, 
#          #ERC.LAFPP.pension_GenFund = 100 * ERC.LAFPP.pension / GenFund.proj,
#          #ERC.LAFPP_GenFund = 100 * ERC.LAFPP / GenFund.proj,
#          ERC.LACERS_GenFund = 100 * ERC.LACERS / GenFund.proj) %>% 
#   #ERC.tot_GenFund = 100 * ERC.tot / GenFund.proj
#   select(-ERC, -ERC.LAFPP, -ERC.LAFPP.pension)
# 
# 
# results_fiscal <- 
#   results_all %>% 
#   left_join(df_estimates) %>% 
#   mutate(ERC.LAFPP.pension = ERC,
#          ERC.LAFPP = ERC.LAFPP.pension + ERC.LAFPP.health,
#          ERC.tot    = ERC.LAFPP + ERC.LACERS, 
#          ERC.LAFPP.pension_GenFund = 100 * ERC.LAFPP.pension / GenFund.proj,
#          ERC.LAFPP_GenFund = 100 * ERC.LAFPP / GenFund.proj,
#          ERC.LACERS_GenFund = 100 * ERC.LACERS / GenFund.proj,
#          ERC.tot_GenFund = 100 * ERC.tot / GenFund.proj) %>% 
#   select(runname,run.policyScn, run.returnScn, run.policyScn.lab, run.returnScn.lab,
#          Tier, sim, year, C, EEC, ERC, GenFund.proj, ERC_PR,
#          ERC.LAFPP, ERC.LAFPP.health, ERC.LAFPP.pension,
#          ERC.LACERS,
#          ERC.tot,
#          ERC.LAFPP.pension_GenFund,
#          ERC.LAFPP_GenFund,
#          ERC.LACERS_GenFund,
#          ERC.tot_GenFund,
#          NC) 
# 
# 
# fig_projGenFund <- 
#   results_fiscal %>% filter(runname == "RS1", sim == 0) %>% 
#   ggplot(aes(x = year, y = GenFund.proj/1e6)) + 
#   geom_bar(stat = "identity", fill = "skyblue2", color = "grey50", width = 0.5) + 
#   theme_bw() + 
#   RIG.theme() + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 20000, 1000), labels = comma(seq(0, 20000, 1000))) + 
#   labs(title = "Projected General fund of the Los Angeles City",
#        y = "$Million",
#        x = NULL)
# fig_projGenFund
# 
# 
# 
# # #**********************************************************************************************
# ##  Fiscal analysis: Deterministic Run  ####
# #**********************************************************************************************
# ## Deterministic run
# results_fiscal.det <- 
#   results_fiscal %>%  filter(sim ==0, Tier == "sumTiers") %>% 
#   select(runname, Tier, run.policyScn, run.returnScn, run.policyScn.lab, run.returnScn.lab, sim, year,
#          ERC.LAFPP.pension_GenFund, 
#          ERC.LAFPP_GenFund,
#          ERC.tot_GenFund)
# 
# results_fiscal.det
# 
# # LAFPP pension ERC
# fig_det_LAFPP <- 
#   results_fiscal.det %>% 
#   filter(run.returnScn %in% paste0("RS", c(1, 3, 5)), run.policyScn %in% c("noCap", "cap", "cap.allTiers" )) %>% 
#   ggplot(aes(x = year, y = ERC.LAFPP.pension_GenFund, color = run.returnScn.lab))  + 
#   facet_grid(.~run.policyScn.lab) + 
#   geom_point() + 
#   geom_line()+ 
#   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
#   scale_y_continuous(breaks = seq(0, 100, 2)) + 
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "Return scenarios") + 
#   coord_cartesian(ylim = c(0,20)) + 
#   theme_bw() + 
#   RIG.theme() +
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   labs(title = "ERC for LAFPP (pension only) as a percentage of General Fund of LA",
#        subtitle = "Deterministic runs",
#        x = "Year",
#        y = "Percent")
# fig_det_LAFPP
# 
# results_fiscal.det %>% 
#   filter(run.returnScn %in% paste0("RS", c(1)), run.policyScn %in% c("noCap" )) 
# 
# 
# 
# results_all %>% filter(runname == "RS1", sim == 0) %>% select(year, SC, C)
# results_fiscal.det %>% filter(runname == "RS1") %>% select(year,  ERC.LAFPP.pension_GenFund)
# results_fiscal.det %>% filter(runname == "RS3") %>% select(year,  ERC.LAFPP.pension_GenFund)
# 
# 
# 
# # LAFPP pension ERC
# 
# n1 <- "Notes:"
# n2 <- "\n   Scenario 1: annual "
# n3 <- "\n   Scenario 4: Expected compound return is 6.6%, standard deviation is 12%"
# fig.caption <- paste0(n1, n2, n3)
# 
# fig.labels <- c("Scenario 1: Assumption Achieved:\nDeterministic\nAnnual return = 7.5%",
#                 "Deterministic version of \nScenario 6: Target Asset Allocation\nAnnual return = 6.1%")
# 
# fig_det_LAFPP.noCap <- 
#   results_fiscal.det %>% 
#   filter(run.returnScn %in% paste0("RS", c(1, 5)), run.policyScn %in% c("noCap")) %>% 
#   ggplot(aes(x = year, y = ERC.LAFPP.pension_GenFund, color = run.returnScn.lab, shape = run.returnScn.lab))  + 
#   geom_point() + 
#   geom_line()+ 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 100, 2)) + 
#   scale_color_manual(values = c(RIG.blue, RIG.red, RIG.blue), name = "Return scenarios",
#                      labels = fig.labels) + 
#   scale_shape_manual(values = c(16, 17, 15), name = "Return scenarios",
#                      labels = fig.labels) + 
#   coord_cartesian(ylim = c(0,18)) + 
#   theme_bw() + 
#   RIG.theme() +
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
#   labs(title = "Employer contribution as a percentage of City general fund revenue",
#        subtitle = "Deterministic runs",
#        #caption = fig.caption,
#        x = "Year",
#        y = "Percent")
# fig_det_LAFPP.noCap
# 
# 
# 
# #**********************************************************************************************
# ## Stochastic runs: policies  ####
# #**********************************************************************************************
# ## Stochastic runs
# 
# results_fiscal.stch <- 
#   results_fiscal %>%  filter(Tier == "sumTiers", sim >0) %>%
#   group_by(run.returnScn, run.policyScn, year) %>% 
#   summarise(
#     ERC.LAFPP.pension_GenFund.q10   = quantile(ERC.LAFPP.pension_GenFund, 0.1,  na.rm = T),
#     ERC.LAFPP.pension_GenFund.q25   = quantile(ERC.LAFPP.pension_GenFund, 0.25, na.rm = T),
#     ERC.LAFPP.pension_GenFund.q50   = quantile(ERC.LAFPP.pension_GenFund, 0.50, na.rm = T),
#     ERC.LAFPP.pension_GenFund.q75   = quantile(ERC.LAFPP.pension_GenFund, 0.75, na.rm = T),
#     ERC.LAFPP.pension_GenFund.q90   = quantile(ERC.LAFPP.pension_GenFund, 0.90, na.rm = T),
#     
#     ERC.tot_GenFund.q10   = quantile(ERC.tot_GenFund, 0.1,  na.rm = T),
#     ERC.tot_GenFund.q25   = quantile(ERC.tot_GenFund, 0.25, na.rm = T),
#     ERC.tot_GenFund.q50   = quantile(ERC.tot_GenFund, 0.50, na.rm = T),
#     ERC.tot_GenFund.q75   = quantile(ERC.tot_GenFund, 0.75, na.rm = T),
#     ERC.tot_GenFund.q90   = quantile(ERC.tot_GenFund, 0.90, na.rm = T))
# 
# results_fiscal.stch
# 
# # Distribution under current policy and assumption is met
# 
# 
# # LAFPP ERC  
# fig_stch.LAFPP <- 
#   results_fiscal.stch %>% 
#   filter(run.returnScn %in% paste0("RS", c(1)), run.policyScn %in% c("noCap")) %>% 
#   select(run.returnScn, run.policyScn, year, 
#          ERC.LAFPP.pension_GenFund.q25, 
#          ERC.LAFPP.pension_GenFund.q50, 
#          ERC.LAFPP.pension_GenFund.q75) %>% 
#   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
#   mutate(qtile = factor(qtile, levels = c("ERC.LAFPP.pension_GenFund.q75", "ERC.LAFPP.pension_GenFund.q50", "ERC.LAFPP.pension_GenFund.q25"),
#                         labels = c("75th percentile", "50th percentile", "25th percentile"))) %>% 
#   ggplot(aes(x = year, y = value , color = qtile))  + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 100, 2)) + 
#   scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green), name = "") + 
#   coord_cartesian(ylim = c(0,20)) + 
#   theme_bw() + 
#   RIG.theme() +
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
#   labs(title = "Distribution of ERC for LAFPP (pension only) \nas a percentage of General Fund of LA",
#        subtitle = "Current policy; expected return = 7.5%",
#        x = "Year",
#        y = "Percent")
# fig_stch.LAFPP
# 
# results_fiscal.stch %>% filter(run.returnScn == "RS1", run.policyScn == "noCap")
# results_fiscal.stch %>% filter(run.returnScn == "RS4", run.policyScn == "noCap")
# results_fiscal.stch %>% filter(run.returnScn == "RS5", run.policyScn == "noCap")
# 
# #**********************************************************************************************
# ## Low returns in early years ####
# #**********************************************************************************************
# labs.lowReturns <- c("Scenario 2: \nAssumption achieved", 
#                      "Scenario 3: \n5 years of low returns", 
#                      "Scenario 4: \n15 years of low returns")
# 
# 
# # LAFPP ERC
# fig_lowR.LAFPP <- 
#   results_fiscal.stch %>% 
#   filter(run.returnScn %in% paste0("RS", c(1,2,3)), run.policyScn %in% c("noCap")) %>% 
#   select(run.returnScn, run.policyScn, year,
#          ERC.LAFPP.pension_GenFund.q25, 
#          ERC.LAFPP.pension_GenFund.q50, 
#          ERC.LAFPP.pension_GenFund.q75) %>% 
#   ungroup %>% 
#   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
#   mutate(qtile = factor(qtile, levels = c("ERC.LAFPP.pension_GenFund.q75", 
#                                           "ERC.LAFPP.pension_GenFund.q50", 
#                                           "ERC.LAFPP.pension_GenFund.q25"),
#                         labels = c("75th percentile", 
#                                    "50th percentile", 
#                                    "25th percentile")),
#          run.returnScn = factor(run.returnScn, labels = labs.lowReturns)) %>% 
#   ggplot(aes(x = year, y = value , color = qtile))  + 
#   facet_grid(. ~ run.returnScn) + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2040, 5))) + 
#   scale_y_continuous(breaks = seq(0, 100, 2)) + 
#   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "") + 
#   coord_cartesian(ylim = c(0,29)) + 
#   theme_bw() + 
#   RIG.theme() +
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
#   labs(title = "Distribution of ERC for LAFPP (pension only) \nas a percentage of General Fund of LA",
#        subtitle = "Current policy; low expected returns in early years",
#        x = "Year",
#        y = "Percent")
# fig_lowR.LAFPP
# 
# 
# #**********************************************************************************************
# ## Alternative risk-return profiles ####
# #**********************************************************************************************
# labs.altAssumptions <- c("Scenario 2: \nAssumption Achieved: \nStochastic Base Case", 
#                          "Scenario 5: \nHigh Volatility", 
#                          "Scenario 6: \nTarget Asset Allocation")
# 
# 
# # LAFPP ERC
# 
# 
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 5: Expected compound return is 7.5%, standard deviation is 17.2%"
# n4 <- "\n   Scenario 6: Expected compound return is 6.1%, standard deviation is 13.4%"
# fig.caption <- paste0(n1, n2, n3, n4)
# 
# fig_alt.LAFPP <- 
#   results_fiscal.stch %>% 
#   filter(run.returnScn %in% paste0("RS", c(1,4,5)), run.policyScn %in% c("noCap")) %>% 
#   select(run.returnScn, run.policyScn, year,
#          ERC.LAFPP.pension_GenFund.q25, 
#          ERC.LAFPP.pension_GenFund.q50, 
#          ERC.LAFPP.pension_GenFund.q75) %>% 
#   ungroup %>% 
#   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
#   mutate(qtile = factor(qtile, levels = c("ERC.LAFPP.pension_GenFund.q75", 
#                                           "ERC.LAFPP.pension_GenFund.q50", 
#                                           "ERC.LAFPP.pension_GenFund.q25"),
#                         labels = c("75th percentile", 
#                                    "50th percentile", 
#                                    "25th percentile")),
#          run.returnScn = factor(run.returnScn, labels = labs.altAssumptions)) %>% 
#   ggplot(aes(x = year, y = value , color = qtile))  + 
#   facet_grid(. ~ run.returnScn) + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 100, 5)) + 
#   scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green), name = "") + 
#   coord_cartesian(ylim = c(0,29)) + 
#   theme_bw() + 
#   RIG.theme() +
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
#   labs(title = "Distribution of ERC for LAFPP (pension only) \nas a percentage of General Fund of LA under different return scenarios",
#        subtitle = "Current policy",
#        caption = fig.caption,
#        x = NULL,
#        y = "Percent")
# fig_alt.LAFPP 
# 
# 
# 
# 
# 
# n1 <- "Notes:"
# n2 <- "\n   Scenario 2: Expected compound return is 7.5%, standard deviation is 12%"
# n3 <- "\n   Scenario 6: Expected compound return is 6.1%, standard deviation is 13.4%"
# fig.caption <- paste0(n1, n2, n3)
# 
# fig_alt2.LAFPP <- 
#   results_fiscal.stch %>% 
#   filter(run.returnScn %in% paste0("RS", c(2, 5)), run.policyScn %in% c("noCap")) %>% 
#   select(run.returnScn, run.policyScn, year,
#          ERC.LAFPP.pension_GenFund.q25, 
#          ERC.LAFPP.pension_GenFund.q50, 
#          ERC.LAFPP.pension_GenFund.q75) %>% 
#   ungroup %>% 
#   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
#   mutate(qtile = factor(qtile, levels = c("ERC.LAFPP.pension_GenFund.q75", 
#                                           "ERC.LAFPP.pension_GenFund.q50", 
#                                           "ERC.LAFPP.pension_GenFund.q25"),
#                         labels = c("75th percentile", 
#                                    "50th percentile", 
#                                    "25th percentile")),
#          run.returnScn = factor(run.returnScn, labels = labs.altAssumptions[c(1,3)])) %>% 
#   ggplot(aes(x = year, y = value , color = qtile, shape = qtile))  + 
#   facet_grid(. ~ run.returnScn) + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
#   scale_y_continuous(breaks = seq(0, 100, 5)) + 
#   scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green), name = "") + 
#   scale_shape_manual(values = c(17, 16, 15), name = "") + 
#   coord_cartesian(ylim = c(0,25)) + 
#   theme_bw() + 
#   RIG.theme() +
#   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
#   labs(title = "Distribution of employer contribution as a percentage of City general fund revenue \nunder different return scenarios",
#        subtitle = "Current policy",
#        caption = fig.caption,
#        x = NULL,
#        y = "Percent")
# fig_alt2.LAFPP 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggsave(file = paste0(Outputs_folder, "fig15_projGenFund.png"), fig_projGenFund, height = 6*0.9, width = 10*0.9)
# 
# ggsave(file = paste0(Outputs_folder, "fig_det_LAFPP.png"), fig_det_LAFPP, height = 6*0.8, width = 15*0.8)
# 
# ggsave(file = paste0(Outputs_folder, "fig16_det_LAFPP.noCap.png"), fig_det_LAFPP.noCap, height = 7*0.75, width = 10*0.8)
# 
# ggsave(file = paste0(Outputs_folder, "fig_stch.LAFPP.png"), fig_stch.LAFPP, height = 7*0.8, width = 10*0.8)
# 
# ggsave(file = paste0(Outputs_folder, "fig_lowR.LAFPP.png"), fig_lowR.LAFPP, height = 6*0.8, width = 15*0.8)
# 
# ggsave(file = paste0(Outputs_folder, "fig_alt.LAFPP.png"), fig_alt.LAFPP, height = 6*0.75, width = 15*0.75)
# 
# ggsave(file = paste0(Outputs_folder, "fig17_alt2.LAFPP.png"), fig_alt2.LAFPP, height = 7*0.7, width = 13*0.7)
# 
# 
# ggsave(file = paste0(Outputs_folder, "fig15_projGenFund.pdf"), fig_projGenFund, height = 6*0.9, width = 10*0.9)
# ggsave(file = paste0(Outputs_folder, "fig16_det_LAFPP.noCap.pdf"), fig_det_LAFPP.noCap, height = 7*0.75, width = 10*0.8)
# ggsave(file = paste0(Outputs_folder, "fig17_alt2.LAFPP.pdf"), fig_alt2.LAFPP, height = 7*0.7, width = 13*0.7)
# 
# 
# 
# #**************************************************************************
# # LACERS ####
# #**************************************************************************
# 
# ERC.LACERS_2016 <- 565857179
# 
# df_LACERS <- 
#   results_fiscal %>% 
#   filter(Tier == "sumTiers") %>% 
#   select(runname, sim, year,run.policyScn, run.returnScn, ERC, GenFund.proj) %>% 
#   group_by(runname, sim) %>% 
#   mutate( 
#     ERC.growth = ERC/ERC[year == 2016],
#     ERC.LACERS = ERC.growth*ERC.LACERS_2016,
#     ERC.LACERS_GenFund = 100 * ERC.LACERS / GenFund.proj,
#     ERC.LAFPP_GenFund = 100 * ERC / GenFund.proj,
#     ERC.tot_GenFund = 100 * (ERC + ERC.LACERS) / GenFund.proj)
# 
# df_LACERS %>% filter(runname == "RS1", sim == 0)
# df_LACERS %>% filter(runname == "RS5", sim == 0)
# 
# 
# df_LACERS_qctile <- 
#   df_LACERS %>%  filter( sim > 0) %>%
#   group_by(run.returnScn, run.policyScn, year) %>% 
#   summarise(
#     ERC.LACERS_GenFund.q10  = quantile(ERC.LACERS_GenFund, 0.1,  na.rm = T),
#     ERC.LACERS_GenFund.q25  = quantile(ERC.LACERS_GenFund, 0.25, na.rm = T),
#     ERC.LACERS_GenFund.q50  = quantile(ERC.LACERS_GenFund, 0.50, na.rm = T),
#     ERC.LACERS_GenFund.q75  = quantile(ERC.LACERS_GenFund, 0.75, na.rm = T),
#     ERC.LACERS_GenFund.q90  = quantile(ERC.LACERS_GenFund, 0.90, na.rm = T))
# 
# df_LACERS_qctile %>% filter(run.returnScn == "RS1", run.policyScn == "noCap")
# 
# 
# 
# #**************************************************************************
# # For LAFPP ####
# #**************************************************************************
# 
# 
# df_det <- results_all  %>% 
#   left_join(df_revenue) %>% 
#   filter(runname == "RS1", sim == 0, year <= 2045) %>% 
#   select(year, FR_MA, B, ERC,  ERC_PR, GenFund.proj, NC, SC, LG, NC, SC) %>%
#   mutate(ERC_GenFund = 100 * ERC/GenFund.proj,
#          NC.growth  = 100 * (NC - lag(NC))/NC,
#          SC.growth  = 100 * (SC - lag(SC))/SC,
#          NC.factor  = 100*NC / NC[year == 2016],
#          SC.factor  = 100*SC / SC[year == 2016]) %>% 
#   mutate_at(vars(-year, -FR_MA, -ERC_PR, -ERC_GenFund, -NC.growth, -SC.growth, -NC.factor, -SC.factor), funs(./1e6)) %>% 
#   select(year, FR_MA, B, ERC, GenFund.proj, ERC_PR, ERC_GenFund, NC.growth, SC.growth, NC.factor, SC.factor, LG, NC, SC) 
# 
# df_det
# df_det.5y <- df_det %>% filter(year %in% c(2016, seq(2020, 2045,5)))
# 
# 
# write.csv(df_det, file = paste0(Outputs_folder, "LAFPP.csv"))
# write.csv(df_det.5y, file = paste0(Outputs_folder, "LAFPP_5y.csv"))
# 
# 
# df_det %>% filter(year %in% c(2016, seq(2020, 2045, 5)))
# 
# 
# #**************************************************************************
# # For LAFPP 2 ####
# #**************************************************************************
# 
# df_det2 <- results_all  %>% 
#   left_join(df_revenue) %>% 
#   filter(runname == "RS1", sim == 0, year <= 2045) %>% 
#   select(year, AL, FR_MA, ERC, EEC, NC, SC, MA, C, B, PR) %>% 
#   mutate_at(vars(-year, -FR_MA), funs(./1e6)) %>% 
#   mutate(MA_eb  = (MA + C - B) * 1.075,
#          InvInc =  (MA + C - B) * 0.075)
# 
# df_det2 
# 
# 
# 
# write.csv(df_det2, file = paste0(Outputs_folder, "LAFPP2.csv"))

