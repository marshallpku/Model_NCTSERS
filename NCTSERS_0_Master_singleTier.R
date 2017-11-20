

gc()

Tier_select <- paramlist$tier

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************
# Plan information
 # source("NCTSERS_Data_PlanInfo_AV2016.R")
 # source("NCTSERS_Data_MemberData_AV2016.R")

load("Data_inputs/NCTSERS_PlanInfo_AV2016.RData")    # for all tiers
load("Data_inputs/NCTSERS_MemberData_AV2016.RData")  # for all tiers
           

#**********************************************
##              Calibration                ####
#**********************************************


## Liability for active members and normal cost (Data_prep)
  # 1. Adjust benefit factor
    paramlist$bfactor <- paramlist$bfactor *  1.125

  # 2. Adjust salary growth rate
    sal.adj <- TRUE
    f.adj <- 1
    f1 <- 0 #0.3 
    f2 <- 0 #0.125 
    #
    
# Across-the-board increase in salary growth rates (NCTSERS_Model_prepData.R)

  # 3.adj.factor.add = 0.0075
    factor_salgrowth.add <- 0.0075


## Liabilities for retirees and beneficiaries. 
 # 1. reduce initial benefit
  init_retirees_all %<>% mutate(benefit = benefit * 0.96 ) # 0.96) 
 # 2. increase mortality rates for retirees and survivors. (NCTSERS_Model_Decrements)
  mortality.adj <-  1.025


#**********************************************
##   Modify initial data ####
#**********************************************

## Exclude selected type(s) of initial members
#init_actives_all %<>% mutate(nactives = 0)
# init_retirees_all %<>% mutate(nretirees = 0)
# init_beneficiaries_all %<>% mutate(nbeneficiaries = 0)
# init_terms_all %<>% mutate(nterms = 0)
# init_disb_all %<>% mutate(ndisb = 0)

#init_actives_all %<>% mutate(nactives = ifelse(ea == 30 & age == 30, 1, 0) ) 


# ## Exclude the initial amortization basis when testing the program.
# if(!paramlist$useAVamort) init_amort_raw %<>% mutate(amount.annual = 0) 




init_beneficiaries_all %<>% filter(age >= 25) 

paramlist$pct.ca.M <-  0.4 # proportion of males who opt for ca upon retirement
paramlist$pct.ca.F <-  0.4

pct.init.ret.ca <- 0.4  # 0.4
pct.init.ret.la  <- 1 - pct.init.ret.ca

pct.init.disb.ca <-  0
pct.init.disb.la  <- 1 - pct.init.disb.ca

init_retirees.la_all <- init_retirees_all %>%
  mutate(nretirees.la = nretirees * pct.init.ret.la) %>% 
  select(-nretirees)

init_retirees.ca_all <- init_retirees_all %>%
  mutate(nretirees.ca = nretirees * pct.init.ret.ca) %>% 
  select(-nretirees)

init_disb.la_all <- init_disb_all %>%
  mutate(ndisb.la = ndisb * pct.init.disb.la) %>% 
  select(-ndisb)

init_disb.ca_all <- init_disb_all %>%
  mutate(ndisb.ca = ndisb * pct.init.disb.ca) %>% 
  select(-ndisb)




#*********************************************************************************************************
# 1.2 Create decrement tables ####
#*********************************************************************************************************

# Decrement tables
source("NCTSERS_Model_Decrements.R")

# For testing
# termRates1 %<>% mutate_at(vars(-yos), funs(.*0))
# termRates2 %<>% mutate_at(vars(-age), funs(.*0))
# disbRates  %<>% mutate_at(vars(-age), funs(.*0))

list.decrements      <- get_decrements(Tier_select)

#save(list.decrements, file = "./list.decrements.test.RData")
#load("./list.decrements.test.RData")

decrement.model      <- list.decrements$decrement.model
mortality.post.model <- list.decrements$mortality.post.model





#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("NCTSERS_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.2 Create plan data ####
#*********************************************************************************************************

source("NCTSERS_Model_PrepData.R")

salary        <- get_salary_proc(Tier_select)
benefit       <- get_benefit_tier(Tier_select)
benefit.disb  <- get_benefit.disb_tier(Tier_select)
init_pop      <- get_initPop_tier(Tier_select)
entrants_dist <- get_entrantsDist_tier(Tier_select)
#entrants_dist <- numeric(length(paramlist$range_ea))

#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************
source("NCTSERS_Model_Demographics.R")
gc()
pop <- get_Population()

# pop$la %>% filter(year == 2015, year.r == 2015, number.la !=0)




#*********************************************************************************************************
# 3. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("NCTSERS_Model_ContingentAnnuity_generational.R")

# For service retirement
liab.ca <- get_contingentAnnuity(Tier_select,
                                 paramlist$factor.ca, # tier.param[Tier_select, "factor.ca"],
                                 min(paramlist$range_age):100,
                                 apply_reduction = TRUE)

 
# # For disability benefit
# range_age.disb <-  min(paramlist$range_age):100   # max(paramlist$range_age.r)
# liab.disb.ca <- get_contingentAnnuity(Tier_select, 
#                                       paramlist$factor.ca.disb, # tier.param[Tier_select, "factor.ca.disb"],
#                                       range_age.disb, 
#                                       apply_reduction = FALSE) %>% 
#                 rename(age.disb = age.r)

#save(liab.ca, file = "./liab.ca.test.RData")
#load("./liab.ca.test.RData")


liab.disb.ca <-  liab.ca %>% ungroup %>%  mutate_at(vars(-year.r, -age.r, -age, -year), funs(.*0)) %>% rename(age.disb = age.r)
# liab.disb.ca %>% filter(year.r == 2016)
#liab.ca


#*********************************************************************************************************
# 4. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("NCTSERS_Model_IndivLiab.R")
gc()

liab <- get_indivLab(Tier_select)

# term.backup <- liab$term
# termpop.backup <- pop$term
# actpop.backup <- pop$active
# 
# 
# #  liab$term %<>% mutate(B.v = ifelse(start.year)) 
# pop$active %<>% mutate(start.year = year - (age - ea),
#                        number.a = ifelse(start.year >= 2016, 0, number.a)
#                        )
# 
# pop$term %<>% mutate(start.year = year - (age - ea),
#                      number.a = ifelse(start.year >= 2016, 0, number.v)
# )
# 
# 
# pop$term %>% head


#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("NCTSERS_Model_AggLiab.R")
gc()

AggLiab <- get_AggLiab(Tier_select,
                       liab,
                       liab.ca,
                       liab.disb.ca,
                       pop) 

# AggLiab

#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("NCTSERS_Model_Sim.R")
penSim_results <- run_sim(Tier_select, AggLiab)



#*********************************************************************************************************
# 7.  Saving results ####
#*********************************************************************************************************

outputs_list <- list(paramlist = paramlist, 
                     Global_paramlist = Global_paramlist,
                     results     = penSim_results)



#*********************************************************************************************************
# 8. Showing results ####
#*********************************************************************************************************


var_display1 <- c("Tier", "sim", "year", "FR_MA", "MA", "AL", 
                  "AL.act", "AL.act.laca",  "AL.act.v", "AL.la", "AL.ca", "AL.term", "PVFB", "B", "PVFNC",
                  # "AL.disb.la", "AL.disb.ca", "AL.death", "PVFB",
                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                  # "B", "B.la", "B.ca", "B.v", "B.disb.la","B.disb.ca", 
                  "PR", "NC_PR", "NC","ERC")

var_display2 <- c("Tier", "sim", "year", "FR_MA", "MA", "AL", "EEC","ERC","ERC_PR","B", "B.v", "SC", "C", 
                  "nactives", "nretirees", "nla", "n.ca.R1", "n.ca.R0S1", "nterms", 
                  "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )


var_display.cali <- c("runname", "sim", "year", "FR","FR_MA", "MA", "AA", "AL", 
                      "AL.act", "AL.la", "AL.ca",  "AL.disb.la", "AL.term",
                      "PVFB", "PVFNC", 
                      "B", # "B.la", "B.ca", "B.disb.la","B.disb.ca", 
                      # "C",   
                      "NC","SC", "ERC", "EEC",
                      "PR", "nactives", "nterms",
                      "NC_PR", "SC_PR", # "ERC_PR",
                      "UAAL", "PR.growth", "PVFS", "i.r")

# Calibration
penSim_results %>% filter(sim == 0) %>% select(one_of(var_display.cali)) %>% mutate(AL.laca = AL.la + AL.ca)
penSim_results %>% filter(sim == -1) %>% select(one_of(var_display.cali)) %>% mutate(AL1 = lag(AL + NC - B) * 1.072 ) %>% select(runname, sim, year, FR, AA, AL,  AL1, NC, B)

# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display1)) %>% print
# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display2)) %>% print


#*********************************************************************************************************
# 8. Showing risk measures ####
#*********************************************************************************************************
# 
# df_all.stch <- penSim_results  %>% 
#   filter(sim >= 0, year <= 2045)
# 
# 
# df_all.stch %<>%   
#   select(runname, sim, year, AL, MA, EEC, PR, ERC_PR) %>% 
#   group_by(runname, sim) %>% 
#   mutate(FR_MA     = 100 * MA / AL,
#          FR40less  = cumany(FR_MA <= 40),
#          FR100more  = cumany(FR_MA >= 100),
#          FR100more2 = FR_MA >= 100,
#          ERC_high  = cumany(ERC_PR >= 50), 
#          ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>% 
#   group_by(runname, year) %>% 
#   summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
#             FR100more = 100 * sum(FR100more, na.rm = T)/n(),
#             FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
#             ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
#             ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
#             
#             FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
#             FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
#             FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
#             FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
#             FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
#             
#             ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
#             ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
#             ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
#             ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
#             ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)
#             
# 
#   ) %>% 
#   ungroup()
# 
# df_all.stch
# 



