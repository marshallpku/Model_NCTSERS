
gc()

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************


# Plan information
source("PSERS_Data_PlanInfo_AV2015.R")
source("PSERS_Data_MemberData_AV2015.R")

load("Data_inputs/PSERS_PlanInfo_AV2015.RData")    # for all tiers
load("Data_inputs/PSERS_MemberData_AV2015.RData")  # for all tiers
load("Data_inputs/DC_rate.tot.RData")              

init_beneficiaries_all %<>% filter(age >= 25) 


## Exclude selected type(s) of initial members
 #init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
 # init_terms_all %<>% mutate(nterm = 0)
 # init_disb_all  %<>% mutate(ndisb = 0) 


pct.init.ret.la <-  0.75
pct.init.ret.ca  <- 1 - pct.init.ret.la

pct.init.disb.la <-  1
pct.init.disb.ca  <- 1 - pct.init.disb.la

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


init_retirees.la_all %>% summarise(sumb = sum(nretirees.la * benefit))



#*********************************************************************************************************
# 1.2 Create decrement tables ####
#*********************************************************************************************************

# Decrement tables
source("PSERS_Model_Decrements.R")

list.decrements.tCD <- get_decrements("tCD")
list.decrements.tE <- get_decrements("tE")
list.decrements.tF <- get_decrements("tF")



decrement.model.tCD      <- list.decrements.tCD$decrement.model
mortality.post.model.tCD <- list.decrements.tCD$mortality.post.model

decrement.model.tE      <- list.decrements.tE$decrement.model
mortality.post.model.tE <- list.decrements.tE$mortality.post.model

decrement.model.tF      <- list.decrements.tF$decrement.model
mortality.post.model.tF <- list.decrements.tF$mortality.post.model



#*****************************************************
##   Calibration and Modification of initial data ####
#*****************************************************

# 1. PVFB and AL of actives
paramlist$bfactor <- paramlist$bfactor * 1.1



## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
 # init_terms_all %<>% mutate(nterm = 0)
 

 

# 
# ### Calibration:
# 
# # 1. PVFB.retirees
#   # By calibrating benefit factor for survivors   
# 
# calibFactor_factor.ca <- 1
# tier.param %<>% mutate(factor.ca = pmin(1.1, factor.ca * calibFactor_factor.ca))
# row.names(tier.param) <- tier.param$tier
# 
# 
# # 2. PVFB.act
#   # By calibrating benefit factor for service retirees
# calibFactor_bfactor <- 1.08
# bfactor %<>% mutate_each(funs(pmin(1, .*calibFactor_bfactor)), -yos )
# 
# 
# # 3. EEC
#   # Aggregate EEC rate (total EEC / projected payroll) in 2016 AV is about 9.75%
#   # Tier specific ERC rates are no more than 9%. 
#   # Aggergate EEC rate is 9.64% after calibration  
# 
# calibFactor_EEC.rate <- 1.1
# tier.param %<>% mutate(EEC.rate = pmin(1.1, EEC.rate * calibFactor_EEC.rate))
# row.names(tier.param) <- tier.param$tier
# 
# 
# # 3. Initial benefit payments
#  # calibrated to match the budgeted non-DROP payment for FY2016-2017  
# init_beneficiaries_all %<>% mutate(benefit = benefit * 0.989589)
# init_retirees.ca_all   %<>% mutate(benefit = benefit * 0.989589)
# init_retirees.la_all   %<>% mutate(benefit = benefit * 0.989589)
# init_disb.ca_all       %<>% mutate(benefit = benefit * 0.989589)
# init_disb.la_all       %<>% mutate(benefit = benefit * 0.989589)






#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("PSERS_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



i.r_supplement <-  
  cbind(rep(paramlist$i, 4),
        matrix(c(0.0343, 0.0796, 0.01491, 0.0304), 4, Global_paramlist$nsim + 1))

i.r_geoReturn <- rbind(i.r_supplement, i.r) %>% 
  as.data.frame %>% 
  mutate_all(funs(get_rollingReturns(., "moving", 10)))

i.r_geoReturn[1:9, ] <- rbind(i.r_supplement, i.r)[1:9,] %>% 
  as.data.frame %>% 
  mutate_all(funs(get_rollingReturns(., "expanding"))) %>% 
  as.matrix()

i.r_geoReturn <- i.r_geoReturn[-(1:4),]


#*********************************************************************************************************
# 1.4 Create plan data ####
#*********************************************************************************************************
source("PSERS_Model_PrepData.R")

# Create data for each tier

salary.tCD  <- get_salary_proc("tCD")
salary.tE  <- get_salary_proc("tE")
salary.tF  <- get_salary_proc("tF")


benefit.tCD <- get_benefit_tier("tCD")
benefit.tE <- get_benefit_tier("tE")
benefit.tF <- get_benefit_tier("tF")


benefit.disb.tCD <- get_benefit.disb_tier("tCD")
benefit.disb.tE <- get_benefit.disb_tier("tE")
benefit.disb.tF <- get_benefit.disb_tier("tF")


init_pop.tCD <- get_initPop_tier("tCD")
init_pop.tE <- get_initPop_tier("tE")
init_pop.tF <- get_initPop_tier("tF")


entrants_dist.tCD <- numeric(length(paramlist$range_ea))
entrants_dist.tE  <- get_entrantsDist_tier("tE") * share.tE
entrants_dist.tF  <- get_entrantsDist_tier("tF") * share.tF


benefit.tCD
benefit.disb.tCD


#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************

source("PSERS_Model_Demographics_allTiers.R")
pop <- get_Population_allTiers_PSERS()
  
gc()


#*********************************************************************************************************
# 4. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("PSERS_Model_ContingentAnnuity.R")

range_age.r.ca <- min(paramlist$range_age.r):100
liab.ca.tCD  <- get_contingentAnnuity("tCD", tier.param["tCD", "factor.ca"], range_age.r.ca, TRUE, decrement.model_ = decrement.model.tCD)
liab.ca.tE  <- get_contingentAnnuity("tE", tier.param["tE", "factor.ca"], range_age.r.ca, TRUE, decrement.model_ = decrement.model.tE)
liab.ca.tF  <- get_contingentAnnuity("tF", tier.param["tF", "factor.ca"], range_age.r.ca, TRUE, decrement.model_ = decrement.model.tF)


range_age.disb.ca <-  min(paramlist$range_age): 100 #max(paramlist$range_age.r)
liab.disb.ca.tCD  <-  get_contingentAnnuity("tCD", tier.param["tCD", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tCD) %>% rename(age.disb = age.r)
liab.disb.ca.tE  <-   get_contingentAnnuity("tE", tier.param["tE", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tE) %>% rename(age.disb = age.r)
liab.disb.ca.tF  <-   get_contingentAnnuity("tF", tier.param["tF", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tF) %>% rename(age.disb = age.r)




#*********************************************************************************************************
# 3. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("PSERS_Model_IndivLiab.R")
gc()

liab.tCD <- get_indivLab("tCD",
                         decrement.model.tCD,
                         salary.tCD,
                         benefit.tCD,
                         benefit.disb.tCD,
                         mortality.post.model.tCD,
                         liab.ca.tCD,
                         liab.disb.ca.tCD)


liab.tE <- get_indivLab("tE",
                        decrement.model.tE,
                        salary.tE,
                        benefit.tE,
                        benefit.disb.tE,
                        mortality.post.model.tE,
                        liab.ca.tE,
                        liab.disb.ca.tE)


liab.tF <- get_indivLab("tF",
                        decrement.model.tF,
                        salary.tF,
                        benefit.tF,
                        benefit.disb.tF,
                        mortality.post.model.tF,
                        liab.ca.tF,
                        liab.disb.ca.tF)


liab.tE$active %>% filter(year == 2017, ea == 30) %>% select(year, ea, age, DC_EEC)


#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("PSERS_Model_AggLiab.R")
gc()


AggLiab.tCD <- get_AggLiab("tCD",
                          liab.tCD,
                          liab.ca.tCD,
                          liab.disb.ca.tCD,
                          pop$pop.tCD,
                          mortality.post.model.tCD) 


AggLiab.tE <- get_AggLiab("tE",
                          liab.tE,
                          liab.ca.tE,
                          liab.disb.ca.tE,
                          pop$pop.tE,
                          mortality.post.model.tE) 


AggLiab.tF <- get_AggLiab("tF",
                          liab.tF,
                          liab.ca.tF,
                          liab.disb.ca.tF,
                          pop$pop.tF,
                          mortality.post.model.tF) 


AggLiab.sumTiers <- 
  get_AggLiab_sumTiers(AggLiab.tCD, AggLiab.tE, AggLiab.tF) 


if(paramlist$tier == "sumTiers"){  
  PR.Tiers <- AggLiab.tCD$active %>% as.data.frame %>% select(year, PR_tCD = PR.sum)  %>% 
    left_join(AggLiab.tE$active  %>% as.data.frame %>% select(year, PR_tE  = PR.sum)) %>% 
    left_join(AggLiab.tF$active  %>% as.data.frame %>% select(year, PR_tF  = PR.sum)) %>% 
    as.matrix
} else {
  PR.Tiers <- NULL 
}
    
if(paramlist$tier == "sumTiers"){  
  DC.Tiers <- AggLiab.tCD$active %>% as.data.frame %>% select(year, DC_EEC_tCD = DC_EEC.sum, DC_ERC_tCD = DC_ERC.sum)  %>% 
    left_join(AggLiab.tE$active  %>% as.data.frame %>% select(year, DC_EEC_tE  = DC_EEC.sum, DC_ERC_tE  = DC_ERC.sum)) %>% 
    left_join(AggLiab.tF$active  %>% as.data.frame %>% select(year, DC_EEC_tF  = DC_EEC.sum, DC_ERC_tF  = DC_ERC.sum)) %>% 
    as.matrix
} else {
  DC.Tiers <- NULL 
}


# AggLiab.tCD$active
# AggLiab.tE$active
# AggLiab.tF$active


#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("PSERS_Model_Sim.R")


penSim_results.sumTiers <- run_sim("sumTiers", AggLiab.sumTiers)


outputs_list <- list(paramlist = paramlist, 
                     Global_paramlist = Global_paramlist,
                     results     = penSim_results.sumTiers)



#*********************************************************************************************************
# 7.1  Showing results: Joint simulation of all tiers ####
#*********************************************************************************************************
# Use full outputs include:
  # penSim_results.sumTiers
  # AggLiab.t76; AggLiab.tCD3; AggLiab.tm13
# NEXT STEP: extract useful variables from AggLiab.XXX files, so we can still see liability dynamics of each tier
#            when we simulate(do the loop) all tiers jointly. 



# penSim_results.sumTiers %<>% mutate() 

var_display1 <- c("sim", "year", "FR", "MA", "AL", 
                  # "AL.act", "AL.disb.la", "AL.act.disb", "AL.act.death", "AL.act.v", "AL.la", "AL.ca", "AL.term", "PVFB", "B",
                  # "AL.disb.la", "AL.disb.ca", "AL.death", "PVFB",
                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                  # "B", "B.la", "B.ca", "B.v", "B.disb.la","B.disb.ca", 
                  "PR.growth", "ERC.final",
                  "LG", "Amort_basis",
                  "PR", "NC_PR","SC_PR")



var_display2 <- c("Tier", "sim", "year", "FR_MA", "MA", "AL", "EEC","ERC","ERC_PR","B", "B.v", "SC", "C", 
                  "nactives", "nretirees", "nla", "n.ca.R1", "n.ca.R0S1", "nterms", 
                  "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )



var_display.cali1 <- c("sim", "year", "FR","FR_MA", "MA", "AA", "AL","UAAL", 
                       "PVFB", "AL.act",
                     
                      "NC","SC", "ERC", "ERC.final", "EEC",
                      #"PR",
                      #"NC","SC",
                      "ERC.final_PR",
                      "DC_ERC_PR",
                      "NC_PR", "SC_PR", 
                      "EEC_PR",
                      "B",
                      "UAAL")

var_display.cali2 <- c("sim", "year", 
                       "AL.act", "AL.act.v", "AL.la", "AL.ca", "AL.disb.la", "AL.term",
                       "PVFB.laca", "PVFB.disb",  
                       "B.la", "B.ca", "B.disb.la",   #  "B.disb.ca", 
                       "PR", "nactives", "nla")


kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(one_of(var_display.cali1)), digits = 2) %>% print 
kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(one_of(var_display.cali2)), digits = 2) %>% print 


kable(penSim_results.sumTiers %>% filter(sim == -1) %>% select(one_of(var_display1)), digits = 2) %>% print 
kable(penSim_results.sumTiers %>% filter(sim == 1) %>% select(one_of(var_display1)), digits = 2) %>% print 


#*********************************************************************************************************
#   8. Showing risk measures ####
#*********************************************************************************************************

df_all.stch <- penSim_results.sumTiers  %>% 
  filter(sim >= 0, year <= 2045)


df_all.stch %<>%   
  select(runname, sim, year, AL, MA, EEC, PR, ERC.final_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high  = cumany(ERC.final_PR >= 40), 
         ERC_hike  = cumany(na2zero(ERC.final_PR - lag(ERC.final_PR, 5) >= 10))) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC.final_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC.final_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC.final_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC.final_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC.final_PR, 0.9, na.rm = T)
            
            
  ) %>% 
  ungroup()

df_all.stch




paramlist$salgrowth_amort


liab.tE$active %>% filter(year == 2017, ea == age) %>%  select(year, age, ea, sx, starts_with("NCx"))




