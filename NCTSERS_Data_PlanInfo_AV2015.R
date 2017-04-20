# This script imports plan information from PlanInfo-PSERS_AV2015.xlsx

# This script performs necessary transformations (eg. truncation, expansion, imputation) to the original data, 
# but do not create new variables (eg. computing weighted average. )

# Data sources:
  # Data_inputs/PSERS_PlanInfo_AV2015.xlsx

# List of outputs
  # mortality_PSERS, 
  # retRates, 
  # termRates, 
  # disbRates, 
  # bfactors, 
  # salgrowth
  # tier.param
  # init_amort_raw

# Output file:
  # Data_inputs/LAFPP_PlanInfo.RData


#### To do list
# 1. More smoothed imputation of decrements



# plan information file:
file_planInfo <- "Data_inputs/PSERS_PlanInfo_AV2015.xlsx"


#*********************************************************************************************************
#                      ## Tools ####
#*********************************************************************************************************





#*********************************************************************************************************
#                           ## Mortality tables ####
#*********************************************************************************************************

# mortality for active members
# mortality for retirees and beneficiaries 
# mortality for disabled

mortality_PSERS <- data.frame(age = 20:120) %>% 
  left_join(read_ExcelRange(file_planInfo, sheet="Death_decAct", "B2", "B3", colTypes="numeric")) %>% 
  left_join(read_ExcelRange(file_planInfo, sheet="Death_decRet", "B2", "B3", colTypes="numeric")) %>% 
  mutate_all(na2zero)

# mortality_PSERS


#*********************************************************************************************************
#                      ## Retirement rates  ####
#*********************************************************************************************************
retRates <- read_ExcelRange(file_planInfo, sheet="Ret_dec", "B2", "B3", colTypes="numeric")

# Early retirement rates
# Superannuation retirement rates

retRates <- data.frame(age = 20:74) %>% 
  left_join(read_ExcelRange(file_planInfo, sheet="Ret_dec", "B2", "B3", colTypes="numeric")) %>% 
  mutate_all(na2zero)

#retRates


#*********************************************************************************************************
#                      ## Termination rates  ####
#*********************************************************************************************************

# Term rates with yos < 5
# Term rates with 5 <= yos < 10
# Term rates with yos >= 10

termRates_raw <- data.frame(age = 20:74) %>% 
  left_join(read_ExcelRange(file_planInfo, sheet="Term_dec", "B2", "B3", colTypes="numeric")) %>% 
  mutate_all(na2zero)
# termRates_raw 


termRates <- expand.grid(ea = 20:74, age = 20:74) %>% 
  mutate(yos = age - ea) %>% 
  filter(age >= ea) %>% 
  arrange(ea, age) %>% 
  left_join(termRates_raw) %>% 
  mutate(qxt.male = ifelse(yos < 5, qxt.male.yosLT5,
                    ifelse(yos < 10, qxt.male.yosLT10,
                    ifelse(yos >=10, qxt.male.yosGTE10, 0))),
         
         qxt.female = ifelse(yos < 5, qxt.female.yosLT5,
                      ifelse(yos < 10, qxt.female.yosLT10,
                      ifelse(yos >=10, qxt.female.yosGTE10, 0)))
  ) %>% 
  select(age, ea, yos, qxt.male, qxt.female) %>% 
  mutate_all(na2zero)

#termRates



#*********************************************************************************************************
#                      ## disability rates     ####
#*********************************************************************************************************

disbRates <- data.frame(age = 20:74) %>% 
  left_join(read_ExcelRange(file_planInfo, sheet="Disb_dec", "B2", "B3", colTypes="numeric")) %>% 
  mutate_all(na2zero)

#disbRates



#*********************************************************************************************************
#                      ## Salary growth rates  ####
#*********************************************************************************************************

salgrowth <- read_ExcelRange(file_planInfo, sheet="SalaryGrowth", "B2", "B3", colTypes="numeric")


#*********************************************************************************************************
#                      ## Tier specific parameters ####
#*********************************************************************************************************

tier.param <- read_ExcelRange(file_planInfo, sheet="Tier.param", colTypes="character") %>% 
  mutate_at(vars(-tier), funs(as.numeric))

row.names(tier.param) <- tier.param$tier

tier.param



#*********************************************************************************************************
#                      ## Initial Amortization Basis  ####
#*********************************************************************************************************

init_amort_raw <- read_ExcelRange(file_planInfo, sheet = "Init_amort", colTypes="character")

init_amort_raw %<>% 
  mutate_at(vars(-tier, -type, -amort.method), funs(as.numeric))

#init_amort_raw %>% str
#init_amort_raw

# amort_cp(18104409000, 0.075, 20, 0.0214)
# amort_cp(618361000,  0.075,  21, 0.0214)

x <- mapply(amort_cp, p = init_amort_raw$balance, m = init_amort_raw$year.remaining,
       MoreArgs = list(i = 0.075, g = 0.0213))

pay1 <- numeric(length(x))
for (i in 1:length(pay1)) pay1[i] <- x[[i]][1]

data.frame(pay1.AV = init_amort_raw$annual.payment,
           pay1.calc = pay1) %>%
  mutate(diff.pct = 100*pay1.calc/pay1.AV - 100)



#*********************************************************************************************************
#                      ## Initial unrecognized return  ####
#*********************************************************************************************************

init_unrecReturns.unadj <- read_ExcelRange(file_planInfo, sheet = "Init_unrecReturn", colTypes="numeric") 
# init_unrecReturns.unadj

save(mortality_PSERS, retRates, termRates, disbRates, salgrowth, tier.param, init_unrecReturns.unadj,
     file  = "Data_inputs/PSERS_PlanInfo_AV2015.RData")




