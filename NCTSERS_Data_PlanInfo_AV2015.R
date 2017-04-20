# This script imports plan information from NCTSERS_PlanInfo_AV2015.xlsx

# This script performs necessary transformations (eg. truncation, expansion, imputation) to the original data, 
# but do not create new variables (eg. computing weighted average) All new variables, which might be run-specific are 
# created in Model_PrepData.R.

# Data sources:
  # Data_inputs/NCTSERS_mortality.RData
  # Data_inputs/NCTSERS_PlanInfo.xlsx

# List of outputs
  # mortality_NCTSERS, 
  # retRates, 
  # termRates, 
  # disbRates, 
  # salgrowth
  # tier.param
  # init_amort_raw

# Output file:
  # Data_inputs/NCTSERS_PlanInfo.RData


#### To do list
# 1. More smoothed imputation of decrements



# Death after retirement (General employees)
# - RP-2014 Total Data Set for Healthy Annuitants Mortality Tabel.
# - Rates for male members are multiplied by 108% for ages 50-78, and by 124% for ages greater than 78. 
# - Rates for female members are multiplied by 81% for ages 50-78, and by 113% for ages greater than 78. 
# - RP-2014 Total Data Set Employee Mortality Table (with no adjustments) is used for ages less than 50. 

# Death after retirement (Teachers and Other education employees)
# - RP-2014 Total Data Set for Healthy Annuitants Mortality Tabel with white collar adjustment
# - Rates for male members are multiplied by 92% for ages 50-78, and by 120% for ages greater than 78. 
# - Rates for female members are multiplied by 78% for ages 50-78, and by 108% for ages greater than 78. 
# - RP-2014 Total Data Set Employee Mortality Table (with white collar adjustment) is used for ages less than 50. 

# Death after retirement (Law emforement officers)
# - RP-2014 Total Data Set for Healthy Annuitants Mortality Tabel.
# - RP-2014 Total Data Set Employee Mortality Table (with no adjustment) is used for ages less than 50. 

# Death after retirement (survivors of decreased members)
# - RP-2014 Total Data Set for Healthy Annuitants Mortality Tabel.
# - Rates for all members are multiplied by 123% for ages greater than 50.
# - RP-2014 Total Data Set Employee Mortality Table (with no adjustments) is used for ages less than 50. 

# Death after retirement (disabled members at retirement)
# - RP-2014 Total Data Set for disabled Annuitants Mortality Tabel.
# - Rates for male members are multiplied by 103% for all ages. 
# - Rates for female members are multiplied by 99% for all ages.

# Death prior to retirement 
# - RP-2014 Total Data Set Employee Mrrtality Table for general employees and law enforcement officers
# - RP-2014 RP-2014 White Collar Employee Mortality Table for teachers and other education employees. 

# MOrtality projection: all rates are projected from 2014 using generational improvement with Scale MP-2015.




# plan information file:
file_planInfo <- "Data_inputs/NCTSERS_PlanInfo_AV2015.xlsx"


#*********************************************************************************************************
#                      ## Tools ####
#*********************************************************************************************************



#*********************************************************************************************************
#                      ## Mortality tables ####
#*********************************************************************************************************
# Read mortality table created by NCTSERS_Data_RP2014.R.
load("Data_inputs/NCTSERS_RP2014.RData")


mortality <- 
  mortality_RP2014 %>% 
  select(-gender) %>% 
  spread(type, qxm_proj) %>% 
  mutate(qxm.post.gen.male   = ifelse(age < 50,  qxm.employee.M,
                               ifelse(age <= 78, qxm.healthyRet.M * 1.08,
                                                 qxm.healthyRet.M * 1.24)),
         qxm.post.gen.female = ifelse(age < 50,  qxm.employee.F,
                               ifelse(age <= 78, qxm.healthyRet.F * 0.81,
                                                 qxm.healthyRet.F * 1.13)),
         
         
         qxm.post.tchedu.male   = ifelse(age <  50, qxm.wcEmployee.M,
                                  ifelse(age <= 78, qxm.wcHealthyRet.M * 0.92,
                                                    qxm.wcHealthyRet.M * 1.20)),
         qxm.post.tchedu.female = ifelse(age <  50, qxm.wcEmployee.F,
                                  ifelse(age <= 78, qxm.wcHealthyRet.F * 0.78,
                                                    qxm.wcHealthyRet.F * 1.08)),
         
         
         qxm.post.law.male   = ifelse(age < 50,  qxm.employee.M, qxm.healthyRet.M),
         qxm.post.law.female = ifelse(age < 50,  qxm.employee.F, qxm.healthyRet.F),
         
         
         qxm.post.sur.male   = ifelse(age < 50,  qxm.employee.M, qxm.healthyRet.M * 1.23),
         qxm.post.sur.female = ifelse(age < 50,  qxm.employee.F, qxm.healthyRet.F * 1.23),
         
         qxmd.male   =  qxm.disbRet.M * 1.03,
         qxmd.female =  qxm.disbRet.F * 0.99,
         
         qxm.pre.gen.male   = qxm.employee.M, 
         qxm.pre.gen.female = qxm.employee.F,

         qxm.pre.law.male   = qxm.employee.M, 
         qxm.pre.law.female = qxm.employee.F,
         
         qxm.pre.tch.male   = qxm.wcEmployee.M, 
         qxm.pre.tch.female = qxm.wcEmployee.F,
         
         qxm.pre.edu.male   = qxm.wcEmployee.M, 
         qxm.pre.edu.female = qxm.wcEmployee.F) %>% 
  select(year, age, 
         qxm.post.gen.male, 
         qxm.post.gen.female, 
         
         qxm.post.tchedu.male,
         qxm.post.tchedu.female,
         
         qxm.post.law.male,
         qxm.post.law.female,
         
         qxm.post.sur.male,
         qxm.post.sur.female,
         
         qxmd.male,
         qxmd.female,
         
         qxm.pre.gen.male,
         qxm.pre.gen.female,
         
         qxm.pre.tch.male,
         qxm.pre.tch.female,
         
         qxm.pre.edu.male,
         qxm.pre.edu.female,
         
         qxm.pre.law.male,
         qxm.pre.law.female
         )

mortality 


#*********************************************************************************************************
#                      ## Retirement rates  ####
#*********************************************************************************************************
retRates <- read_ExcelRange(file_planInfo, sheet="Ret_dec")


#*********************************************************************************************************
#                      ## benefit factors  ####
#*********************************************************************************************************
# bfactor <- read_ExcelRange(file_planInfo, sheet="Ret_bfactor", "B2", "B3", colTypes="numeric")




#*********************************************************************************************************
#                      ## Termination rates  ####
#*********************************************************************************************************
# Term rates for yos < 5
termRates1 <- read_ExcelRange(file_planInfo, sheet="Term_dec1")

# Term rates for yos >=5 (given every 5 years, need to expand to all ages)
termRates2 <- read_ExcelRange(file_planInfo, sheet="Term_dec2") # %>% rename(age.match = age)


# termRates2 <- data.frame(age = 20:74) %>% 
#               mutate(age.match = floor(2*age/10)*10/2) %>% 
#               left_join(termRates2) %>% 
#               select(-age.match) %>% 
#               mutate(qxt.age = ifelse(!is.na(qxt.age), qxt.age,
#                                       qxt.age[age == max(age[!is.na(qxt.age)])]
#                                       )
#              )
# 
# 
# termRates <- expand.grid(ea = 20:74, age = 20:74) %>% 
#   mutate(yos = age - ea) %>% 
#   filter(age >= ea) %>% 
#   arrange(ea, age) %>% 
#   left_join(termRates1) %>% 
#   left_join(termRates2) %>% 
#   mutate(qxt = ifelse(yos < 5, qxt.yos, qxt.age)) %>% 
#   select(ea, age, yos, qxt)

#termRates %>% arrange(ea, age)


#*********************************************************************************************************
#                      ## disability rates  ####
#*********************************************************************************************************
# Assume disability rates are 0 after age 74. (when all actives are assumed retired)
disbRates <- read_ExcelRange(file_planInfo, sheet="Disb_dec") # %>% rename(age.match = age)

# disbRates <- data.frame(age = 20:74) %>% 
#   mutate(age.match = floor(2*age/10)*10/2) %>% 
#   left_join(disbRates) %>% 
#   select(-age.match) %>% 
#   mutate(qxd.nonduty = ifelse(age >  max(age[!is.na(qxd.nonduty)]), 
#                                      qxd.nonduty[age == max(age[!is.na(qxd.nonduty)])],
#                                      qxd.nonduty),
#          qxd.nonduty = ifelse(age <  min(age[!is.na(qxd.nonduty)]), 
#                                      qxd.nonduty[age == min(age[!is.na(qxd.nonduty)])],
#                                      qxd.nonduty),
#  
#          qxd.duty    = ifelse(age > max(age[!is.na(qxd.duty)]), 
#                                     qxd.duty[age == max(age[!is.na(qxd.duty)])],
#                                     qxd.duty),
#          qxd.duty    = ifelse(age < min(age[!is.na(qxd.duty)]), 
#                                     qxd.duty[age == min(age[!is.na(qxd.duty)])],
#                                     qxd.duty)
#          
#          )


#*********************************************************************************************************
#                      ## Salary growth rates  ####
#*********************************************************************************************************

salgrowth <- read_ExcelRange(file_planInfo, sheet="SalaryGrowth") # %>% rename(age.match = age)

# salgrowth <- data.frame(age = 20:74) %>% 
#   mutate(age.match = ifelse(age > 65, 65, floor(2*age/10)*10/2)) %>% 
#   left_join(salgrowth) %>% 
#   select(-age.match)


#*********************************************************************************************************
#                      ## Tier specific parameters ####
#*********************************************************************************************************

# tier.param <- read_ExcelRange(file_planInfo, sheet="Tier.param", colTypes="character") %>% 
#   mutate_each(funs(as.numeric), -tier)
# 
# row.names(tier.param) <- tier.param$tier
# 


#*********************************************************************************************************
#                      ## Initial Amortization Basis  ####
#*********************************************************************************************************

init_amort_raw <-  read_ExcelRange(file_planInfo, sheet = "Init_amort", colTypes="character")
  
init_amort_raw %<>%
  mutate(year.est = year(year.est)) %>%
  mutate_at(vars(-tier,  -type, -amort.method), funs(as.numeric))

# init_amort_raw # %>% str


#*********************************************************************************************************
#                      ## Initial unrecognized return  ####
#*********************************************************************************************************

init_unrecReturns.unadj <- read_ExcelRange(file_planInfo, sheet = "Init_unrecReturn") 



#*********************************************************************************************************
#                      ## External fund   ####
#*********************************************************************************************************
# extFund.unadj <- read_ExcelRange(file_planInfo, sheet = "External_Fund", colTypes="numeric") 




save(mortality, retRates, termRates1, termRates2, disbRates, 
     salgrowth, 
     # tier.param, 
     init_amort_raw, 
     init_unrecReturns.unadj, 
     file  = "Data_inputs/NCTSERS_PlanInfo_AV2015.RData")







