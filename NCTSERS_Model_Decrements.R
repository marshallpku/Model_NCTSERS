
# Inputs:
  # "Data_inputs/LAFPP_PlanInfo.RData", for decrements
  # "Data_inputs/LAFPP_MemberData.RData", for gender and occupation ratios


# Final outputs
 # 1. decrement.model
 # 2. mortality.post.model


# Summary of decrements
 # 1. Actives:
   # Mortality(qxm.pre): weighted average of gen(M/F),tch(M/F), edu(M/F), law(M/F); two dimensional (year x age)
   # Retirees (qxr)    : weighted average of gen(M/F),tch(M/F), edu(M/F), law(M/F); spline smoothing needed
   # Termination(qxt)  : weighted average of gen(M/F),tch(M/F), edu(M/F), law(M/F); spline smoothing needed
   # Disability (qxd)  : weighted average of gen(M/F),tch(M/F), edu(M/F), law(M/F); spline smoothing needed

 # 2. Retirees
   # Mortality of annuitants(qxm.post):          weighted average of Gen(M/F), tch and edu(M/F), law(M/F); two dimensional (year x age)
   # Mortality of beneficiaries (qxm.post.sur):  weighted average of Gen(M/F), tch and edu(M/F), law(M/F); two dimensional (year x age)

 # 3. Disabled Retirees:
   # Mortality:  weighted average of Gen(M/F), tch and edu(M/F), law(M/F); two dimensional (year x age)
   # (assume life annuity and no beneficiaries for disabled retirees)

 # 4. Terms:
   # Mortality: use qxm.post
   # (assume life annuity and no beneficiaries for vested terms. )
 



get_decrements <- function(Tier_select,
                           .Global_paramlist = Global_paramlist,
                           .paramlist = paramlist){

  
# Tier_select <- "t1"
# .Global_paramlist = Global_paramlist
# .paramlist = paramlist

assign_parmsList(.Global_paramlist, envir = environment())
assign_parmsList(.paramlist,        envir = environment())

# mortality
# occupGender

#*************************************************************************************************************
#                                Prepare mortality tables for NCTSERS                     #####                  
#*************************************************************************************************************

#mortality

mortality.model <- data.frame(age = range_age) %>% 
  left_join(mortality) %>% 
  mutate(
         # mortality for actives
         qxm.pre = qxm.pre.tch.male   * occupGender["actives", "share.tch.male"]   + 
                   qxm.pre.tch.female * occupGender["actives", "share.tch.female"] + 
                   qxm.pre.edu.male   * occupGender["actives", "share.edu.male"]   + 
                   qxm.pre.edu.female * occupGender["actives", "share.edu.female"] + 
                   qxm.pre.law.male   * occupGender["actives", "share.law.male"]   + 
                   qxm.pre.law.female * occupGender["actives", "share.law.female"] +
                   qxm.pre.gen.male   * occupGender["actives", "share.gen.male"]   + 
                   qxm.pre.gen.female * occupGender["actives", "share.gen.female"],
         
         # mortaltiy for healthy retirees
         qxm.post.male = qxm.post.tchedu.male   * (occupGender["actives", "share.tch.male"] + occupGender["actives", "share.edu.male"]) + 
                         qxm.post.law.male   * occupGender["actives", "share.law.male"] + 
                         qxm.post.gen.male   * occupGender["actives", "share.gen.male"],

         qxm.post.female = qxm.post.tchedu.female * (occupGender["actives", "share.tch.female"] + occupGender["actives", "share.edu.female"]) + 
                           qxm.post.law.female * occupGender["actives", "share.law.female"] +
                           qxm.post.gen.female * occupGender["actives", "share.gen.female"],
         
         # mortaltiy for beneficiaries (note that male and female shares are switched)
         qxm.post.sur = qxm.post.sur.male   * occupGender["actives", "pct.female.all"] +
                        qxm.post.sur.female * occupGender["actives", "pct.male.all"],

         # mortaltiy for disabled retirees (note that male and female shares are switched)
         qxm.d  = qxmd.male   * occupGender["actives", "pct.male.all"] + 
                  qxmd.female * occupGender["actives", "pct.female.all"],
         
         # mortality for vested terms
         qxm.term = qxm.post.male * occupGender["actives", "pct.male.all"] +  qxm.post.female * occupGender["actives", "pct.female.all"],
         
         qxm.deathBen = qxm.post.sur
         
         ) %>% 
  select(year, age, qxm.pre, 
                    qxm.post.male,
                    qxm.post.female,
                    qxm.post.sur.male, 
                    qxm.post.sur.female, 
                    qxm.post.sur, 
                    qxm.d,
                    qxm.term,
                    qxm.deathBen)



#*************************************************************************************************************
#                              Calibration for          NCTSERS                     #####                  
#*************************************************************************************************************

# Goal: Calibrate PVFB for retirees and beneficiearies downward by 5% to 10%. 
# Method: Increasing qxm.post.male/female, qxm.post.sur.male/female. 

# mortality.adj <- 1.025

mortality.model %<>% 
  mutate(qxm.post.male   = mortality.adj * qxm.post.male,
         qxm.post.female = mortality.adj * qxm.post.female,
         
         qxm.post.sur.male   = mortality.adj * qxm.post.sur.male,
         qxm.post.sur.female = mortality.adj * qxm.post.sur.female)


mortality.model %<>% mutate_at(vars(-age, -year), funs(ifelse(age == 120, 1, .) ))
mortality.model %<>% mutate_at(vars(-age, -year), funs(ifelse(. > 1, 1, .)))



        
         
## Compute present values of life annuity(with cola) at each retirement age in each year, using uni-sex mortality with age dependent weights
  # Why using age dependent weights:
    # Using the age dependent weights, the number of total members calculated using aggregate members and weighted mortality would be the same
    # as that obtained by calculating the members for males and females separately and summing them up. This is required by the the actuarially 
    # equivalence between life annuity and contingent annuity. 
 

mortality.post.model <- expand.grid(year.r = (init.year - 2):(init.year + 100),
                                    age    = range_age, 
                                    age.r  = range_age # modified for init retirees of NCTSERS 
                                    # min(range_age.r):max.age
                                    ) %>% 
  filter(age >= age.r) %>% 
  mutate(year = year.r + (age - age.r)) %>% 
  left_join(mortality.model) %>%
  group_by(year.r, age.r) %>%  
  mutate(
    pxm.post.male   = 1 - qxm.post.male,
    pxm.post.female = 1 - qxm.post.female,
    
    pRxm.male     = occupGender["actives", "pct.male.all"]   * ifelse(age == min(age), 1, lag(cumprod(pxm.post.male))),  # proportion of male left in each year after retirement
    pRxm.female   = occupGender["actives", "pct.female.all"] * ifelse(age == min(age), 1, lag(cumprod(pxm.post.female))),# same thing, for female
    
    w.male   = pRxm.male / (pRxm.male + pRxm.female),
    w.female = pRxm.female / (pRxm.male + pRxm.female),
    
    qxm.post.W = qxm.post.male * w.male + qxm.post.female * w.female, # dynamically weighted mortality
    pxm.post.W = 1 - qxm.post.W,
    
    COLA.scale =  (1 + cola)^(row_number() - 1 ), #(1 + tier.param[Tier_select,"cola"])^(row_number() - 1 ),
    B =  COLA.scale,
    ax.r.W     =  get_tla(pxm.post.W, i, COLA.scale),
    liab.la.W = B * ax.r.W    # "la" for life annuity. liability (also PV) for $1's benefit payment at retirement. 
  )  %>% 
  mutate_all(funs(ifelse(is.nan(.), 0, .))) %>% 
  select(year.r, age.r, age, year, qxm.post.W, pxm.post.W, ax.r.W)

# mortality.post.model


# Construct mortality rate for terms: 
 # before r.vben:  qxm.pre
 # after  r.vben:  qxm.post.W with age.r == r.vben
 
# mortality.model %<>% left_join(mortality.post.model %>% ungroup %>%  
#                                filter(age.r == tier.param[Tier_select,"r.vben"]) %>% 
#                                select(age, qxm.post.term = qxm.post.W)) %>% 
#                      mutate(qxm.term = ifelse(age < tier.param[Tier_select,"r.vben"], qxm.pre, qxm.post.term)) %>% 
#                      select(-qxm.post.term)
# WARNING: Simplified for NCTSERS. qxm.term is defined in mortality.model already without using age-specific weighted mortality for retirees. 




#*************************************************************************************************************
#                                Prepare disability rate for NCTSERS                     #####                  
#*************************************************************************************************************

# Expand rates through 20 to 65; coerce negative values to 0
# Rates for age > 65 are equal to 65

disbRates

disbrates.model <- 
disbRates %>% gather(var, value, -age) %>% 
  splong("age", c(20:65)) %>% 
  spread(var, value) %>% 
  mutate(qxd.male.law   = ifelse(age >= 55, 0, qxd.male.law),
         qxd.female.law = ifelse(age >= 55, 0, qxd.female.law),
         
         qxd = qxd.male.tch   * occupGender["actives", "share.tch.male"] + 
               qxd.female.tch * occupGender["actives", "share.tch.female"] + 
               qxd.male.edu   * occupGender["actives", "share.edu.male"] + 
               qxd.female.edu * occupGender["actives", "share.edu.female"] + 
               qxd.male.law   * occupGender["actives", "share.law.male"] + 
               qxd.female.law * occupGender["actives", "share.law.female"] +
               qxd.male.gen   * occupGender["actives", "share.gen.male"] + 
               qxd.female.gen * occupGender["actives", "share.gen.female"]
         ) %>% 
  select(age, qxd)

disbrates.model <-  
  data.frame(age = min.age:r.max) %>% 
  left_join(disbrates.model) %>% 
  mutate(qxd = ifelse(age > 65, qxd[age == 65], qxd))
  
#disbrates.model



#*************************************************************************************************************
#                                Prepare terminations rates for NCTSERS                     #####                  
#*************************************************************************************************************

# term rates
termRates1
termRates2.fill <-  
  data.frame(age = min.age:r.max ) %>% 
  left_join(termRates2 %>% gather(var, value, -age) %>% 
              splong("age", c(25:45)) %>% 
              spread(var, value) 
  ) %>% 
  gather(var, value, -age) %>% 
  group_by(var) %>% 
  mutate(value = ifelse(age < 25, value[age == 25], value),
         value = ifelse(age > 45, value[age == 45], value)) %>% 
  spread(var, value)
# termRates2.fill


termrates.model <- 
  expand.grid(age = min.age:r.max, ea = range_ea) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  left_join(termRates1) %>% 
  left_join(termRates2.fill) %>% 
  mutate(qxt.yos = qxt.male.tch.yos   * occupGender["actives", "share.tch.male"] + 
                   qxt.female.tch.yos * occupGender["actives", "share.tch.female"] + 
                   qxt.male.edu.yos   * occupGender["actives", "share.edu.male"] + 
                   qxt.female.edu.yos * occupGender["actives", "share.edu.female"] + 
                   qxt.male.law.yos   * occupGender["actives", "share.law.male"] + 
                   qxt.female.law.yos * occupGender["actives", "share.law.female"] +
                   qxt.male.gen.yos   * occupGender["actives", "share.gen.male"] + 
                   qxt.female.gen.yos * occupGender["actives", "share.gen.female"],

         qxt.age = qxt.male.tch.age   * occupGender["actives", "share.tch.male"] + 
                   qxt.female.tch.age * occupGender["actives", "share.tch.female"] + 
                   qxt.male.edu.age   * occupGender["actives", "share.edu.male"] + 
                   qxt.female.edu.age * occupGender["actives", "share.edu.female"] + 
                   qxt.male.law.age   * occupGender["actives", "share.law.male"] + 
                   qxt.female.law.age * occupGender["actives", "share.law.female"] +
                   qxt.male.gen.age   * occupGender["actives", "share.gen.male"] + 
                   qxt.female.gen.age * occupGender["actives", "share.gen.female"],
         
         qxt = ifelse(yos < 5, qxt.yos, qxt.age )
         ) %>% 
  select(ea, age, qxt)
        
termrates.model 

termrates.model


#*************************************************************************************************************
#                                Prepare retirement rates rates for NCTSERS                     #####                  
#*************************************************************************************************************

# retirement rates

# fill age for each yos. yos is not filled                                  
retRates.spreadage <- 
  retRates %>% 
  gather(var, value, -yos, -age) %>% 
  mutate(var.yos = paste0(var, yos)) %>% 
  select(var.yos, age, value) %>% 
  splong("age", 50:75) %>% 
  mutate(yos = as.numeric(str_extract(var.yos, "\\d{1,2}")),
         var = str_extract(var.yos, "\\D+")) %>% 
  select(-var.yos) %>% 
  mutate(value = ifelse(age < 60, 0, value)) %>% # Drop retirement rates for law enforcement before 60. 
  spread(var, value) %>% 
  arrange(yos, age)

retRates.spreadage

retrates.model <- expand.grid(yos = 0:(r.max - min.age), age = r.min:r.max) %>% 
  mutate(yos.match = ceiling(yos * 2/10) * 10/2,
         yos.match = ifelse(yos.match > 35, 35, yos.match)) %>% 
  left_join(retRates.spreadage %>% rename(yos.match = yos)) %>% 
  mutate_at(vars(-yos, -age), funs(ifelse(yos == 0, 0, .))) %>% # will cause problem if grouped by age first
  group_by(age) %>% 
  mutate(qxr = qxr.male.tch   * occupGender["actives", "share.tch.male"] + 
               qxr.female.tch * occupGender["actives", "share.tch.female"] + 
               qxr.male.edu   * occupGender["actives", "share.edu.male"] + 
               qxr.female.edu * occupGender["actives", "share.edu.female"] + 
               qxr.law        * (occupGender["actives", "share.law.male"] + occupGender["actives", "share.law.female"]) + 
               qxr.male.gen   * occupGender["actives", "share.gen.male"] + 
               qxr.female.gen * occupGender["actives", "share.gen.female"]) %>% 
  select(yos, age, qxr) %>% 
  arrange(yos, age) 




#*************************************************************************************************************
#                      2. Putting together decrements and calculate surviving rates  ####
#*************************************************************************************************************

# Create decrement table and calculate probability of survival
#decrement.model <- expand.grid(start.year = (init.year - (max.age - min.age)):(init.year + nyear - 1), age = range_age, ea = range_ea) %>% 
 decrement.model <- expand.grid(start.year = 1915:(init.year + nyear - 1), age = range_age, ea = range_ea) %>% 
  mutate(yos = age - ea,
         year = start.year + yos) %>% 
  filter(age >= ea) %>% 
  left_join(mortality.model) %>%                  # mortality 
  left_join(termrates.model)  %>%                 # termination
  left_join(disbrates.model)  %>%                 # disability
  left_join(retrates.model) %>%                   # early retirement
  select(ea, age, everything()) %>%          
  arrange(start.year, ea, age)  %>%
  colwise(na2zero)(.) %>% 
  group_by(start.year, ea) 

decrement.model




#*************************************************************************************************************
#                      3. Adjustments to decrement tables  ####
#*************************************************************************************************************

# Retirement rates are effective only when eligibility requirements are met
# non-law enforcement requirements are used

# Term rates are zero when eligible for retirement benefit.


# Unreduced benefit
   # 1.age >= 65, yos >=5, or
   # 2.age >=60, yos >=25, or
   # 3.yos >=30

# Reduced benefit
   # 1. age >=60, yos >=5, or
   # 2. age >=50, yos >=20, and does not meet 1 and unreduced benefit

# vesting rules: yos >= 5

# assume zero term rates after age 60. 


decrement.model %<>% 
  # group_by(start.year, ea) %>% 
  mutate(# Early retirement
         elig_retFull = ifelse((age >= 65 & yos >= 5)| 
                               (age >= 60 & yos >= 25)| 
                                yos >= 30, 1, 0),
         
         # superannuation retirement
         elig_retReduced1 =  ifelse((age >= 60 & yos >= 5) & elig_retFull == 0, 1, 0),
         elig_retReduced2 =  ifelse((age >= 50 & yos >= 20) & elig_retReduced1 == 0 & elig_retFull == 0, 1, 0),
         
         
         # The age first eligible for superannuation in a start.year-ea group
         age_retFullFirst = age[min(which(elig_retFull == 1))],

         
         # adjust retirement rates
         qxr = ifelse(elig_retFull == 1| elig_retReduced1 == 1 | elig_retReduced2 == 1, qxr, 0),
         
         
         # Vesting
         elig_vest = ifelse(yos >= 5,  1, 0),
         
         # adjust term rates
         
         qxt = ifelse(elig_retFull == 1| elig_retReduced1 == 1 | elig_retReduced2 == 1, 0, qxt),
         qxt = ifelse(age >= 60, 0, qxt)
         )
 


#*************************************************************************************************************
#                                   3. Modify retirement rates ####
#*************************************************************************************************************

# Adjustment to the decrement table:
  # Move qxr.a backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is lead(qxr.a(t))*(1 - qxt.a(t-1) - qxm.a(t-1) - qxd.a(t-1))
  # For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
  # which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
  # whether they will die at r.max)      

pct.ca <- pct.ca.M * occupGender["actives", "pct.male.all"] + pct.ca.F * occupGender["actives", "pct.female.all"]
pct.la <- 1 - pct.ca


decrement.model %<>% 
  # group_by(start.year, ea) %>%  
  mutate(qxr = ifelse(age == r.max - 1,
                             1 - qxt - qxm.pre - qxd, 
                             lead(qxr) * (1 - qxt - qxm.pre - qxd)), # Total probability of retirement
         qxr.la = ifelse(age == r.max, 0 , qxr * pct.la),  # Prob of opting for life annuity
         qxr.ca = ifelse(age == r.max, 0 , qxr * pct.ca),   # Prob of opting for contingent annuity
         
         qxd.la = ifelse(age == r.max, 0 , qxd * pct.la),  # Prob of opting for life annuity
         qxd.ca = ifelse(age == r.max, 0 , qxd * pct.ca)
         
         # qxd.la = ifelse(age == r.max, 0 , qxd * 1),  # Prob of opting for life annuity
         # qxd.ca = ifelse(age == r.max, 0 , qxd * 0)
         
)   
         

######!!!! need to construct retirement age dependent mortality for life annuitants.
  # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
  #mutate(qxm.la.r   = qxm.r) 


#*************************************************************************************************************
#                                            4. compute various survival rates ####
#*************************************************************************************************************


decrement.model %<>% 
  #group_by(start.year, ea) %>% 
  mutate( pxm.pre = 1 - qxm.pre,
          pxm.deathBen = 1 - qxm.deathBen,
          pxm.d        = 1 - qxm.d,
          pxm.term     = 1 - qxm.term,
          
          pxT     = 1 - qxt - qxd - qxm.pre - qxr,                            
          
          pxRm        = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm.pre))), # prob of surviving up to r.max, mortality only
          px_r.full_m = order_by(-age, cumprod(ifelse(age >= r.vben, 1, pxm.pre))), # Should be deleted later
          px_r.vben_m = order_by(-age, cumprod(ifelse(age >= r.vben, 1, pxm.term))),
          px_r.vsuper_m = order_by(-age, cumprod(ifelse(age >= r.vben, 1, pxm.pre))) # order_by(-age, cumprod(ifelse(age >= age_superFirst, 1, pxm.pre)))
          
          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
  ) %>% 
  mutate_each(funs(na2zero)) %>% 
  ungroup


list(decrement.model = decrement.model,
     mortality.post.model = mortality.post.model)

}


# get_decrements("t5")

# decrement.model %>% select(ea, age, px_r.vsuper_m, elig_super)








