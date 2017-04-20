# Simulation of the demograhics of UCRP

## Modifications on the original model
#1. Need to calculate the number of LSC claims(by ea, age) for each year. (Can be calculated after the loop) 
#2. Need to calculate the number of new retirees opting for contingent annuity(by ea, age) for each year. (Can be calculated after the loop) 
#3. The mortality for retirees are now retirement age dependent.


get_Population_allTiers_PSERS <- function(
                           init_pop.tCD_         = init_pop.tCD,
                           entrants_dist.tCD_    = entrants_dist.tCD ,
                           decrement.model.tCD_  = list.decrements.tCD$decrement.model,
                           mortality.post.model.tCD_ = mortality.post.model.tCD,
                           
                           init_pop.tE_         = init_pop.tE,
                           entrants_dist.tE_    = entrants_dist.tE ,
                           decrement.model.tE_   = list.decrements.tE$decrement.model ,
                           mortality.post.model.tE_ = mortality.post.model.tE,
                           
                           init_pop.tF_         = init_pop.tF,
                           entrants_dist.tF_    = entrants_dist.tF ,
                           decrement.model.tF_   = list.decrements.tF$decrement.model ,
                           mortality.post.model.tF_ = mortality.post.model.tF,
            
                           paramlist_        = paramlist,
                           Global_paramlist_ = Global_paramlist){

## Inputs
# - range_ea:         all possible entry ages  
# - range_age:        range of age
# - nyear:            number of years in simulation
# - wf_growth:        growth rate of the size of workforce
# - no_entrance:      no new entrants into the workforce if set "TRUE". Overrides "wf_growth"
# - Decrement table:  from Model_Decrements.R  
# - Initial workforce for each type:
#    - init_pop$actives:   matrix, max ea by max age
#    - init_pop$retirees:  matrix, max ea by max age


## An array is created for each of the 6 status:
#  (1)Active     (dim = 3)
#  (2)Terminated (dim = 4)
#  (3)Retired    (dim = 4)
#  (4)Disabled   (dim = 3) later will be expanded to dim = 4, with additional dim being year.disb
#  (5)Dead       (dim = 3) We do not really need an array for dead, what's needed is only the total number of dead.  

# Run the section below when developing new features.   
  # init_pop.tCD_         = init_pop.tCD
  # entrants_dist.tCD_    = entrants_dist.tCD
  # decrement.model.tCD_  = list.decrements.tCD$decrement.model
  # mortality.post.model.tCD_ = mortality.post.model.tCD
  # 
  # init_pop.tE_         = init_pop.tE
  # entrants_dist.tE_    = entrants_dist.tE
  # decrement.model.tE_   = list.decrements.tE$decrement.model
  # mortality.post.model.tE_ = mortality.post.model.tE
  # 
  # init_pop.tF_         = init_pop.tF
  # entrants_dist.tF_    = entrants_dist.tF
  # decrement.model.tF_   = list.decrements.tF$decrement.model
  # mortality.post.model.tF_ = mortality.post.model.tF
  # 
  # paramlist_        = paramlist
  # Global_paramlist_ = Global_paramlist

    

 assign_parmsList(Global_paramlist_, envir = environment())
 assign_parmsList(paramlist_,        envir = environment())  

 # pct.QSS <- pct.ca.F * pct.female + pct.ca.M * pct.male

#*************************************************************************************************************
#                                     Creating arrays for each status, FOR ALL TIERS ####
#*************************************************************************************************************

## In each 3D array, dimension 1(row) represents entry age, dimension 2(column) represents attained age,
# dimension 3(depth) represents number of year, dimension 4(terms only) represents the termination year. 
wf_dim      <- c(length(range_ea), length(range_age), nyear)
wf_dimnames <- list(range_ea, range_age, init.year:(init.year + nyear - 1))

# The array of terminated has 4 dimensions: ea x age x year x year of termination
wf_dim.term      <- c(length(range_ea), length(range_age), nyear, nyear + 1)
wf_dimnames.term <- list(range_ea, range_age, init.year:(init.year + nyear - 1), (init.year - 1) :(init.year + nyear - 1))


# The array of retirees has 4 dimensions: ea x age x year x year of retirement
wf_dim.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.la <- list(range_ea, range_age, init.year:(init.year + nyear - 1), init.year:(init.year + nyear - 1))


# The array of death beneficiearies has 4 dimensions: ea x age x year x year of death(of the active)
wf_dim.deathBen      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.deathBen <- list(range_ea, range_age, init.year:(init.year + nyear - 1), init.year:(init.year + nyear - 1))


# The array of disability retirees has 4 dimensions: ea x age x year x year of disability(of the active)
wf_dim.disb.la      <- c(length(range_ea), length(range_age), nyear, nyear)
wf_dimnames.disb.la <- list(range_ea, range_age, init.year:(init.year + nyear - 1), init.year:(init.year + nyear - 1))

wf_active.tCD <- wf_active.tE <- wf_active.tF  <- array(0, wf_dim, dimnames = wf_dimnames)
wf_disb.tCD   <- wf_disb.tE   <- wf_disb.tF    <- array(0, wf_dim, dimnames = wf_dimnames) 
wf_dead.tCD   <- wf_dead.tE   <- wf_dead.tF    <- array(0, wf_dim, dimnames = wf_dimnames)
wf_term.tCD   <- wf_term.tE   <- wf_term.tF    <- array(0, wf_dim.term,    dimnames = wf_dimnames.term)
wf_la.tCD     <- wf_la.tE     <- wf_la.tF      <- array(0, wf_dim.la, dimnames = wf_dimnames.la)


wf_deathBen.tCD  <- wf_deathBen.tE  <- wf_deathBen.tF  <- array(0, wf_dim.deathBen, dimnames = wf_dimnames.deathBen)
wf_disb.la.tCD   <- wf_disb.la.tE   <- wf_disb.la.tF   <- array(0, wf_dim.disb.la,  dimnames = wf_dimnames.disb.la)


newDeath.act.tCD <-  newDeath.act.tE <-   newDeath.act.tF   <- numeric(nyear)
newDeath.ret.tCD <-  newDeath.ret.tE <-   newDeath.ret.tF   <- numeric(nyear)
newDeath.term.tCD <- newDeath.term.tE <-  newDeath.term.tF  <- numeric(nyear)

newDisb.act.tCD <- newDisb.act.tE <- newDisb.act.tF <- numeric(nyear)


#*************************************************************************************************************
#                                     Setting  initial population, TIER SPECIFIC ####
#*************************************************************************************************************

# Setting inital distribution of workforce and retirees.
# Note on initial retirees: It is assumed that all initial retirees entered the workforce at age 54 and retireed in year 1. 
# Altough this may produce yos greater than r.max - ea.min, it is irrelevant to the calculation since we do not care about initial retirees' yos.  
# 

# Tier CD
wf_active.tCD[, , 1]   <- init_pop.tCD_$actives 
wf_la.tCD[, , 1, 1]    <- init_pop.tCD_$retirees
wf_term.tCD[, , 1, 1]  <- init_pop.tCD_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.tCD[, , 1, 1]  <- init_pop.tCD_$disb

# Tier E
wf_active.tE[, , 1]   <- init_pop.tE_$actives 
wf_la.tE[, , 1, 1]    <- init_pop.tE_$retirees
wf_term.tE[, , 1, 1]  <- init_pop.tE_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.tE[, , 1, 1]  <- init_pop.tE_$disb

# Tier F
wf_active.tF[, , 1]   <- init_pop.tF_$actives 
wf_la.tF[, , 1, 1]    <- init_pop.tF_$retirees
wf_term.tF[, , 1, 1]  <- init_pop.tF_$terms   # note that the initial terms are assigned to year.term = init.year - 1
wf_disb.la.tF[, , 1, 1]  <- init_pop.tF_$disb



#*************************************************************************************************************
#                                     Defining population dynamics, TIER SPECIFIC  ####
#*************************************************************************************************************

## Transition matrices ####

# Assume the actual decrement rates are the same as the rates in decrement tables.
# Later we may allow the actual decrement rates to differ from the assumed rates. 


# Define a function that produce transition matrices from decrement table. 
make_dmat <- function(qx, df = decrement_wf) {
  # inputs:
  # qx: character, name of the transition probability to be created.
  # df: data frame, decrement table.
  # returns:
  # a transtion matrix
  df %<>% select_("age", "ea", qx) %>% ungroup %>% spread_("ea", qx, fill = 0) %>% select(-age) %>% t # need to keep "age" when use spread
  dimnames(df) <- wf_dimnames[c(1,2)] 
  return(df)
}


# select decrement tables

decrement_wf.tCD <- decrement.model.tCD_ %>% mutate_each(funs(na2zero)) # just for safety 
decrement_wf.tE <- decrement.model.tE_   %>% mutate_each(funs(na2zero)) # just for safety 
decrement_wf.tF <- decrement.model.tF_   %>% mutate_each(funs(na2zero)) # just for safety 

# The transition matrices are defined below. The probabilities (eg. qxr for retirement) of flowing
# from the current status to the target status for a cell(age and ea combo) are given in the corresponding
# cell in the transtition matrices. 


## For Tier 1

  # Where do the active go
  p_active2term.tCD    <- make_dmat("qxt",     decrement_wf.tCD)
  p_active2disb.tCD    <- make_dmat("qxd",     decrement_wf.tCD)
  p_active2disb.la.tCD <- make_dmat("qxd.la",  decrement_wf.tCD)
  p_active2dead.tCD    <- make_dmat("qxm.pre", decrement_wf.tCD)
  p_active2deathBen.tCD<- make_dmat("qxm.pre", decrement_wf.tCD)
  p_active2disb.tCD    <- make_dmat("qxd",     decrement_wf.tCD)
  p_active2retired.tCD <- make_dmat("qxr",     decrement_wf.tCD)   # This include all three types of retirement: contingent annuity, and life annuity.
  p_active2la.tCD      <- make_dmat("qxr.la",  decrement_wf.tCD)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.tCD    <- make_dmat("qxm.term", decrement_wf.tCD) 
  
  # Where do the death beneficiaries go
  p_deathBen2dead.tCD <- make_dmat("qxm.deathBen", decrement_wf.tCD)
  
  # Where do the disability retirees go
  p_disb.la2dead.tCD <- make_dmat("qxm.d", decrement_wf.tCD)
  
  
## For Tier 2
  
  # Where do the active go
  p_active2term.tE    <- make_dmat("qxt",     decrement_wf.tE)
  p_active2disb.tE    <- make_dmat("qxd",     decrement_wf.tE)
  p_active2disb.la.tE <- make_dmat("qxd.la",  decrement_wf.tE)
  p_active2dead.tE    <- make_dmat("qxm.pre", decrement_wf.tE)
  p_active2deathBen.tE<- make_dmat("qxm.pre", decrement_wf.tE) 
  p_active2disb.tE    <- make_dmat("qxd",     decrement_wf.tE)
  p_active2retired.tE <- make_dmat("qxr",     decrement_wf.tE)   # This include all three types of retirement: contingent annuity, and life annuity.
  p_active2la.tE      <- make_dmat("qxr.la",  decrement_wf.tE)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.tE    <- make_dmat("qxm.term", decrement_wf.tE) # need to modify later
  
  # Where do the death beneficiaries go
  p_deathBen2dead.tE <- make_dmat("qxm.deathBen", decrement_wf.tE)
  
  # Where do the disability retirees go
  p_disb.la2dead.tE <- make_dmat("qxm.d", decrement_wf.tE)
  
  
## For Tier 3
  
  # Where do the active go
  p_active2term.tF    <- make_dmat("qxt",     decrement_wf.tF)
  p_active2disb.tF    <- make_dmat("qxd",     decrement_wf.tF)
  p_active2disb.la.tF <- make_dmat("qxd.la",  decrement_wf.tF)
  p_active2dead.tF    <- make_dmat("qxm.pre", decrement_wf.tF)
  p_active2deathBen.tF<- make_dmat("qxm.pre", decrement_wf.tF)
  p_active2disb.tF    <- make_dmat("qxd",     decrement_wf.tF)
  p_active2retired.tF <- make_dmat("qxr",     decrement_wf.tF)   # This include all three types of retirement: contingent annuity, and life annuity.
  p_active2la.tF      <- make_dmat("qxr.la",  decrement_wf.tF)   # Prob of retiring as a life annuitant.
  
  # Where do the terminated go
  p_term2dead.tF    <- make_dmat("qxm.term", decrement_wf.tF) # need to modify later
  
  # Where do the death beneficiaries go
  p_deathBen2dead.tF <- make_dmat("qxm.deathBen", decrement_wf.tF)
  
  # Where do the disability retirees go
  p_disb.la2dead.tF <- make_dmat("qxm.d", decrement_wf.tF)
  

  
# Where do the retirees go
  # Before we find better approach, the age.r dependent mortality for retirees are given in a data frame containing all combos 
  # of year, year.r, ea, and age that exist in wf_la. 

  p_la2dead.tCD <- 
    expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
    #filter(age >= ea) %>% 
    mutate(age.r = age - (year - year.r)) %>% 
    left_join(mortality.post.model.tCD_ %>% select(age.r, age, qxm.post.W)) %>%
    mutate(qxm.post.W = na2zero(qxm.post.W)) %>% 
    arrange(year, year.r, age, ea)

  p_la2dead.tE <-
    expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
    #filter(age >= ea) %>%
    mutate(age.r = age - (year - year.r)) %>%
    left_join(mortality.post.model.tE_ %>% select(age.r, age, qxm.post.W)) %>%
    mutate(qxm.post.W = na2zero(qxm.post.W)) %>%
    arrange(year, year.r, age, ea)

  p_la2dead.tF <-
    expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)) %>%
    #filter(age >= ea) %>%
    mutate(age.r = age - (year - year.r)) %>%
    left_join(mortality.post.model.tF_ %>% select(age.r, age, qxm.post.W)) %>%
    mutate(qxm.post.W = na2zero(qxm.post.W)) %>%
    arrange(year, year.r, age, ea)

  
#*************************************************************************************************************
#                                     Creating a function to calculate new entrants ####
#*************************************************************************************************************

# define function for determining the number of new entrants 
calc_entrants <- function(wf0, wf1, delta, dist, no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after  decrement.  
  # delta: growth rate of workforce
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(r.max - 1)
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0 <- sum(wf0[,as.character(working_age)], na.rm = TRUE)
  size1 <- sum(wf1[,as.character(working_age)], na.rm = TRUE)
  
  # computing new entrants
  size_target <- size0*(1 + delta)   # size of the workforce next year
  size_hire   <- size_target - size1 # number of workers need to hire
  ne <- size_hire*dist               # vector, number of new entrants by age
  
  # Create the new entrant matrix 
  NE <- wf0; NE[ ,] <- 0
  
  if (no.entrants){ 
    return(NE) 
  } else {
    NE[, rownames(NE)] <- diag(ne) # place ne on the matrix of new entrants
    return(NE)
  } 
}

# test the function 
# wf0 <- wf_active[, , 1]
# wf1 <- wf_active[, , 1]*(1 - p_active2term)
# sum(wf0, na.rm = T) - sum(wf1, na.rm = T)
# sum(calc_entrants(wf0, wf1, 0), na.rm = T)


#*************************************************************************************************************
#                               Creating a function to calculate new entrants, FOR ALL TIERS ####
#*************************************************************************************************************

# define function for determining the number of new entrants 
calc_entrants_allTiers <- function(wf0.tCD,  wf0.tE,  wf0.tF, 
                                   wf1.tCD,  wf1.tE,  wf1.tF, 
                                   dist.tCD, dist.tE, dist.tF,
                                   # newEnt_byTier,
                                   delta, 
                                   no.entrants = FALSE){
  # This function deterimine the number of new entrants based on workforce before and after decrement and workforce 
  # growth rate. 
  # inputs:
  # wf0: a matrix of workforce before decrement. Typically a slice from wf_active
  # wf1: a matrix of workforce after  decrement.  
  # delta: growth rate of workforce
  # newEnt_byTier: named vector, proportion of new entrants entering each tier. names must be "tCD", "tE", "tmF"
  # returns:
  # a matrix with the same dimension of wf0 and wf1, with the number of new entrants in the corresponding cells,
  # and 0 in all other cells. 
  
  # working age
  working_age <- min(range_age):(r.max - 1) # FOR ALL TIERS
  # age distribution of new entrants
  # dist <- rep(1/nrow(wf0), nrow(wf0)) # equally distributed for now. 
  
  # compute the size of workforce before and after decrement
  size0.tCD <- sum(wf0.tCD[,as.character(working_age)], na.rm = TRUE)
  size1.tCD <- sum(wf1.tCD[,as.character(working_age)], na.rm = TRUE)
  
  size0.tE <- sum(wf0.tE[,as.character(working_age)], na.rm = TRUE)
  size1.tE <- sum(wf1.tE[,as.character(working_age)], na.rm = TRUE)
  
  size0.tF <- sum(wf0.tF[,as.character(working_age)], na.rm = TRUE)
  size1.tF <- sum(wf1.tF[,as.character(working_age)], na.rm = TRUE)
  
  
  
  # computing new entrants
  size_target <- (size0.tCD + size0.tE + size0.tF) * (1 + delta)   # size of the workforce next year
  size_hire   <- size_target - (size1.tCD + size1.tE + size1.tF)  # number of workers need to hire
  
  ne.tCD <- size_hire  * dist.tCD # vector, number of new entrants by age
  ne.tE  <- size_hire  * dist.tE 
  ne.tF  <- size_hire  * dist.tF 

  
  
  # Create the new entrant matrix 
  NE.tCD <- wf0.tCD  ;  NE.tCD[ ,]  <- 0
  NE.tE <- wf0.tE  ;  NE.tE[ ,]  <- 0
  NE.tF <- wf0.tF  ;  NE.tF[ ,]  <- 0

  if (no.entrants){ 
    return(NE = list(NE.tCD = NE.tCD, NE.tE = NE.tE, NE.tF = NE.tF)) 
  
  } else {
    NE.tCD[, rownames(NE.tCD)] <- diag(ne.tCD) # place ne on the matrix of new entrants
    NE.tE[,  rownames(NE.tE)]  <- diag(ne.tE)
    NE.tF[,  rownames(NE.tF)]  <- diag(ne.tF)

    return(NE = list(NE.tCD = NE.tCD, NE.tE = NE.tE, NE.tF = NE.tF)) 
  } 
}  

# # test the function 
#   wf0.t76  <- wf_active.t76[, , 1]
#   wf0.tCD3  <- wf_active.tCD3[, , 1]
#   wf0.tm13 <- wf_active.tm13[, , 1]
#   
#   wf1.t76  <- wf_active.t76[, , 1] *(1 - p_active2term.t76)
#   wf1.tCD3  <- wf_active.tCD3[, , 1] *(1 - p_active2term.tCD3)
#   wf1.tm13 <- wf_active.tm13[, , 1]*(1 - p_active2term.tm13)
#   
#   (sum(wf0.t76 + wf0.tCD3 + wf0.tm13, na.rm = T) - sum(wf1.t76 + wf1.tCD3 + wf1.tm13, na.rm = T))
#   
#   NE <- calc_entrants_allTiers(wf0.t76, wf0.tCD3, wf0.tm13,
#                              wf1.t76,  wf1.tCD3,  wf1.tm13, 
#                              entrants_dist.t76, entrants_dist.tCD3, entrants_dist.tm13,
#                              c(t76 = 0, t13 = 0.8, tm13 = 0.2), 0)
#   sapply(NE, sum) %>% sum
#   
  
  


#*************************************************************************************************************
#                                     Simulating the evolution of population  ####
#*************************************************************************************************************


# In each iteration, a flow matrix for each possible transition(eg. active to retired) is created 
# (if we wanted to track the flow in each period, we create flow arrays instead of flow matrices)

# Define the shifting matrix. When left mutiplied by a workforce matrix, it shifts each element one cell rightward(i.e. age + 1)
# A square matrix with the dimension length(range_age)
# created by a diagal matrix without 1st row and last coloumn
A <- diag(length(range_age) + 1)[-1, -(length(range_age) + 1)] 



# Now the next slice of the array (array[, , i + 1]) is defined
# wf_active[, , i + 1] <- (wf_active[, , i] + inflow_active[, , i] - outflow_active[, , i]) %*% A + wf_new[, , i + 1]
# i runs from 2 to nyear. 

for (j in 1:(nyear - 1)){
  #    j <-  1  
  
  #*******************************************
  # Stage 1 Seperations by type in each tier *
  #*******************************************
  
  ## Tier CD
    # compute the inflow to and outflow
    active2term.tCD    <- wf_active.tCD[, , j] * p_active2term.tCD  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.tCD    <- wf_active.tCD[, , j] * p_active2dead.tCD
    active2retired.tCD <- wf_active.tCD[, , j] * p_active2retired.tCD    # This will be used to calculate the number of actives leaving the workforce
    active2la.tCD      <- wf_active.tCD[, , j] * p_active2la.tCD          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.tCD<- wf_active.tCD[, , j] * p_active2deathBen.tCD
    active2disb.tCD    <- wf_active.tCD[, , j] * p_active2disb.tCD
    active2disb.la.tCD <- wf_active.tCD[, , j] * p_active2disb.la.tCD
    
    # Where do the terminated_vested go
    term2dead.tCD  <- wf_term.tCD[, , j, ] * as.vector(p_term2dead.tCD)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.tCD   <- wf_la.tCD[, , j, ] * (p_la2dead.tCD %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
    

    # Where do the QSSs of death benefit go
    deathBen2dead.tCD  <- wf_deathBen.tCD[, , j, ] * as.vector(p_deathBen2dead.tCD)
    
    # Where do the disability retirees go
    disb.la2dead.tCD  <- wf_disb.la.tCD[, , j, ] * as.vector(p_disb.la2dead.tCD)
    
    
  ## Tier E
    # compute the inflow to and outflow
    active2term.tE    <- wf_active.tE[, , j] * p_active2term.tE  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.tE    <- wf_active.tE[, , j] * p_active2dead.tE
    active2retired.tE <- wf_active.tE[, , j] * p_active2retired.tE    # This will be used to calculate the number of actives leaving the workforce
    active2la.tE      <- wf_active.tE[, , j] * p_active2la.tE          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.tE<- wf_active.tE[, , j] * p_active2deathBen.tE
    active2disb.tE    <- wf_active.tE[, , j] * p_active2disb.tE
    active2disb.la.tE <- wf_active.tE[, , j] * p_active2disb.la.tE
    
    # Where do the terminated_vested go
    term2dead.tE  <- wf_term.tE[, , j, ] * as.vector(p_term2dead.tE)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.tE   <- wf_la.tE[, , j, ] * (p_la2dead.tE %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
    
 
    # Where do the QSSs of death benefit go
    deathBen2dead.tE  <- wf_deathBen.tE[, , j, ] * as.vector(p_deathBen2dead.tE)
    
    # Where do the disability retirees go
    disb.la2dead.tE  <- wf_disb.la.tE[, , j, ] * as.vector(p_disb.la2dead.tE)
    
  ## Tier F
    # compute the inflow to and outflow
    active2term.tF    <- wf_active.tF[, , j] * p_active2term.tF  # This will join wf_term[, , j + 1, j + 1], note that workers who terminate in year j won't join the terminated group until j+1. 
    active2dead.tF    <- wf_active.tF[, , j] * p_active2dead.tF
    active2retired.tF <- wf_active.tF[, , j] * p_active2retired.tF    # This will be used to calculate the number of actives leaving the workforce
    active2la.tF      <- wf_active.tF[, , j] * p_active2la.tF          # This will join wf_la[, , j + 1, j + 1].
    active2deathBen.tF<- wf_active.tF[, , j] * p_active2deathBen.tF
    active2disb.tF    <- wf_active.tF[, , j] * p_active2disb.tF
    active2disb.la.tF <- wf_active.tF[, , j] * p_active2disb.la.tF
    
    # Where do the terminated_vested go
    term2dead.tF  <- wf_term.tF[, , j, ] * as.vector(p_term2dead.tF)           # a 3D array, each slice(3rd dim) contains the # of death in a termination age group
    
    # Where do the retired go
    la2dead.tF   <- wf_la.tF[, , j, ] * (p_la2dead.tF %>% filter(year == j + init.year - 1))[["qxm.post.W"]]     # as.vector(p_retired2dead) # a 3D array, each slice(3rd dim) contains the # of death in a retirement age group    
    
    # Where do the QSSs of death benefit go
    deathBen2dead.tF  <- wf_deathBen.tF[, , j, ] * as.vector(p_deathBen2dead.tF)
    
    # Where do the disability retirees go
    disb.la2dead.tF  <- wf_disb.la.tF[, , j, ] * as.vector(p_disb.la2dead.tF)
  
  
 

  #***********************************
  # Stage 2 Seperations in each tier *
  #***********************************
  
  ### Total inflow and outflow for each status
  
  # Calculating new entrants
  out_active.tCD   <- active2term.tCD + active2disb.tCD + active2retired.tCD + active2dead.tCD 
  out_active.tE   <- active2term.tE + active2disb.tE + active2retired.tE + active2dead.tE
  out_active.tF   <- active2term.tF + active2disb.tF + active2retired.tF + active2dead.tF

  
  
  new_entrants_allTiers <- 
  calc_entrants_allTiers(wf_active.tCD[, , j], 
                         wf_active.tE[, , j], 
                         wf_active.tF[, , j], 
                         
                         
                         wf_active.tCD[, , j]  - out_active.tCD, 
                         wf_active.tE[, , j]  - out_active.tE,
                         wf_active.tF[, , j]  - out_active.tF,

                
                         entrants_dist.tCD_,
                         entrants_dist.tE_,
                         entrants_dist.tF_,
                         
                         #newEnt_byTier_before2019,
                         
                         wf_growth,
                         
                         no.entrants = no_entrance) # new entrants

           
  # new_entrants_allTiers %>% names
  # new_entrants <- calc_entrants(wf_active[, , j], wf_active[, , j] - out_active, wf_growth, dist = .entrants_dist, no.entrants = no_entrance) # new entrants
  
  
  ## Tier CD
    out_term.tCD <- term2dead.tCD    # This is a 3D array 
    in_term.tCD  <- active2term.tCD  # This is a matrix
    
    out_la.tCD <- la2dead.tCD        # This is a 3D array (ea x age x year.retire)
    in_la.tCD  <- active2la.tCD      # This is a matrix
    
    out_deathBen.tCD <- deathBen2dead.tCD        # This is a 3D array (ea x age x year.retire)
    in_deathBen.tCD  <- active2deathBen.tCD    # This is a matrix
    
    out_disb.la.tCD <- disb.la2dead.tCD        # This is a 3D array (ea x age x year.retire)
    in_disb.la.tCD  <- active2disb.la.tCD    # This is a matrix
    
    
    in_dead.tCD <- active2dead.tCD +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.tCD, c(1,2), sum) + apply(la2dead.tCD, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.tCD, c(1,2), sum)  

    
  ## Tier E
    out_term.tE <- term2dead.tE    # This is a 3D array 
    in_term.tE  <- active2term.tE  # This is a matrix
    
    out_la.tE <- la2dead.tE        # This is a 3D array (ea x age x year.retire)
    in_la.tE  <- active2la.tE      # This is a matrix
    
    out_deathBen.tE <- deathBen2dead.tE        # This is a 3D array (ea x age x year.retire)
    in_deathBen.tE  <- active2deathBen.tE    # This is a matrix
    
    out_disb.la.tE <- disb.la2dead.tE        # This is a 3D array (ea x age x year.retire)
    in_disb.la.tE  <- active2disb.la.tE    # This is a matrix
    
    in_dead.tE <- active2dead.tE +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.tE, c(1,2), sum) + apply(la2dead.tE, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.tE, c(1,2), sum) 
  
  ## Tier F
    out_term.tF <- term2dead.tF    # This is a 3D array 
    in_term.tF  <- active2term.tF  # This is a matrix
    
    out_la.tF <- la2dead.tF        # This is a 3D array (ea x age x year.retire)
    in_la.tF  <- active2la.tF      # This is a matrix
    
    out_deathBen.tF <- deathBen2dead.tF        # This is a 3D array (ea x age x year.retire)
    in_deathBen.tF  <- active2deathBen.tF    # This is a matrix
    
    out_disb.la.tF <- disb.la2dead.tF        # This is a 3D array (ea x age x year.retire)
    in_disb.la.tF  <- active2disb.la.tF    # This is a matrix
    
    in_dead.tF <- active2dead.tF +                                        # In LAFPP model, since life annuitants are only part of the total retirees, in_dead does not reflect the total number of death. 
      apply(term2dead.tF, c(1,2), sum) + apply(la2dead.tF, c(1,2), sum) + # get a matirix of ea x age by summing over year.term/year.retiree
      apply(disb.la2dead.tF, c(1,2), sum)
    
    
  #*********************************************
  # Stage 3  Calculate workforce for next year. 
  #*********************************************  
  
  ## Tier CD
    wf_active.tCD[, , j + 1]  <- (wf_active.tCD[, , j] - out_active.tCD) %*% A + new_entrants_allTiers$NE.tCD
    
    wf_term.tCD[, , j + 1, ]      <- apply((wf_term.tCD[, , j, ] - out_term.tCD), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.tCD[, , j + 1, j + 1] <- in_term.tCD %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.tCD[, ,j + 1, ]       <- apply((wf_la.tCD[, , j, ] - out_la.tCD), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.tCD[, , j + 1, j + 1] <- in_la.tCD %*% A
    
    wf_dead.tCD[, ,   j + 1]    <- (wf_dead.tCD[, , j] + in_dead.tCD) %*% A
    
    wf_deathBen.tCD[, , j + 1, ]      <- apply((wf_deathBen.tCD[, , j, ] - out_deathBen.tCD), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.tCD[, , j + 1, j + 1] <- in_deathBen.tCD %*% A
    
    wf_disb.la.tCD[, , j + 1, ]      <- apply((wf_disb.la.tCD[, , j, ] - out_disb.la.tCD), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.tCD[, , j + 1, j + 1] <- in_disb.la.tCD %*% A
    
    newDeath.act.tCD[j]  <- sum(active2dead.tCD)
    newDeath.ret.tCD[j]  <- sum(la2dead.tCD)
    # newDeath.term[j] <- sum()
    
    newDisb.act.tCD[j] <- sum(active2disb.tCD)
  
  
  
  ## Tier E
    wf_active.tE[, , j + 1]  <- (wf_active.tE[, , j] - out_active.tE) %*% A + new_entrants_allTiers$NE.tE
    
    wf_term.tE[, , j + 1, ]      <- apply((wf_term.tE[, , j, ] - out_term.tE), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.tE[, , j + 1, j + 1] <- in_term.tE %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.tE[, ,j + 1, ]       <- apply((wf_la.tE[, , j, ] - out_la.tE), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.tE[, , j + 1, j + 1] <- in_la.tE %*% A
    
    wf_dead.tE[, ,   j + 1]    <- (wf_dead.tE[, , j] + in_dead.tE) %*% A
    
    wf_deathBen.tE[, , j + 1, ]      <- apply((wf_deathBen.tE[, , j, ] - out_deathBen.tE), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.tE[, , j + 1, j + 1] <- in_deathBen.tE %*% A
    
    wf_disb.la.tE[, , j + 1, ]      <- apply((wf_disb.la.tE[, , j, ] - out_disb.la.tE), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.tE[, , j + 1, j + 1] <- in_disb.la.tE %*% A
    
    newDeath.act.tE[j]  <- sum(active2dead.tE)
    newDeath.ret.tE[j]  <- sum(la2dead.tE)
    # newDeath.term[j] <- sum()
    
    newDisb.act.tE[j] <- sum(active2disb.tE)

      
  ## Tier F
    wf_active.tF[, , j + 1]  <- (wf_active.tF[, , j] - out_active.tF) %*% A + new_entrants_allTiers$NE.tF
    
    wf_term.tF[, , j + 1, ]      <- apply((wf_term.tF[, , j, ] - out_term.tF), 3, function(x) x %*% A) %>% array(wf_dim.term[-3])
    wf_term.tF[, , j + 1, j + 1] <- in_term.tF %*% A     # Note that termination year j = 1 correponds to init.year - 1
    
    wf_la.tF[, ,j + 1, ]       <- apply((wf_la.tF[, , j, ] - out_la.tF), 3, function(x) x %*% A) %>% array(wf_dim.la[-3])
    wf_la.tF[, , j + 1, j + 1] <- in_la.tF %*% A
    
    wf_dead.tF[, ,   j + 1]    <- (wf_dead.tF[, , j] + in_dead.tF) %*% A
    
    wf_deathBen.tF[, , j + 1, ]      <- apply((wf_deathBen.tF[, , j, ] - out_deathBen.tF), 3, function(x) x %*% A) %>% array(wf_dim.deathBen[-3])
    wf_deathBen.tF[, , j + 1, j + 1] <- in_deathBen.tF %*% A
    
    wf_disb.la.tF[, , j + 1, ]      <- apply((wf_disb.la.tF[, , j, ] - out_disb.la.tF), 3, function(x) x %*% A) %>% array(wf_dim.disb.la[-3])
    wf_disb.la.tF[, , j + 1, j + 1] <- in_disb.la.tF %*% A
    
    newDeath.act.tF[j]  <- sum(active2dead.tF)
    newDeath.ret.tF[j]  <- sum(la2dead.tF)
    # newDeath.term[j] <- sum()
    
    newDisb.act.tF[j] <- sum(active2disb.tF)
  
    
}



#*************************************************************************************************************
#                                     Transform Demographic Data to Data Frames   ####
#*************************************************************************************************************

## Convert 3D arrays of actives, retired and terms to data frame, to be joined by liability data frames

get_df.wf_active <- function(df){
    df <- adply(df, 3, function(x) {df = as.data.frame(x); df$ea = as.numeric(rownames(x));df}) %>% 
    rename(year = X1) %>%
    gather(age, number.a, -ea, -year) %>% 
    mutate(year = f2n(year), age = as.numeric(age)) %>% 
    filter(age >= ea)
}
 wf_active.tCD <- get_df.wf_active(wf_active.tCD)
 wf_active.tE <- get_df.wf_active(wf_active.tE)
 wf_active.tF <- get_df.wf_active(wf_active.tF)


 
get_df.wf_la <- function(df){
  df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.r = init.year:(init.year + nyear - 1)),
                   number.la = as.vector(df)) %>% 
    filter(age >= ea)
}
 wf_la.tCD  <- get_df.wf_la(wf_la.tCD)
 wf_la.tE  <- get_df.wf_la(wf_la.tE)
 wf_la.tF  <- get_df.wf_la(wf_la.tF)



get_df.wf_term <- function(df){
    df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.term = (init.year-1):(init.year + nyear - 1)),
                     number.v = as.vector(df)) %>% 
    filter(age >= ea)
}
 wf_term.tCD  <- get_df.wf_term(wf_term.tCD)
 wf_term.tE  <- get_df.wf_term(wf_term.tE)
 wf_term.tF  <- get_df.wf_term(wf_term.tF)

 
 
get_df.wf_deathBen <- function(df){
   df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.death = init.year:(init.year + nyear - 1)),
                    number.deathBen = as.vector(df)) %>% 
     filter(age >= ea)
 }
 wf_deathBen.tCD  <- get_df.wf_deathBen(wf_deathBen.tCD)
 wf_deathBen.tE  <- get_df.wf_deathBen(wf_deathBen.tE)
 wf_deathBen.tF  <- get_df.wf_deathBen(wf_deathBen.tF)

 
get_df.wf_disb.la <- function(df){
   df <- data.frame(expand.grid(ea = range_ea, age = range_age, year = init.year:(init.year + nyear - 1), year.disb = init.year:(init.year + nyear - 1)),
                    number.disb.la = as.vector(df)) %>% 
     filter(age >= ea)
 }
 wf_disb.la.tCD  <- get_df.wf_disb.la(wf_disb.la.tCD)
 wf_disb.la.tE  <- get_df.wf_disb.la(wf_disb.la.tE)
 wf_disb.la.tF  <- get_df.wf_disb.la(wf_disb.la.tF)

 


#*************************************************************************************************************
#                                     Number of members opting for contingent annuity   ####
#*************************************************************************************************************

get_wf_new.ca <- function(df_actives, decrement){
  wf_new.ca <- df_actives %>% left_join(decrement %>% select(age, ea, qxr.ca)) %>% 
               mutate(new_ca  = number.a * qxr.ca,
                      year = year + 1,
                      age  = age + 1)
}

wf_new.ca.tCD  <- get_wf_new.ca(wf_active.tCD, decrement_wf.tCD)
wf_new.ca.tE  <- get_wf_new.ca(wf_active.tE, decrement_wf.tE)
wf_new.ca.tF  <- get_wf_new.ca(wf_active.tF, decrement_wf.tF)



get_wf_new.disb.ca <- function(df_actives, decrement){
  wf_new.disb.ca <- df_actives %>% left_join(decrement %>% select(age, ea, qxd.ca)) %>% 
    mutate(new_disb.ca  = number.a * qxd.ca,
           year = year + 1,
           age  = age + 1)
}

wf_new.disb.ca.tCD  <- get_wf_new.disb.ca(wf_active.tCD, decrement_wf.tCD)
wf_new.disb.ca.tE  <- get_wf_new.disb.ca(wf_active.tE, decrement_wf.tE)
wf_new.disb.ca.tF  <- get_wf_new.disb.ca(wf_active.tF, decrement_wf.tF)



# Final outputs

pop <- list(   
     pop.tCD = list(active  = wf_active.tCD,  term = wf_term.tCD, disb.la = wf_disb.la.tCD, la = wf_la.tCD, deathBen = wf_deathBen.tCD, dead = wf_dead.tCD, new_ca = wf_new.ca.tCD, new_disb.ca = wf_new.disb.ca.tCD),
     pop.tE = list(active  = wf_active.tE,  term = wf_term.tE, disb.la = wf_disb.la.tE, la = wf_la.tE, deathBen = wf_deathBen.tE, dead = wf_dead.tE, new_ca = wf_new.ca.tE, new_disb.ca = wf_new.disb.ca.tE),
     pop.tF = list(active  = wf_active.tF,  term = wf_term.tF, disb.la = wf_disb.la.tF, la = wf_la.tF, deathBen = wf_deathBen.tF, dead = wf_dead.tF, new_ca = wf_new.ca.tF, new_disb.ca = wf_new.disb.ca.tF)

)

return(pop)

}





