# Calibrate liabilities and benefit flows from the model to match the actual values


get_calibAggLiab <- function( AggLiab_ = AggLiab, 
                              paramlist_        = paramlist,
                              Global_paramlist_ = Global_paramlist){
  
  
  # Run the section below when developing new features.  
  # AggLiab_ = AggLiab
  # paramlist_        = paramlist
  # Global_paramlist_ = Global_paramlist
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(paramlist_,        envir = environment())
  
  
  
  
 
   
  #*************************************************************************************************************
  #                                     ## Calibrating liabilities for current retirees   ####
  #************************************************************************************************************* 
  
  # Target: 
  #  1. Total benefit payment in year 1
  #  2. reported/estiamted value of Al for current retires in year 1
  #
  # Method:
  #  1. calibrate the year 1 benefit: calib_factor_benY1 = benY1_actual/benY1_model; get calibrated year-1 benefit
  #  2. calibrate the benefit cash flow using a polynomial (1+g)^i, i = 0, 1, ..., (nyear - 1), such that the PV of the benefit cash flow is 
  #     equal to the reported/estimated AL for current retirees in year 1. 
  
  
  target_benY1 <- 4343259132
  target_AL    <- 42235329807 + 3764216305
  
  df_initBen <- 
    as.data.frame(AggLiab_$la.current.init) %>% 
    left_join(AggLiab_$ca.current.init %>% as.data.frame) %>% 
    left_join(AggLiab_$disb.la.current.init %>% as.data.frame) %>% 
    left_join(AggLiab_$term.current.init %>% as.data.frame) %>% 
    mutate(ALx.initBen = ALx.la.sum + liab.ca.sum + ALx.disb.la.sum + ALx.v.sum,
           B.initBen   = B.la.sum + B.ca.sum +  B.disb.la.sum +B.v.sum ) %>% 
    select(year, ALx.initBen, B.initBen)
    
  df_initBen

    
  calib_factor_benY1 <- target_benY1 / df_initBen[1, "B.initBen"]
  
  df_initBen %<>% 
    mutate(B.initBen.calib = B.initBen * calib_factor_benY1)

  
  AL_model           <- df_initBen[1, "ALx.initBen"]
  B_series.original  <- df_initBen[,  "B.initBen"]

  AL_sub  <- sum(B_series.original*((1+i)^(-(0:(length(B_series.original)-1)))))
  diff_AL <- AL_model - AL_sub 
  
  B_series_calib1 <- c(B_series.original, diff_AL*(1+i)^(-length(B_series.original))) * calib_factor_benY1
  B_series_calib1
  
  
  fun_objective <- function(g){
    (target_AL - sum(B_series_calib1 *1/((1+i)^(0:(length(B_series_calib1 )-1))) * g^(0:(length(B_series_calib1 )-1))))^2
  }
  
  
  optim_ub <- 1.5
  optim_lb <- 0.5
  
  # optim_ub <- 10
  # optim_lb <- -10
  
  g_out <- optimize(fun_objective, c(optim_lb, optim_ub))
  g <- g_out$minimum
  
  B_series_calib2 <- B_series_calib1 * g^(0:(length(B_series_calib1)-1))
  AL_Y1 <- sum(B_series_calib2*1/((1+i)^(0:(length(B_series_calib2)-1))))
  
  
  Err.AL_al.current.init <- AL_Y1 /  target_AL - 1
  Err.AL_al.current.init
  
  df_initBen$B.initBen.calib <- B_series_calib2[-length(B_series_calib2)] 
  df_initBen$ALx.initBen.calib[1] <- AL_Y1
  
  for(j in 2:nrow(df_initBen)){
    df_initBen$ALx.initBen.calib[j] <- 
      (df_initBen$ALx.initBen.calib[j - 1] - df_initBen$B.initBen.calib[j - 1]) * (1 + i)
  }

  df_initBen

  AggLiab_$initBen_calib <- df_initBen
  
  # #*************************************************************************************************************
  # #                                     ## Calibrating liabilities for current retirees   ####
  # #************************************************************************************************************* 
  # 
  # AggLiab_$active.current.calib         <- AggLiab_actives.current  %>% as.matrix
  # AggLiab_$active.la.carrent.new.calib  <- AggLiab_la.current.new   %>% as.matrix
  # AggLiab_$active.la.carrent.init.calib <- AggLiab_la.current.init  %>% as.matrix
  # AggLiab_$active.entrants.calib        <- AggLiab_actives.entrants %>% as.matrix
  # AggLiab_$la.entrants.calib            <- AggLiab_la.entrants      %>% as.matrix
  # 
  # 
  # AggLiab_$active.calib <- 
  # bind_rows(AggLiab_actives.current,  
  #           AggLiab_actives.entrants) %>% 
  #   group_by(year) %>% 
  #   summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  #   as.matrix
  # 
  # AggLiab_$la.calib <- 
  #   bind_rows(AggLiab_la.current.new ,
  #             AggLiab_la.current.init,
  #             AggLiab_la.entrants ) %>% 
  #   group_by(year) %>% 
  #   summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  #   as.matrix
  # 
  # AggLiab_$calib_factor_actives <- calib_factor_actives
  # AggLiab_$calib_factor_benY1   <- calib_factor_benY1
  # AggLiab_$calib_g              <- g
  # AggLiab_$calib_errorAL        <- Err.AL_al.current.init
  # 
  # AggLiab_$planData_list <- planData_list_
  
  return(AggLiab_)

}
