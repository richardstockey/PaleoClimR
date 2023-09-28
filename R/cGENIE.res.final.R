###################################################
# cGENIE.res.final.R
# Rich Stockey 20230922
# designed to import .res files and make them normal data frames
# THEN to just extract final data point
###################################################
# options include
# "_surTIF" -
###################################################
# full comments to follow...


cGENIE.res.final <- function(var, sub_var = "default", experiment){
  res.frame <- cGENIE.res.import(var = var,
                                 experiment = experiment)

  if(sub_var == "default"){
    if(var == "ocn_temp"){
      val <- res.frame$`_surT (ice-free) (C)`[nrow(res.frame)]
    }else if(var == "ocn_O2"){
      val <- res.frame$`global mean O2 (mol kg-1)`[nrow(res.frame)]
    }else{
      val <- res.frame[nrow(res.frame), 2]
    }

  }
  return(val)
}
