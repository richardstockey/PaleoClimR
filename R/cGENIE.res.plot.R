###################################################
# cGENIE.res.plot.R
# Rich Stockey 20230921
# designed to import .res files and make them normal data frames
# THEN to plot them as time series\
# this builds on cGENIE.res.import
###################################################
# options include
# "_surTIF" -
###################################################
# full comments to follow...

cGENIE.res.plot <- function(var, sub_var = "default", experiment){
  library(ggplot2)
  res.frame <- cGENIE.res.import(var = var,
                            experiment = experiment)

  if(sub_var == "default"){
    if(var == "ocn_temp"){
      plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = `_surT (ice-free) (C)`))+
        geom_line()+
        theme_bw()+
        theme(panel.border = element_rect(fill=NA,color="black", linetype="solid"),
              axis.line = element_line(lineend = 'square'),
              axis.text = element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      }else if(var == "ocn_O2"){
        plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = `global mean O2 (mol kg-1)`))+
          geom_line()+
          theme_bw()+
          theme(panel.border = element_rect(fill=NA,color="black", linetype="solid"),
                axis.line = element_line(lineend = 'square'),
                axis.text = element_text(color="black"),
                legend.justification=c(1,1), legend.position=c(.98,.36),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      }else if(var == "atm_pO2"){
      plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = `global pO2 (atm)`))+
        geom_line()+
        theme_bw()+
        theme(panel.border = element_rect(fill=NA,color="black", linetype="solid"),
              axis.line = element_line(lineend = 'square'),
              axis.text = element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      }else if(var == "atm_pCO2"){
        plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = `global pCO2 (atm)`))+
          geom_line()+
          theme_bw()+
          theme(panel.border = element_rect(fill=NA,color="black", linetype="solid"),
                axis.line = element_line(lineend = 'square'),
                axis.text = element_text(color="black"),
                legend.justification=c(1,1), legend.position=c(.98,.36),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      }else{
      col_no <- 2
      y_name <- names(res.frame)[col_no]
      plot <- ggplot(data = res.frame, aes(x = `% time (yr)` , y = get(names(res.frame)[col_no])))+
        geom_line()+
        ylab(y_name)+
        theme_bw()+
        theme(panel.border = element_rect(fill=NA,color="black", linetype="solid"),
              axis.line = element_line(lineend = 'square'),
              axis.text = element_text(color="black"),
              legend.justification=c(1,1), legend.position=c(.98,.36),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }

  }
  return(plot)
}
