library(frame)
library(tidyverse)
library(extraDistr)
library(impact)

####### 
### functions for checking inputs template
#######
###It should be noted that these checks are so naive. There is plenty of room for improvement.

### writing the check function
wthr_check <- function(x){
  nms_1 <- names(x)
  if( ncol(x) == 4){
    if(all(nms_1 == c("tm","T","W","DFMC"))){  
      status <- "np" # no problem
    }else{
        status <- "p"
      }
  }else{
    status <- "p" # has a problem number of columns are different or name of columns are different
  }
  return(status)
}
site_check <-  function(x){
  nms_1 <- names(x)
  if( ncol(x) ==8 ){ 
    if(all(nms_1 == c("record",	"site",	"slope",	"wind",	"temp",	"dfmc",	"oHorizon",	"fLine" ))){
    status <- "np" # no problem 
    }else{
      status <- "p"
    }
  }else{
    status <- "p" # has a problem number of columns are different or name of columns are different
  }
  return(status)
}

stctr_check <- function(x){
  nms_1 <- names(x)
  if( ncol(x) ==11){
    if(all(nms_1 == c("record","site","NS","El","Mid","Can","ns_e","ns_m","e_m","e_c","m_c"))){
    status <- "np" # no problem 
    }else{
      status <- "p"
    }
  }else{
    status <- "p" # has a problem number of columns are different or name of columns are different
  }
  return(status)
} 


flora_check <- function(x){
  nms_1 <- names(x)
  if( ncol(x) ==13){ 
    if(all(nms_1 == c("record",	"site",	"species","moisture","stratum",
                                    "comp","base","he","ht","top","w","openness","clump"))){
    status <- "np" # no problem 
    }else{
     status <- "p"
    }
  }else{
    status <- "p" # has a problem number of columns are different or name of columns are different
  }
  return(status)
}


cover_check <- function(x){
  nms_1 <- names(x)
  if( ncol(x) ==11){
    if(all(nms_1 == c("record",	"species",	"stratum",	"sName",	"exp_a",	"exp_b",	
                      "aQ",	"bQ",	"cQ",	"aLin",	"bLin"))){
    status <- "np" # no problem 
    }else{
      status <- "p"
    }
    }else{
      status <- "p"
  }
  return(status)
}

growth_check <-  function(x){
  nms_1 <- names(x)
  if( ncol(x) ==7){ 
    if(all(nms_1 == c("record",	"stratum"	,"Species",	"max"	,"rate"	,"aLin"	,"bLin"))){
    status <- "np" # no problem 
    }else{
      status <- "p"
  }
    }else{
    status <- "p" # has a problem number of columns are different or name of columns are different
  }
  return(status)
}

traits_check <-  function(x){
  
  nms_1 <- names(x)
  if( ncol(x) ==9){
    if(all(nms_1 == c("name",	"propDead",	"leafForm",	"leafThickness",	
                                   "leafWidth",	"leafLength",	"leafSeparation",	"stemOrder",	"ignitionTemp"))){
    status <- "np" # no problem
    }else{
      status <- "p"
    }
  }else{
    status <- "p" # has a problem number of columns are different or name of columns are different
  }
  return(status)
}