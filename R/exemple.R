

event <- c(rep(0,40),rep(1,15))
idp <- paste0("P",1:55)
sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
dtindex_case <- c(seq(1,40,1),seq(5,47,3))
dtindex_control <- dtindex_case # Data de censura
diabetes <- seq(2,110,2)
heartdis <- seq(110,2,-2)
diabetes <- c(rep(1,55))
heartdis <- c(rep(100,55))
library(data.table)
dat <- data.table::data.table(event,idp,sex,byear,diabetes,heartdis,dtindex_case,dtindex_control)


kk<-match_density_incidence(dt=dat,
                                  id="idp",
                                  llistaPS=c("sex"),
                                  eventcontrol=F,
                                  reemplacement=F,
                                  numcores=NA,
                                  Ncontrols=1,
                                  seed=123)

library(dplyr)
