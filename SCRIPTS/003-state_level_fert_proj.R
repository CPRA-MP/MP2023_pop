###################
### DATA PREP
##################
rm(list=ls())
source('./R/SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./R/SCRIPTS/002-fertdataload.R')   # loading the base data
# source('./R/SCRIPTS/001-fipscodes.R')      # Getting a Fips List

races <- unique(K05_pop$RACE2) # creating a looping variable
states <- unique(K05_pop$STATE) # creating a looping variable
fertrats<- data.frame() # making the empty dataframe to hold the results.
for(this.state in states){
for(this.race in races){
    K05t <- K05_pop[which(K05_pop$RACE2 == this.race & K05_pop$STATE == this.state),] %>%
      group_by(YEAR, RACE2, SEX, AGE) %>%
      dplyr::summarise(POPULATION = sum(POPULATION)) %>%
      ungroup()
    newbornst <- K05t %>%
      filter(AGE == 1) %>% # AGE 1 = newborns.
      group_by(RACE2, YEAR)  %>%
      dplyr::summarise(Newborns = sum(POPULATION))
    newbornst2 <- K05t %>%
      filter(AGE == 2) %>% # AGE 1 = newborns.
      group_by(RACE2, YEAR)  %>%
      dplyr::summarise(Newborns2 = sum(POPULATION))
    childbearingt <- K05t %>%
      filter(AGE %in% c(4,5,6,7,8,9,10,11), # women ages 15-49
             SEX == 2 ) %>% 
      group_by(YEAR) %>%
      dplyr::summarise(Women1550 = sum(POPULATION[AGE %in% c(4,5,6,7,8,9,10)]),
                       Women2055 = sum(POPULATION[AGE %in% c(5,6,7,8,9,10,11)])) %>%
      left_join(., newbornst) %>%
      left_join(., newbornst2) %>%
      mutate(fertrat1 = Newborns/Women1550,
             fertrat2 = Newborns2/Women2055) %>%
      filter(YEAR <= 2010)
    childbearingt$SEX <- "Female"
    childbearingt[mapply(is.infinite, childbearingt)] <- NA
    childbearingt[mapply(is.nan, childbearingt)] <- NA
    childbearingt[is.na(childbearingt)] <-0
    num <- seq(1,FORLEN,10)
    predcwr = function(ccr, sex, x, DF){
      hyndtran = function(ccr,DF){log((ccr - a) / (b - ccr))}
      b <- max(DF[[as.character(ccr)]][which(DF$RACE== x)])*1.01
      a <- -0.00000001
      y <-as_data_frame(hyndtran(DF[[as.character(ccr)]][which(DF$STATE== x & DF$SEX == sex & DF$RACE == this.race)]))
      
      num <- seq(1,FORLEN,5)
      pred<- tryCatch(round(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),],5)
                      , error=function(e) array(hyndtran(DF$fertrat[which.max(DF$YEAR)]), c(STEPS)))
      pred2 <-(b-a)*exp(pred)/(1+exp(pred))+a
      return(round(pred2,6))#
    }
    forecast<- forecast(arima(childbearingt$fertrat1, order = arima_order), h= FORLEN)
    mean <- forecast$mean[c(num)]
    lower <- forecast$lower[c(num)]
    upper <- forecast$upper[c(num)]
    forecasst1<- as.tibble(t(rbind(mean, lower, upper)), rat = "n011")
    forecasst1$rat = "n01"
    
    forecast<- forecast(arima(childbearingt$fertrat2, order = arima_order), h= FORLEN)
    mean <- forecast$mean[c(num)]
    lower <- forecast$lower[c(num)]
    upper <- forecast$upper[c(num)]
    forecasst2<- as.tibble(t(rbind(mean, lower, upper)))
    forecasst2$rat = "n02"
    dat <- data.frame()
    dat<-rbind(dat,forecasst2, forecasst1)  %>%
                 mutate(STATE = this.state,
                        RACE= this.race,
                        SEX = 2)
    fertrats <- rbind(fertrats, dat)
}
}




write_csv(fertrats, "./R/DATA-PROCESSED/state-level-fert-rates_20152070.csv")