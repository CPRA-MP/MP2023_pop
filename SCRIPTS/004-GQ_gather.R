rm(list=ls())
library(tidyverse)
library(tigris)
library(censusapi)
library(tidycensus)
library(data.table)

# ENTER YOUR CENSUS API KEY HERE
 key <- ""


fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  dplyr::rename(state = X1,
         STATEID = X2,
         CNTYID = X3,
         NAME = X4) %>%
  filter(STATEID == "22" ｜ GEOID %in% c("28045", "28047", "28059"))
stateid = unlist(list(unique(fipslist$GEOID)))


getgq_2010 = function(x){
  tryCatch({
    v<- c("COUNTY","NAME", sort(paste0("P012",LETTERS[1:7], rep(str_pad(seq(3,49), 3, pad = "0"),7))))  
    
    dat2 = list()
    for(i in 1:7){
      tryCatch({
        totpop3 <- getCensus(name="dec/sf1", # This is the Estimates datafile
                             vintage = 2010, # Vintage year is set to the variable set above
                             key = key, # inputting my Census API key
                             # vars = "group(PCT13)", # gathering these variables
                             var = c("COUNTY", "NAME", paste0("group(P12", LETTERS[i],")")),
                             region="block group:*",
                             regionin=paste0("state:", substr(x,1,2), "+county:", substr(x,3,5))
                             # regionin=paste0("state:22")
        ) %>%
          pivot_longer(cols = c(8:57), names_to = "name", values_to = "TOTAL")
        dat2[[i]] <- totpop3}
        , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    totpop <- rbindlist(dat2) %>%
      left_join(., list) %>%
      mutate(Racecode = substr(name, 5, 5)) %>%
      separate(label, c("Other", "Sex", "pAge"), sep = "!!") %>%
      separate(pAge, c("Age", "Drop"), sep = " t") %>%
      mutate(GEOID = paste0(state, county, tract, block_group),
        Age = case_when(
        Age == "Under 5 years" ~ "0",
        Age == "18 and 19 years" ~ "15",
        Age == "20 years" ~ "20",
        Age == "21 years" ~ "20",
        Age == "22" ~ "20",
        Age == "60 and 61 years" ~ "60",
        Age == "62" ~ "60",
        Age == "65 and 66 years" ~ "65",
        Age == "67" ~ "65",
        Age == "85 years and over" ~ "85",
        TRUE ~ as.character(Age)),
        Race = case_when(
          Racecode %in% c("B", "C", "D", "E", "F", "G") ~ "Non-White",
          Racecode == "A" ~ "White"
        ))  %>%
      dplyr::select(-concept, -Racecode, -name)  %>%
      group_by(state, county, tract, GEOID, Sex, Race, Age) %>%
      dplyr::summarise(TOTAL = sum(TOTAL)) %>%
      na.omit
    
    v<- c("COUNTY","NAME", sort(paste0("PCT013",LETTERS[1:8], rep(str_pad(seq(3,49), 3, pad = "0"),8))))
    
    dat = list()
    for(i in 1:7){
      tryCatch({
        totpop3 <- getCensus(name="dec/sf1", # This is the Estimates datafile
                             vintage = 2010, # Vintage year is set to the variable set above
                             key = key, # inputting my Census API key
                             # vars = v, # gathering these variables
                             var = c("COUNTY", "NAME", paste0("group(PCT13", LETTERS[i],")")),
                             region="tract:*",
                             regionin=paste0("state:", substr(x,1,2), "+county:", substr(x,3,5))
        ) %>%
          pivot_longer(cols = c(7:56), names_to = "name", values_to = "TOTAL")
        dat[[i]] <- totpop3}
        , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    hhpop <- rbindlist(dat) %>%
      left_join(., list) %>%
      mutate(Racecode = substr(name, 7, 7)) %>%
      separate(label, c("Other", "Sex", "pAge"), sep = "!!") %>%
      separate(pAge, c("Age", "Drop"), sep = " t") %>%
      mutate(Age = case_when(
        Age == "Under 5 years" ~ "0",
        Age == "18 and 19 years" ~ "15",
        Age == "20 years" ~ "20",
        Age == "21 years" ~ "20",
        Age == "22" ~ "20",
        Age == "60 and 61 years" ~ "60",
        Age == "62" ~ "60",
        Age == "65 and 66 years" ~ "65",
        Age == "67" ~ "65",
        Age == "85 years and over" ~ "85",
        TRUE ~ as.character(Age)),
        Race = case_when(
          # Racecode == "B" ~ "BLACK, NH",
          Racecode %in% c("B", "C", "D", "E", "F", "G") ~ "Non-White",
          # Racecode == "H" ~ "HISPANIC",
          Racecode == "A" ~ "White"
        ))  %>%
      dplyr::select(-concept, -Racecode, -name) %>%
      group_by(state, county, tract, Sex, Race, Age) %>%
      dplyr::summarise(HHPOP = sum(TOTAL)) %>%
      na.omit 
    

        totpop3 <- getCensus(name="dec/sf1", # This is the Estimates datafile
                             vintage = 2010, # Vintage year is set to the variable set above
                             key = key, # inputting my Census API key
                             vars = c("COUNTY", "NAME", paste0("P029", LETTERS[1:7],rep("026",7))), # gathering these variables
                             # var = c("COUNTY", "NAME", paste0("group(P29", LETTERS[i],")")),
                             region="block group:*",
                             regionin=paste0("state:", substr(x,1,2), "+county:", substr(x,3,5))
        ) %>%
          pivot_longer(cols = c(7:13), names_to = "name", values_to = "GQTOT")
      
    gqtot <- totpop3 %>%
      mutate(GEOID = paste0(state, county, tract, block_group),
             Racecode = substr(name, 5, 5),
             Race = case_when(
               # Racecode == "B" ~ "BLACK, NH",
               Racecode %in% c("B", "C", "D", "E", "F", "G") ~ "Non-White",
               # Racecode == "H" ~ "HISPANIC",
               Racecode == "A" ~ "White"
             ))  %>%
      dplyr::select(-Racecode, -name) %>%
      group_by(state, county, tract, GEOID,  Race) %>%
      dplyr::summarise(GQTOT = sum(GQTOT)) %>%
      group_by(state, county, Race, tract)
    gqtot[mapply(is.infinite, gqtot)] <- NA
    gqtot[is.na(gqtot)] <-0
    

    joined <- left_join(totpop, gqtot) %>%
      left_join(hhpop) %>%
      # filter(tract == "960800", Race == "Non-White") %>%
    # 1st rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(HHest = sum(TOTAL),
             GQallocate = HHest-HHPOP,
             GQ = TOTAL /sum(TOTAL)*GQallocate) %>%
    # 2nd rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = (GQTOT / GQtot2)*GQ) %>%
    # 3rd rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ = ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
    # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
           GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
    
    # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
      
      # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
      # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
      # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
      # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
      
      # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
      
      # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
      # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ)) %>%
      # 4th rake
      group_by(state, county, tract, GEOID, Race) %>%
      mutate(GQtot2 = sum(GQ),
             GQ = ifelse(is.nan((GQTOT / GQtot2)*GQ), 0, (GQTOT / GQtot2)*GQ)) %>%
      # 5th rake
      group_by(state, county, tract, Sex, Age, Race) %>%
      mutate(GQ =  ifelse(is.nan(GQallocate/sum(GQ)*GQ), 0, GQallocate/sum(GQ)*GQ),
             HH= TOTAL-GQ) %>%
      mutate(
      Age = as.numeric(Age),
    YEAR = 2010,
    STATE = as.numeric(state),
    COUNTY = as.numeric(county),
    SEX = case_when(
      Sex == "Female" ~ "FEMALE",
      Sex == "Male" ~ "MALE"
    ),
    AGEGRP = case_when(
      Age == 0 ~ 1,
      Age == 5 ~ 2,
      Age == 10 ~ 3,
      Age == 15~ 4,
      Age == 20 ~ 5,
      Age == 25 ~ 6,
      Age == 30 ~ 7,
      Age == 35 ~ 8,
      Age == 40 ~ 9,
      Age == 45 ~ 10,
      Age == 50 ~ 11,
      Age == 55 ~ 12,
      Age == 60 ~ 13,
      Age == 65 ~ 14,
      Age == 70 ~ 15,
      Age == 75 ~ 16,
      Age == 80 ~ 17,
      Age == 85 ~ 18
    )) %>%
    # separate(NAME, "County", sep = " County") %>%
    dplyr::rename(RACE = Race) %>%
      ungroup() %>%
    dplyr::select(GEOID, Sex, RACE, GQ, HH, YEAR, STATE, COUNTY, SEX, AGEGRP)
      
    #   
    # 
    #   # 5th rake
    #   group_by(state, county, tract, Sex, Age, Race) %>%
    #   mutate(GQ4 =  GQallocate/sum(GQ)*GQ)
    #   mutate(GQ2 = ifelse(is.nan(GQallocate/sum(GQ2)*GQ2), 0, GQallocate/sum(GQ2)*GQ2)) %>%
    #   # 6th rake
    #   group_by(state, county, tract, GEOID, Race) %>%
    #   mutate(GQtot2 = sum(GQ2),
    #          GQ2 = (GQTOT / GQtot2)*GQ) %>%
    #   
    #   
    #   group_by(state, county, tract) %>%
    #   mutate(GQfrac = sum(GQTOT))
    # 
    # joined <- left_join(totpop, hhpop) %>%
    #   ungroup() %>%
    #   mutate(GEOID = paste0(state, county,tract),
    #     GQ = TOTAL - HHPOP,
    #          Age = as.numeric(Age),
    #          YEAR = 2010,
    #          STATE = as.numeric(state),
    #          COUNTY = as.numeric(county),
    #          SEX = case_when(
    #            Sex == "Female" ~ "FEMALE",
    #            Sex == "Male" ~ "MALE"
    #          ),
    #          AGEGRP = case_when(
    #            Age == 0 ~ 1,
    #            Age == 5 ~ 2,
    #            Age == 10 ~ 3,
    #            Age == 15~ 4,
    #            Age == 20 ~ 5,
    #            Age == 25 ~ 6,
    #            Age == 30 ~ 7,
    #            Age == 35 ~ 8,
    #            Age == 40 ~ 9,
    #            Age == 45 ~ 10,
    #            Age == 50 ~ 11,
    #            Age == 55 ~ 12,
    #            Age == 60 ~ 13,
    #            Age == 65 ~ 14,
    #            Age == 70 ~ 15,
    #            Age == 75 ~ 16,
    #            Age == 80 ~ 17,
    #            Age == 85 ~ 18
    #          )) %>%
    #   # separate(NAME, "County", sep = " County") %>%
    #   dplyr::rename(RACE = Race) %>%
    #   dplyr::select(GEOID, Sex, RACE, GQ, YEAR, STATE, COUNTY, SEX, AGEGRP)
      # dplyr::select(-state, -county, -County, -Sex, -Age, -TOTAL, -HHPOP)
   
    return(joined)
  }
  
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

getgq_2000 = function(x){
  tryCatch({
    
    # listing the available census variables in the population estimates agegroups data file
    # list <- listCensusMetadata(name = "dec/sf1", vintage = 2000, type ="variables")
  
    list <- listCensusMetadata(name = "dec/sf1", vintage = 2000, type ="variables")
    v<- c("COUNTY","NAME", sort(paste0("P012",LETTERS[1:7], rep(str_pad(seq(3,49), 3, pad = "0"),7))))  
    # Getting the census data from the API
    dat = list()
    for(i in 1:7){
      tryCatch({
    totpop3 <- getCensus(name="dec/sf1", # This is the Estimates datafile
                        vintage = 2000, # Vintage year is set to the variable set above
                        key = key, # inputting my Census API key
                        # vars = v, # gathering these variables
                        var = c("COUNTY", "NAME", paste0("group(P012", LETTERS[i],")")),
                        region="tract:*",
                        regionin=paste0("state:", x)
                        )%>%
      pivot_longer(cols = c(7:56), names_to = "name", values_to = "TOTAL")
    dat[[i]] <- totpop3}
    , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    totpop <- rbindlist(dat)%>%
      left_join(., list) %>%
      mutate(Racecode = substr(name, 5, 5)) %>%
      separate(label, c("Other", "Sex", "pAge"), sep = "!!") %>%
      separate(pAge, c("Age", "Drop"), sep = " t") %>%
      mutate(Age = case_when(
        Age == "Under 5 years" ~ "0",
        Age == "18 and 19 years" ~ "15",
        Age == "20 years" ~ "20",
        Age == "21 years" ~ "20",
        Age == "22" ~ "20",
        Age == "60 and 61 years" ~ "60",
        Age == "62" ~ "60",
        Age == "65 and 66 years" ~ "65",
        Age == "67" ~ "65",
        Age == "85 years and over" ~ "85",
        TRUE ~ as.character(Age)),
        Race = case_when(
          Racecode %in% c("B", "C", "D", "E", "F", "G") ~ "Non-White",
          Racecode == "A" ~ "White"
        ))  %>%
      dplyr::select(-concept, -Racecode, -name)  %>%
      group_by(state, county, tract, Sex, Race, Age) %>%
      dplyr::summarise(TOTAL = sum(TOTAL),
                       nhh = sum(n())) %>%
      na.omit

 v<- c("COUNTY","NAME", sort(paste0("PCT013",LETTERS[1:8], rep(str_pad(seq(3,49), 3, pad = "0"),8))))

 dat = list()
 for(i in 1:7){
   tryCatch({
   totpop3 <- getCensus(name="dec/sf1", # This is the Estimates datafile
                        vintage = 2000, # Vintage year is set to the variable set above
                        key = key, # inputting my Census API key
                        # vars = v, # gathering these variables
                        var = c("COUNTY", "NAME", paste0("group(PCT013", LETTERS[i],")")),
                        region="tract:*",
                        regionin=paste0("state:", x)
   )%>%
     pivot_longer(cols = c(7:56), names_to = "name", values_to = "TOTAL")
   dat[[i]] <- totpop3}
   , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
 }
 hhpop <- rbindlist(dat) %>%
   left_join(., list) %>%
   mutate(Racecode = substr(name, 7, 7)) %>%
   separate(label, c("Other", "Sex", "pAge"), sep = "!!") %>%
   separate(pAge, c("Age", "Drop"), sep = " t") %>%
   mutate(Age = case_when(
     Age == "Under 5 years" ~ "0",
     Age == "18 and 19 years" ~ "15",
     Age == "20 years" ~ "20",
     Age == "21 years" ~ "20",
     Age == "22" ~ "20",
     Age == "60 and 61 years" ~ "60",
     Age == "62" ~ "60",
     Age == "65 and 66 years" ~ "65",
     Age == "67" ~ "65",
     Age == "85 years and over" ~ "85",
     TRUE ~ as.character(Age)),
     Race = case_when(
       # Racecode == "B" ~ "BLACK, NH",
       Racecode %in% c("B", "C", "D", "E", "F", "G") ~ "Non-White",
       # Racecode == "H" ~ "HISPANIC",
       Racecode == "A" ~ "White"
     ))  %>%
   dplyr::select(-concept, -Racecode, -name) %>%
   group_by(state, county, tract, Sex, Race, Age) %>%
   dplyr::summarise(HHPOP = sum(TOTAL)) %>%
   na.omit
 
    
    
    joined <- left_join(totpop, hhpop) %>%
      ungroup() %>%
      mutate(GEOID = paste0(state, county,tract),
             GQ = TOTAL - HHPOP,
             Age = as.numeric(Age),
             YEAR = 2000,
             STATE = as.numeric(state),
             COUNTY = as.numeric(county),
             SEX = case_when(
               Sex == "Female" ~ "FEMALE",
               Sex == "Male" ~ "MALE"
             ),
             AGEGRP = case_when(
               Age == 0 ~ 1,
               Age == 5 ~ 2,
               Age == 10 ~ 3,
               Age == 15~ 4,
               Age == 20 ~ 5,
               Age == 25 ~ 6,
               Age == 30 ~ 7,
               Age == 35 ~ 8,
               Age == 40 ~ 9,
               Age == 45 ~ 10,
               Age == 50 ~ 11,
               Age == 55 ~ 12,
               Age == 60 ~ 13,
               Age == 65 ~ 14,
               Age == 70 ~ 15,
               Age == 75 ~ 16,
               Age == 80 ~ 17,
               Age == 85 ~ 18
             )) %>%
      # separate(NAME, "County", sep = " County") %>%
      dplyr::rename(RACE = Race) %>%
      dplyr::select(GEOID, Sex, RACE, GQ, YEAR, STATE, COUNTY, SEX, AGEGRP)

    return(joined)
  }
  
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

list <- listCensusMetadata(name = "dec/sf1", vintage = "2010", type ="variables")


dat <- lapply(stateid, getgq_2010)
GQ2010 <- rbindlist(dat)

list <- listCensusMetadata(name = "dec/sf1", vintage = "2000", type ="variables")

dat<-lapply(stateid, getgq_2000)
GQ2000 <- rbindlist(dat)

write_csv(GQ2010, "./R/DATA-PROCESSED/gq_2010.csv")
write_csv(GQ2000, "./R/DATA-PROCESSED/gq_2000.csv")

