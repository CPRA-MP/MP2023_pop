# ###------DATA LOAD-----
# ## @knitr basedataload
# 
source('./R/SCRIPTS/000-Libraries.R')      # loading in the libraries
# 

# # Data is in awful shape so I built a codebook in excel :-/
# codebook <- read.xlsx("./R/DATA/1990_codebook.xlsx", sheet ="_19902010") %>%
#   dplyr::select(Group, RACE, SEX, AGE)


## Data was too large for a single file from NHGIS, so I had to split the data pull.
## This chunk reads both CSV's in, filters to just Louisiana, converts from Wide to Tall.
## Then it joins the Group variable with the codebook and drops all NA values.
## Finally, it creates a new White/Non-White race group and summarises by AGE/SEX/RACE.
##
# The csv's are really large and will need to be unpacked to run this script again.
# a <- rbind(
#   read_csv("./R/DATA/nhgis0012_ts_geog2010_blck_grp.csv") %>%
#     filter(STATE == "Louisiana"｜ paste0(STATEA, COUNTYA) %in% c("28045", "28047", "28059")) %>%
#     gather(Group, Population, 10:405 ),
#   read_csv("./R/DATA//nhgis0013_ts_geog2010_blck_grp.csv") %>%
#     filter(STATE == "Louisiana"｜ paste0(STATEA, COUNTYA) %in% c("28045", "28047", "28059")) %>%
#     gather(Group, Population, 10:273 )
#   ) %>%
#   left_join(., codebook) %>%
#   na.omit  %>%
#   mutate(RACE2 = ifelse(RACE == "White", "White", "Non-White"),
#          SEX = ifelse(SEX == "Female", 2,1)) %>%
#   group_by(DATAYEAR, STATE, COUNTY, TRACTA, BLCK_GRPA, GISJOIN, RACE2, SEX, AGE) %>%
#   dplyr::summarise(Pop = sum(Population)) %>%
#   ungroup() %>%
#   dplyr::select(everything(), YEAR = DATAYEAR, RACE = RACE2)

saveRDS(a, "./R/DATA-PROCESSED/population_data.RDS")

K05_pop<-readRDS("./R/DATA-PROCESSED/population_data.RDS")


launch_year = 2010

SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-12
# FORECAST LENGTH. SINCE THE PROJECTION INTERVAL IS 5 YEARS IT IS (STEPS*5)
FORLEN<-(STEPS*5)

years <- 0
years$YEAR <- seq(launch_year+10,launch_year+(STEPS*10), 10)
