
## Getting data from the ACS for HH Income (White Alone Not Hispanic)
incdat_whitenh <- get_acs(
  table = c("B19001H"), # This is the table
  geography = "tract", # At the tract geography
  state = c("LA", "MS") # For all tracts in Louisiana and Mississippi
) %>%
  mutate(race = "White, NH", # These data have to be recoded into income brackets.
         income = case_when(
           variable == "B19001H_002" ~ 9999,
           variable == "B19001H_003" ~ 14999,
           variable == "B19001H_004" ~ 19999,
           variable == "B19001H_005" ~ 24999,
           variable == "B19001H_006" ~ 29999,
           variable == "B19001H_007" ~ 34999,
           variable == "B19001H_008" ~ 39999,
           variable == "B19001H_009" ~ 44999,
           variable == "B19001H_010" ~ 49999,
           variable == "B19001H_011" ~ 59999,
           variable == "B19001H_012" ~ 74999,
           variable == "B19001H_013" ~ 99999,
           variable == "B19001H_014" ~ 124999,
           variable == "B19001H_015" ~ 149999,
           variable == "B19001H_016" ~ 199999,
           variable == "B19001H_017" ~ 200000,
         ))

## Getting data from the ACS for HH Income (All Population)
incdat_total <- get_acs(
  table = c("B19001"),
  geography = "block group",
  state = c("LA", "MS")
)

# Income uses PUMA data as well and I need a 'crosswalk' to put each tract/block group in their respective PUMAS
pumacrosswalk <- read.csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt",
                          colClasses=c("character")) %>%
  mutate(TRACT = paste0(STATEFP, COUNTYFP, TRACTCE), # Minor data wrangling to put GEOIDs in the same format
         STPUMA = paste0(STATEFP, PUMA5CE)) %>%
  dplyr::select(STPUMA, TRACT)

# This comes from a native file in the TidyCensus package that contains the PUMA variables.
pums_vars_2018 <- pums_variables %>%
  filter(year == 2018, survey == "acs5")%>%
  distinct(var_code, var_label, data_type, level)

# Getting the PUMs data for Louisiana. I grab Sex, Age, HH Income, Race, and Hispanic Origin.
la_pums <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "HINCP", "RAC1P", "HISP"),
  state = "LA",
  survey = "acs5"
)

ms_pums <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "HINCP", "RAC1P", "HISP"),
  state = "MS",
  survey = "acs5"
)

# Putting Louisiana and Mississippi in the same data frame. Minor data wrangling at the top of the chunk.
pumsdat <- rbind(la_pums, ms_pums) %>%
  mutate(
    RACE = case_when(
      RAC1P == 1 & HISP == "01" ~ "White",
      TRUE ~ "Non-White"
    ),
    AGE = case_when(
    AGEP < 5 ~ 1,
    AGEP < 10 ~ 2,
    AGEP < 15 ~ 3,
    AGEP < 20 ~ 4,
    AGEP < 25 ~ 5,
    AGEP < 30 ~ 6,
    AGEP < 35 ~ 7,
    AGEP < 40 ~ 8,
    AGEP < 45 ~ 9,
    AGEP < 50 ~ 10,
    AGEP < 55 ~ 11,
    AGEP < 60 ~ 12,
    AGEP < 65 ~ 13,
    AGEP < 70 ~ 14,
    AGEP < 75 ~ 15,
    AGEP < 80 ~ 16,
    AGEP < 85 ~ 17,
    AGEP >= 85 ~ 18
  ),
  HHINC = case_when(
    HINCP < 10000 ~ 9999,
    HINCP < 15000 ~ 14999,
    HINCP < 20000 ~ 19999,
    HINCP < 25000 ~ 24999,
    HINCP < 30000 ~ 29999,
    HINCP < 35000 ~ 34999,
    HINCP < 40000 ~ 39999,
    HINCP < 45000 ~ 44999,
    HINCP < 50000 ~ 49999,
    HINCP < 60000 ~ 59999,
    HINCP < 75000 ~ 74999,
    HINCP < 100000 ~ 99999,
    HINCP < 125000 ~ 124999,
    HINCP < 150000 ~ 149999,
    HINCP < 200000 ~ 199999,
    HINCP >= 200000 ~ 200000,
  )) %>%
  group_by(ST, PUMA, AGE, RACE, SEX, HHINC) %>%
  dplyr::summarise(pop = sum(PWGTP))%>% # Summing the person weights.
  group_by(ST,PUMA, AGE, RACE, SEX) %>%
  mutate(freq = pop / sum(pop), # Calculating the # of people in each HHIncome Group by Age/Sex/Race
         STPUMA = paste0(ST, PUMA),
         SEX = as.numeric(SEX))

# Pulling in the CBG projections and merging with the Crosswalk (To put CBGs in PUMAS) and then then PUMS data (to estimate HH income in CBGS)
projections <- read_csv("./DATA-PROCESSED/DELIVERABLES/allproj.csv") %>%
  mutate(TRACT= substr(GEOID,1,11)) %>%
  left_join(., pumacrosswalk) %>%
  left_join(., pumsdat) %>%
  mutate(incpersons = freq * A) %>% # "A" is the Projected Population. So multiplying the Projected Population by each HH Income category
  filter(!is.na(HHINC)) %>%
  group_by(YEAR, 
           FIPS, 
           TRACT, GEOID, 
           HHINC) %>% 
  dplyr::summarise(incpersons = sum(incpersons)) # Summing them all up.

# Calculating the Median Household Income.
proj2 <- projections %>%
  mutate(width = lead(HHINC) - HHINC,
    csum = cumsum(incpersons),
         tot = sum(incpersons),
         mid = if_else((csum/tot)<=0.5,1,0),
         midpoint = tot/2,
         MedianIncome = (((midpoint-csum)/lead(incpersons))) * width + HHINC) %>%
  filter(mid == 1) %>%
  filter(HHINC == max(HHINC)) %>%
  ungroup() %>%
  group_by(GEOID) %>%
  arrange(GEOID, YEAR) %>%
  mutate(diff = MedianIncome / lag(MedianIncome))

# Cleaning up the final datafile.
projectionsincome <- proj2 %>%
  dplyr::select(YEAR, FIPS, GEOID, MedianIncome)

# write.csv(projectionsincome, "./DATA-PROCESSED/DELIVERABLES/projections_income.csv")


