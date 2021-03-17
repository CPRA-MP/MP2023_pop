
## Getting Poverty Data from the ACS. B17001=Pov by Sex by Age. The age categories are not standardized, so I standardize them.
povdat_whitenh <- get_acs(
  table = c("B17001"),
  geography = "tract",
  state = c("LA", "MS")
) %>%
  mutate(agegroup = case_when(
           variable %in% c("B17001_004", "B17001_018", "B17001_033", "B17001_047") ~ "0-4",
           variable %in% c("B17001_005", "B17001_006", "B17001_007",
                           "B17001_019", "B17001_020", "B17001_021",
                           "B17001_034", "B17001_035", "B17001_036",
                           "B17001_049", "B17001_050", "B17001_051") ~ "5-14",
           variable %in% c("B17001_008","B17001_009", "B17001_010",
                           "B17001_022","B17001_023", "B17001_024",
                           "B17001_037", "B17001_038", "B17001_039",
                           "B17001_051", "B17001_052", "B17001_053") ~ "15-24",
           variable %in% c("B17001_011", "B17001_025",
                           "B17001_040", "B17001_054"
           ) ~ "25-34",
           variable %in% c("B17001_012", "B17001_026",
                           "B17001_041", "B17001_055"
           ) ~ "35-44",
           variable %in% c("B17001_013", "B17001_027",
                           "B17001_042", "B17001_056"
           ) ~ "45-54",
           variable %in% c("B17001_014", "B17001_028",
                           "B17001_043", "B17001_057"
           ) ~ "55-64",
           variable %in% c("B17001_015", "B17001_029",
                           "B17001_044", "B17001_058"
           ) ~ "65-74",
           variable %in% c("B17001_016", "B17001_030",
                           "B17001_045", "B17001_059"
           ) ~ "75+",
           
         ),
         SEX = case_when(
           variable %in% c(paste0("B17001_",str_pad(seq(4,16,1), 3, pad = "0")),
                           paste0("B17001_",str_pad(seq(33,45,1), 3, pad = "0"))) ~ 1,
           variable %in% c(paste0("B17001_",str_pad(seq(18,30,1), 3, pad = "0")),
                           paste0("B17001_",str_pad(seq(47,59,1), 3, pad = "0"))) ~ 2,
         ),
         poverty = case_when(
           variable %in% paste0("B17001_", str_pad(seq(4,30,1), 3, pad="0")) ~ "InPoverty",
           variable %in% paste0("B17001_", str_pad(seq(33,59,1), 3, pad="0")) ~ "AbovePoverty")) %>%
  dplyr::select(everything(), -moe, -variable, -NAME) %>%
  filter(!is.na(SEX)) %>%
  group_by(GEOID, SEX, agegroup, poverty) %>%
  dplyr::summarise(estimate = sum(estimate)) %>%
  mutate(per = estimate/ sum(estimate)) %>% # Calculating the % of the population In Poverty and Above Poverty
  dplyr::select(-estimate) %>%
  ungroup() %>%
  pivot_wider(names_from = poverty,
              values_from = per) %>%
  dplyr::select(everything(), TRACT = GEOID)

# Loading in the projection data. The Age groups are recategorized to be in the same format as the poverty age groups.
projections <- read_csv("./DATA-PROCESSED/DELIVERABLES/allproj.csv") %>%
  mutate(TRACT= substr(GEOID,1,11),
         agegroup = case_when(
           AGE %in% 1 ~ "0-4",
           AGE %in% c(2,3) ~ "5-14",
           AGE %in% c(4,5) ~ "15-24",
           AGE %in% c(6,7) ~ "25-34",
           AGE %in% c(8,9) ~ "35-44",
           AGE %in% c(10,11) ~ "45-54",
           AGE %in% c(12,13) ~ "55-64",
           AGE %in% c(14,15) ~ "65-74",
           AGE %in% c(16,17,18) ~ "75+"
         )) %>%
  group_by(YEAR, FIPS, GEOID, SEX, TRACT, agegroup) %>%
  dplyr::summarise(A = sum(A)) %>%
  left_join(., povdat_whitenh) %>%
  mutate(inpoverty = InPoverty * A,
         nopoverty = AbovePoverty * A) %>%
  group_by(YEAR, 
           FIPS, 
           TRACT, GEOID) %>%
  dplyr::summarise(InPoverty = sum(inpoverty),
                   Nopoverty = sum(nopoverty)) %>%
  mutate(povrate = InPoverty / (InPoverty + Nopoverty)) 

# write.csv(projections, "./DATA-PROCESSED/DELIVERABLES/projections_poverty.csv")