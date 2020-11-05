source('R/SCRIPTS/000-Libraries.R')      # loading in the libraries
library(mapview)

K05_pop <- readRDS("./R/DATA-PROCESSED/population_data.RDS")
K05_pop$GEOID <- paste0(substr(K05_pop$GISJOIN,2,3),
                        substr(K05_pop$GISJOIN,5,7),
                        K05_pop$TRACTA, K05_pop$BLCK_GRPA)
K05_pop$COUNTYRACE <- paste0(K05_pop$GEOID, "_", K05_pop$RACE)
K05_pop$FIPS <- substr(K05_pop$GEOID,1,5)
K05_pop <- K05_pop %>%
  group_by(YEAR, RACE, SEX, AGE, GEOID, FIPS) %>%
  dplyr::summarise(A = sum(Pop),
                   B = sum(Pop),
                   C = sum(Pop))

blkgrps_la <- block_groups("22", cb = TRUE)
blkgrps_ms <- block_groups("28", cb = TRUE)
blkgrps <- rbind(blkgrps_la, blkgrps_ms)
proj <- read_csv("./R/DATA-PROCESSED/DELIVERABLES/allproj.csv")  %>%
  rbind(., K05_pop)

tots_blkgrp <- proj %>%
  group_by(YEAR, GEOID) %>%
  dplyr::summarise(A = sum(A),
                   B = sum(B),
                   C = sum(C))

# mapdat <- right_join(blkgrps, tots_blkgrp)
# class(mapdat)
# mapdat2070 <- mapdat %>% filter(YEAR == 2070) %>% ungroup()
# mapview(mapdat2070,  zcol = "change_A",
#         at = seq(-2000, 22000, 2000), legend = TRUE,
#         col.regions=brewer.pal(9, "YlGnBu"))

covmatrix <- tots_blkgrp  %>% dplyr::select(GEOID, YEAR, A) %>%
  filter(YEAR >= 2020) %>%
  pivot_wider(., names_from = GEOID, values_from = A) %>%
  tibble::column_to_rownames('YEAR')
request <- cov(covmatrix)
write.csv(request, "./R/DATA-PROCESSED/DELIVERABLES/covariance_matrix.csv")
write.csv(tots_blkgrp, "./R/DATA-PROCESSED/DELIVERABLES/totals_file.csv")
tots1 <- tots_blkgrp %>%
  # filter(SEX == 2) %>%
  group_by(GEOID, AGE) %>%
  mutate(change_A = A - A[YEAR ==2020])

ggplot(tots1, 
       aes(x= YEAR, y = change_A, group = GEOID)) +
  geom_line()