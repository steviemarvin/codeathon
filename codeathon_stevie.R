library(tidyverse)
library(stats)
library(haven)
library(here)
library(writexl)


pulse44_raw <- read_csv(here("pulse2022_puf_45.csv"))
#changes them to lowercase
names(pulse44_raw) <- tolower(names(pulse44_raw))


pulse44_clean <- pulse44_raw %>% 
  #keeping only the variables I need
  select(scram, rrace, rhispanic, tbirth_year, eeduc, thhld_numkid, anxious, worry, interest, curfoodsuf, 
         foodrsnrv1, freefood, tenure, mortconf, down, hlthins4, snap_yn, mortconf, ) %>% 
  rename(educ = eeduc,
         num_child = thhld_numkid,
         medicaid = hlthins4,
         race = rrace,
         hispanic = rhispanic)

#refer to PREE codeathon doc for details, but cleaning up the -99 and -88 variables
pulse44_clean[pulse44_clean == -88] <- NA
pulse44_clean[pulse44_clean < 0] <- 0


pulse44_clean <- pulse44_clean %>% 
  mutate(new_educ = case_when(educ == 1 ~ 1,
                              educ == 2 ~ 1,
                              educ == 3 ~ 2,
                              educ == 4 ~ 3,
                              educ == 5 ~ 3,
                              educ == 6 ~ 4,
                              educ == 7 ~ 4),
         new_educ = labelled(new_educ, c("Less than a high school diploma" = 1, "High school diploma or GED" = 2, 
                                         "Some college/Associate's degree" = 3, "Bachelor's degree or higher" = 4)),
         new_race = case_when(race == 1 & hispanic == 1 ~ 1,
                              race == 2 & hispanic == 1 ~ 2,
                              race == 3 & hispanic == 1 ~ 3,
                              race == 4 & hispanic == 1 ~ 4,
                              TRUE ~ 5),
         new_race = labelled(new_race, c("White" = 1, "Black" = 2, "Asian" = 3, "Other/mixed race" = 4, "Hispanic, any race" = 5)),
         food_insec = case_when(curfoodsuf == 1 ~ 0,
                                curfoodsuf > 1 & foodrsnrv1 == 1 ~ 1,
                                freefood == 1 ~ 1,
                                # marking as 2 because can't put them in 0 yet,,,,
                                TRUE ~ 2),
         food_insec = labelled(food_insec, c("Food insecure" = 1, "Food secure" = 0)),
         housing_conf = case_when(tenure == 1 ~ 1,
                                  tenure == 4 ~ 1,
                                  mortconf == 1 | mortconf == 2 ~ 0,
                                  mortconf >= 3 ~ 1,
                                  # marking as 2 because can't put them in 0 yet,,,,
                                  TRUE ~ 2),
         housing_conf = labelled(housing_conf, c("Confident in payment" = 1, "Not confident in payment" = 0)),
         # 1) not at all; 2) several days ; 3) more than half the days; 4) nearly everyday
         depr_anxi = case_when(anxious > 1 ~ 1,
                               worry > 1 ~ 1,
                               interest > 1 ~ 1,
                               down > 1 ~ 1, 
                               TRUE ~ 0),
         fam_child = if_else(num_child > 0, 1, 0),
         fam_child = labelled(fam_child, c("Child present" = 1, "Child no present" = 0)), 
         mutate(across(new_educ | food_insec | housing_conf | new_race | fam_child, ~ as.character(as_factor(.x)))))

pulse44_sum_fun <- function(i){
  pulse44_clean %>% 
    group_by({{i}}, fam_child, new_race) %>% 
    summarize(
      depr_anxi = mean(depr_anxi, na.rm = TRUE)
    )
}

pulse44_educ <- pulse44_sum_fun(new_educ)
pulse44_food <- pulse44_sum_fun(food_insec)
pulse44_housing <- pulse44_sum_fun(housing_conf)


write_xlsx(pulse44_educ, here("pulse45_educ.xlsx"))
write_xlsx(pulse44_food, here("pulse45_food.xlsx"))
write_xlsx(pulse44_housing, here("pulse45_housing.xlsx"))
