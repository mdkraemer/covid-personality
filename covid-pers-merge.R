# packages
library(tidyverse)
library(haven)
library(psych)

#### load data ####

soep_rki <- haven::read_dta("data/_rkisoep_big5_pre_11062021.dta")
names(soep_rki)

soep_big5 <- haven::read_dta("data/bf5-soep-core-is.dta")
names(soep_big5)

# this dataset contains every respondent who has ever filled out at least one Big Five item (validly)
soep_big5 %>% group_by(core, syear) %>% summarise(obs=n())

#### score Big Five ####

# Big Five: openness to experience
alpha_open_soep <- soep_big5 %>% 
  select(starts_with("open")) %>%
  psych::alpha(check.keys = TRUE)
soep_big5$open <- soep_big5 %>%
  select(starts_with("open")) %>%
  reverse.code(keys=alpha_open_soep$keys, items = .) %>%
  rowMeans(na.rm=T)

# Big Five: conscientiousness
alpha_cons_soep <- soep_big5 %>% 
  select(starts_with("cons")) %>%
  psych::alpha(check.keys = TRUE)
soep_big5$cons <- soep_big5 %>%
  select(starts_with("cons")) %>%
  reverse.code(keys=alpha_cons_soep$keys, items = .) %>%
  rowMeans(na.rm=T)

# Big Five: extraversion
alpha_extra_soep <- soep_big5 %>% 
  select(starts_with("extra")) %>%
  psych::alpha(check.keys = TRUE)
soep_big5$extra <- soep_big5 %>%
  select(starts_with("extra")) %>%
  reverse.code(keys=alpha_extra_soep$keys, items = .) %>%
  rowMeans(na.rm=T)

# Big Five: agreeableness
alpha_agree_soep <- soep_big5 %>% 
  select(starts_with("agree")) %>%
  psych::alpha(check.keys = TRUE)
soep_big5$agree <- soep_big5 %>%
  select(starts_with("agree")) %>%
  reverse.code(keys=alpha_agree_soep$keys, items = .) %>%
  rowMeans(na.rm=T)

# Big Five: neuroticism
alpha_neuro_soep <- soep_big5 %>% 
  select(starts_with("neuro")) %>%
  psych::alpha(check.keys = TRUE)
soep_big5$neuro <- soep_big5 %>%
  select(starts_with("neuro")) %>%
  reverse.code(keys=alpha_neuro_soep$keys, items = .) %>%
  rowMeans(na.rm=T)

# fill in longitudinal missings (in 2019) from earlier waves 
soep_big5 <- soep_big5 %>% 
  mutate_at(.vars = vars(c(open, cons, extra, agree, neuro)), 
            ~ifelse(is.nan(.), NA, .)) # these are 'NaN' for some reason

# percentage missing in 2019
soep_big5 %>% group_by(syear) %>% summarize(pct_miss_open = sum(is.na(open)) / n(),
                                            pct_miss_cons = sum(is.na(cons)) / n(),
                                            pct_miss_extra= sum(is.na(extra)) / n(),
                                            pct_miss_agree = sum(is.na(agree)) / n(),
                                            pct_miss_neuro = sum(is.na(neuro)) / n())
# use fill() to replace missing values for Big FIve variables 
# https://tidyr.tidyverse.org/reference/fill.html
# (only really useful in our case if one domain was completely missing in one year, but the others not)
soep_big5 <- soep_big5 %>% group_by(pid) %>% 
  fill(c(open, cons, extra, agree, neuro), .direction = "down") %>% ungroup()

# in addition to filling in, we also need to take each respondent's row with the maximum value 
# (some respondents are no longer present in 2019 data, but are in 2020 SOEP-RKI data)
soep_big5 <- soep_big5 %>% group_by(pid) %>% slice_max(syear) %>% ungroup() %>% rename(lastwave = syear)
soep_big5 %>% group_by(lastwave) %>% summarise(obs = n())
soep_big5 <- soep_big5 %>% select(pid, open, cons, extra, agree, neuro, core, lastwave)

#### merge data ####

covid_pers <- left_join(soep_rki, soep_big5)
summary(covid_pers)
save(covid_pers, file = "data/covid_pers.rda")