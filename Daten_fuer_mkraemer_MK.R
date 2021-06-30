library(haven)
library(tidyverse)
library(psych)


# Pfade
path <- '\\\\hume/rdc-gen/generations/soep-core/Gewichtung/ErsteWelle/SOEP_RKI'
# path_cons13 <- '\\\\hume/rdc-gen/consolidated/soep-core/soep.v36/consolidated13' # old
path_cons13 <- '\\\\hume/rdc-prod/complete/soep-core/soep.v36' # new
path_Gewichtung <- '\\\\hume/rdc-gen/generations/soep-core/Gewichtung'
path_arch <- '\\\\hume/rdc-arch/consolidate/soep-core/v37'

# Hochrechnungsfaktoren der vorangehenden Wellen
hhrf <- read_dta(file.path(path_Gewichtung, 'hrf2020', 'hhrf.dta'))
# not useful (?)
# Ränder für u.a. RKI-SOEP 
totals <- read_dta(file.path(path, 'Daten', 'raender_soeprki.dta'))
# not useful (?)

# Einsatzstichprobe für RKI-SOEP
einsatz <- read_sav('\\\\hume/rdc-arch/received/RKI/Brutto/Bruttoband_DIW_alle Tranchen_20210412_final.sav')
# final_neu %in% c(1,2) -> "vollständig" / "unvollständig" 
einsatz <- einsatz %>% 
  mutate(pid = as.numeric(PNRFEST),
         cid = as.numeric(urhh),
         hid = as.numeric(HNR))

# Nettodaten mit Befragungsinfos 
netto <- read_dta('\\\\hume/rdc-arch/consolidate/RKI/Aufbereitet/bkbiorki.dta')
# not useful (?)

# Corona-Fallzahlen nach Kreisen
covid <- read_dta(file.path(path, 'Daten', 'Fallzahlen_Corona.dta'))
covid <- covid %>% mutate(kkz = as.numeric(RS))
covid %>% summarise(n=n(), N = n_distinct(kkz)) # 401 kkz
# we need one line per kkz for the merge = one date
# start of the field period is 2020-10-02 -> pick last date from Sept.
covid <- covid %>% filter(Datum <= "2020-09-29")
covid <- covid %>% group_by(kkz) %>% slice_max(Datum) %>% ungroup()
# still 401 kkz -> I guess everyone has every date
summary(covid$Datum)

# Infektionscluster des RKI
cluster <- read.table(file.path(path, 'Daten', 'RKI', 'pamWard_Cluster4.csv'),
                      header = TRUE, sep = ';')

# Dummies für die Modellierung der Teilnahme auf HH-Ebene
bleib <- read_dta(file.path(path, "Daten", "bleib.dta"), encoding = "latin1") 

# Infos aus PGEN, HGEN, PL für die Modellierung der Teilnahme auf P-Ebene
pgen <- read_dta(file.path(path_cons13,'pgen.dta'))
pgen <- pgen %>% 
  arrange(pid, syear) %>% 
  filter(!duplicated(pid, fromLast = TRUE)) %>% 
  select(pid, pgnation, pgfamstd, pgemplst, pgoeffd, pgcasmin, pgpsbil) %>% 
  mutate(pg_nat_ger = ifelse(pgnation==1, 1, 0),
         pg_nat_other = ifelse(pgnation!=1, 1, 0),
         pg_fam_mar = ifelse(pgfamstd %in% c(1,7), 1, 0),
         pg_fam_sep = ifelse(pgfamstd %in% c(2,8), 1, 0),
         pg_fam_single = ifelse(pgfamstd==3, 1, 0),
         pg_fam_div = ifelse(pgfamstd==4, 1, 0),
         pg_fam_wid = ifelse(pgfamstd==4, 1, 0),
         pg_empl_full = ifelse(pgemplst==1, 1, 0),
         pg_empl_part = ifelse(pgemplst==2, 1, 0),
         pg_empl_train = ifelse(pgemplst==3, 1, 0),
         pg_empl_marg = ifelse(pgemplst==4, 1, 0),
         pg_empl_unem = ifelse(pgemplst==5, 1, 0),
         pg_empl_disab = ifelse(pgemplst==6, 1, 0),
         pg_oeffd = ifelse(pgoeffd==1, 1, 0),
         pg_edu_school = ifelse(pgcasmin==0 & !is.na(pgcasmin), 1, 0), 
         pg_edu_low = ifelse(pgcasmin %in% c(1,2,4), 1, 0), 
         pg_edu_mid = ifelse(pgcasmin %in% c(3,5,6,7), 1, 0), 
         pg_edu_high = ifelse(pgcasmin %in% c(8,9), 1, 0), 
         pg_edu_na = ifelse(pgcasmin %in% c(-1,-7), 1, 0), 
         pg_deg_sec = ifelse(pgpsbil==1, 1, 0),
         pg_deg_int = ifelse(pgpsbil==2, 1, 0),
         pg_deg_tech = ifelse(pgpsbil==3, 1, 0),
         pg_deg_abi = ifelse(pgpsbil==4, 1, 0),
         pg_deg_other = ifelse(pgpsbil==5, 1, 0),
         pg_deg_no = ifelse(pgpsbil %in% c(6,8), 1, 0),
         pg_deg_notyet = ifelse(pgpsbil==7, 1, 0)) %>% 
  select(-c(pgnation, pgfamstd, pgemplst, pgoeffd, pgcasmin, pgpsbil))


hgen <- read_dta(file.path(path_cons13,'hgen.dta'))
hgen <- hgen %>% 
  arrange(hid, syear) %>% 
  filter(!duplicated(hid, fromLast = TRUE)) %>% 
  select(hid, starts_with('hgeqp'), starts_with('hgtyp')) %>% 
  mutate(
    hg_air = ifelse(hgeqpair==1, 1, 0),
    hg_alm = ifelse(hgeqpalm==1, 1, 0),
    hg_bas = ifelse(hgeqpbas==1, 1, 0),
    hg_fhea = ifelse(hgeqpfhea==1, 1, 0),
    hg_gar = ifelse(hgeqpgar==1, 1, 0),
    hg_hea = ifelse(hgeqphea==1, 1, 0),
    hg_insul = ifelse(hgeqpinsul==1, 1, 0),
    hg_iwc = ifelse(hgeqpiwc==1, 1, 0),
    hg_kit = ifelse(hgeqpkit==1, 1, 0),
    hg_lif = ifelse(hgeqplif==1, 1, 0),
    hg_mglass = ifelse(hgeqpmglass==1, 1, 0),
    hg_nobar = ifelse(hgeqpnobar==1, 1, 0),
    hg_nrj = ifelse(hgeqpnrj==1, 1, 0),
    hg_park = ifelse(hgeqppark==1, 1, 0),
    hg_shw = ifelse(hgeqpshw==1, 1, 0),
    hg_sol = ifelse(hgeqpsol==1, 1, 0),
    hg_tel = ifelse(hgeqptel==1, 1, 0),
    hg_ter = ifelse(hgeqpter==1, 1, 0),
    hg_wat = ifelse(hgeqpwat==1, 1, 0),
    hg_typ1 = ifelse(hgtyp1hh==1, 1, 0),
    hg_typ2 = ifelse(hgtyp1hh==2, 1, 0),
    hg_typ3 = ifelse(hgtyp1hh==3, 1, 0),
    hg_typ4 = ifelse(hgtyp1hh==4, 1, 0),
    hg_typ5 = ifelse(hgtyp1hh==5, 1, 0),
    hg_typ6 = ifelse(hgtyp1hh==6, 1, 0),
    hg_typ7 = ifelse(hgtyp1hh==7, 1, 0),
    hg_typ8 = ifelse(hgtyp1hh==8, 1, 0)
  ) %>% 
  select(hid, starts_with("hg_"))

pl <- read_dta(file.path(path_cons13,'pl.dta'), 
               col_select = c('pid','syear',
                              'ple0008','plh0007','plh0171','plh0172', #'plb0022_h',
                              'plh0035','plh0182','ple0097','plh0011_h','plh0180',
                              #'plh0212','plh0216','plh0221','plh0224','plh0219', # draw big five from other source
                              #'plh0225','plh0217','plh0222','plh0226','plh0218',
                              #'plh0223','plh0255','plh0220',
                              'plh0244')) %>% 
  mutate(
    p_health1 = ifelse(ple0008==1, 1, 0), # 2 = ref. cat. 
    p_health3 = ifelse(ple0008==3, 1, 0),
    p_health4 = ifelse(ple0008==4, 1, 0),
    p_health5 = ifelse(ple0008==5, 1, 0),
    p_politic1 = ifelse(plh0007==1, 1, 0), # 3 = ref. cat. 
    p_politic2 = ifelse(plh0007==2, 1, 0), 
    p_politic4 = ifelse(plh0007==4, 1, 0), 
    p_healthlow = ifelse(plh0171 %in% c(0:3), 1, 0), 
    p_sleeplow = ifelse(plh0172 %in% c(0:3), 1, 0),
    p_helwor1 = ifelse(plh0035==1, 1, 0), # 2 = ref. cat. 
    p_helwor3 = ifelse(plh0035==3, 1, 0), 
    p_lifelow = ifelse(plh0182 %in% c(0:3), 1, 0),
    p_hipriv = ifelse(ple0097==2, 1, 0), 
    p_partypref = ifelse(plh0011_h==1, 1, 0),
    p_familylow = ifelse(plh0180 %in% c(0:3), 1, 0),
    p_future1 = ifelse(plh0244==1, 1, 0),  # 2 = ref. cat. 
    p_future3 = ifelse(plh0244==3, 1, 0),
    p_future4 = ifelse(plh0244==4, 1, 0),
  ) %>% 
  arrange(pid, syear) %>% 
  filter(!duplicated(pid, fromLast = TRUE)) %>% 
  select(pid, starts_with("p_"))


soep_big5 <- haven::read_dta("data/bf5-soep-core-is.dta")
names(soep_big5)

# this dataset contains every respondent who has ever filled out at least one Big Five item (validly)
soep_big5 %>% group_by(core, syear) %>% summarise(obs=n())

# score Big Five (copied from 'covid-pers-merge.R')

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
soep_big5 <- soep_big5 %>% select(pid, open, cons, extra, agree, neuro)


bleib_last <- bleib %>% 
  arrange(hid, syear) %>% 
  filter(!duplicated(hid, fromLast = TRUE)) %>% 
  select(-syear, -cid)

get_kkz <- read_dta(file.path(path_cons13,'regionl.dta'))
get_kkz <- get_kkz %>% 
  arrange(cid, syear) %>% 
  filter(!duplicated(cid, fromLast = TRUE)) %>% 
  mutate(kkz = ifelse(kkz<0 & kkz_rek90>0, kkz_rek90, kkz)) %>% 
  select(cid, kkz) %>% 
  filter(kkz>0)

# filter out soep-is
in_soepis <- read_dta("S:/DATA2/SOEP-IS/SOEP-IS 2019 Generierung HiWi/Data/finaldata/DE/p.dta")
in_soepis <- in_soepis %>% group_by(pid) %>% slice_max(syear) %>% ungroup() %>% 
  select(pid) %>% mutate(in_is = 1)

einsatz <- left_join(einsatz, in_soepis, by="pid")
einsatz <- einsatz %>% filter(is.na(in_is)) %>% select(-in_is)
einsatz <- einsatz %>% select(pid, cid, hid, ev_gesamt, dbs_gesamt, pcr_gesamt)

# merge
covmerged <- left_join(einsatz, pl, by="pid")
# 1128 NAs in pl variables
covmerged <- covmerged %>% mutate(missing_pl = ifelse(is.na(p_lifelow), 1, 0)) %>% 
  mutate_at(vars(starts_with("p_")), funs(replace_na(., 0)))

covmerged <- left_join(covmerged, pgen, by="pid")
# 829 NAs in pgen variables
covmerged <- covmerged %>% mutate(missing_pgen = ifelse(is.na(pg_fam_mar), 1, 0)) %>% 
  mutate_at(vars(starts_with("pg_")), funs(replace_na(., 0)))

covmerged <- left_join(covmerged, hgen, by="hid")
# 499 NAs in hgen variables
covmerged <- covmerged %>% mutate(missing_hgen = ifelse(is.na(hg_gar), 1, 0)) %>% 
  mutate_at(vars(starts_with("hg_")), funs(replace_na(., 0)))

covmerged <- left_join(covmerged, bleib_last, by="hid")
# 376 NAs in bleib variables
covmerged <- covmerged %>% mutate(missing_bleib = ifelse(is.na(single), 1, 0)) %>% 
  mutate_at(vars(-c(1:80)), funs(replace_na(., 0)))

covmerged <- covmerged %>% select(-c(offen, gewiss, extra, neuro, vertraeg))
covmerged <- left_join(covmerged, soep_big5, by="pid")
# 1510-1515 NAs in Big5 variables
covmerged <- covmerged %>% mutate(missing_big5 = ifelse(is.na(open), 1, 0)) %>% 
  mutate_at(vars(c(agree, cons, neuro, extra, open)), funs(replace_na(., 0)))

# merge via kkz
covmerged <- left_join(covmerged, get_kkz, by="cid")
# 16 NAs in kkz
covmerged <- left_join(covmerged, covid, by="kkz") 
# 1126 NAs in covid variables
covmerged <- covmerged %>% mutate(missing_covid = ifelse(is.na(Inzidenz), 1, 0)) %>% 
  mutate_at(vars(c(Infizierte_kumuliert, Inzidenz)), funs(replace_na(., 0)))

covmerged <- left_join(covmerged, cluster, by="kkz")
# 1126 NAs in cluster variables
covmerged <- covmerged %>% mutate(missing_cluster = ifelse(is.na(pamWardCluster4), 1, 0)) %>% 
  mutate_at(vars(pamWardCluster4), funs(replace_na(., 0)))


covmerged <- covmerged %>% select(-c(email, sample1, adresse, antwort, stichp, 
                                     pershv, hstamm),
                                  -c(MeldeLandkreis, RS, kkz, Datum, X)) # pamWardCluster4 ??

# save for later use
save(covmerged, file = "data/covmerged.rda")


