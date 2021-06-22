
# Pfade
path <- '\\\\hume/rdc-gen/generations/soep-core/Gewichtung/ErsteWelle/SOEP_RKI'
# path_cons13 <- '\\\\hume/rdc-gen/consolidated/soep-core/soep.v36/consolidated13' # old
path_cons13 <<- '/media/rdc-prod/complete/soep-core/soep.v36' # new
path_Gewichtung <<- '\\\\hume/rdc-gen/generations/soep-core/Gewichtung'
path_arch   <<- '\\\\hume/rdc-arch/consolidate/soep-core/v37'

# Hochrechnungsfaktoren der vorangehenden Wellen
HHRF <- read_dta(file.path(path_Gewichtung, 'hrf2020', 'hhrf.dta'))
# Ränder für u.a. RKI-SOEP 
TOTALS <- read_dta(file.path(path, 'Daten', 'raender_soeprki.dta'))

# Einsatzstichprobe für RKI-SOEP
EINSATZ <- read_sav('\\\\hume/rdc-arch/received/RKI/Brutto/Bruttoband_DIW_alle Tranchen_20210412_final.sav')
# Nettodaten mit Befragungsinfos 
Netto <- read_dta('\\\\hume/rdc-arch/consolidate/RKI/Aufbereitet/bkbiorki.dta')

# Corona-Fallzahlen nach Kreisen
COV <- read_dta(file.path(path, 'Daten', 'Fallzahlen_Corona.dta'))
# Infektionscluster des RKI
Cluster <- read.table(file.path(path, 'Daten', 'RKI', 'pamWard_Cluster4.csv'),
                      header = TRUE, sep = ';')

# Dummies für die Modellierung der Teilnahme auf HH-Ebene
Bleib <- read_dta(file.path(path, "Daten", "bleib.dta"), encoding = "latin1") 

# Infos aus PGEN, HGEN, PL für die Modellierung der Teilnahme auf P-Ebene
PGEN <- read_dta(file.path(path_cons13,'pgen.dta'))
PGEN <- PGEN %>% 
  arrange(pid, syear) %>% 
  filter(!duplicated(pid, fromLast = TRUE)) %>% 
  select(pid, pgnation, pgfamstd, pgemplst, pgoeffd, pgcasmin, pgpsbil)


HGEN <- read_dta(file.path(path_cons13,'hgen.dta'))
HGEN <- HGEN %>% 
  arrange(hid, syear) %>% 
  filter(!duplicated(hid, fromLast = TRUE)) %>% 
  select(hid, starts_with('hgeqp'), starts_with('hgtyp'))

PL <- read_dta(file.path(path_cons13,'pl.dta'), 
               col_select = c('pid','syear',
                              'plb0022_h','ple0008','plh0007','plh0171','plh0172',
                              'plh0035','plh0182','ple0097','plh0011_h','plh0180',
                              'plh0212','plh0216','plh0221','plh0224','plh0219',
                              'plh0225','plh0217','plh0222','plh0226','plh0218',
                              'plh0223','plh0255','plh0220','plh0244'))


