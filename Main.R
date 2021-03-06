# Load required libs
library(config)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# Create dirs (dir.create() does not crash when dir already exists)
dir.create(here::here("Data"), showWarnings = FALSE)
dir.create(here::here("Reports"), showWarnings = FALSE)
dir.create(here::here("SQL"), showWarnings = FALSE)

# Create constants
# Set threshold for error relative frequency
freq_lim = 0.03

#########################################################################################
# Data Extraction #######################################################################
#########################################################################################

# Set JAVA_HOME, set max. memory, and load rJava library
java_version = config::get("java_version", file = "C:\\Users\\PoorJ\\Projects\\config.yml")
Sys.setenv(JAVA_HOME = java_version$JAVA_HOME)
options(java.parameters = "-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

# Get credentials
ablak <-
  config::get("ablak" , file = "C:\\Users\\PoorJ\\Projects\\config.yml")

datamnr <-
  config::get("datamnr" , file = "C:\\Users\\PoorJ\\Projects\\config.yml")


# Create connection driver
jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "C:\\Users\\PoorJ\\Desktop\\ojdbc7.jar")


# Open connection: ablak ----------------------------------------------------------------
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = ablak$server,
    user = ablak$uid,
    password = ablak$pwd
  )

# Fetch data
readQuery <-
  function(file)
    paste(readLines(file, warn = FALSE), collapse = "\n")

query_error_freq <-
  readQuery(here::here("SQL", "autouw_error_freq.sql"))
query_error_pattern <- "select * from t_kpm_error_pattern_history"
query_autouw <- "select * from t_kpm_history"
query_autouw_dict <- "select * from t_autouw_dict"

autouw <- dbGetQuery(jdbcConnection, query_autouw)
autouw_dict <- dbGetQuery(jdbcConnection, query_autouw_dict)
autouw_error_freq <- dbGetQuery(jdbcConnection, query_error_freq)
autouw_error_pattern <-
  dbGetQuery(jdbcConnection, query_error_pattern)

# Close db connection: ablak
dbDisconnect(jdbcConnection)


# Open connection: kontakt---------------------------------------------------------------
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = datamnr$server,
    user = datamnr$uid,
    password = datamnr$pwd
  )

# Fetch data
query_autouw_cost <- "select * from t_kpm_err_pattern_cost"
query_num_wdays <- "select * from t_mnap"

autouw_cost <- dbGetQuery(jdbcConnection, query_autouw_cost)
num_wdays <- dbGetQuery(jdbcConnection, query_num_wdays)

# Close db connection: kontakt
dbDisconnect(jdbcConnection)


#########################################################################################
# Analyze Attempt & Success Rates on a Monthly Basis ####################################
#########################################################################################

# Transformations
autouw_main <- autouw[!is.na(autouw$MODTYP), ]
autouw_main$IDOSZAK <-
  paste0(substr(autouw_main$IDOSZAK, 1, 4), "/", substr((autouw_main$IDOSZAK), 6, 7))

autouw_main <-  autouw_main %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lak�s",
                            TRUE ~ .$MODTYP))


# Compute autoUW main KPIs --------------------------------------------------------------
# Compute attempt rate
auw_attempt <-  autouw_main %>%
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "I") %>%
  select(IDOSZAK, KISERELT)


# Compute success rate within total
auw_success_total <-  autouw_main %>%
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_TELJES)


# Compute success rate within attempted
auw_success_attempt <-  autouw_main %>%
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_KISERELT)


# Merge KPI results
auw_main <- auw_attempt %>%
  left_join(auw_success_attempt, by = "IDOSZAK") %>%
  left_join(auw_success_total, by = "IDOSZAK") %>%
  gather(MUTATO, PCT,-IDOSZAK)


# Save for dashboard output
write.csv(auw_main,
          here::here("Data", "p2_auw_main.csv"),
          row.names = FALSE)


# Compute autoUW KPIs for each product line ---------------------------------------------
# Compute attempt rate for each product line
auw_prod_attempt <-  autouw_main %>%
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, MODTYP, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "N") %>%
  mutate(KISERELT = 1 - KISERELT) %>%
  select(IDOSZAK, MODTYP, KISERELT)


# Compute success rate for each product line within total
auw_prod_success_total <-   autouw_main %>%
  group_by(IDOSZAK, MODTYP, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  group_by(IDOSZAK, MODTYP, KPM = KPM == "Sikeres") %>%
  summarize(SIKER_PER_TELJES = sum(SIKER_PER_TELJES)) %>%
  filter(KPM == FALSE) %>%
  mutate(SIKER_PER_TELJES = 1 - SIKER_PER_TELJES) %>%
  select(IDOSZAK, MODTYP, SIKER_PER_TELJES)


# Compute success rate for each product line within attempted
auw_prod_success_attempt <-   autouw_main %>%
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, MODTYP, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, MODTYP, SIKER_PER_KISERELT)


# Merge KPI results for each product line
auw_prod <- auw_prod_attempt %>%
  left_join(auw_prod_success_attempt, by = c("IDOSZAK", "MODTYP")) %>%
  left_join(auw_prod_success_total, by = c("IDOSZAK", "MODTYP")) %>%
  gather(MUTATO, PCT,-IDOSZAK,-MODTYP)


# Save for dashboard output
write.csv(auw_prod,
          here::here("Data", "p2_auw_prod.csv"),
          row.names = FALSE)



# Compute autoUW KPIs for each product line  & media ------------------------------------
# Compute attempt rate for each product line & media
auw_prod_media_attempt <-  autouw_main %>%
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, MODTYP, PAPIR_TIPUS, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  ungroup() %>%
  tidyr::complete(IDOSZAK, MODTYP, PAPIR_TIPUS, ATTEMPT, fill = list(TOTAL = 0)) %>%
  group_by(IDOSZAK, MODTYP, PAPIR_TIPUS) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  mutate_all(funs(replace(., is.nan(.), 0))) %>%
  filter(ATTEMPT == "I") %>%
  select(IDOSZAK, MODTYP, PAPIR_TIPUS, KISERELT)


# Compute success rate for each product line & media within attempted
auw_prod_media_success_total <-  autouw_main %>%
  group_by(IDOSZAK, MODTYP, PAPIR_TIPUS, KPM) %>%
  summarise (SUBTOTAL = n()) %>%
  ungroup() %>%
  tidyr::complete(IDOSZAK, MODTYP, PAPIR_TIPUS, KPM, fill = list(SUBTOTAL = 0)) %>%
  group_by(IDOSZAK, MODTYP, PAPIR_TIPUS, KPM = KPM == "Sikeres") %>%
  ungroup() %>%
  group_by(IDOSZAK, MODTYP, PAPIR_TIPUS, KPM) %>%
  summarize(TOTAL = sum(SUBTOTAL)) %>%
  mutate(SIKER_PER_TOTAL = TOTAL / sum(TOTAL)) %>%
  mutate_all(funs(replace(., is.nan(.), 0))) %>%
  filter(KPM == 1) %>%
  select(IDOSZAK, MODTYP, PAPIR_TIPUS, SIKER_PER_TOTAL)


# Compute success rate for each product line & media within attempted
auw_prod_media_success_attemp <-  autouw_main %>%
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, MODTYP, PAPIR_TIPUS, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, MODTYP, PAPIR_TIPUS, SIKER_PER_KISERELT)


# Merge KPI results for each product line & media
auw_prod_media <- auw_prod_media_attempt %>%
  left_join(auw_prod_media_success_attemp,
            by = c("IDOSZAK", "MODTYP", "PAPIR_TIPUS")) %>%
  left_join(auw_prod_media_success_total,
            by = c("IDOSZAK", "MODTYP", "PAPIR_TIPUS")) %>%
  gather(MUTATO, PCT,-IDOSZAK,-MODTYP,-PAPIR_TIPUS)


# Save for dashboard output
write.csv(auw_prod_media,
          here::here("Data", "p2_auw_prod_media.csv"),
          row.names = FALSE)



# Compute autoUW KPIs for TPML per each contact type ------------------------------------
autouw_tpml <- autouw_main %>% filter(MODTYP == "GFB")

# Compute attempt rate for each product line & media
auw_tpml_ctype_attempt <-  autouw_tpml %>%
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, GFB_KOTES_NEV, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  ungroup() %>%
  tidyr::complete(IDOSZAK, GFB_KOTES_NEV, ATTEMPT, fill = list(TOTAL = 0)) %>%
  replace_na(list(GFB_KOTES_NEV = "Egy�b")) %>%
  group_by(IDOSZAK, GFB_KOTES_NEV) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  mutate_all(funs(replace(., is.nan(.), 0))) %>%
  filter(ATTEMPT == "I") %>%
  select(IDOSZAK, GFB_KOTES_NEV, KISERELT)


# Compute success rate for TPML per each kontact type
auw_tpml_ctype_success_total <-  autouw_tpml %>%
  group_by(IDOSZAK, GFB_KOTES_NEV, KPM) %>%
  summarise (SUBTOTAL = n()) %>%
  ungroup() %>%
  tidyr::complete(IDOSZAK, GFB_KOTES_NEV, KPM, fill = list(SUBTOTAL = 0)) %>%
  replace_na(list(GFB_KOTES_NEV = "Egy�b")) %>%
  group_by(IDOSZAK, GFB_KOTES_NEV, KPM = KPM == "Sikeres") %>%
  ungroup() %>%
  group_by(IDOSZAK, GFB_KOTES_NEV, KPM) %>%
  summarize(TOTAL = sum(SUBTOTAL)) %>%
  mutate(SIKER_PER_TOTAL = TOTAL / sum(TOTAL)) %>%
  mutate_all(funs(replace(., is.nan(.), 0))) %>%
  filter(KPM == 1) %>%
  select(IDOSZAK, GFB_KOTES_NEV, SIKER_PER_TOTAL)


# Compute success rate for TPML per each contact type
auw_tpml_ctype_success_attemp <-  autouw_tpml %>%
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, GFB_KOTES_NEV, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, GFB_KOTES_NEV, SIKER_PER_KISERELT)


# Merge KPI results for TPML per each contact type
auw_tpml_ctype <- auw_tpml_ctype_attempt %>%
  left_join(auw_tpml_ctype_success_total,
            by = c("IDOSZAK", "GFB_KOTES_NEV")) %>%
  left_join(auw_tpml_ctype_success_attemp,
            by = c("IDOSZAK", "GFB_KOTES_NEV")) %>%
  gather(MUTATO, PCT,-IDOSZAK,-GFB_KOTES_NEV)


# Save for dashboard output
write.csv(auw_tpml_ctype,
          here::here("Data", "p2_auw_tpml_ctype.csv"),
          row.names = FALSE)



#########################################################################################
# Analyze Error Type Frequencies  #######################################################
#########################################################################################

# Transformations
# Extract freqs from last 3 months to get valid understanding of present root-causes
autouw_error_freq <-
  autouw_error_freq[!is.na(autouw_error_freq$MODTYP), ]

autouw_error_freq <-  autouw_error_freq %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lak�s",
                            TRUE ~ .$MODTYP)) %>%
  left_join(autouw_dict, by = c("HIBAAZON"))

autouw_error_freq_last3 <-
  autouw_error_freq %>% filter(ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3))

autouw_error_freq_last3$IDOSZAK <-
  paste0(substr(autouw_error_freq_last3$IDOSZAK, 1, 4),
         "/",
         substr((autouw_error_freq_last3$IDOSZAK), 6, 7))

autouw_error_freq$IDOSZAK <-
  paste0(substr(autouw_error_freq$IDOSZAK, 1, 4), "/", substr((autouw_error_freq$IDOSZAK), 6, 7))


# Freq of errors for whole dataset
freq <- autouw_error_freq_last3 %>%
  group_by(HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  ungroup() %>%
  arrange(desc(TOTAL)) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL),
         GYAK_KUM = cumsum(GYAKORISAG)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(freq,
          here::here("Data", "p3_error_freq.csv"),
          row.names = FALSE)


# Freq of errors per prod line
freq_prod <- autouw_error_freq_last3 %>%
  group_by(MODTYP, HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(freq_prod,
          here::here("Data", "p3_error_freq_prod.csv"),
          row.names = FALSE)


# Freq time series for most common errors (of last 3 months) per prod line
# Most common errors extracted from last 3 months then track them backwards in time
most_common <- unique(freq_prod$HIBAAZON)
autouw_error_freq_mc <- autouw_error_freq %>%
  filter(HIBAAZON %in% most_common)

freq_prod_mc <- autouw_error_freq_mc %>%
  group_by(IDOSZAK, MODTYP, HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  arrange(IDOSZAK, MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(IDOSZAK, MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL))


# Save for dashboard output
write.csv(freq_prod_mc,
          here::here("Data", "p3_error_freq_prod_mc.csv"),
          row.names = FALSE)



#########################################################################################
# Analyze Error Pattern Frequencies  ####################################################
#########################################################################################

# Transformations
# Extract patterns from last 3 months to get valid understanding of present root-causes
autouw_error_pattern <-
  autouw_error_pattern[!is.na(autouw_error_pattern$MODTYP),]

autouw_error_pattern <-  autouw_error_pattern %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lak�s",
                            TRUE ~ .$MODTYP))

autouw_error_pattern_last3 <-
  autouw_error_pattern %>% filter(ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3))

autouw_error_pattern$IDOSZAK <-
  paste0(substr(autouw_error_pattern$IDOSZAK, 1, 4), "/", substr((autouw_error_pattern$IDOSZAK), 6, 7))

autouw_error_pattern_last3$IDOSZAK <-
  paste0(substr(autouw_error_pattern_last3$IDOSZAK, 1, 4),
         "/",
         substr((autouw_error_pattern_last3$IDOSZAK), 6, 7))


# Freq of patterns for whole dataset
freq_pattern <- autouw_error_pattern_last3 %>%
  group_by(HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  ungroup() %>%
  arrange(desc(TOTAL)) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL),
         GYAK_KUM = cumsum(GYAKORISAG)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(freq_pattern,
          here::here("Data", "p4_error_freq_pattern.csv"),
          row.names = FALSE)


# Freq of patterns per prod line
freq_pattern_prod <- autouw_error_pattern_last3 %>%
  group_by(MODTYP, HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(
  freq_pattern_prod,
  here::here("Data", "p4_error_freq_pattern_prod.csv"),
  row.names = FALSE
)


# Freq time series for most common patterns per prod line
most_common_pattern <- unique(freq_pattern_prod$HIBA_MINTA)
autouw_error_pattern_mc <- autouw_error_pattern %>%
  filter(HIBA_MINTA %in% most_common_pattern)

error_prod_mc <- autouw_error_pattern_mc %>%
  group_by(IDOSZAK, MODTYP, HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  arrange(IDOSZAK, MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(IDOSZAK, MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL))


# Save for dashboard output
write.csv(
  error_prod_mc,
  here::here("Data", "p4_error_freq_pattern_prod_mc.csv"),
  row.names = FALSE
)



#########################################################################################
# Analyze Error Pattern Costs  ##########################################################
#########################################################################################

# Transformations
autouw_cost <-
  autouw_cost[!is.na(autouw_cost$MODTYP),]

autouw_cost <-  autouw_cost %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lak�s",
                            TRUE ~ .$MODTYP))


# Compute monthly total cost -----------------------------------------------------------
cost_monthly <- autouw_cost %>%
  group_by(IDOSZAK) %>%
  summarize(HIBA_IDO = sum(IDO_PERC)) %>%
  ungroup() %>%
  left_join(num_wdays, by = c("IDOSZAK")) %>%
  mutate(FTE = HIBA_IDO / 60 / 7 / MNAP,
         IDOSZAK = paste0(substr(IDOSZAK, 1, 4), "/", substr((IDOSZAK), 6, 7))) %>%
  left_join(auw_main[auw_main$MUTATO == "SIKER_PER_TELJES",], by = c("IDOSZAK")) %>%
  rename(SIKER_PER_TELJES = PCT) %>%
  select(IDOSZAK, FTE, SIKER_PER_TELJES)


# Save for dashboard output
write.csv(cost_monthly,
          here::here("Data", "p1_cost_monthly.csv"),
          row.names = FALSE)


# Compute monthly total cost per prod line ----------------------------------------------
cost_prod_monthly <- autouw_cost %>%
  group_by(IDOSZAK, MODTYP) %>%
  summarize(HIBA_IDO = sum(IDO_PERC)) %>%
  ungroup() %>%
  left_join(num_wdays, by = c("IDOSZAK")) %>%
  mutate(FTE = HIBA_IDO / 60 / 7 / MNAP,
         IDOSZAK = paste0(substr(IDOSZAK, 1, 4), "/", substr((IDOSZAK), 6, 7))) %>%
  left_join(auw_prod[auw_prod$MUTATO == "SIKER_PER_TELJES",], by = c("IDOSZAK", "MODTYP")) %>%
  rename(SIKER_PER_TELJES = PCT) %>%
  select(IDOSZAK, MODTYP, FTE, SIKER_PER_TELJES)


# Save for dashboard output
write.csv(cost_prod_monthly,
          here::here("Data", "p5_cost_prod_monthly.csv"),
          row.names = FALSE)



# Compute monthly total cost per error pattern ------------------------------------------
# NOTE: normalized FTE computed for last 3 months: must make sure both FTE and N are computed
# for 3 months period
wdays_last3 <-
  num_wdays %>% filter(
    ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3) &
      ymd_hms(IDOSZAK) < floor_date(Sys.Date(), unit = "month")
  ) %>%
  select(MNAP) %>% sum

fte_last3 <- autouw_cost %>%
  filter(ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3)) %>%
  group_by(MODTYP, HIBA_MINTA) %>%
  summarize(HIBA_IDO = sum(IDO_PERC)) %>%
  ungroup() %>%
  mutate(FTE = HIBA_IDO / 60 / 7 / wdays_last3)

cost_pattern_last3 <-
  freq_pattern_prod %>% left_join(fte_last3, by = c("MODTYP", "HIBA_MINTA")) %>%
  mutate(FAJL_FTE = FTE / TOTAL * 1000) %>%
  select(MODTYP, HIBA_MINTA, GYAKORISAG, FTE, FAJL_FTE) %>%
  arrange(MODTYP, desc(FTE))


# Save for dashboard output
write.csv(cost_pattern_last3,
          here::here("Data", "p5_cost_pattern_last3.csv"),
          row.names = FALSE)



#########################################################################################
# LSZB-OKE ##############################################################################
#########################################################################################

# Compute autoUW main KPIs for Home per Product -----------------------------------------
# Compute attempt rate
home_prod_attempt <-  autouw_main %>%
  filter(MODTYP == "Lak�s" & MODKOD %in% c("21968", "21972")) %>% 
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, MODKOD, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "I") %>%
  select(IDOSZAK, MODKOD, KISERELT)


# Compute success rate within total
home_prod_success_total <-  autouw_main %>%
  filter(MODTYP == "Lak�s" & MODKOD %in% c("21968", "21972")) %>% 
  group_by(IDOSZAK, MODKOD, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, MODKOD, SIKER_PER_TELJES)


# Compute success rate within attempted
home_prod_success_attempt <-  autouw_main %>%
  filter(MODTYP == "Lak�s" & MODKOD %in% c("21968", "21972")) %>% 
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, MODKOD, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, MODKOD, SIKER_PER_KISERELT)


# Merge KPI results
home_prod_main <- home_prod_attempt %>%
  left_join(home_prod_success_attempt, by = c("IDOSZAK", "MODKOD")) %>%
  left_join(home_prod_success_total, c("IDOSZAK", "MODKOD")) %>%
  gather(MUTATO, PCT,-MODKOD, -IDOSZAK) %>% 
  ungroup()


# Save for dashboard output
write.csv(home_prod_main,
          here::here("Data", "home_prod_main.csv"),
          row.names = FALSE)


# Freq of errors per Home product --------------------------------------------------------
home_prod_error_freq <- autouw_error_freq_last3 %>%
  filter(MODTYP == "Lak�s" & MODKOD %in% c("21968", "21972")) %>% 
  group_by(MODKOD, HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODKOD, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODKOD) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(home_prod_error_freq,
          here::here("Data", "home_prod_error_freq.csv"),
          row.names = FALSE)


# Freq of patterns per Home prods---------------------------------------------------------
home_freq_pattern_prod <- autouw_error_pattern_last3 %>%
  filter(MODTYP == "Lak�s" & MODKOD %in% c("21968", "21972")) %>% 
  group_by(MODKOD, HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODKOD, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODKOD) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= freq_lim)


# Save for dashboard output
write.csv(
  home_freq_pattern_prod,
  here::here("Data", "home_prod_freq_pattern.csv"),
  row.names = FALSE
)