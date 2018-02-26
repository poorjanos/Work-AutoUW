# Load required libs
library(config)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)


#########################################################################################
# Data Extraction #######################################################################
#########################################################################################

# Set JAVA_HOME, set max. memory, and load rJava library
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_60")
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

kontakt <-
  config::get("kontakt" , file = "C:\\Users\\PoorJ\\Projects\\config.yml")


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

query_error_freq <- readQuery(here::here("SQL", "autouw_error_freq.sql"))
query_error_pattern <- "select * from t_kpm_error_pattern_history"
query_autouw <- "select * from t_kpm_history"
query_autouw_dict <- "select * from t_autouw_dict"

autouw <- dbGetQuery(jdbcConnection, query_autouw)
autouw_dict <- dbGetQuery(jdbcConnection, query_autouw_dict)
autouw_error_freq <- dbGetQuery(jdbcConnection, query_error_freq)
autouw_error_pattern <- dbGetQuery(jdbcConnection, query_error_pattern)

# Close db connection: ablak
dbDisconnect(jdbcConnection)


# Open connection: kontakt---------------------------------------------------------------
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = kontakt$server,
    user = kontakt$uid,
    password = kontakt$pwd
  )

# Fetch data
query_autouw_cost <- "select * from t_kpm_err_pattern_cost_history"
query_num_wdays <- "select * from t_mnap"

autouw_cost <- dbGetQuery(jdbcConnection, query_autouw_cost)
num_wdays <- dbGetQuery(jdbcConnection, query_num_wdays)

# Close db connection: kontakt
dbDisconnect(jdbcConnection)


#########################################################################################
# Analyze Attempt & Success Rates on a Monthly Basis ####################################
#########################################################################################

# Transformations
autouw_main <- autouw[!is.na(autouw$MODTYP),]
autouw_main$IDOSZAK <-
  paste0(substr(autouw_main$IDOSZAK, 1, 4), "/", substr((autouw_main$IDOSZAK), 6, 7))

autouw_main <-  autouw_main %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
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
  gather(MUTATO, PCT, -IDOSZAK)


# Plot main metrics
ggplot(auw_main, aes(IDOSZAK, PCT, group = MUTATO, colour = MUTATO)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 15) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Százlékos arány",
       x = "Hónap",
       colour = "Mérõszám")




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
  gather(MUTATO, PCT, -IDOSZAK, -MODTYP)


# Plot product line metrics
ggplot(auw_prod, aes(IDOSZAK, PCT, group = MUTATO, colour = MUTATO)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 15) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(. ~ MODTYP) +
  labs(y = "Százlékos arány",
       x = "Hónap",
       colour = "Mérõszám")



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
  gather(MUTATO, PCT, -IDOSZAK, -MODTYP, -PAPIR_TIPUS)


# Plot product line & media metrics
ggplot(auw_prod_media, aes(IDOSZAK, PCT, group = MUTATO, colour = MUTATO)) +
  geom_line(size = 1) +
  geom_point(size = 1.5, shape = 15) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  facet_grid(MODTYP ~ PAPIR_TIPUS) +
  labs(y = "Százlékos arány",
       x = "Hónap",
       colour = "Mérõszám") +
  geom_jitter()


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
  replace_na(list(GFB_KOTES_NEV = "Egyéb")) %>% 
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
  replace_na(list(GFB_KOTES_NEV = "Egyéb")) %>% 
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
  gather(MUTATO, PCT, -IDOSZAK, -GFB_KOTES_NEV)


# Plot TPML per each contact type metrics
ggplot(auw_tpml_ctype, aes(IDOSZAK, PCT, group = MUTATO, colour = MUTATO)) +
  geom_line(size = 1) +
  geom_point(size = 1.5, shape = 15) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  facet_grid(. ~ GFB_KOTES_NEV) +
  labs(y = "Százlékos arány",
       x = "Hónap",
       colour = "Mérõszám") +
  geom_jitter()


#########################################################################################
# Analyze Error Type Frequencies  #######################################################
#########################################################################################

# Transformations
# Extract freqs from last 3 months to get valid understanding of present root-causes
autouw_error_freq <-
  autouw_error_freq[!is.na(autouw_error_freq$MODTYP),]

autouw_error_freq <-  autouw_error_freq %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP)) %>%
  left_join(autouw_dict, by = c("HIBAAZON"))

autouw_error_freq_last3 <-
  autouw_error_freq %>% filter(ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3))

autouw_error_freq_last3$IDOSZAK <-
  paste0(substr(autouw_error_freq_last3$IDOSZAK, 1, 4), "/", substr((autouw_error_freq_last3$IDOSZAK), 6, 7))

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
  filter(GYAKORISAG >= 0.01)

ggplot(freq, aes(x = factor(freq$HIBA, levels = freq$HIBA[order(freq$GYAKORISAG)]), y = GYAKORISAG)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(label = percent) +
  #theme_minimal() +
  coord_flip() +
  labs(y = "Relatív gyakoriság",
       x = "Hiba")


# Freq of errors per prod line
freq_prod <- autouw_error_freq_last3 %>%
  group_by(MODTYP, HIBAAZON, HIBA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= 0.01)

ggplot(freq_prod, aes(
  x = factor(freq_prod$HIBA, levels = freq_prod$HIBA[order(freq_prod$GYAKORISAG)]),
  y = GYAKORISAG
)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(label = percent) +
  #theme_minimal() +
  coord_flip() +
  labs(y = "Relatív gyakoriság",
       x = "Hiba") +
  
  facet_grid(. ~ MODTYP)


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


ggplot(freq_prod_mc[freq_prod_mc$MODTYP == "GFB",],
       aes(
         x = IDOSZAK,
         y = GYAKORISAG,
         group = 1
       )) +
  geom_line(size = 1) +
  geom_point(size = 1, shape = 15) +
  scale_y_continuous(label = percent) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  facet_wrap(~HIBA, ncol = 8, labeller = label_wrap_gen(width=20))


ggplot(freq_prod_mc[freq_prod_mc$MODTYP == "Lakás",],
       aes(
         x = IDOSZAK,
         y = GYAKORISAG,
         group = 1
       )) +
  geom_line(size = 1) +
  geom_point(size = 1, shape = 15) +
  scale_y_continuous(label = percent) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  facet_wrap(~HIBA, ncol = 8, labeller = label_wrap_gen(width=20))



#########################################################################################
# Analyze Error Pattern Frequencies  ####################################################
#########################################################################################

# Transformations
# Extract patterns from last 3 months to get valid understanding of present root-causes
autouw_error_pattern <-
  autouw_error_pattern[!is.na(autouw_error_pattern$MODTYP), ]

autouw_error_pattern <-  autouw_error_pattern %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP))

autouw_error_pattern_last3 <-
  autouw_error_pattern %>% filter(ymd_hms(IDOSZAK) >= floor_date(Sys.Date(), unit = "month") - months(3))

autouw_error_pattern$IDOSZAK <-
  paste0(substr(autouw_error_pattern$IDOSZAK, 1, 4), "/", substr((autouw_error_pattern$IDOSZAK), 6, 7))

autouw_error_pattern_last3$IDOSZAK <-
  paste0(substr(autouw_error_pattern_last3$IDOSZAK, 1, 4),
         "/",
         substr((autouw_error_pattern_last3$IDOSZAK), 6, 7))


# Freq of patterns per prod line
freq_pattern <- autouw_error_pattern_last3 %>%
  group_by(MODTYP, HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  arrange(MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL)) %>%
  filter(GYAKORISAG >= 0.01)


ggplot(freq_pattern, aes(
  x = factor(freq_pattern$HIBA_MINTA, levels = freq_pattern$HIBA_MINTA[order(freq_pattern$GYAKORISAG)]),
  y = GYAKORISAG
)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(label = percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 140)) +
  theme(axis.text.y = element_text(size = 7.5)) +
  #theme_minimal() +
  coord_flip() +
  labs(y = "Relatív gyakoriság",
       x = "Hiba") +
  facet_grid(. ~ MODTYP)


# Freq time series for most common patterns per prod line
most_common_pattern <- unique(freq_pattern$HIBA_MINTA)
autouw_error_pattern_mc <- autouw_error_pattern %>% 
                        filter(HIBA_MINTA %in% most_common_pattern)

error_prod_mc <- autouw_error_pattern_mc %>%
  group_by(IDOSZAK, MODTYP, HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  arrange(IDOSZAK, MODTYP, desc(TOTAL)) %>%
  ungroup() %>%
  group_by(IDOSZAK, MODTYP) %>%
  mutate(GYAKORISAG = TOTAL / sum(TOTAL))


ggplot(error_prod_mc[error_prod_mc$MODTYP == "GFB",],
       aes(
         x = IDOSZAK,
         y = GYAKORISAG,
         group = 1
       )) +
  geom_line(size = 1) +
  geom_point(size = 1, shape = 15) +
  scale_y_continuous(label = percent) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  facet_wrap(~HIBA_MINTA, ncol = 8, labeller = label_wrap_gen(width=25))


ggplot(error_prod_mc[error_prod_mc$MODTYP == "Lakás",],
       aes(
         x = IDOSZAK,
         y = GYAKORISAG,
         group = 1
       )) +
  geom_line(size = 1) +
  geom_point(size = 1, shape = 15) +
  scale_y_continuous(label = percent) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  facet_wrap(~HIBA_MINTA, ncol = 8, labeller = label_wrap_gen(width=25))



#########################################################################################
# Analyze Error Pattern Costs  ##########################################################
#########################################################################################

# Transformations
autouw_cost <-
  autouw_cost[!is.na(autouw_cost$MODTYP), ]

autouw_cost <-  autouw_cost %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP))


# Compute monthly total cost -----------------------------------------------------------
cost_monthly <- autouw_cost %>%
  group_by(IDOSZAK) %>%
  summarize(HIBA_IDO = sum(IDO_PERC)) %>%
  ungroup() %>%
  left_join(num_wdays, by = c("IDOSZAK")) %>%
  mutate(FTE = HIBA_IDO / 60 / 7 / MNAP,
         IDOSZAK = paste0(substr(IDOSZAK, 1, 4), "/", substr((IDOSZAK), 6, 7))) %>% 
  left_join(auw_main[auw_main$MUTATO == "SIKER_PER_TELJES", ], by=c("IDOSZAK")) %>% 
  rename(SIKER_PER_TELJES = PCT) %>% 
  select(IDOSZAK, FTE, SIKER_PER_TELJES)


ggplot(cost_monthly, aes(x = IDOSZAK, group = 1)) +
  geom_line(aes(y = SIKER_PER_TELJES, colour = 'siker')) +
  geom_line(aes(y = FTE/30, colour = "fte")) +
  scale_y_continuous(labels = percent, sec.axis = sec_axis(~.*30, name = "FTE [db]")) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Sikerarány [%]",
       x = "Idõszak",
       colour = "Paraméter")








