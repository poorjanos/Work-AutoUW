# Load required libs
library(config)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(tidyr)


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

# Create connection driver and open connection
jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "C:\\Users\\PoorJ\\Desktop\\ojdbc7.jar")

jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = ablak$server,
    user = ablak$uid,
    password = ablak$pwd
  )

# Fetch data
query_autouw <- "select * from t_kpm_history"
autouw_df <- dbGetQuery(jdbcConnection, query_autouw)

# Close db connection
dbDisconnect(jdbcConnection)


#########################################################################################
# Analyze Attempt & Success Rates on a Monthly Basis ##############################################
#########################################################################################

# Transformations
autouw_df <- autouw_df[!is.na(autouw_df$MODTYP),]
autouw_df$IDOSZAK <-
  paste0(substr(autouw_df$IDOSZAK, 1, 4), "/", substr((autouw_df$IDOSZAK), 6, 7))

autouw_df <-  autouw_df %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP))


# Compute AutoUW main KPIs ##############################################################
# Compute Attempt Rate
auw_attempt <-  autouw_df %>%
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "I") %>%
  select(IDOSZAK, KISERELT)


# Compute Success Rate Within Total
auw_success_total <-  autouw_df %>%
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_TELJES)


# Compute Success Rate Within Attempted
auw_success_attempt <-  autouw_df %>%
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_KISERELT)

# Merge KPI Results
auw_main <- auw_attempt %>%
  left_join(auw_success_attempt, by = "IDOSZAK") %>%
  left_join(auw_success_total, by = "IDOSZAK") %>%
  gather(MUTATO, PCT, -IDOSZAK)


# Plot Main Metrics
ggplot(auw_main, aes(IDOSZAK, PCT, group = MUTATO, colour = MUTATO)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 15) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Százlékos arány",
       x = "Hónap",
       colour = "Mérõszám")




# Compute AutoUW KPIs for Each Product Line #############################################
# Compute Attempt Rate for Each Product Line
auw_prod_attempt <-  autouw_df %>%
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, MODTYP, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "N") %>%
  mutate(KISERELT = 1 - KISERELT) %>%
  select(IDOSZAK, MODTYP, KISERELT)


# Compute Success Rate for Each Product Line Within Total
auw_prod_success_total <-   autouw_df %>%
  group_by(IDOSZAK, MODTYP, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  group_by(IDOSZAK, MODTYP, KPM = KPM == "Sikeres") %>%
  summarize(SIKER_PER_TELJES = sum(SIKER_PER_TELJES)) %>%
  filter(KPM == FALSE) %>%
  mutate(SIKER_PER_TELJES = 1 - SIKER_PER_TELJES) %>%
  select(IDOSZAK, MODTYP, SIKER_PER_TELJES)


# Compute Success Rate for Each Product Line Within Attempted
auw_prod_success_attempt <-   autouw_df %>%
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, MODTYP, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, MODTYP, SIKER_PER_KISERELT)


# Merge KPI Results for Each Product Line
auw_prod <- auw_prod_attempt %>%
  left_join(auw_prod_success_attempt, by = c("IDOSZAK", "MODTYP")) %>%
  left_join(auw_prod_success_total, by = c("IDOSZAK", "MODTYP")) %>%
  gather(MUTATO, PCT, -IDOSZAK, -MODTYP)


# Plot Product Line Metrics
ggplot(auw_prod, aes(IDOSZAK, PCT, group = MUTATO, colour = MUTATO)) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 15) +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(. ~ MODTYP) +
  labs(y = "Százlékos arány",
       x = "Hónap",
       colour = "Mérõszám")



# Compute AutoUW KPIs for Each Product Line  & Media ####################################
# Compute Attempt Rate for Each Product Line & Media
auw_prod_media_attempt <-  autouw_df %>%
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


# Compute Success Rate for Each Product Line & Media Within Attempted
auw_prod_media_success_total <-  autouw_df %>%
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


# Compute Success Rate for Each Product Line & Media Within Attempted
auw_prod_media_success_attemp <-  autouw_df %>%
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, MODTYP, PAPIR_TIPUS, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, MODTYP, PAPIR_TIPUS, SIKER_PER_KISERELT)


# Merge KPI Results for Each Product Line & Media
auw_prod_media <- auw_prod_media_attempt %>%
  left_join(auw_prod_media_success_attemp,
            by = c("IDOSZAK", "MODTYP", "PAPIR_TIPUS")) %>%
  left_join(auw_prod_media_success_total,
            by = c("IDOSZAK", "MODTYP", "PAPIR_TIPUS")) %>%
  gather(MUTATO, PCT, -IDOSZAK, -MODTYP, -PAPIR_TIPUS)


# Plot Product Line & Media Metrics
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


# Compute AutoUW KPIs for TPML per Each Contact Type ####################################
autouw_tpml <- autouw_df %>% filter(MODTYP == "GFB")

# Compute Attempt Rate for Each Product Line & Media
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


# Compute Success Rate for TPML per Each Contact Type 
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


# Compute Success Rate for TPML per Each Contact Type 
auw_tpml_ctype_success_attemp <-  autouw_tpml %>%
  filter(KPM != "Nincs") %>%
  group_by(IDOSZAK, GFB_KOTES_NEV, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, GFB_KOTES_NEV, SIKER_PER_KISERELT)


# Merge KPI Results for TPML per Each Contact Type
auw_tpml_ctype <- auw_tpml_ctype_attempt %>%
  left_join(auw_tpml_ctype_success_total,
            by = c("IDOSZAK", "GFB_KOTES_NEV")) %>%
  left_join(auw_tpml_ctype_success_attemp,
            by = c("IDOSZAK", "GFB_KOTES_NEV")) %>%
  gather(MUTATO, PCT, -IDOSZAK, -GFB_KOTES_NEV)


# Plot TPML per Each Contact Type Metrics
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
