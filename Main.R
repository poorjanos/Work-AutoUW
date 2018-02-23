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
readQuery <-
    function(file)
      paste(readLines(file, warn = FALSE), collapse = "\n")

error_freq <- readQuery(here::here("SQL", "autouw_error_freq.sql"))
error_pattern <- readQuery(here::here("SQL", "autouw_error_pattern_string.sql"))
query_autouw <- "select * from t_kpm_history"
query_autouw_dict <- "select * from t_autouw_dict"

autouw_error_freq <- dbGetQuery(jdbcConnection, query_autouw)
autouw_dict <- dbGetQuery(jdbcConnection, query_autouw_dict)
autouw_error_freq <- dbGetQuery(jdbcConnection, error_freq)
autouw_error_pattern <- dbGetQuery(jdbcConnection, error_pattern)


# Close db connection
dbDisconnect(jdbcConnection)


#########################################################################################
# Analyze Attempt & Success Rates on a Monthly Basis ####################################
#########################################################################################

# Transformations
autouw_error_freq <- autouw_error_freq[!is.na(autouw_error_freq$MODTYP),]
autouw_error_freq$IDOSZAK <-
  paste0(substr(autouw_error_freq$IDOSZAK, 1, 4), "/", substr((autouw_error_freq$IDOSZAK), 6, 7))

autouw_error_freq <-  autouw_error_freq %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP))


# Compute autoUW main KPIs ##############################################################
# Compute attempt rate
auw_attempt <-  autouw_error_freq %>%
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "I") %>%
  select(IDOSZAK, KISERELT)


# Compute success rate within total
auw_success_total <-  autouw_error_freq %>%
  group_by(IDOSZAK, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  filter(KPM == "Sikeres") %>%
  select(IDOSZAK, SIKER_PER_TELJES)


# Compute success rate within attempted
auw_success_attempt <-  autouw_error_freq %>%
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




# Compute autoUW KPIs for each product line #############################################
# Compute attempt rate for each product line
auw_prod_attempt <-  autouw_error_freq %>%
  mutate(ATTEMPT = case_when(.$KPM %in% c("Sikeres", "Sikertelen") ~ 'I',
                             TRUE ~ 'N')) %>%
  group_by(IDOSZAK, MODTYP, ATTEMPT) %>%
  summarise (TOTAL = n()) %>%
  mutate(KISERELT = TOTAL / sum(TOTAL)) %>%
  filter(ATTEMPT == "N") %>%
  mutate(KISERELT = 1 - KISERELT) %>%
  select(IDOSZAK, MODTYP, KISERELT)


# Compute success rate for each product line within total
auw_prod_success_total <-   autouw_error_freq %>%
  group_by(IDOSZAK, MODTYP, KPM) %>%
  summarise (TOTAL = n()) %>%
  mutate(SIKER_PER_TELJES = TOTAL / sum(TOTAL)) %>%
  group_by(IDOSZAK, MODTYP, KPM = KPM == "Sikeres") %>%
  summarize(SIKER_PER_TELJES = sum(SIKER_PER_TELJES)) %>%
  filter(KPM == FALSE) %>%
  mutate(SIKER_PER_TELJES = 1 - SIKER_PER_TELJES) %>%
  select(IDOSZAK, MODTYP, SIKER_PER_TELJES)


# Compute success rate for each product line within attempted
auw_prod_success_attempt <-   autouw_error_freq %>%
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



# Compute autoUW KPIs for each product line  & media ####################################
# Compute attempt rate for each product line & media
auw_prod_media_attempt <-  autouw_error_freq %>%
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
auw_prod_media_success_total <-  autouw_error_freq %>%
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
auw_prod_media_success_attemp <-  autouw_error_freq %>%
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


# Compute autoUW KPIs for TPML per each contact type ####################################
autouw_tpml <- autouw_error_freq %>% filter(MODTYP == "GFB")

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
autouw_error_freq <-
  autouw_error_freq[!is.na(autouw_error_freq$MODTYP), ]
autouw_error_freq$IDOSZAK <-
  paste0(substr(autouw_error_freq$IDOSZAK, 1, 4), "/", substr((autouw_error_freq$IDOSZAK), 6, 7))

autouw_error_freq <-  autouw_error_freq %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP)) %>%
  left_join(autouw_dict, by = c("HIBAAZON"))


# Freq of errors for whole dataset
freq <- autouw_error_freq %>%
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
freq_prod <- autouw_error_freq %>%
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


# Freq time series for most common errors per prod line
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
autouw_error_pattern <-
  autouw_error_pattern[!is.na(autouw_error_pattern$MODTYP), ]
autouw_error_pattern$IDOSZAK <-
  paste0(substr(autouw_error_pattern$IDOSZAK, 1, 4), "/", substr((autouw_error_pattern$IDOSZAK), 6, 7))

autouw_error_pattern <-  autouw_error_pattern %>%
  mutate(MODTYP = case_when(.$MODTYP == "Vagyon" ~ "Lakás",
                            TRUE ~ .$MODTYP)) 

# Freq of patterns per prod line
freq_pattern <- autouw_error_pattern %>%
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

# Separate elems of pattern
# max_elems <- max(str_count(freq_pattern$HIBA_MINTA, '_')) + 1
# sep_col_names <- sapply(seq(max_elems), function(x) paste0('elem', x))
# freq_pattern <- freq_pattern %>%
#                 separate(HIBA_MINTA, sep_col_names, '_', convert = TRUE)
# 
# freq_pattern[is.na(freq_pattern)] <- 0

# Join with dictionary to get error names
# for (i in seq_along(sep_col_names)){
#   start_col_name <- colnames(freq_pattern)[i]
#   colnames(freq_pattern)[i] <- "HIBAAZON"
#   freq_pattern <- freq_pattern %>% left_join(autouw_dict, by=c("HIBAAZON"))
#   colnames(freq_pattern)[i] <- start_col_name
#   new_col_name <- paste0("HIBA", i)
#   colnames(freq_pattern)[length(freq_pattern)] <- new_col_name
#   freq_pattern[is.na(freq_pattern[new_col_name]), new_col_name] <- ''
# }
