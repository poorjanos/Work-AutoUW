# Run ad-hoc analysis for one-touch processing of failed autoUW home prods

# Fetch data by custom query
pattern <- dbGetQuery(
  jdbcConnection,
  "
                      SELECT   vonalkod,
                 szerzazon,
                      idoszak,
                      modtyp,
                      papir_tipus,
                      gfb_kotes_nev,
                      hiba_minta,
                      SUM (cklido) AS ido_perc,
                      count(vonalkod) as erintes_db
                      FROM   (SELECT   a.vonalkod,
                      a.szerzazon,
                      a.idoszak,
                      a.modtyp,
                      a.papir_tipus,
                      a.gfb_kotes_nev,
                      a.hiba_minta,
                      (b.f_int_end - b.f_int_begin) * 1440 AS cklido
                      FROM   (select * from t_kpm_error_pattern_history@dl_peep where modtyp = 'Vagyon') a,
                      afc.t_afc_wflog_lin2 b,
                      kontakt.t_lean_alirattipus c
                      WHERE       a.vonalkod = b.f_ivk
                      AND b.f_alirattipusid = c.f_alirattipusid(+)
                      AND (b.f_int_end - b.f_int_begin) * 1440 < 45
                      AND (b.f_int_end - b.f_int_begin) * 86400 > 1
                      AND afc.afc_wflog_intezkedes (b.f_ivkwfid,
                      b.f_logid) IS NOT NULL
                      AND UPPER (
                      kontakt.basic.get_userid_login (b.f_userid)
                      ) NOT IN
                      ('MARKIB', 'SZERENCSEK')
                      AND c.f_lean_tip = 'AL')
                      GROUP BY   vonalkod,
                      szerzazon,
                      idoszak,
                      modtyp,
                      papir_tipus,
                      gfb_kotes_nev,
                      hiba_minta
                      ORDER BY   idoszak
                      "
)

# Transform data
pattern <- pattern[!is.na(pattern$MODTYP), ]
pattern$IDOSZAK <-
  paste0(substr(pattern$IDOSZAK, 1, 4), "/", substr((pattern$IDOSZAK), 6, 7))

pattern <- pattern %>%
  mutate(MODTYP = case_when(
    .$MODTYP == "Vagyon" ~ "Lakás",
    TRUE ~ .$MODTYP
  ))

# Subset for last 3 months
pattern_last3 <-
  pattern %>%
  filter(stringr::str_detect(IDOSZAK, "2018"))

# Gen freq of patterns
freq_pattern <- pattern_last3 %>%
  filter(MODTYP == "Lakás", ERINTES_DB == 1) %>%
  group_by(HIBA_MINTA) %>%
  summarize(TOTAL = n()) %>%
  ungroup() %>%
  arrange(desc(TOTAL)) %>%
  mutate(
    GYAKORISAG = TOTAL / sum(TOTAL),
    GYAK_KUM = cumsum(GYAKORISAG)
  ) %>%
  filter(GYAKORISAG >= freq_lim)

# Plot pattern freqs
ggplot(freq_pattern, aes(
  x = factor(freq_pattern$HIBA_MINTA, levels = freq_pattern$HIBA_MINTA[order(freq_pattern$GYAKORISAG)]),
  y = GYAKORISAG
)) +
  geom_bar(stat = "identity") +
  theme(axis.text.y = element_text(size = 10)) +
  scale_y_continuous(label = percent) +
  coord_flip() +
  labs(
    y = "Relatív gyakoriság",
    x = "Hibamintázat"
  ) +
  ggtitle("KPM hiba után egy érintéssel menesztett lakás ajánlatok \n leggyakoribb hibamintázatai (2018)")