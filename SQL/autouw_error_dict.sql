/* Formatted on 2018.02.22. 14:30:42 (QP5 v5.115.810.9015) */
  SELECT   DISTINCT hibaazon, hiba, count(szerzazon)
    FROM   (SELECT   a.szerzazon,
                     modtyp,
                     papir_tipus,
                     gfb_kotes_nev,
                     TRUNC (a.erkdat, 'mm') AS idoszak,
                     f_hibaszam AS hibaazon,
                     REGEXP_REPLACE (f_szoveg, '[[:digit:]]|\([^()]*\)', '')
                        AS hiba
              FROM   t_kpm_history a, ab_t_akr_naplo b
             WHERE       a.szerzazon = b.f_szerz_azon
                     AND a.kpm_hiba_dat = b.f_datum
                     AND a.kpm = 'Sikertelen')
GROUP BY hibaazon, hiba
ORDER BY hibaazon, hiba