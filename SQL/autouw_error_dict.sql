--implemented as part of proc_autouw_history
CREATE TABLE t_autouw_dict
as
SELECT   DISTINCT
         FIRST_VALUE(hibaazon)
            OVER (PARTITION BY hibaazon
                  ORDER BY gyak DESC
                  ROWS UNBOUNDED PRECEDING)
            AS hibaazon,
         FIRST_VALUE(hiba)
            OVER (PARTITION BY hibaazon
                  ORDER BY gyak DESC
                  ROWS UNBOUNDED PRECEDING)
            AS hiba
  FROM   (  SELECT   DISTINCT hibaazon, hiba, COUNT (szerzazon) AS gyak
              FROM   (SELECT   a.szerzazon,
                               modtyp,
                               papir_tipus,
                               gfb_kotes_nev,
                               TRUNC (a.erkdat, 'mm') AS idoszak,
                               f_hibaszam AS hibaazon,
                               CASE
                                  WHEN INSTR (
                                          REGEXP_REPLACE (
                                             f_szoveg,
                                             '[[:digit:]]|\([^()]*\)',
                                             ''
                                          ),
                                          '.'
                                       ) <> 0
                                  THEN
                                     SUBSTR (
                                        REGEXP_REPLACE (
                                           f_szoveg,
                                           '[[:digit:]]|\([^()]*\)',
                                           ''
                                        ),
                                        0,
                                        INSTR (
                                           REGEXP_REPLACE (
                                              f_szoveg,
                                              '[[:digit:]]|\([^()]*\)',
                                              ''
                                           ),
                                           '.'
                                        )
                                     )
                                  WHEN INSTR (
                                          REGEXP_REPLACE (
                                             f_szoveg,
                                             '[[:digit:]]|\([^()]*\)',
                                             ''
                                          ),
                                          ':'
                                       ) <> 0
                                  THEN
                                     SUBSTR (
                                        REGEXP_REPLACE (
                                           f_szoveg,
                                           '[[:digit:]]|\([^()]*\)',
                                           ''
                                        ),
                                        0,
                                        INSTR (
                                           REGEXP_REPLACE (
                                              f_szoveg,
                                              '[[:digit:]]|\([^()]*\)',
                                              ''
                                           ),
                                           ':'
                                        )
                                     )
                                  ELSE
                                     REGEXP_REPLACE (f_szoveg,
                                                     '[[:digit:]]|\([^()]*\)',
                                                     '')
                               END
                                  AS hiba
                        FROM   t_kpm_history a, ab_t_akr_naplo b
                       WHERE       a.szerzazon = b.f_szerz_azon
                               AND a.kpm_hiba_dat = b.f_datum
                               AND a.kpm = 'Sikertelen')
          GROUP BY   hibaazon, hiba
          ORDER BY   hibaazon, hiba)