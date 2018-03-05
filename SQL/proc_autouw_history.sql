CREATE OR REPLACE PROCEDURE POORJ.kpm_naplo
IS
BEGIN
   INSERT INTO t_kpm_history (idoszak,
                              vonalkod,
                              szerzazon,
                              modkod,
                              modtyp,
                              erkdat,
                              szerzdat,
                              elutdat,
                              stornodat,
                              papir_tipus,
                              kpm,
                              kpm_hiba_dat,
                              gfb_kotes_nev)
      SELECT   idoszak,
               vonalkod,
               szerzazon,
               modkod,
               modtyp,
               erkdat,
               szerzdat,
               elutdat,
               stornodat,
               papir_tipus,
               kpm,
               kpm_hiba_dat,
               gfb_kotes_nev
        FROM   t_erk_kpm;
COMMIT;

   EXECUTE IMMEDIATE 'TRUNCATE TABLE  t_autouw_dict';

   INSERT INTO t_autouw_dict (hibaazon, hiba)
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
                                           REGEXP_REPLACE (
                                              f_szoveg,
                                              '[[:digit:]]|\([^()]*\)',
                                              ''
                                           )
                                     END
                                        AS hiba
                              FROM   t_kpm_history a, ab_t_akr_naplo b
                             WHERE       a.szerzazon = b.f_szerz_azon
                                     AND a.kpm_hiba_dat = b.f_datum
                                     AND a.kpm = 'Sikertelen')
                GROUP BY   hibaazon, hiba
                ORDER BY   hibaazon, hiba);
COMMIT;
END kpm_naplo;
/