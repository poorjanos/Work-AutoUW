CREATE OR REPLACE PROCEDURE POORJ.kpm_ktg
IS
BEGIN
   INSERT INTO t_kpm_err_pattern_cost_history (vonalkod,
                                               szerzazon,
                                               idoszak,
                                               modtyp,
                                               papir_tipus,
                                               gfb_kotes_nev,
                                               hiba_minta,
                                               ido_perc)
        SELECT   vonalkod,
                 szerzazon,
                 idoszak,
                 modtyp,
                 papir_tipus,
                 gfb_kotes_nev,
                 hiba_minta,
                 SUM (cklido) AS ido_perc
          FROM   (SELECT   a.vonalkod,
                           a.szerzazon,
                           a.idoszak,
                           a.modtyp,
                           a.papir_tipus,
                           a.gfb_kotes_nev,
                           a.hiba_minta,
                           (b.f_int_end - b.f_int_begin) * 1440 AS cklido
                    FROM   (SELECT   *
                              FROM   t_kpm_error_pattern_history@dl_peep
                             WHERE   idoszak >=
                                        ADD_MONTHS (TRUNC (SYSDATE, 'mm'), -1))
                           a,
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
      ORDER BY   idoszak;
COMMIT;
END kpm_ktg;
/
