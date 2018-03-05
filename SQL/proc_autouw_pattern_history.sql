CREATE OR REPLACE PROCEDURE POORJ.kpm_minta_naplo
IS
BEGIN
   INSERT INTO t_kpm_error_pattern_history (vonalkod,
                                            szerzazon,
                                            idoszak,
                                            modtyp,
                                            papir_tipus,
                                            gfb_kotes_nev,
                                            hiba_minta)
      SELECT   vonalkod,
               szerzazon,
               idoszak,
               modtyp,
               papir_tipus,
               gfb_kotes_nev,
               SUBSTR (trace, 0, INSTR (trace, '***0') - 1) AS hiba_minta
        FROM   (SELECT   vonalkod,
                         szerzazon,
                         idoszak,
                         modtyp,
                         papir_tipus,
                         gfb_kotes_nev,
                            e1
                         || '***'
                         || e2
                         || '***'
                         || e3
                         || '***'
                         || e4
                         || '***'
                         || e5
                         || '***'
                         || e6
                         || '***'
                         || e7
                         || '***'
                         || e8
                         || '***'
                         || e9
                         || '***'
                         || e10
                         || '***'
                         || e11
                         || '***'
                         || e12
                            AS trace
                  FROM   (SELECT   DISTINCT
                                   vonalkod,
                                   szerzazon,
                                   idoszak,
                                   modtyp,
                                   papir_tipus,
                                   gfb_kotes_nev,
                                   FIRST_VALUE(e1)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e1,
                                   FIRST_VALUE(e2)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e2,
                                   FIRST_VALUE(e3)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e3,
                                   FIRST_VALUE(e4)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e4,
                                   FIRST_VALUE(e5)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e5,
                                   FIRST_VALUE(e6)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e6,
                                   FIRST_VALUE(e7)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e7,
                                   FIRST_VALUE(e8)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e8,
                                   FIRST_VALUE(e9)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e9,
                                   FIRST_VALUE(e10)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e10,
                                   FIRST_VALUE(e11)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e11,
                                   FIRST_VALUE(e12)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e12,
                                   FIRST_VALUE(e13)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e13,
                                   FIRST_VALUE(e14)
                                      OVER (PARTITION BY szerzazon
                                            ORDER BY hibaazon
                                            ROWS UNBOUNDED PRECEDING)
                                      e14
                            FROM   (SELECT   vonalkod,
                                             szerzazon,
                                             idoszak,
                                             modtyp,
                                             papir_tipus,
                                             gfb_kotes_nev,
                                             hiba AS e1,
                                             LEAD (
                                                hiba,
                                                1,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e2,
                                             LEAD (
                                                hiba,
                                                2,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e3,
                                             LEAD (
                                                hiba,
                                                3,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e4,
                                             LEAD (
                                                hiba,
                                                4,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e5,
                                             LEAD (
                                                hiba,
                                                5,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e6,
                                             LEAD (
                                                hiba,
                                                6,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e7,
                                             LEAD (
                                                hiba,
                                                8,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e8,
                                             LEAD (
                                                hiba,
                                                9,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e9,
                                             LEAD (
                                                hiba,
                                                10,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e10,
                                             LEAD (
                                                hiba,
                                                12,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e11,
                                             LEAD (
                                                hiba,
                                                13,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e12,
                                             LEAD (
                                                hiba,
                                                14,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e13,
                                             LEAD (
                                                hiba,
                                                15,
                                                0
                                             )
                                                OVER (PARTITION BY szerzazon
                                                      ORDER BY hibaazon)
                                                AS e14,
                                             hibaazon
                                      FROM   (SELECT   c.*, d.hiba
                                                FROM   (  SELECT   DISTINCT
                                                                   vonalkod,
                                                                   a.szerzazon,
                                                                   modtyp,
                                                                   papir_tipus,
                                                                   gfb_kotes_nev,
                                                                   TRUNC (
                                                                      a.erkdat,
                                                                      'mm'
                                                                   )
                                                                      AS idoszak,
                                                                   f_hibaszam
                                                                      AS hibaazon
                                                            FROM   t_erk_kpm a,
                                                                   ab_t_akr_naplo b
                                                           WHERE   a.szerzazon =
                                                                      b.f_szerz_azon
                                                                   AND a.kpm_hiba_dat =
                                                                         b.f_datum
                                                                   AND a.kpm =
                                                                         'Sikertelen'
                                                                   AND f_hibaszam <>
                                                                         '99999'
                                                        ORDER BY   idoszak,
                                                                   a.szerzazon,
                                                                   f_hibaszam)
                                                       c,
                                                       t_autouw_dict d
                                               WHERE   c.hibaazon =
                                                          d.hibaazon))));
COMMIT;
END kpm_minta_naplo;
/
