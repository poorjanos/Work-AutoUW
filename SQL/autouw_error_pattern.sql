SELECT   szerzazon,
         idoszak,
         modtyp,
         papir_tipus,
         gfb_kotes_nev,
         SUBSTR (trace, 0, INSTR (trace, '_0') - 1) AS hiba_minta
  FROM   (SELECT   szerzazon,
                   idoszak,
                   modtyp,
                   papir_tipus,
                   gfb_kotes_nev,
                      e1
                   || '_'
                   || e2
                   || '_'
                   || e3
                   || '_'
                   || e4
                   || '_'
                   || e5
                   || '_'
                   || e6
                   || '_'
                   || e7
                   || '_'
                   || e8
                   || '_'
                   || e9
                   || '_'
                   || e10
                   || '_'
                   || e11
                   || '_'
                   || e12
                      AS trace
            FROM   (SELECT   DISTINCT
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
                      FROM   (SELECT   szerzazon,
                                       idoszak,
                                       modtyp,
                                       papir_tipus,
                                       gfb_kotes_nev,
                                       hibaazon AS e1,
                                       LEAD (
                                          hibaazon,
                                          1,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e2,
                                       LEAD (
                                          hibaazon,
                                          2,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e3,
                                       LEAD (
                                          hibaazon,
                                          3,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e4,
                                       LEAD (
                                          hibaazon,
                                          4,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e5,
                                       LEAD (
                                          hibaazon,
                                          5,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e6,
                                       LEAD (
                                          hibaazon,
                                          6,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e7,
                                       LEAD (
                                          hibaazon,
                                          8,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e8,
                                       LEAD (
                                          hibaazon,
                                          9,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e9,
                                       LEAD (
                                          hibaazon,
                                          10,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e10,
                                       LEAD (
                                          hibaazon,
                                          12,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e11,
                                       LEAD (
                                          hibaazon,
                                          13,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e12,
                                       LEAD (
                                          hibaazon,
                                          14,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e13,
                                       LEAD (
                                          hibaazon,
                                          15,
                                          0
                                       )
                                          OVER (PARTITION BY szerzazon
                                                ORDER BY hibaazon)
                                          AS e14,
                                       hibaazon
                                FROM   (  SELECT   DISTINCT
                                                   a.szerzazon,
                                                   modtyp,
                                                   papir_tipus,
                                                   gfb_kotes_nev,
                                                   TRUNC (a.erkdat, 'mm')
                                                      AS idoszak,
                                                   f_hibaszam AS hibaazon
                                            FROM   t_kpm_history a,
                                                   ab_t_akr_naplo b
                                           WHERE   a.szerzazon = b.f_szerz_azon
                                                   AND a.kpm_hiba_dat =
                                                         b.f_datum
                                                   AND a.kpm = 'Sikertelen'
                                                   AND f_hibaszam <> '99999'
                                        ORDER BY   idoszak,
                                                   a.szerzazon,
                                                   f_hibaszam))))