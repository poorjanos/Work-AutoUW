CREATE OR REPLACE PROCEDURE POORJ.havi_kpm
IS
BEGIN
   EXECUTE IMMEDIATE 'TRUNCATE TABLE t_kpm_siker';

   EXECUTE IMMEDIATE 'TRUNCATE TABLE t_erk_kpm';

   COMMIT;

   INSERT INTO t_kpm_siker (F_VONALKOD)
      SELECT   DISTINCT f_vonalkod
        FROM   ab_t_akr_esemeny
       WHERE   f_datum >= ADD_MONTHS (TRUNC (SYSDATE, 'mm'), -1)
               AND f_esemeny IN
                        ('D2', 'M3', '7S', '7T', '72', '77', '7E', '7A', 'M3');

   INSERT INTO t_erk_kpm (vonalkod)
      SELECT   DISTINCT f_vonalkod
        FROM   ab_t_akr_esemeny
       WHERE   f_datum BETWEEN ADD_MONTHS (TRUNC (SYSDATE, 'mm'), -1)
                           AND  TRUNC (SYSDATE, 'mm') - 1
               AND f_esemeny IN
                        ('01',
                         '31',
                         '35',
                         '70',
                         '37',
                         '38',
                         'DE',
                         'DA',
                         'M0',
                         'M1',
                         'M2',
                         'M3',
                         'M4',
                         '7D',
                         'E0',
                         'E1',
                         'E3',
                         '28');

   COMMIT;


   UPDATE   t_erk_kpm a
      SET   szerzazon =
               (  SELECT   MAX (f.f_szerz_azon)
                    FROM   ab_t_akr_ajanlat f
                   WHERE   f.f_vonalkod = a.vonalkod
                GROUP BY   f.f_vonalkod);

   COMMIT;


   UPDATE   t_erk_kpm a
      SET   idoszak =
               (SELECT   ADD_MONTHS (TRUNC (SYSDATE, 'mm'), -1) FROM DUAL);

   COMMIT;



   UPDATE   t_erk_kpm a
      SET   (modkod) =
               (SELECT   b.f_modkod
                  FROM   ab_t_akr_kotveny b
                 WHERE   b.f_szerz_azon = a.szerzazon);

   COMMIT;



   UPDATE   t_erk_kpm a
      SET   erkdat =
               (SELECT   MIN (b.f_datum)
                  FROM   ab_t_akr_esemeny b
                 WHERE   b.f_esemeny IN
                               ('01',
                                '31',
                                '35',
                                '70',
                                '37',
                                '38',
                                'DE',
                                'DA',
                                'M0',
                                'M1',
                                'M2',
                                'M3',
                                'M4',
                                '7D',
                                'E0',
                                'E1',
                                'E3',
                                '28')
                         AND b.f_vonalkod = a.vonalkod);

   COMMIT;

   UPDATE   t_erk_kpm a
      SET   (szerzdat) =
               (SELECT   MIN (b.f_datum)
                  FROM   ab_t_akr_esemeny b
                 WHERE   b.f_esemeny IN
                               ('14',
                                '18',
                                '50',
                                '51',
                                '52',
                                '53',
                                '72',
                                '77',
                                '79',
                                '7T',
                                '7S',
                                '7E',
                                '7A',
                                'M3')
                         AND b.f_vonalkod = a.vonalkod);

   COMMIT;



   UPDATE   t_erk_kpm a
      SET   elutdat =
               (SELECT   MIN (f_datum)
                  FROM   ab_t_akr_esemeny
                 WHERE   f_esemeny IN ('15', '19', '41', '45', '48')
                         AND f_vonalkod = a.vonalkod);

   COMMIT;



   UPDATE   t_erk_kpm a
      SET   stornodat =
               (SELECT   MIN (f_datum)
                  FROM   ab_t_akr_esemeny
                 WHERE   f_esemeny IN ('04', '40', '9A', '9B', '9C')
                         AND f_vonalkod = a.vonalkod);

   COMMIT;



   UPDATE   t_erk_kpm a
      SET   modtyp = 'Life'
    WHERE   a.modkod LIKE '13%' OR a.modkod LIKE '11%' OR a.modkod LIKE '15%';

   UPDATE   t_erk_kpm a
      SET   modtyp = 'Vagyon'
    WHERE   a.modkod LIKE '21%'
            OR    a.modkod LIKE '12%'
              AND a.modkod NOT LIKE '217%'
              AND a.modkod NOT LIKE '218%'
              AND a.modkod NOT LIKE '2186%';

   UPDATE   t_erk_kpm a
      SET   modtyp = 'Casco'
    WHERE   a.modkod LIKE '218%' AND a.modkod NOT LIKE '2186%';

   UPDATE   t_erk_kpm a
      SET   modtyp = 'GFB'
    WHERE   a.modkod LIKE '35%';

   UPDATE   t_erk_kpm a
      SET   modtyp = 'VVR'
    WHERE      a.modkod LIKE '217%'
            OR a.modkod LIKE '2186%'
            OR a.modkod LIKE '22%'
            OR a.modkod LIKE '23%'
            OR a.modkod LIKE '24%'
            OR a.modkod LIKE '33%'
            OR a.modkod LIKE '34%'
            OR a.modkod LIKE '36%';

   COMMIT;

   UPDATE   t_erk_kpm a
      SET   erk_esemeny =
               (SELECT   DISTINCT
                         FIRST_VALUE(f_esemeny)
                            OVER (PARTITION BY f_vonalkod
                                  ORDER BY f_datum DESC)
                  FROM   ab_t_akr_esemeny b
                 WHERE   a.vonalkod = b.f_vonalkod
                         AND f_esemeny IN
                                  ('01',
                                   '31',
                                   '32',
                                   '35',
                                   '70',
                                   '37',
                                   '38',
                                   'DE',
                                   'DA',
                                   'M0',
                                   'M1',
                                   'M4',
                                   '7D',
                                   'E0',
                                   'E1',
                                   'E3',
                                   '28'));

   COMMIT;

   UPDATE   t_erk_kpm a
      SET   papir_tipus = 'Papir'
    WHERE   erk_esemeny IN ('01', '31');


   UPDATE   t_erk_kpm a
      SET   papir_tipus = 'FE'
    WHERE   erk_esemeny IN ('32', '35');

   --FE

   UPDATE   t_erk_kpm a
      SET   papir_tipus = 'Ajpotlo'
    WHERE   erk_esemeny IN ('70');


   UPDATE   t_erk_kpm a
      SET   papir_tipus = 'Elektra'
    WHERE   erk_esemeny IN ('DE');


   UPDATE   t_erk_kpm a
      SET   papir_tipus = 'Elek'
    WHERE   erk_esemeny IN ('DA');


   UPDATE   t_erk_kpm a
      SET   papir_tipus = 'MySigno'
    WHERE   erk_esemeny IN ('M0', 'M1', 'M4');


   UPDATE   t_erk_kpm a
      SET   papir_tipus = 'Tavert'
    WHERE   erk_esemeny IN ('37', '38', '7D', '28');


   UPDATE   t_erk_kpm a
      SET   papir_tipus = 'Enyil'
    WHERE   erk_esemeny IN ('E0', 'E1', 'E3');

   COMMIT;

   UPDATE   t_erk_kpm a
      SET   kpm = 'Sikeres'
    WHERE   vonalkod IN (SELECT   f_vonalkod FROM t_kpm_siker);

   COMMIT;

   UPDATE   t_erk_kpm a
      SET   kpm = 'Sikertelen'
    WHERE   vonalkod IN
                  (SELECT   f_vonalkod
                     FROM   ab_t_akr_esemeny
                    WHERE   f_datum >= ADD_MONTHS (TRUNC (SYSDATE, 'mm'), -1)
                            AND f_esemeny IN ('D3', '76', 'M2'));

   COMMIT;

   UPDATE   t_erk_kpm a
      SET   kpm = 'Nincs'
    WHERE   kpm IS NULL;

   COMMIT;

   UPDATE   t_erk_kpm a
      SET   (kpm_hiba_dat) =
               (SELECT   MIN (b.f_datum)
                  FROM   ab_t_akr_esemeny b
                 WHERE   b.f_esemeny IN ('D3', '76', 'M2')
                         AND b.f_vonalkod = a.vonalkod);

   COMMIT;

   UPDATE   t_erk_kpm a
      SET   gfb_kotes =
               (SELECT   b.f_kotes_ok
                  FROM   ab_t_kotveny_gfb b
                 WHERE   a.szerzazon = b.f_szerz_azon);

   COMMIT;

   UPDATE   t_erk_kpm
      SET   gfb_kotes_nev = 'Tul_valtas'
    WHERE   gfb_kotes = 1;

   UPDATE   t_erk_kpm
      SET   gfb_kotes_nev = 'DNF_ujra'
    WHERE   gfb_kotes = 2;

   UPDATE   t_erk_kpm
      SET   gfb_kotes_nev = 'Evford_valt'
    WHERE   gfb_kotes = 3;

   UPDATE   t_erk_kpm
      SET   gfb_kotes_nev = 'Kozos_meg'
    WHERE   gfb_kotes = 4;

   UPDATE   t_erk_kpm
      SET   gfb_kotes_nev = 'Uztart_valt'
    WHERE   gfb_kotes = 5;

   UPDATE   t_erk_kpm
      SET   gfb_kotes_nev = 'Orokles'
    WHERE   gfb_kotes = 6;

   UPDATE   t_erk_kpm
      SET   gfb_kotes_nev = 'Uj_fhely'
    WHERE   gfb_kotes = 7;

   UPDATE   t_erk_kpm
      SET   gfb_kotes_nev = 'Egyeb'
    WHERE   gfb_kotes IS NOT NULL AND gfb_kotes > 7;

   COMMIT;

   DELETE FROM   t_erk_kpm
         WHERE   erkdat < ADD_MONTHS (TRUNC (SYSDATE, 'mm'), -1);

   COMMIT;


   EXECUTE IMMEDIATE 'CREATE OR REPLACE SYNONYM fpack FOR pack@dijtart_exdbp_f400';

   EXECUTE IMMEDIATE 'CREATE OR REPLACE SYNONYM fproposal FOR proposal@dijtart_exdbp_f400';

   COMMIT;


   INSERT INTO t_erk_kpm (idoszak,
                                vonalkod,
                                szerzazon,
                                modkod,
                                modtyp,
                                erkdat,
                                szerzdat,
                                elutdat,
                                stornodat,
                                papir_tipus,
                                erk_esemeny,
                                kpm,
                                kpm_hiba_dat,
                                gfb_kotes,
                                gfb_kotes_nev)
      SELECT   ADD_MONTHS (TRUNC (SYSDATE, 'mm'), -1),
               TO_CHAR (fproposal.proposal_idntfr),
               TO_CHAR (fpack.oid_to_idntfr (fproposal.contract_oid)),
               fproposal.product_code,
               'Life',
               fproposal.arrival_date,
               fpack.proposal_contract_date (fproposal.proposal_idntfr),
               fproposal.rejection_date,
               fpack.proposal_cancel_date (fproposal.proposal_idntfr),
               CASE
                  WHEN fproposal.front_end = 'N'
                  THEN
                     'Papir'
                  WHEN fproposal.front_end = 'I'
                       AND fproposal.front_end_type IS NULL
                  THEN
                     'FE'
                  WHEN fproposal.front_end_type = 'MYSIG'
                  THEN
                     'MySigno'
                  WHEN fproposal.front_end_type = 'ENYIL'
                  THEN
                     'Enyil'
                  WHEN fproposal.front_end_type = 'TAVERT'
                  THEN
                     'Tavert'
               END,
               NULL,
               'Nincs',
               NULL,
               NULL,
               NULL
        FROM   fproposal
       WHERE   fproposal.arrival_date BETWEEN ADD_MONTHS (
                                                 TRUNC (SYSDATE, 'mm'),
                                                 -1
                                              )
                                          AND  TRUNC (SYSDATE, 'mm') - 1
               AND fproposal.cntry_flg LIKE 'HU';
END havi_kpm;
/
