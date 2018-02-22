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
END kpm_naplo;
/
