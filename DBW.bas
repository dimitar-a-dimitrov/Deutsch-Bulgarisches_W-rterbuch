 0  REM **********
 1  REM *        *
 2  REM * re~nik *
 3  REM *        *
 4  REM **********
 5  ONERR  GOTO 1050
 6  CLEAR 
 7 D$ =  CHR$ (4): REM CTRL+D
 8 M$ =  CHR$ (13): REM RETURN
 9 RA$ = "=": REM razdelitel
 10  TEXT : NORMAL : SPEED= 255: HOME 
 12  SHLOAD 1
 15  PRINT M$D$"CLOSE"
 17  HOME 
 20  PRINT "         nemsko-bylgarski re~nik       "
 30  PRINT : PRINT "1.tyrsene na dumi w re~nika"
 40  PRINT "2.wywevdane na nowi dumi"
 45  PRINT "3.syzdawanE na spisyk za zapazwane na     dumi i tehnite bylgarski zna~eniq"
 47  PRINT "4.obu~enie"
 48  PRINT "5.katalog na disketata"
 50  PRINT "6.kraj"
 60  PRINT : PRINT "molq, wywedete: ";: GET OT$
 65 OT =  VAL (OT$)
 70  ON OT GOTO 90,370,820,1015,1011,1020
 80  VTAB 10: GOTO 60
 90  REM tyrsene
 91 V = 0
 92 VV = 0
 93 QQ = 1
 95 A$ = ""
 100  HOME : PRINT "       tyrsene na dumi w re~nika       "
 110  PRINT : PRINT "wywedete dumata:";
 111  GET AA$
 112  IF AA$ = M$ THEN 130
 113  IF AA$ =  CHR$ (136) OR AA$ =  CHR$ (8) THEN  PRINT AA$;:A$ =  MID$ (A$,1, LEN (A$) - 1): GOTO 111
 114  IF  ASC (AA$) >  = 0 AND  ASC (AA$) <  = 31 THEN 111
 115  IF  ASC (AA$) >  = 128 AND  ASC (AA$) <  = 159 THEN 111
 117 A$ = A$ + AA$
 118  PRINT AA$;
 119  GOTO 111
 130  PRINT : IF A$ = "" THEN 0
 131  IF  RIGHT$ (A$,1) <  > " " THEN 133
 132 A$ =  MID$ (A$,1, LEN (A$) - 1): GOTO 131
 133 B$ =  LEFT$ (A$,1)
 135  IF  ASC (B$) >  =  ASC (" TO ") AND  ASC (B$) <  =  ASC (" SQR ") THEN B$ =  CHR$ ( ASC (B$) - 128)
 140  IF  ASC (B$) <  ASC ("A") OR  ASC (B$) >  ASC ("Z") THEN 10
 145 I = 0
 150  PRINT D$"VERIFY D"B$
 160  PRINT D$"OPEN D"B$
 180  PRINT D$"READ D"B$
 190  GET E$
 191  IF  PEEK (64435) = 6 AND  PEEK (64448) = 234 THEN  IF  PEEK (64286) = 173 OR  PEEK (64286) = 250 THEN 200
 192  IF  ASC (E$) =  > 160 AND  ASC (E$) =  < 255 THEN E$ =  CHR$ ( ASC (E$) - 128)
 200  IF E$ = RA$ THEN 230
 210 DU$ = DU$ + E$
 220  GOTO 190
 230  IF DU$ = A$ THEN 240
 232 DU$ = ""
 234  GET E$
 235  IF E$ = M$ THEN 190
 236  GOTO 234
 240 DU$ = DU$ + RA$
 250  GET E$
 260  IF E$ = M$ THEN 290
 270 DU$ = DU$ + E$
 280  GOTO 250
 290  PRINT M$D$"CLOSE D"B$
 300  PRINT : PRINT "dumata ima slednoto zna~enie:"
 310  PRINT DU$
 320 DU$ = ""
 330  PRINT : PRINT "}e tyrsite li druga duma? (d/n) ";: GET O$
 340  IF O$ = "D" OR O$ = "d" OR O$ = " THEN " OR O$ = " STR$ " THEN 90
 350  IF O$ = "N" OR O$ = "n" OR O$ = " OR " OR O$ = " ERROR " THEN 0
 360  GOTO 320
 370  REM wywevdane
 371 V = 1
 372 VV = 1
 373 VL = 0
 375  POKE 222,0
 380  HOME : PRINT "         wywevdane na nowi dumi        "
 390  PRINT : INPUT "kolko dumi }e wywedete? ";BR$
 400 BR =  VAL (BR$)
 420  DIM DI$(BR - 1)
 430  PRINT "wywevdajte wyw format nem.=bylg."
 440  FOR S = 0 TO BR - 1
 450  PRINT "izraz #"S + 1":";
 460  GET E$
 461  IF E$ =  CHR$ (8) OR E$ =  CHR$ (136) THEN  PRINT E$;:DI$(S) =  MID$ (DI$(S),1, LEN (DI$(S)) - 1): GOTO 460
 462  IF E$ = M$ THEN 510
 463  IF  ASC (E$) >  = 0 AND  ASC (E$) <  = 31 THEN 460
 464  IF  ASC (E$) = 128 AND  ASC (E$) <  = 159 THEN 460
 480 DI$(S) = DI$(S) + E$
 490  PRINT E$;
 500  GOTO 460
 510  IF S = BR - 1 THEN  PRINT M$: GOTO 530
 520  PRINT M$: NEXT S
 530  PRINT "dopusnali li ste nqkakwa gre{ka pri     wywevdaneto? (d/n) ": GET O$
 540  IF O$ = "D" OR O$ = "d" OR O$ = " THEN " OR O$ = " STR$ " THEN 570
 550  IF O$ = "N" OR O$ = "n" OR O$ = " OR " OR O$ = " ERROR " THEN 720
 560  GOTO 530
 570  IF BR = 1 THEN 590
 575  PRINT : INPUT "w koj nomer izraz? ";H$
 580 H =  VAL (H$)
 590  PRINT "molq, wywedete izraza otnowo."
 595  IF BR = 1 THEN H = 1
 600  PRINT "izraz #"H":";
 610 DI$(H - 1) = ""
 615 VL = 1
 620  GET E$
 625  IF E$ =  CHR$ (8) OR E$ =  CHR$ (136) THEN  PRINT E$;:DI$(S) =  MID$ (DI$(S),1, LEN (DI$(S)) - 1): GOTO 620
 630  IF E$ = M$ THEN 670
 634  IF  ASC (E$) >  = 0 AND  ASC (E$) <  = 31 THEN 620
 637  IF  ASC (E$) >  = 128 AND  ASC (E$) <  = 159 THEN 620
 640 DI$(H - 1) = DI$(H - 1) + E$
 650  PRINT E$;
 660  GOTO 620
 670  PRINT : PRINT "dopusnali li ste druga gre{ka pri       wywevdaneto (d/n) ";
 680  GET O$
 690  IF O$ = "D" OR O$ = "d" OR O$ = " THEN " OR O$ = " STR$ " THEN 570
 700  IF O$ = "N" OR O$ = "n" OR O$ = " OR " OR O$ = " ERROR " THEN 720
 710  GOTO 670
 720  PRINT : PRINT "zapisyt zapo~na."
 730  FOR XY = 0 TO BR - 1
 735  IF  PEEK (222) = 6 OR  PEEK (222) = 10 THEN  FOR XY = XY + 1 TO BR - 1
 740 B$ =  LEFT$ (DI$(XY),1)
 741  IF  ASC (B$) >  =  ASC (" TO ") AND  ASC (B$) <  =  ASC (" SQR ") THEN B$ =  CHR$ ( ASC (B$) - 128)
 745  PRINT 
 750  REM 
 760  PRINT D$"APPEND D"B$
 770  PRINT D$"WRITE D"B$
 780  PRINT DI$(XY)
 790  PRINT D$"CLOSE D"B$
 800  NEXT 
 805  IF XY = BR THEN 0
 810  GOTO 0
 820  REM syzdawane na tekstow        fajl
 821 V = 2
 830  HOME : PRINT "syzdawane na spisyk za zapazwane na dumi      i tehnite bylgarski zna~eniq"
 840  PRINT : VTAB 4: INPUT "s koq bukwa zapo~wat dumite, koito }e sezapiswat w spisyka? ";PY$
 850  IF  LEN (PY$) > 1 THEN 0
 851  IF  ASC (PY$) >  =  ASC (" TO ") AND  ASC (PY$) <  =  ASC (" SQR ") THEN PY$ =  CHR$ ( ASC (PY$) - 128)
 852  IF  ASC (PY$) <  ASC ("A") OR  ASC (PY$) >  ASC ("Z") THEN 0
 860  VTAB 6: INPUT "sigurni li ste, ~e towa e bukwata? (d/n)";PT$
 870  IF PT$ = "D" OR PT$ = "d" OR PT$ = " THEN " OR PT$ = " STR$ " THEN 900
 880  IF PT$ = "N" OR PT$ = "n" OR PT$ = " OR " OR PT$ = " ERROR " THEN 820
 890  GOTO 860
 900  PRINT : PRINT "slovete w diskowoto ustrojstwo disketa- ta, wyrhu koqto }e se syzdawa spisykyt, i natisnete <RETURN> !"
 910  PRINT : FLASH : PRINT "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
 920  PRINT "!";: NORMAL : PRINT  SPC( 15)"wnimanie" SPC( 15): FLASH : PRINT "!";
 930  PRINT "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";: NORMAL 
 940  PRINT "ako wyrhu disketata sy}estwuwa spisyk s dumi, koito zapo~wat s wywedenata ot wasbukwa, tozi spisyk }e byde iztrit!!!"
 950  PRINT "w slu~aj, ~e takyw spisyk sy}estwuwa i  ne velaete da go uni}ovite, natisnete   klawi{, razli~en ot <RETURN>. ";: GET PL$
 960  IF PL$ =  CHR$ (13) THEN 980
 970  GOTO 10
 980  PRINT M$D$"OPEN D"PY$
 985  PRINT D$"UNLOCK D"PY$
 990  PRINT D$"DELETE D"PY$
 1000  PRINT D$"OPEN D"PY$
 1010  GOTO 10
 1011  REM katalog
 1012  HOME : PRINT : PRINT D$"CATALOG"
 1013  GET NC$: GOTO 1
 1015  REM obu~enie
 1016 V = 4
 1017  PRINT : PRINT  CHR$ (4)"RUN DEUTSCH"
 1018  END 
 1020  REM kraj
 1030  TEXT : NORMAL : HOME 
 1040  END 
 1050  REM obrabotka na gre{ki
 1060  IF  PEEK (222) = 4 THEN 1140
 1070  IF  PEEK (222) = 5 THEN 1150
 1080  IF  PEEK (222) = 6 THEN 1160
 1090  IF  PEEK (222) = 8 THEN 1240
 1100  IF  PEEK (222) = 9 THEN 1250
 1110  IF  PEEK (222) = 10 THEN 1260
 1120  IF  PEEK (222) = 16 THEN 15
 1125  IF  PEEK (222) = 53 THEN 1330
 1130  RUN 
 1140  PRINT : PRINT "disketata e za}itena ot zapis.": GET NC$: CLEAR : GOTO 1
 1150  PRINT : PRINT "tazi duma ne figurira w spisyka, kojto  se namira na tazi disketa."
 1151  GET NC$: PRINT 
 1152  PRINT M$D$"CLOSE D"B$
 1154  CLEAR : GOTO 1
 1160  IF V = 0 THEN  PRINT : PRINT "wyrhu tazi disketa ne sy}estwuwa spisyk,w kojto da figurira tazi duma.": GET NC$: GOTO 0
 1170  IF V = 4 THEN  PRINT : PRINT "wyrhu tazi disketa ne se namira obu~awa-}ata programa.": GET NC$: GOTO 0
 1180  IF V = 1 THEN  PRINT : PRINT "wyrhu tazi disketa ne sy}estwuwa spisyk, wyrhu kojto da se zapi{e dumata i syot-wetnoto i zna~enie."
 1185  IF BR = 1 THEN  GET NC$: GOTO 0
 1190  PRINT "izrazyt e: "DI$(XY)
 1195  IF XY = BR - 1 THEN  GET NC$: GOTO 0
 1200  PRINT "}e velaete li zapis na ostanalite dumi? (d/n) ";: GET NC$
 1210  IF NC$ = "D" OR NC$ = "d" OR NC$ = " THEN " OR NC$ = " STR$ " THEN 735
 1220  IF NC$ = "N" OR NC$ = "n" OR NC$ = " OR " OR NC$ = " ERROR " THEN  CLEAR : GOTO 0
 1230  GOTO 1200
 1240  PRINT : PRINT "prowerete systoqnieto na disketata i    diskowoto ustrojstwo.": GET NC$: RESUME 
 1250  PRINT : PRINT "disketata e pylna i wyrhu neq powe~e ne move da se zapiswa.": GET NC$: CLEAR : GOTO 1
 1260  GOTO 1280
 1280  PRINT : PRINT "spisykyt e za}iten sre}u wywevdane na   nowi dumi.": PRINT "izrazyt e: ";DI$(XY)
 1285  IF BR = 1 THEN  GET NC$: GOTO 0
 1290  PRINT "velaete li da se zapi{at ostanalite     dumi? (d/n) ";: GET NC$
 1300  IF NC$ = "D" OR NC$ = "d" OR NC$ = " THEN " OR NC$ = " STR$ " THEN 735
 1310  IF NC$ = "N" OR NC$ = "n" OR NC$ = " OR " OR NC$ = " ERROR " THEN  CLEAR : GOTO 0
 1320  GOTO 1290
 1330  IF VV = 0 THEN A$ = "":AA$ = "": GOTO 90
 1340  IF VV = 1 THEN DI$(S) = "":E$ = "": PRINT ":";
 1345  IF VL = 1 THEN 620
 1346 VL = 0
 1347  GOTO 460
 63999  REM  DIMITER DIMITROV            BURGAS, (C) 1991
