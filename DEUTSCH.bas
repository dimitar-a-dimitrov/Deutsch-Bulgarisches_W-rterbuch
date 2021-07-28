5  ONERR  GOTO 530
 10  FOR I = 770 TO 790
 20  READ A: POKE I,A: NEXT 
 22  RESTORE 
 30  DATA 173,48,192,136,208,5,206,1,3,240,9,202,208,245,174,0,3,76,2,3,96
 40  HOME 
 50  PRINT "******** DEUTSCH ********"
 60  PRINT "        *********"
 70  PRINT : PRINT : PRINT 
 80  REM МЛАД КОНСТРУКТОР, СОФИЯ, 1984, МВ
 85  REM  VERSION 2.0 BY              DIMITER DIMITROV (C)1991
 90  PRINT "ВЪВЕДЕТЕ БРОЯ НА ДУМИТЕ, КОИТО ИСКАТЕ   ДА НАУЧИТЕ"
 91  INPUT "N= ";N
 100 M = 11 * N
 110  DIM D$(M),D1$(M),E$(M)
 120  HOME 
 130  PRINT "ВЪВЕДЕТЕ ДУМИТЕ, КОИТО ИСКАТЕ ДА НАУЧИТЕ"
 140  PRINT "DEUTSCH=БЪЛГАРСКИ": PRINT 
 150  FOR Z = 1 TO N
 151  VTAB Z + 5: HTAB 1
 152  GET AA$
 153  IF AA$ =  CHR$ (13) OR AA$ =  CHR$ (141) THEN  PRINT AA$;: GOTO 160
 154  IF AA$ =  CHR$ (8) OR AA$ =  CHR$ (136) THEN 600
 155  IF AA$ =  CHR$ (9) OR AA$ =  CHR$ (10) OR AA$ =  CHR$ (11) OR AA$ =  CHR$ (21) THEN 152
 156  IF AA$ =  CHR$ (137) OR AA$ =  CHR$ (138) OR AA$ =  CHR$ (139) OR AA$ =  CHR$ (149) THEN 152
 157 D$(Z) = D$(Z) + AA$
 158  PRINT AA$;
 159  GOTO 152
 160  VTAB Z + 5: HTAB 12: PRINT "=";
 161  GET AA$
 162  IF AA$ =  CHR$ (13) OR AA$ =  CHR$ (141) THEN  PRINT AA$;: GOTO 170
 163  IF AA$ =  CHR$ (8) OR AA$ =  CHR$ (136) THEN 640
 164  IF AA$ =  CHR$ (9) OR AA$ =  CHR$ (10) OR AA$ =  CHR$ (11) OR AA$ =  CHR$ (21) THEN 161
 165  IF AA$ =  CHR$ (137) OR AA$ =  CHR$ (138) OR AA$ =  CHR$ (139) OR AA$ =  CHR$ (149) THEN 161
 166 D1$(Z) = D1$(Z) + AA$
 167  PRINT AA$;
 168  GOTO 161
 170  NEXT Z
 180  FOR I = 1 TO M:K = 0
 190  HOME 
 200  IF F$ = D$(I) THEN K = K + 1
 210  IF K = 10 THEN 290
 220  PRINT D1$(I)
 230  IF D$(I) = "" THEN  GOTO 300
 239 E$(I) = ""
 240  GET AA$
 241  IF AA$ =  CHR$ (13) OR AA$ =  CHR$ (141) THEN 250
 242  IF AA$ =  CHR$ (8) OR AA$ =  CHR$ (136) THEN 560
 243  IF AA$ =  CHR$ (9) OR AA$ =  CHR$ (10) OR AA$ =  CHR$ (11) OR AA$ =  CHR$ (21) THEN 240
 244  IF AA$ =  CHR$ (137) OR AA$ =  CHR$ (138) OR AA$ =  CHR$ (139) OR AA$ =  CHR$ (149) THEN 240
 245 E$(I) = E$(I) + AA$
 246  PRINT AA$;
 247  GOTO 240
 250  IF E$(I) >  < D$(I) THEN  GOTO 380
 260  PRINT : PRINT "БРАВО !": PRINT 
 280  GOSUB 440
 290  NEXT I
 300  PRINT "ВИЕ СТЕ УСВОИЛИ ДУМИТЕ !"
 310  PRINT  CHR$ (4)"RUN DBW"
 320  PRINT "ТРЯБВА ДА УЧИТЕ ПОВЕЧЕ !"
 330  FOR C = 1 TO 3000: NEXT C
 340  FOR L = N + 1 TO M
 350 D$(L) = "":D1$(L) = "": NEXT L
 360 L = N:J = 0:F$ = ""
 370  GOTO 180
 380  PRINT : PRINT "ГРЕШКА": PRINT 
 390  GOSUB 490
 400 J = J + 1
 410 L = N + J: IF L = M THEN  GOTO 320
 420 D$(L) = D$(I):D1$(L) = D1$(I):F$ = D$(I)
 430  GOTO 190
 440  FOR X = 1 TO 5
 450  RESTORE 
 456  READ B,C: POKE 768,B: POKE 769,C
 460  DATA 42,100,42,100,42,100,24,80,31,250
 470  CALL 770: NEXT X
 480  RETURN 
 490  FOR Y = 1 TO 50
 500 SOUND =  PEEK ( - 16336)
 510  NEXT Y
 520  RETURN 
 530  IF  PEEK (222) = 6 THEN  PRINT : PRINT "СЛОЖЕТЕ ДИСКЕТАТА С ПРОГРАМАТА РЕЧНИК И НАТИСНЕТЕ КЛАВИШ": GET NC$: PRINT : PRINT  CHR$ (4)"RUN DBW"
 540  IF  PEEK (222) = 8 THEN  PRINT : PRINT "ПРОВЕРЕТЕ СЪСТОЯНИЕТО НА ДИСКЕТАТА И    ДИСКОВОТО УСТРОЙСТВО.": GET NC$: PRINT : PRINT  CHR$ (4)"RUN DBW"
 550  END 
 560  IF  LEN (E$(I)) = 0 THEN 240
 570  PRINT AA$;
 580 E$(I) =  MID$ (E$(I),1, LEN (E$(I)) - 1)
 590  GOTO 240
 600  IF  LEN (D$(Z)) = 0 THEN 152
 610  PRINT AA$;
 620 D$(Z) =  MID$ (D$(Z),1, LEN (D$(Z)) - 1)
 630  GOTO 152
 640  IF  LEN (D1$(Z)) = 0 THEN 161
 650  PRINT AA$;
 660 D1$(Z) =  MID$ (D1$(Z),1, LEN (D1$(Z)) - 1)
 670  GOTO 161
