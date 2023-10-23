10 REM SUPER STARTREK - MAY 16,1978 - REQUIRES 24K MEMORY
30 REM
40 REM ****        **** STAR TREK ****        ****
50 REM **** SIMULATION OF A MISSION OF THE STARSHIP ENTERPRISE,
60 REM **** AS SEEN ON THE STAR TREK TV SHOW.
70 REM **** ORIGIONAL PROGRAM BY MIKE MAYFIELD, MODIFIED VERSION
80 REM **** PUBLISHED IN DEC'S "101 BASIC GAMES", BY DAVE AHL.
90 REM **** MODIFICATIONS TO THE LATTER (PLUS DEBUGGING) BY BOB
100 REM *** LEEDOM - APRIL & DECEMBER 1974,
110 REM *** WITH A LITTLE HELP FROM HIS FRIENDS . . .
120 REM *** COMMENTS, EPITHETS, AND SUGGESTIONS SOLICITED --
130 REM *** SEND TO:  R. C. LEEDOM
140 REM ***           WESTINGHOUSE DEFENSE & ELECTRONICS SYSTEMS CNTR.
150 REM ***           BOX 746, M.S. 338
160 REM ***           BALTIMORE, MD  21203
170 REM ***
180 REM *** CONVERTED TO MICROSOFT 8 K BASIC 3/16/78 BY JOHN GORDERS
190 REM *** LINE NUMBERS FROM VERSION STREK7 OF 1/12/75 PRESERVED AS
200 REM *** MUCH AS POSSIBLE WHILE USING MULTIPLE STATEMENTS PER LINE
205 REM *** SOME LINES ARE LONGER THAN 72 CHARACTERS; THIS WAS DONE
210 REM *** BY USING "?" INSTEAD OF "PRINT" WHEN ENTERING LINES
215 REM ***
220 PRINT:PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
221 PRINT"                                    ,------*------,"
222 PRINT"                    ,-------------   '---  ------'"
223 PRINT"                     '-------- --'      / /"
224 PRINT"                         ,---' '-------/ /--,"
225 PRINT"                          '----------------'":PRINT
226 PRINT"                    THE USS ENTERPRISE --- NCC-1701"
227 PRINT:PRINT : PRINT : PRINT : PRINT
260 REM CLEAR 600
270 Z$="                         "
330 DIM G(8,8),C(9,2),K(3,3),N(3),Z(8,8),D(8)
370 T=INT(RND(1)*20+20)*100:T0 = T : T9 = 25 + INT(RND(1) * 10) : D0 = 0 : E = 3000 : E0 = E
440 P=10:P0 = P : S9 = 200 : S = 0 : B9 = 2 : K9 = 0 : X$ = "" : X0$ = " IS "
470 DEF FND(D)=SQR((K(I,1)-S1)^2+(K(I,2)-S2)^2)
475 DEF FNR(R)=INT(RND(R)*7.98+1.01)
480 REM INITIALIZE ENTERPRIZE'S POSITION
490 Q1=FNR(1):Q2 = FNR(1) : S1 = FNR(1) : S2 = FNR(1)
530 FORI=1TO9:C(I, 1) = 0 : C(I, 2) = 0 : NEXTI
540 C(3,1)=-1
541 C(2, 1) = -1 
542 C(4, 1) = -1 
543 C(4, 2) = -1
544 C(5, 2) = -1 
545 C(6, 2) = -1
600 C(1,2)=1:C(2, 2) = 1 : C(6, 1) = 1 : C(7, 1) = 1 : C(8, 1) = 1 : C(8, 2) = 1 : C(9, 2) = 1
670 FORI=1TO8:D(I) = 0 : NEXTI
710 A1$="NAVSRSLRSPHATORSHEDAMCOMXXX"

720 REM ************************************************
810 REM SETUP WHAT EXISTS IN GALAXY . . .
815 REM K3= # KLINGONS  B3= # STARBASES  S3 = # STARS
816 REM K9= TOTAL # KLINGONS  B9= TOTAL # STARBASES
720 REM ************************************************
820 FOR I=1 TO 8
821     FOR J = 1 TO 8 
822         K3 = 0 
823         Z(I, J) = 0 
824         R1 = RND(1)
850         IF R1 > .98 THEN K3=3 :K9 = K9 + 3 : GOTO 980
860         IF R1 > .95 THEN K3=2: K9 = K9 + 2 : GOTO 980
870         IF R1 > .80 THEN K3=1: K9 = K9 + 1
980         B3=0
REM
981         IF RND(1) > .96 THEN B3=1:B9 = B9 + 1
1040        G(I,J) = K3*100+B3*10+FNR(1)
1041    NEXT J 
1042 NEXT I 
1043 IF K9 > T9 THEN T9 = K9 + 1
1100 IFB9<>0THEN1200
1150 IFG(Q1,Q2)<200THENG(Q1,Q2)=G(Q1,Q2)+120:K9 = K9 + 1
1160 B9=1:G(Q1, Q2) = G(Q1, Q2) + 10 : Q1 = FNR(1) : Q2 = FNR(1)
1200 K7=K9
1201 IF B9 <> 1 THEN X$="S"
1202 X0$=" ARE "
1230 PRINT"YOUR ORDERS ARE AS FOLLOWS:"
1240 PRINT"     DESTROY THE";K9;"KLINGON WARSHIPS WHICH HAVE INVADED"
1252 PRINT"   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS"
1260 PRINT"   ON STARDATE";T0+T9;"  THIS GIVES YOU";T9;"DAYS.  THERE";X0$
1272 PRINT"  ";B9;"STARBASE";X$;" IN THE GALAXY FOR RESUPPLYING YOUR SHIP"
1280 PRINT
1300 I=RND(1)

1301 REM ******************************************************************
1310 REM HERE ANY TIME NEW QUADRANT ENTERED
1311 REM ******************************************************************
1320 Z4=Q1:Z5=Q2:K3=0:B3=0:S3=0:G5=0:D4=.5*RND(1):Z(Q1,Q2)=G(Q1,Q2)
1390 IFQ1<1ORQ1>8ORQ2<1ORQ2>8THEN1600
1430 GOSUB 9030:PRINT:IF T0<>T THEN 1490
1460 PRINT"YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED"
1470 PRINT"IN THE GALACTIC QUADRANT, '";G2$;"'.":GOTO 1500


1490 PRINT"NOW ENTERING ";G2$;" QUADRANT . . ."
1500 PRINT:K3=INT(G(Q1,Q2)*.01):B3=INT(G(Q1,Q2)*.1)-10*K3
1540 S3=G(Q1,Q2)-100*K3-10*B3:IFK3=0THEN1590
1560 PRINT"COMBAT AREA      CONDITION RED":IFS>200THEN1590
1580 PRINT"   SHIELDS DANGEROUSLY LOW"
1590 FORI=1TO3:K(I,1)=0:K(I,2)=0:NEXTI
1600 FORI=1TO3:K(I,3)=0:NEXTI:Q$=Z$+Z$+Z$+Z$+Z$+Z$+Z$+LEFT$(Z$,17)

1601 REM ******************************************************************
1660 REM POSITION ENTERPRISE IN QUADRANT, THEN PLACE "K3" KLINGONS, &
1670 REM "B3" STARBASES, & "S3" STARS ELSEWHERE.
1671 REM ******************************************************************
1680 A$="<*>":Z1=S1:Z2=S2:GOSUB8670:IFK3<1THEN1820
1720 FORI=1TOK3:GOSUB8590:A$="+K+":Z1=R1:Z2=R2
1780 GOSUB8670:K(I,1)=R1:K(I,2)=R2:K(I,3)=S9*(0.5+RND(1)):NEXTI
1820 IFB3<1THEN1910
1880 GOSUB8590:A$=">!<":Z1=R1:B4=R1:Z2=R2:B5=R2:GOSUB8670
1910 FORI=1TOS3:GOSUB8590:A$=" * ":Z1=R1:Z2=R2:GOSUB8670:NEXTI
1980 GOSUB 6430

1981 REM ******************************************************************
1982 REM CHECK FOR FATAL ERRORS
1983 REM ******************************************************************
1990 IF S+E > 10 THEN IF E > 10 OR D(7)=0 THEN 2060 REM ****** GET COMMAND **********
2020 PRINT
2021 PRINT " ** FATAL ERROR **   YOU'VE JUST STRANDED YOUR SHIP IN "
2030 PRINT "SPACE"
2031 PRINT "YOU HAVE INSUFFICIENT MANEUVERING ENERGY,";
2040 PRINT " AND SHIELD CONTROL"
2041 PRINT "IS PRESENTLY INCAPABLE OF CROSS";
2050 PRINT "-CIRCUITING TO ENGINE ROOM!!"
2051 GOTO 6220


2051 REM ******************************************************************
2052 REM GET COMMAND
2053 REM ******************************************************************
2060 INPUT"COMMAND";A$
2080 FOR I=1 TO 9
2081    IF LEFT$(A$,3) <> MID$(A1$,3*I-2,3) THEN 2160   REM ****** EXIT LOOP ******
2140    ON I GOTO 2300,1980,4000,4260,4700,5530,5690,7290,6270
2160 NEXT I
2161 PRINT"ENTER ONE OF THE FOLLOWING:"
2180 PRINT"  NAV  (TO SET COURSE)"
2190 PRINT"  SRS  (FOR SHORT RANGE SENSOR SCAN)"
2200 PRINT"  LRS  (FOR LONG RANGE SENSOR SCAN)"
2210 PRINT"  PHA  (TO FIRE PHASERS)"
2220 PRINT"  TOR  (TO FIRE PHOTON TORPEDOES)"
2230 PRINT"  SHE  (TO RAISE OR LOWER SHIELDS)"
2240 PRINT"  DAM  (FOR DAMAGE CONTROL REPORTS)"
2250 PRINT"  COM  (TO CALL ON LIBRARY-COMPUTER)"
2260 PRINT"  XXX  (TO RESIGN YOUR COMMAND)"
2261 PRINT
2262 GOTO 1990 REM **** CHECK FOR FATAL ERRORS

2261 REM ******************************************************************
2290 REM COURSE CONTROL BEGINS HERE
2291 REM ******************************************************************
2300 INPUT "COURSE (0-9)"; C1
2301 IF C1=9 THEN C1=1
2310 IF C1 >= 1 AND C1 < 9 THEN 2350
2330 PRINT "   LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"
2331 GOTO 1990 REM **** CHECK FOR FATAL ERRORS

2331 REM *****************************************************************
2332 REM WARP FACTOR SELECTION
2333 REM *****************************************************************
2350 X$="8"
2351 IF D(1) < 0 THEN X$="0.2"
2360 PRINT "WARP FACTOR (0-";X$;")";
2361 INPUT W1
2362 IF D(1) < 0 AND W1 > .2 THEN 2470
2380 IF W1 > 0 AND W1 <= 8 THEN 2490
2390 IF W1=0 THEN 1990
2420 PRINT "   CHIEF ENGINEER SCOTT REPORTS 'THE ENGINES WON'T TAKE";
2430 PRINT " WARP ";W1;"!'":GOTO1990
2470 PRINT "WARP ENGINES ARE DAMAGED.  MAXIUM SPEED = WARP 0.2":GOTO1990
2490 N = INT(W1*8+.5)
2491 IF E-N >= 0 THEN 2590
2500 PRINT "ENGINEERING REPORTS   'INSUFFICIENT ENERGY AVAILABLE"
2510 PRINT"                       FOR MANEUVERING AT WARP";W1;"!'"
2530 IF S < N-E OR D(7) < 0 THEN 1990
2550 PRINT "DEFLECTOR CONTROL ROOM ACKNOWLEDGES";S;"UNITS OF ENERGY"
2560 PRINT "                         PRESENTLY DEPLOYED TO SHIELDS."
2570 GOTO 1990 REM **** CHECK FOR FATAL ERRORS

2571 REM **********************************************************
2580 REM KLINGONS MOVE/FIRE ON MOVING STARSHIP . . .
2581 REM **********************************************************
2590 FOR I = 1 TO K3
2591    IF K(I,3)=0 THEN 2700
2610    A$="   "
2611    Z1=K(I,1)
2612    Z2=K(I,2)
2613    GOSUB 8670
2614    GOSUB 8590
2660    K(I,1)=Z1:
2661    K(I,2)=Z2
2662    A$="+K+"
2663    GOSUB 8670
2700 NEXT I
2701 GOSUB 6000
2702 D1=0
2702 D6=W1
2703 IF W1 >= 1 THEN D6=1
2770 FOR I=1 TO 8
2771    IF D(I) >= 0 THEN 2880
2790    D(I) = D(I) + D6
2791    IF D(I) > -.1 AND D(I) < 0 THEN D(I)= -.1 : GOTO 2880
2800    IF D(I) < 0 THEN 2880
2810    IF D1 <> 1 THEN D1=1 : PRINT "DAMAGE CONTROL REPORT:  ";
2840    PRINTTAB(8); : R1=I : GOSUB 8790 : PRINT G2$;" REPAIR COMPLETED."
2880 NEXTI
2881 IFRND(1)>.2THEN3070
2910 R1=FNR(1):IFRND(1)>=.6THEN3000
2930 D(R1)=D(R1)-(RND(1)*5+1):PRINT"DAMAGE CONTROL REPORT:  ";
2960 GOSUB8790:PRINTG2$;" DAMAGED":PRINT:GOTO3070
3000 D(R1)=D(R1)+RND(1)*3+1:PRINT"DAMAGE CONTROL REPORT:  ";
3030 GOSUB8790:PRINTG2$;" STATE OF REPAIR IMPROVED":PRINT

3031 REM **********************************************************
3060 REM BEGIN MOVING STARSHIP
3061 REM **********************************************************
3070 A$="   ":Z1=INT(S1):Z2=INT(S2):GOSUB8670
3110 X1=C(C1,1)+(C(C1+1,1)-C(C1,1))*(C1-INT(C1)):X=S1:Y=S2
3140 X2=C(C1,2)+(C(C1+1,2)-C(C1,2))*(C1-INT(C1)):Q4=Q1:Q5=Q2
3170 FOR I=1 TO N
3171    S1 = S1+X1
3172    S2 = S2+X2
3173    IF S1 < 1 OR S1 >=9 OR S2 < 1 OR S2 >= 9 THEN 3500
3240    S8 = INT(S1) * 24 + INT(S2) * 3 - 26
3241    IF MID$(Q$,S8,2) ="  " THEN 3360
3320    S1 = INT(S1-X1)
3321    S2 = INT(S2-X2)
3322    PRINT"WARP ENGINES SHUT DOWN AT ";
3350    PRINT"SECTOR";S1;",";S2;"DUE TO BAD NAVAGATION":GOTO3370
3360 NEXT I
3361 S1=INT(S1):S2=INT(S2)
3370 A$="<*>":Z1=INT(S1):Z2=INT(S2):GOSUB8670:GOSUB3910:T8=1
3430 IFW1<1THENT8=.1*INT(10*W1)
3450 T=T+T8:IFT>T0+T9THEN6220
3470 REM SEE IF DOCKED, THEN GET COMMAND
3480 GOTO 1980

3481 REM **********************************************************
3490 REM EXCEEDED QUADRANT LIMITS
3491 REM **********************************************************
3500 X=8*Q1+X+N*X1:Y=8*Q2+Y+N*X2:Q1=INT(X/8):Q2=INT(Y/8):S1=INT(X-Q1*8)
3550 S2=INT(Y-Q2*8):IFS1=0THENQ1=Q1-1:S1=8
3590 IFS2=0THENQ2=Q2-1:S2=8
3620 X5=0:IFQ1<1THENX5=1:Q1=1:S1=1
3670 IFQ1>8THENX5=1:Q1=8:S1=8
3710 IFQ2<1THENX5=1:Q2=1:S2=1
3750 IFQ2>8THENX5=1:Q2=8:S2=8
3790 IFX5=0THEN3860
3800 PRINT"LT. UHURA REPORTS MESSAGE FROM STARFLEET COMMAND:"
3810 PRINT"  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER"
3820 PRINT"  IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.'"
3830 PRINT"CHIEF ENGINEER SCOTT REPORTS  'WARP ENGINES SHUT DOWN"
3840 PRINT"  AT SECTOR";S1;",";S2;"OF QUADRANT";Q1;",";Q2;".'"
3850 IFT>T0+T9THEN6220
3860 IF8*Q1+Q2=8*Q4+Q5THEN3370
3870 T=T+1:GOSUB3910:GOTO1320

3871 REM **********************************************************
3900 REM MANEUVER ENERGY S/R **
3901 REM **********************************************************
3910 E=E-N-10:IFE>=0THENRETURN
3930 PRINT"SHIELD CONTROL SUPPLIES ENERGY TO COMPLETE THE MANEUVER."
3940 S=S+E:E=0:IFS<=0THENS=0
3980 RETURN

3981 REM *********************************************************
3990 REM LONG RANGE SENSOR SCAN CODE
39991 REM *********************************************************
4000 IFD(3)<0THENPRINT"LONG RANGE SENSORS ARE INOPERABLE":GOTO1990
4030 PRINT"LONG RANGE SCAN FOR QUADRANT";Q1;",";Q2
4040 O1$="-------------------":PRINTO1$
4060 FOR I=Q1-1 TO Q1+1
4061    N(1)=-1
4062    N(2)=-2
4063    N(3)=-3
4064    FOR J = Q2-1 TO Q2+1
4120        IF I>0 AND I<9 AND J>0 AND J<9 THEN N(J-Q2+2)=G(I,J):Z(I,J)=G(I,J)
4180    NEXTJ
4181    FOR L = 1 TO 3
4182        PRINT": "
4183        IF N(L) < 0 THEN PRINT "*** "; : GOTO4230
4210        PRINT RIGHT$(STR$(N(L)+1000),3);" ";
4230    NEXTL
4231    PRINT":"
4232    PRINT O1$
4233 NEXT I
4234 GOTO 1990

4231 REM *********************************************************
4250 REM PHASER CONTROL CODE BEGINS HERE
4251 REM *********************************************************
4260 IFD(4)<0THENPRINT"PHASERS INOPERATIVE":GOTO1990
4265 IFK3>0THEN4330
4270 PRINT"SCIENCE OFFICER SPOCK REPORTS  'SENSORS SHOW NO ENEMY SHIPS"
4280 PRINT"                                IN THIS QUADRANT'":GOTO1990
4330 IFD(8)<0THENPRINT"COMPUTER FAILURE HAMPERS ACCURACY"
4350 PRINT"PHASERS LOCKED ON TARGET;  ";
4360 PRINT"ENERGY AVAILABLE =";E;"UNITS"
4370 INPUT"NUMBER OF UNITS TO FIRE";X:IFX<=0THEN1990
4400 IFE-X<0THEN4360
4410 E=E-X:IFD(7)<0THENX=X*RND(1)
4450 H1=INT(X/K3):FORI=1TO3:IFK(I,3)<=0THEN4670
4480 H=INT((H1/FND(0))*(RND(1)+2)):IFH>.15*K(I,3)THEN4530
4500 PRINT"SENSORS SHOW NO DAMAGE TO ENEMY AT ";K(I,1);",";K(I,2):GOTO4670
4530 K(I,3)=K(I,3)-H:PRINTH;"UNIT HIT ON KLINGON AT SECTOR";K(I,1);",";
4550 PRINTK(I,2):IFK(I,3)<=0THENPRINT"*** KLINGON DESTROYED ***":GOTO4580
4560 PRINT"   (SENSORS SHOW";K(I,3);"UNITS REMAINING)":GOTO4670
4580 K3=K3-1:K9=K9-1:Z1=K(I,1):Z2=K(I,2):A$="   ":GOSUB8670
4650 K(I,3)=0:G(Q1,Q2)=G(Q1,Q2)-100:Z(Q1,Q2)=G(Q1,Q2):IFK9<=0THEN6370
4670 NEXTI:GOSUB6000:GOTO1990
4690 REM PHOTON TORPEDO CODE BEGINS HERE
4700 IFP<=0THENPRINT"ALL PHOTON TORPEDOES EXPENDED":GOTO 1990
4730 IFD(5)<0THENPRINT"PHOTON TUBES ARE NOT OPERATIONAL":GOTO1990
4760 INPUT"PHOTON TORPEDO COURSE (1-9)";C1:IFC1=9THENC1=1
4780 IFC1>=1ANDC1<9THEN4850
4790 PRINT"ENSIGN CHEKOV REPORTS,  'INCORRECT COURSE DATA, SIR!'"
4800 GOTO1990
4850 X1=C(C1,1)+(C(C1+1,1)-C(C1,1))*(C1-INT(C1)):E=E-2:P=P-1
4860 X2=C(C1,2)+(C(C1+1,2)-C(C1,2))*(C1-INT(C1)):X=S1:Y=S2
4910 PRINT"TORPEDO TRACK:"
4920 X=X+X1:Y=Y+X2:X3=INT(X+.5):Y3=INT(Y+.5)
4960 IFX3<1ORX3>8ORY3<1ORY3>8THEN5490
5000 PRINT"               ";X3;",";Y3:A$="   ":Z1=X:Z2=Y:GOSUB8830
5050 IFZ3<>0THEN4920
5060 A$="+K+":Z1=X:Z2=Y:GOSUB8830:IFZ3=0THEN5210
5110 PRINT"*** KLINGON DESTROYED ***":K3=K3-1:K9=K9-1:IFK9<=0THEN6370
5150 FORI=1TO3:IFX3=K(I,1)ANDY3=K(I,2)THEN5190
5180 NEXTI:I=3
5190 K(I,3)=0:GOTO5430
5210 A$=" * ":Z1=X:Z2=Y:GOSUB8830:IFZ3=0THEN5280
5260 PRINT"STAR AT";X3;",";Y3;"ABSORBED TORPEDO ENERGY.":GOSUB6000:GOTO1990
5280 A$=">!<":Z1=X:Z2=Y:GOSUB8830:IFZ3=0THEN4760
5330 PRINT" *** STARBASE DESTROYED ***":B3=B3-1:B9=B9-1
5360 IFB9>0ORK9>T-T0-T9THEN5400
5370 PRINT" THAT DOES IT, CAPTAIN!!  YOU ARE HEREBY RELIEVED OF COMMAND"
5380 PRINT"AND SENTENCED TO 99 STARDATES AT HARD LABOR ON CYGNUS 12!!"
5390 GOTO 6270
5400 PRINT"STARFLEET COMMAND REVIEWING YOUR RECORD TO CONSIDER"
5410 PRINT"COURT MARTIAL!":D0=0
5430 Z1=X:Z2=Y:A$="   ":GOSUB8670
5470 G(Q1,Q2)=K3*100+B3*10+S3:Z(Q1,Q2)=G(Q1,Q2):GOSUB6000:GOTO1990
5490 PRINT"TORPEDO MISSED":GOSUB6000:GOTO1990
5520 REM SHIELD CONTROL
5530 IFD(7)<0THENPRINT"SHIELD CONTROL INOPERABLE":GOTO1990
5560 PRINT"ENERGY AVAILABLE =";E+S;:INPUT"NUMBER OF UNITS TO SHIELDS";X
5580 IFX<0ORS=XTHENPRINT"<SHIELDS UNCHANGED>":GOTO1990
5590 IFX<=E+STHEN5630
5600 PRINT"SHIELD CONTROL REPORTS  'THIS IS NOT THE FEDERATION TREASURY.'"
5610 PRINT"<SHIELDS UNCHANGED>":GOTO1990
5630 E=E+S-X:S=X:PRINT"DEFLECTOR CONTROL ROOM REPORT:"
5660 PRINT"  'SHIELDS NOW AT";INT(S);"UNITS PER YOUR COMMAND.'":GOTO1990
5680 REM DAMAGE CONTROL
5690 IFD(6)>=0THEN5910
5700 PRINT"DAMAGE CONTROL REPORT NOT AVAILABLE":IFD0=0THEN1990
5720 D3=0:FORI=1TO8:IFD(I)<0THEND3=D3+.1
5760 NEXTI:IFD3=0THEN1990
5780 PRINT:D3=D3+D4:IFD3>=1THEND3=.9
5810 PRINT"TECHNICIANS STANDING BY TO EFFECT REPAIRS TO YOUR SHIP;"
5820 PRINT"ESTIMATED TIME TO REPAIR:";.01*INT(100*D3);"STARDATES"
5840 INPUT "WILL YOU AUTHORIZE THE REPAIR ORDER (Y/N)";A$
5860 IFA$<>"Y"THEN 1990
5870 FORI=1TO8:IFD(I)<0THEND(I)=0
5890 NEXTI:T=T+D3+.1
5910 PRINT:PRINT"DEVICE             STATE OF REPAIR":FORR1=1TO8
5920 GOSUB8790:PRINTG2$;LEFT$(Z$,25-LEN(G2$));INT(D(R1)*100)*.01
5950 NEXTR1:PRINT:IFD0<>0THEN5720
5980 GOTO 1990
5990 REM KLINGONS SHOOTING
6000 IFK3<=0THENRETURN
6010 IFD0<>0THENPRINT"STARBASE SHIELDS PROTECT THE ENTERPRISE":RETURN
6040 FORI=1TO3:IFK(I,3)<=0THEN6200
6060 H=INT((K(I,3)/FND(1))*(2+RND(1))):S=S-H:K(I,3)=K(I,3)/(3+RND(0))
6080 PRINTH;"UNIT HIT ON ENTERPRISE FROM SECTOR";K(I,1);",";K(I,2)
6090 IFS<=0THEN6240
6100 PRINT"      <SHIELDS DOWN TO";S;" UNITS>":IFH<20THEN6200
6120 IFRND(1)>.6ORH/S<=.02THEN6200
6140 R1=FNR(1):D(R1)=D(R1)-H/S-.5*RND(1):GOSUB8790
6170 PRINT"DAMAGE CONTROL REPORTS ";G2$;" DAMAGED BY THE HIT'"
6200 NEXTI:RETURN
6210 REM END OF GAME
6220 PRINT"IT IS STARDATE";T:GOTO 6270
6240 PRINT:PRINT"THE ENTERPRISE HAS BEEN DESTROYED.  THEN FEDERATION ";
6250 PRINT"WILL BE CONQUERED":GOTO 6220

6251 REM ********************************************************************
6252 REM END OF MISSION
6253 REM ********************************************************************
6270 PRINT"THERE WERE";K9;"KLINGON BATTLE CRUISERS LEFT AT"
6280 PRINT"THE END OF YOUR MISSION."
6290 PRINT:PRINT:IFB9=0THEN6360
6310 PRINT"THE FEDERATION IS IN NEED OF A NEW STARSHIP COMMANDER"
6320 PRINT"FOR A SIMILAR MISSION -- IF THERE IS A VOLUNTEER,"
6330 INPUT"LET HIM STEP FORWARD AND ENTER 'AYE'";A$:IFA$="AYE"THEN10
6360 END

6370 PRINT"CONGRULATIONS, CAPTAIN!  THEN LAST KLINGON BATTLE CRUISER"
6380 PRINT"MENACING THE FDERATION HAS BEEN DESTROYED.":PRINT
6400 PRINT"YOUR EFFICIENCY RATING IS";1000*(K7/(T-T0))^2:GOTO6290

6401 REM ********************************************************************
6420 REM SHORT RANGE SENSOR SCAN & STARTUP SUBROUTINE
6421 REM ********************************************************************
6430 FORI=S1-1TOS1+1:FORJ=S2-1TOS2+1
6450 IFINT(I+.5)<1ORINT(I+.5)>8ORINT(J+.5)<1ORINT(J+.5)>8THEN6540
6490 A$=">!<":Z1=I:Z2=J:GOSUB8830:IFZ3=1THEN6580
6540 NEXTJ:NEXTI:D0=0:GOTO6650
6580 D0=1:C$=" DOCKED":E=E0:P=P0
6620 PRINT" SHIELDS DROPPED FOR DOCKING PURPOSES":S=0:GOTO6720
6650 IFK3>0THENC$=" *RED*":GOTO6720
6660 C$=" GREEN":IFE<E0*.1THENC$="YELLOW"
6720 IFD(2)>=0THEN6770
6730 PRINT:PRINT"*** SHORT RANGE SENSORS ARE OUT ***":PRINT:RETURN
6770 O1$="---------------------------------":PRINTO1$:FORI=1TO8
6820 FORJ=(I-1)*24+1TO(I-1)*24+22STEP3:PRINT" ";MID$(Q$,J,3);:NEXTJ
6830 ONIGOTO6850,6900,6960,7020,7070,7120,7180,7240
6850 PRINT"        STARDATE          ";INT(T*10)*.1:GOTO7260
6900 PRINT"        CONDITION          ";C$:GOTO7260
6960 PRINT"        QUADRANT          ";Q1;",";Q2:GOTO7260
7020 PRINT"        SECTOR            ";S1;",";S2:GOTO7260
7070 PRINT"        PHOTON TORPEDOES  ";INT(P):GOTO7260
7120 PRINT"        TOTAL ENERGY      ";INT(E+S):GOTO7260
7180 PRINT"        SHIELDS           ";INT(S):GOTO7260
7240 PRINT"        KLINGONS REMAINING";INT(K9)
7260 NEXTI:PRINTO1$:RETURN

7261 REM ********************************************************************
7280 REM LIBRARY COMPUTER CODE
7281 REM ********************************************************************
7290 IFD(8)<0THENPRINT"COMPUTER DISABLED":GOTO1990
7320 INPUT"COMPUTER ACTIVE AND AWAITING COMMAND";A: IF A < 0 THEN 1990
7350 PRINT:H8=1: ON A+1 GOTO 7540,7900,8070,8500,8150,7400
7360 PRINT"FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER:"
7370 PRINT"   0 = CUMULATIVE GALACTIC RECORD"
7372 PRINT"   1 = STATUS REPORT"
7374 PRINT"   2 = PHOTON TORPEDO DATA"
7376 PRINT"   3 = STARBASE NAV DATA"
7378 PRINT"   4 = DIRECTION/DISTANCE CALCULATOR"
7380 PRINT"   5 = GALAXY 'REGION NAME' MAP":PRINT:GOTO7320

7381 REM ********************************************************************
7390 REM SETUP TO CHANGE CUM GAL RECORD TO GALAXY MAP
7391 REM ********************************************************************
7400 H8=0:G5=1:PRINT"                        THE GALAXY":GOTO7550

7401 REM ********************************************************************
7530 REM CUM GALACTIC RECORD
7540 REM INPUT"DO YOU WANT A HARDCOPY? IS THE TTY ON (Y/N)";A$
7542 REM IFA$="Y"THENPOKE1229,2:POKE1237,3:NULL1
7543 REM *********************************************************************
7543 PRINT:PRINT"        ";
7544 PRINT"COMPUTER RECORD OF GALAXY FOR QUADRANT";Q1;",";Q2
7546 PRINT
7550 PRINT"       1     2     3     4     5     6     7     8"
7560 O1$="     ----- ----- ----- ----- ----- ----- ----- -----"
7570 PRINTO1$:FORI=1TO8:PRINTI;:IFH8=0THEN7740
7630 FORJ=1TO8:PRINT"   ";:IFZ(I,J)=0THENPRINT"***";:GOTO7720
7700 PRINTRIGHT$(STR$(Z(I,J)+1000),3);
7720 NEXTJ:GOTO7850
7740 Z4=I:Z5=1:GOSUB9030:J0=INT(15-.5*LEN(G2$)):PRINTTAB(J0);G2$;
7800 Z5=5:GOSUB 9030:J0=INT(39-.5*LEN(G2$)):PRINTTAB(J0);G2$;
7850 PRINT:PRINTO1$:NEXTI:PRINT:GOTO1990

7851 REM ******************************************************************
7890 REM STATUS REPORT
7891 REM ******************************************************************
7900 PRINT "   STATUS REPORT:":X$="":IFK9>1THENX$="S"
7940 PRINT"KLINGON";X$;" LEFT: ";K9
7960 PRINT"MISSION MUST BE COMPLETED IN";.1*INT((T0+T9-T)*10);"STARDATES"
7970 X$="S":IFB9<2THENX$="":IFB9<1THEN8010
7980 PRINT"THE FEDERATION IS MAINTAINING";B9;"STARBASE";X$;" IN THE GALAXY"
7990 GOTO5690
8010 PRINT"YOUR STUPIDITY HAS LEFT YOU ON YOUR ON IN"
8020 PRINT"  THE GALAXY -- YOU HAVE NO STARBASES LEFT!":GOTO5690

8021 REM ******************************************************************
8060 REM TORPEDO, BASE NAV, D/D CALCULATOR
8061 REM ******************************************************************
8070 IF K3 <=0 THEN 4270
8080 X$="":IFK3>1THENX$="S"
8090 PRINT"FROM ENTERPRISE TO KLINGON BATTLE CRUSER";X$
8100 H8=0:FORI=1TO3:IFK(I,3)<=0THEN8480
8110 W1=K(I,1):X=K(I,2)
8120 C1=S1:A=S2:GOTO8220

8121 REM ******************************************************************
8122 REM DIRECTION/DISTANCE CALCULATOR
8123 REM ******************************************************************
8150 PRINT"DIRECTION/DISTANCE CALCULATOR:"
8160 PRINT"YOU ARE AT QUADRANT ";Q1;",";Q2;" SECTOR ";S1;",";S2
8170 PRINT"PLEASE ENTER":INPUT"  INITIAL COORDINATES (X,Y)";C1,A
8200 INPUT"  FINAL COORDINATES (X,Y)";W1,X

8220 X=X-A:A=C1-W1:IFX<0THEN8350
8250 IFA<0THEN8410
8260 IFX>0THEN8280
8270 IFA=0THENC1=5:GOTO8290
8280 C1=1
8290 IFABS(A)<=ABS(X)THEN8330
8310 PRINT"DIRECTION =";C1+(((ABS(A)-ABS(X))+ABS(A))/ABS(A)):GOTO8460
8330 PRINT"DIRECTION =";C1+(ABS(A)/ABS(X)):GOTO8460

8350 IFA>0THENC1=3:GOTO8420
8360 IFX<>0THENC1=5:GOTO8290

8410 C1=7
8420 IFABS(A)>=ABS(X)THEN8450
8430 PRINT"DIRECTION =";C1+(((ABS(X)-ABS(A))+ABS(X))/ABS(X)):GOTO8460
8450 PRINT"DIRECTION =";C1+(ABS(X)/ABS(A))
8460 PRINT"DISTANCE =";SQR(X^2+A^2):IFH8=1THEN1990
8480 NEXTI:GOTO1990
8500 IFB3<>0THENPRINT"FROM ENTERPRISE TO STARBASE:":W1=B4:X=B5:GOTO8120
8510 PRINT"MR. SPOCK REPORTS,  'SENSORS SHOW NO STARBASES IN THIS";
8520 PRINT" QUADRANT.'":GOTO1990
8580 REM FIND EMPTY PLACE IN QUADRANT (FOR THINGS)
8590 R1=FNR(1):R2=FNR(1):A$="   ":Z1=R1:Z2=R2:GOSUB8830:IFZ3=0THEN8590
8600 RETURN
8660 REM INSERT IN STRING ARRAY FOR QUADRANT
8670 S8=INT(Z2-.5)*3+INT(Z1-.5)*24+1
8675 IF LEN(A$)<>3THEN PRINT"ERROR":STOP
8680 IFS8=1THENQ$=A$+RIGHT$(Q$,189):RETURN
8690 IFS8=190THENQ$=LEFT$(Q$,189)+A$:RETURN
8700 Q$=LEFT$(Q$,S8-1)+A$+RIGHT$(Q$,190-S8):RETURN

8701 REM ******************************************************************
8780 REM PRINTS DEVICE NAME
8781 REM ******************************************************************
8790 ONR1GOTO8792,8794,8796,8798,8800,8802,8804,8806
8792 G2$="WARP ENGINES":RETURN
8794 G2$="SHORT RANGE SENSORS":RETURN
8796 G2$="LONG RANGE SENSORS":RETURN
8798 G2$="PHASER CONTROL":RETURN
8800 G2$="PHOTON TUBES":RETURN
8802 G2$="DAMAGE CONTROL":RETURN
8804 G2$="SHIELD CONTROL":RETURN
8806 G2$="LIBRARY-COMPUTER":RETURN

8807 REM ******************************************************************
8820 REM STRING COMPARISON IN QUADRANT ARRAY
8821 REM ******************************************************************
8830 Z1=INT(Z1+.5):Z2=INT(Z2+.5):S8=(Z2-1)*3+(Z1-1)*24+1:Z3=0
8890 IFMID$(Q$,S8,3)<>A$THENRETURN
8900 Z3=1:RETURN

8901 REM ******************************************************************
8920 REM <<<< IMPLEMENTED >>>>
9010 REM QUADRANT NAME IN G2$ FROM Z4,Z5 (=Q1,Q2)
9020 REM CALL WITH G5=1 TO GET REGION NAME ONLY
9021 REM ******************************************************************
9030 IF Z5 <= 4 THEN ON Z4 GOTO 9040,9050,9060,9070,9080,9090,9100,9110
9035 GOTO9120
9040 G2$="ANTARES":GOTO9210
9050 G2$="RIGEL":GOTO9210
9060 G2$="PROCYON":GOTO9210
9070 G2$="VEGA":GOTO9210
9080 G2$="CANOPUS":GOTO9210
9090 G2$="ALTAIR":GOTO9210
9100 G2$="SAGITTARIUS":GOTO9210
9110 G2$="POLLUX":GOTO9210
9120 ON Z4 GOTO9130,9140,9150,9160,9170,9180,9190,9200
9130 G2$="SIRIUS":GOTO9210
9140 G2$="DENEB":GOTO9210
9150 G2$="CAPELLA":GOTO9210
9160 G2$="BETELGEUSE":GOTO9210
9170 G2$="ALDEBARAN":GOTO9210
9180 G2$="REGULUS":GOTO9210
9190 G2$="ARCTURUS":GOTO9210
9200 G2$="SPICA"
9210 IFG5<>1THENONZ5GOTO9230,9240,9250,9260,9230,9240,9250,9260
9220 RETURN
9230 G2$=G2$+" I":RETURN
9240 G2$=G2$+" II":RETURN
9250 G2$=G2$+" III":RETURN
9260 G2$=G2$+" IV":RETURN
9270 END