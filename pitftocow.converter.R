# PITF Code to COW Code
# 
# NOTE: Assumes data frame is called 'data' and PITF label is 'code'

data$cowcode <- NA
data$cowcode[data$code=="AFG"] <- 700
data$cowcode[data$code=="ALB"] <- 339
data$cowcode[data$code=="ALG"] <- 615
data$cowcode[data$code=="ANG"] <- 540
data$cowcode[data$code=="ARG"] <- 160
data$cowcode[data$code=="ARM"] <- 371
data$cowcode[data$code=="AUL"] <- 900
data$cowcode[data$code=="AUS"] <- 305
data$cowcode[data$code=="AZE"] <- 373
data$cowcode[data$code=="BAH"] <- 692
data$cowcode[data$code=="BAR"] <- 53
data$cowcode[data$code=="BEL"] <- 211
data$cowcode[data$code=="BEN"] <- 434
data$cowcode[data$code=="BFO"] <- 439
data$cowcode[data$code=="BHM"] <- 31
data$cowcode[data$code=="BHU"] <- 760
data$cowcode[data$code=="BLR"] <- 370
data$cowcode[data$code=="BLZ"] <- 80
data$cowcode[data$code=="BNG"] <- 771
data$cowcode[data$code=="BOL"] <- 145
data$cowcode[data$code=="BOS"] <- 346
data$cowcode[data$code=="BOT"] <- 571
data$cowcode[data$code=="BRA"] <- 140
data$cowcode[data$code=="BUI"] <- 516
data$cowcode[data$code=="BUL"] <- 355
data$cowcode[data$code=="CAM"] <- 811
data$cowcode[data$code=="CAN"] <- 20
data$cowcode[data$code=="CAO"] <- 471
data$cowcode[data$code=="CAP"] <- 402
data$cowcode[data$code=="CEN"] <- 482
data$cowcode[data$code=="CHA"] <- 483
data$cowcode[data$code=="CHL"] <- 155
data$cowcode[data$code=="CHN"] <- 710
data$cowcode[data$code=="COL"] <- 100
data$cowcode[data$code=="COM"] <- 581
data$cowcode[data$code=="CON"] <- 484
data$cowcode[data$code=="COS"] <- 94
data$cowcode[data$code=="CRO"] <- 344
data$cowcode[data$code=="CUB"] <- 40
data$cowcode[data$code=="CYP"] <- 352
data$cowcode[data$code=="CZE"] <- 315
data$cowcode[data$code=="CZR"] <- 316
data$cowcode[data$code=="DEN"] <- 390
data$cowcode[data$code=="DJI"] <- 522
data$cowcode[data$code=="DOM"] <- 42
data$cowcode[data$code=="ZAI"] <- 490
data$cowcode[data$code=="DRV" & data$year<=1975] <- 816
data$cowcode[data$code=="VIE" & data$year>=1976] <- 816
data$cowcode[data$code=="ECU"] <- 130
data$cowcode[data$code=="EGY"] <- 651
data$cowcode[data$code=="EQG"] <- 411
data$cowcode[data$code=="ERI"] <- 531
data$cowcode[data$code=="EST"] <- 366
data$cowcode[data$code=="ETH" & data$year<=1992] <- 530
data$cowcode[data$code=="ETI" & data$year>=1993] <- 530
data$cowcode[data$code=="ETM"] <- 860
data$cowcode[data$code=="FIN"] <- 375
data$cowcode[data$code=="FJI"] <- 950
data$cowcode[data$code=="FRN"] <- 220
data$cowcode[data$code=="GAB"] <- 481
data$cowcode[data$code=="GAM"] <- 420
data$cowcode[data$code=="GDR"] <- 265
data$cowcode[data$code=="GFR" & data$year<=1989] <- 260
data$cowcode[data$code=="GER" & data$year>=1990] <- 260
data$cowcode[data$code=="GHA"] <- 452
data$cowcode[data$code=="GNB"] <- 404
data$cowcode[data$code=="GRC"] <- 350
data$cowcode[data$code=="GUA"] <- 90
data$cowcode[data$code=="GUI"] <- 438
data$cowcode[data$code=="GUY"] <- 110
data$cowcode[data$code=="HAI"] <- 41
data$cowcode[data$code=="HON"] <- 91
data$cowcode[data$code=="HUN"] <- 310
data$cowcode[data$code=="ICE"] <- 395
data$cowcode[data$code=="IND"] <- 750
data$cowcode[data$code=="INS"] <- 850
data$cowcode[data$code=="IRE"] <- 205
data$cowcode[data$code=="IRN"] <- 630
data$cowcode[data$code=="IRQ"] <- 645
data$cowcode[data$code=="ISR"] <- 666
data$cowcode[data$code=="ITA"] <- 325
data$cowcode[data$code=="JAM"] <- 51
data$cowcode[data$code=="JOR"] <- 663
data$cowcode[data$code=="JPN"] <- 740
data$cowcode[data$code=="KEN"] <- 501
data$cowcode[data$code=="KUW"] <- 690
data$cowcode[data$code=="KYR"] <- 703
data$cowcode[data$code=="KZK"] <- 705
data$cowcode[data$code=="LAO"] <- 812
data$cowcode[data$code=="LAT"] <- 367
data$cowcode[data$code=="LBR"] <- 450
data$cowcode[data$code=="LEB"] <- 660
data$cowcode[data$code=="LES"] <- 570
data$cowcode[data$code=="LIB"] <- 620
data$cowcode[data$code=="LIT"] <- 368
data$cowcode[data$code=="MAA"] <- 435
data$cowcode[data$code=="MAC"] <- 343
data$cowcode[data$code=="MAG"] <- 580
data$cowcode[data$code=="MAL"] <- 820
data$cowcode[data$code=="MAS"] <- 590
data$cowcode[data$code=="MAW"] <- 553
data$cowcode[data$code=="MEX"] <- 70
data$cowcode[data$code=="MLD"] <- 359
data$cowcode[data$code=="MLI"] <- 432
data$cowcode[data$code=="MON"] <- 712
data$cowcode[data$code=="MOR"] <- 600
data$cowcode[data$code=="MYA"] <- 775
data$cowcode[data$code=="MZM"] <- 541
data$cowcode[data$code=="NAM"] <- 565
data$cowcode[data$code=="NEP"] <- 790
data$cowcode[data$code=="NEW"] <- 920
data$cowcode[data$code=="NIC"] <- 93
data$cowcode[data$code=="NIG"] <- 475
data$cowcode[data$code=="NIR"] <- 436
data$cowcode[data$code=="NOR"] <- 385
data$cowcode[data$code=="NTH"] <- 210
data$cowcode[data$code=="OMA"] <- 698
data$cowcode[data$code=="PKS" & data$year<=1971] <- 770
data$cowcode[data$code=="PAK" & data$year>=1972] <- 770
data$cowcode[data$code=="PAN"] <- 95
data$cowcode[data$code=="PAR"] <- 150
data$cowcode[data$code=="PER"] <- 135
data$cowcode[data$code=="PHI"] <- 840
data$cowcode[data$code=="PNG"] <- 910
data$cowcode[data$code=="POL"] <- 290
data$cowcode[data$code=="POR"] <- 235
data$cowcode[data$code=="QAT"] <- 694
data$cowcode[data$code=="ROK"] <- 732
data$cowcode[data$code=="RUM"] <- 360
data$cowcode[data$code=="USS" & data$year<=1991] <- 365
data$cowcode[data$code=="RUS" & data$year>=1992] <- 365
data$cowcode[data$code=="RVN"] <- 817
data$cowcode[data$code=="RWA"] <- 517
data$cowcode[data$code=="SAF"] <- 560
data$cowcode[data$code=="SAL"] <- 92
data$cowcode[data$code=="SAU"] <- 670
data$cowcode[data$code=="SEN"] <- 433
data$cowcode[data$code=="SIE"] <- 451
data$cowcode[data$code=="SIN"] <- 830
data$cowcode[data$code=="SLO"] <- 317
data$cowcode[data$code=="SLV"] <- 349
data$cowcode[data$code=="SOL"] <- 940
data$cowcode[data$code=="SOM"] <- 520
data$cowcode[data$code=="SPN"] <- 230
data$cowcode[data$code=="SRI"] <- 780
data$cowcode[data$code=="SUD"] <- 625
data$cowcode[data$code=="SUR"] <- 115
data$cowcode[data$code=="SWA"] <- 572
data$cowcode[data$code=="SWD"] <- 380
data$cowcode[data$code=="SWZ"] <- 225
data$cowcode[data$code=="SYR"] <- 652
data$cowcode[data$code=="TAJ"] <- 702
data$cowcode[data$code=="TAW"] <- 713
data$cowcode[data$code=="TAZ"] <- 510
data$cowcode[data$code=="THI"] <- 800
data$cowcode[data$code=="TKM"] <- 701
data$cowcode[data$code=="TOG"] <- 461
data$cowcode[data$code=="TRI"] <- 52
data$cowcode[data$code=="TUN"] <- 616
data$cowcode[data$code=="TUR"] <- 640
data$cowcode[data$code=="UAE"] <- 696
data$cowcode[data$code=="UGA"] <- 500
data$cowcode[data$code=="UK"] <- 200
data$cowcode[data$code=="UKR"] <- 369
data$cowcode[data$code=="URU"] <- 165
data$cowcode[data$code=="USA"] <- 2
data$cowcode[data$code=="UZB"] <- 704
data$cowcode[data$code=="VEN"] <- 101
data$cowcode[data$code=="YEM" & data$year>=1991] <- 678
data$cowcode[data$code=="YAR" & data$year<=1990] <- 678
data$cowcode[data$code=="YPR"] <- 680
data$cowcode[data$code=="YUG" & data$year<=1991] <- 345
data$cowcode[data$code=="YGS" & data$year>=1992 & data$year<=2006] <- 345
data$cowcode[data$code=="SRB" & data$year>=2007] <- 345
data$cowcode[data$code=="ZAM"] <- 551
data$cowcode[data$code=="ZIM"] <- 552
data$cowcode[data$code=="MNE"] <- 341
data$cowcode[data$code=="KOS"] <- 347
data$cowcode[data$code=="IVO"] <- 437
data$cowcode[data$code=="PRK"] <- 731
data$cowcode[data$code=="GRG"] <- 372
