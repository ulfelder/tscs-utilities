# Assumes target df is "data" and includes PITF country codes with name "sftgcode"

data$colbrit <- ifelse(data$sftgcode=="SUD" | data$sftgcode=="AUL" | data$sftgcode=="BAH" | data$sftgcode=="BHM" | data$sftgcode=="GUY" | data$sftgcode=="BLZ" |  
data$sftgcode=="MYA" | data$sftgcode=="CAN" | data$sftgcode=="CYP" | data$sftgcode=="EGY" | data$sftgcode=="FJI" | data$sftgcode=="GAM" | data$sftgcode=="GHA" |  
data$sftgcode=="IND" | data$sftgcode=="IRQ" | data$sftgcode=="IRE" | data$sftgcode=="JAM" | data$sftgcode=="KEN" | data$sftgcode=="KUW" | data$sftgcode=="NIG" |  
data$sftgcode=="ZAM" | data$sftgcode=="QAT" | data$sftgcode=="SIE" | data$sftgcode=="SIN" | data$sftgcode=="SOM" | data$sftgcode=="SAF" | data$sftgcode=="ZIM" |  
data$sftgcode=="SWA" | data$sftgcode=="SYR" | data$sftgcode=="TAZ" | data$sftgcode=="JOR" | data$sftgcode=="TRI" | data$sftgcode=="UGA" | data$sftgcode=="MAL" |  
data$sftgcode=="MAS" | data$sftgcode=="YPR"  | data$sftgcode=="YEM" | data$sftgcode=="PAK" | data$sftgcode=="BNG" | data$sftgcode=="NEP" | data$sftgcode=="MAW"  |  
data$sftgcode=="PAL" | data$sftgcode=="ISR" | data$sftgcode=="BOT" | data$sftgcode=="LES" | data$sftgcode=="SOL" | data$sftgcode=="SUD", 1, 0)

data$colfrnc <- ifelse(data$sftgcode=="COM" | data$sftgcode=="CAM" | data$sftgcode=="CEN" | data$sftgcode=="CHA" | data$sftgcode=="BEN" | data$sftgcode=="CON" |  
data$sftgcode=="GAB" | data$sftgcode=="GUI" | data$sftgcode=="MOR" | data$sftgcode=="DJI" | data$sftgcode=="MLI" | data$sftgcode=="IVO" | data$sftgcode=="LAO" |  
data$sftgcode=="MAG" | data$sftgcode=="MAA" | data$sftgcode=="NIR" | data$sftgcode=="SEN" | data$sftgcode=="TOG" | data$sftgcode=="TUN" | data$sftgcode=="BFO" |  
data$sftgcode=="VIE"  | data$sftgcode=="ALG" | data$sftgcode=="CAO" | data$sftgcode=="LEB" | data$sftgcode=="HAI" | data$sftgcode=="DRV" | data$sftgcode=="RVN", 1, 0)

data$colespn <- ifelse(data$sftgcode=="CHL" | data$sftgcode=="MEX" | data$sftgcode=="COS" | data$sftgcode=="CUB" | data$sftgcode=="SAL" | data$sftgcode=="GUA" |  
data$sftgcode=="HON" | data$sftgcode=="URU" | data$sftgcode=="NIC" | data$sftgcode=="PAR" | data$sftgcode=="PAN" | data$sftgcode=="ECU" | data$sftgcode=="ARG"  |  
data$sftgcode=="DOM" | data$sftgcode=="PHI" | data$sftgcode=="EQG" | data$sftgcode=="MOR" | data$sftgcode=="TRI" | data$sftgcode=="BOL" | data$sftgcode=="PER"  |  
data$sftgcode=="COL", 1, 0)

data$colport <- ifelse(data$sftgcode=="ANG" | data$sftgcode=="BRA" | data$sftgcode=="GNB" | data$sftgcode=="MZM" | data$sftgcode=="ETM", 1, 0)

data$colbelg <- ifelse(data$sftgcode=="ZAI" | data$sftgcode=="RWA" | data$sftgcode=="BUI", 1, 0)

data$empotto <- ifelse(data$sftgcode=="ALB" | data$sftgcode=="ALG" | data$sftgcode=="ARM" | data$sftgcode=="AZE" | data$sftgcode=="BOS" | data$sftgcode=="BUL" |  
data$sftgcode=="CRO" | data$sftgcode=="CYP" | data$sftgcode=="EGY" | data$sftgcode=="ERI" | data$sftgcode=="GRG" | data$sftgcode=="GRC" | data$sftgcode=="HUN" |  
data$sftgcode=="IRQ" | data$sftgcode=="JOR" | data$sftgcode=="KOS" | data$sftgcode=="KUW" | data$sftgcode=="LEB" | data$sftgcode=="LIB" | data$sftgcode=="MAC" |  
data$sftgcode=="MLD" | data$sftgcode=="MON" | data$sftgcode=="OMA" | data$sftgcode=="QAT" | data$sftgcode=="ROM" | data$sftgcode=="SAU" | data$sftgcode=="SRB" |  
data$sftgcode=="SUD" | data$sftgcode=="SYR" | data$sftgcode=="TUN" | data$sftgcode=="YEM" | data$sftgcode=="YAR" | data$sftgcode=="YPR" | data$sftgcode=="ISR" |  
data$sftgcode=="PAL" | data$sftgcode=="POL", 1, 0)

data$empruss <- ifelse(data$sftgcode=="RUS" | data$sftgcode=="UKR" | data$sftgcode=="POL" | data$sftgcode=="KZK"  | data$sftgcode=="BLR", 1, 0)

data$empaush <- ifelse(data$sftgcode=="AUS"  | data$sftgcode=="HUN" | data$sftgcode=="CRO" | data$sftgcode=="SLV" | data$sftgcode=="BOS" | data$sftgcode=="POL" |  
data$sftgcode=="CZE" | data$sftgcode=="CZR" | data$sftgcode=="SLO" | data$sftgcode=="RUM", 1, 0)

data$empussr <- ifelse(data$sftgcode=="USS" | data$sftgcode=="RUS" | data$sftgcode=="BLR" | data$sftgcode=="EST" | data$sftgcode=="LAT" | data$sftgcode=="LIT" |  
data$sftgcode=="UKR" | data$sftgcode=="ARM" | data$sftgcode=="AZE" | data$sftgcode=="GRG" | data$sftgcode=="TAJ" | data$sftgcode=="TKM" | data$sftgcode=="UZB" |  
data$sftgcode=="KZK" | data$sftgcode=="KYR" | data$sftgcode=="MLD", 1, 0)
