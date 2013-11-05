# This script creates a country-year TSCS data frame with data on Political Instability Task Force (PITF)
# instability episodes, by event type and consolidated. This version is based on the list of events in the
# table through 2012 found at:
# 
# http://www.systemicpeace.org/inscr/PITF%20Consolidated%20Case%20List2012.pdf
#
# The script will need to be modified annually to reset the last observed year (line 48) and to add new
# episodes.

#####################################
# Functions
#####################################

# Adverse regime change
areg.it <- function(code, start, end) {
                areg.dur <- seq(from = 1, to = end - start + 1, by = 1)
                sftgcode <- rep(code, times = end - start + 1)
                year <- seq(from = start, to = end)
                datframe <- as.data.frame(cbind(sftgcode, year, areg.dur))
                return(datframe)
           }

# Ethnic war
ewar.it <- function(code, start, end) {
                ewar.dur <- seq(from = 1, to = end - start + 1, by = 1)
                sftgcode <- rep(code, times = end - start + 1)
                year <- seq(from = start, to = end)
                datframe <- as.data.frame(cbind(sftgcode, year, ewar.dur))
                return(datframe)
           }

# Revolutionary war
rwar.it <- function(code, start, end) {
                rwar.dur <- seq(from = 1, to = end - start + 1, by = 1)
                sftgcode <- rep(code, times = end - start + 1)
                year <- seq(from = start, to = end)
                datframe <- as.data.frame(cbind(sftgcode, year, rwar.dur))
                return(datframe)
           }

###################################
# Events
###################################

library(reshape)

# Set last year
lastyear <- 2012

# Afghanistan

afg.areg.1978 <- areg.it("AFG", 1978, 1979)
afg.rwar.1978 <- rwar.it("AFG", 1978, 1992)
afg.areg.1992 <- areg.it("AFG", 1992, 1996)
afg.ewar.1992 <- ewar.it("AFG", 1992, 2001)
afg.areg.2001 <- areg.it("AFG", 2001, 2002)
afg.rwar.2001 <- rwar.it("AFG", 2001, lastyear)

afg <- merge(afg.areg.1978, afg.rwar.1978, all = TRUE)
afg <- merge(afg, afg.areg.1992, all = TRUE)
afg <- merge(afg, afg.ewar.1992, all = TRUE)
afg <- merge(afg, afg.areg.2001, all = TRUE)
afg <- merge(afg, afg.rwar.2001, all = TRUE)

afg <- melt(afg, id = c("sftgcode", "year"))
afg$value <- as.numeric(as.character(afg$value))  # Req'd to get max fun to work in next line
afg <- cast(afg, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
afg[afg == Inf] <- 0

# Albania

alb.areg.1996 <- areg.it("ALB", 1996, 1996)
alb.rwar.1997 <- rwar.it("ALB", 1997, 1997)

alb <- merge(alb.areg.1996, alb.rwar.1997, all = TRUE)

alb <- melt(alb, id = c("sftgcode", "year"))
alb$value <- as.numeric(as.character(alb$value))  # Req'd to get min fun to work in next line
alb <- cast(alb, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
alb[alb == Inf] <- 0

# Algeria

alg.ewar.1962 <- ewar.it("ALG", 1962, 1962)
alg.rwar.1962 <- rwar.it("ALG", 1962, 1962)
alg.rwar.1991 <- rwar.it("ALG", 1991, 2004)
alg.areg.1992 <- areg.it("ALG", 1992, 1992)

alg <- merge(alg.ewar.1962, alg.rwar.1962, all = TRUE)
alg <- merge(alg, alg.rwar.1991, all = TRUE)
alg <- merge(alg, alg.areg.1992, all = TRUE)

alg <- melt(alg, id = c("sftgcode", "year"))
alg$value <- as.numeric(as.character(alg$value))  # Req'd to get min fun to work in next line
alg <- cast(alg, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
alg[alg == Inf] <- 0

# Angola

ang.ewar.1975 <- ewar.it("ANG", 1975, 2002)
ang.rwar.1975 <- rwar.it("ANG", 1975, 2002)
ang.areg.1992 <- areg.it("ANG", 1992, 1997)

ang <- merge(ang.ewar.1975, ang.rwar.1975, all = TRUE)
ang <- merge(ang, ang.areg.1992, all = TRUE)

ang <- melt(ang, id = c("sftgcode", "year"))
ang$value <- as.numeric(as.character(ang$value))  # Req'd to get min fun to work in next line
ang <- cast(ang, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
ang[ang == Inf] <- 0

# Argentina

arg.areg.1966 <- areg.it("ARG", 1966, 1966)
arg.areg.1976 <- areg.it("ARG", 1976, 1976)

arg <- merge(arg.areg.1966, arg.areg.1976, all = TRUE)

arg <- melt(arg, id = c("sftgcode", "year"))
arg$value <- as.numeric(as.character(arg$value))  # Req'd to get min fun to work in next line
arg <- cast(arg, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
arg[arg == Inf] <- 0

# Armenia

arm.areg.1995 <- areg.it("ARM", 1995, 1996)

arm <- arm.areg.1995

# Azerbaijan

aze.ewar.1988 <- ewar.it("AZE", 1988, 1997)
aze.areg.1993 <- areg.it("AZE", 1993, 1995)

aze <- merge(aze.ewar.1988, aze.areg.1993, all = TRUE)

aze <- melt(aze, id = c("sftgcode", "year"))
aze$value <- as.numeric(as.character(aze$value))  # Req'd to get min fun to work in next line
aze <- cast(aze, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
aze[aze == Inf] <- 0

# Bangladesh

bng.areg.1974 <- areg.it("BNG", 1974, 1975)
bng.ewar.1976 <- ewar.it("BNG", 1976, 1991)
bng.areg.2007 <- areg.it("BNG", 2007, 2007)

bng <- merge(bng.areg.1974, bng.ewar.1976, all = TRUE)
bng <- merge(bng, bng.areg.2007, all = TRUE)

bng <- melt(bng, id = c("sftgcode", "year"))
bng$value <- as.numeric(as.character(bng$value))  # Req'd to get min fun to work in next line
bng <- cast(bng, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
bng[bng == Inf] <- 0

# Belarus

blr.areg.1995 <- areg.it("BLR", 1995, 1996)

blr <- blr.areg.1995

# Benin

ben.areg.1963 <- areg.it("BEN", 1963, 1965)
ben.areg.1965 <- areg.it("BEN", 1972, 1972)

ben <- merge(ben.areg.1963, ben.areg.1965, all = TRUE)

# Bosnia

bos.areg.1992 <- areg.it("BOS", 1992, 1995)
bos.ewar.1992 <- ewar.it("BOS", 1992, 1995)

bos <- merge(bos.areg.1992, bos.ewar.1992, all = TRUE)

bos <- melt(bos, id = c("sftgcode", "year"))
bos$value <- as.numeric(as.character(bos$value))  # Req'd to get min fun to work in next line
bos <- cast(bos, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
bos[bos == Inf] <- 0

# Brazil

bra.areg.1961 <- areg.it("BRA", 1961, 1965)

bra <- bra.areg.1961

# Burkina Faso

bfo.areg.1980 <- areg.it("BFO", 1980, 1980)

bfo <- bfo.areg.1980

# Myanmar

mya.ewar.1948 <- ewar.it("MYA", 1948, lastyear)
mya.areg.1962 <- areg.it("MYA", 1962, 1962)
mya.rwar.1988 <- rwar.it("MYA", 1988, 1989)

mya <- merge(mya.ewar.1948, mya.areg.1962, all = TRUE)
mya <- merge(mya, mya.rwar.1988, all = TRUE)

mya <- melt(mya, id = c("sftgcode", "year"))
mya$value <- as.numeric(as.character(mya$value))  # Req'd to get min fun to work in next line
mya <- cast(mya, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
mya[mya == Inf] <- 0

# Burundi

bui.areg.1963 <- areg.it("BUI", 1963, 1966)
bui.ewar.1972 <- ewar.it("BUI", 1972, 1972)
bui.ewar.1988 <- ewar.it("BUI", 1988, 2005)
bui.areg.1993 <- areg.it("BUI", 1993, 1996)

bui <- merge(bui.areg.1963, bui.ewar.1972, all = TRUE)
bui <- merge(bui, bui.ewar.1988, all = TRUE)
bui <- merge(bui, bui.areg.1993, all = TRUE)

bui <- melt(bui, id = c("sftgcode", "year"))
bui$value <- as.numeric(as.character(bui$value))  # Req'd to get min fun to work in next line
bui <- cast(bui, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
bui[bui == Inf] <- 0

# Cambodia

cam.rwar.1970 <- rwar.it("CAM", 1970, 1975)
cam.areg.1975 <- areg.it("CAM", 1975, 1976)
cam.rwar.1979 <- rwar.it("CAM", 1979, 1991)
cam.areg.1997 <- areg.it("CAM", 1997, 1997)

cam <- merge(cam.rwar.1970, cam.areg.1975, all = TRUE)
cam <- merge(cam, cam.rwar.1979, all = TRUE)
cam <- merge(cam, cam.areg.1997, all = TRUE)

cam <- melt(cam, id = c("sftgcode", "year"))
cam$value <- as.numeric(as.character(cam$value))  # Req'd to get min fun to work in next line
cam <- cast(cam, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
cam[cam == Inf] <- 0

# Central African Republic

cen.areg.2003 <- areg.it("CEN", 2003, 2003)
cen.ewar.2005 <- ewar.it("CEN", 2005, lastyear)

cen <- merge(cen.areg.2003, cen.ewar.2005, all = TRUE)

cen <- melt(cen, id = c("sftgcode", "year"))
cen$value <- as.numeric(as.character(cen$value))  # Req'd to get min fun to work in next line
cen <- cast(cen, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
cen[cen == Inf] <- 0

# Chad

cha.ewar.1965 <- ewar.it("CHA", 1965, 1994)
cha.areg.1979 <- areg.it("CHA", 1979, 1984)

cha <- merge(cha.ewar.1965, cha.areg.1979, all = TRUE)

cha <- melt(cha, id = c("sftgcode", "year"))
cha$value <- as.numeric(as.character(cha$value))  # Req'd to get min fun to work in next line
cha <- cast(cha, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
cha[cha == Inf] <- 0

# Chile

chl.areg.1973 <- areg.it("CHL", 1973, 1973)

chl <- chl.areg.1973

# China

chn.ewar.1956 <- ewar.it("CHN", 1956, 1959)
chn.rwar.1966 <- rwar.it("CHN", 1966, 1969)
chn.ewar.1988 <- ewar.it("CHN", 1988, 1998)
chn.rwar.1989 <- rwar.it("CHN", 1989, 1989)

chn <- merge(chn.ewar.1956, chn.rwar.1966, all = TRUE)
chn <- merge(chn, chn.ewar.1988, all = TRUE)
chn <- merge(chn, chn.rwar.1989, all = TRUE)

chn <- melt(chn, id = c("sftgcode", "year"))
chn$value <- as.numeric(as.character(chn$value))  # Req'd to get min fun to work in next line
chn <- cast(chn, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
chn[chn == Inf] <- 0

# Colombia

col.rwar.1948 <- rwar.it("COL", 1948, 1960)
col.rwar.1975 <- rwar.it("COL", 1975, lastyear)

col <- merge(col.rwar.1948, col.rwar.1975, all = TRUE)

# Comoros

com.areg.1976 <- areg.it("COM", 1976, 1976)
com.areg.1995 <- areg.it("COM", 1995, 1996)
com.areg.1999 <- areg.it("COM", 1999, 1999)

com <- merge(com.areg.1976, com.areg.1995, all = TRUE)
com <- merge(com, com.areg.1999, all = TRUE)

# Republic of Congo

con.areg.1963 <- areg.it("CON", 1963, 1963)
con.areg.1997 <- areg.it("CON", 1997, 1997)
con.rwar.1997 <- rwar.it("CON", 1997, 1999)

con <- merge(con.areg.1963, con.areg.1997, all = TRUE)
con <- merge(con, con.rwar.1997, all = TRUE)

con <- melt(con, id = c("sftgcode", "year"))
con$value <- as.numeric(as.character(con$value))  # Req'd to get min fun to work in next line
con <- cast(con, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
con[con == Inf] <- 0

# Democratic Republic of Congo

zai.areg.1960 <- areg.it("ZAI", 1960, 1965)
zai.ewar.1960 <- ewar.it("ZAI", 1960, 1965)
zai.rwar.1960 <- rwar.it("ZAI", 1960, 1965)
zai.ewar.1977 <- ewar.it("ZAI", 1977, 1978)
zai.ewar.1992 <- ewar.it("ZAI", 1992, lastyear)
zai.areg.1992 <- areg.it("ZAI", 1992, 2003)
zai.rwar.1996 <- rwar.it("ZAI", 1996, 2003)

zai <- merge(zai.areg.1960, zai.rwar.1960, all = TRUE)
zai <- merge(zai, zai.ewar.1960, all = TRUE)
zai <- merge(zai, zai.ewar.1977, all = TRUE)
zai <- merge(zai, zai.ewar.1992, all = TRUE)
zai <- merge(zai, zai.areg.1992, all = TRUE)
zai <- merge(zai, zai.rwar.1996, all = TRUE)

zai <- melt(zai, id = c("sftgcode", "year"))
zai$value <- as.numeric(as.character(zai$value))  # Req'd to get min fun to work in next line
zai <- cast(zai, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
zai[zai == Inf] <- 0

# Croatia

cro.ewar.1991 <- ewar.it("CRO", 1991, 1995)

cro <- cro.ewar.1991

# Cuba

cub.areg.1952 <- areg.it("CUB", 1952, 1955)
cub.rwar.1956 <- rwar.it("CUB", 1956, 1959)
cub.areg.1959 <- areg.it("CUB", 1959, 1961)

cub <- merge(cub.areg.1952, cub.rwar.1956, all = TRUE)
cub <- merge(cub, cub.areg.1959, all = TRUE)

cub <- melt(cub, id = c("sftgcode", "year"))
cub$value <- as.numeric(as.character(cub$value))  # Req'd to get min fun to work in next line
cub <- cast(cub, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
cub[cub == Inf] <- 0

# Cyprus

cyp.ewar.1963 <- ewar.it("CYP", 1963, 1964)
cyp.areg.1963 <- areg.it("CYP", 1963, 1968)
cyp.areg.1974 <- areg.it("CYP", 1974, 1974)
cyp.ewar.1974 <- ewar.it("CYP", 1974, 1974)

cyp <- merge(cyp.ewar.1963, cyp.areg.1963, all = TRUE)
cyp <- merge(cyp, cyp.areg.1974, all = TRUE)
cyp <- merge(cyp, cyp.ewar.1974, all = TRUE)

cyp <- melt(cyp, id = c("sftgcode", "year"))
cyp$value <- as.numeric(as.character(cyp$value))  # Req'd to get min fun to work in next line
cyp <- cast(cyp, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
cyp[cyp == Inf] <- 0

# Czechoslovakia

cze.areg.1968 <- areg.it("CZE", 1968, 1969)

cze <- cze.areg.1968

# Djibouti

dji.ewar.1991 <- ewar.it("DJI", 1991, 1994)

dji <- dji.ewar.1991

# Dominican Republic

dom.areg.1963 <- areg.it("DOM", 1963, 1966)
dom.rwar.1965 <- rwar.it("DOM", 1965, 1965)

dom <- merge(dom.areg.1963, dom.rwar.1965, all = TRUE)

dom <- melt(dom, id = c("sftgcode", "year"))
dom$value <- as.numeric(as.character(dom$value))  # Req'd to get min fun to work in next line
dom <- cast(dom, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
dom[dom == Inf] <- 0

# Ecuador

ecu.areg.1970 <- areg.it("ECU", 1970, 1972)

ecu <- ecu.areg.1970

# Egypt

egy.rwar.1992 <- rwar.it("EGY", 1992, 1999)

egy <- egy.rwar.1992

# El Salvador

sal.areg.1977 <- areg.it("SAL", 1977, 1977)
sal.rwar.1979 <- rwar.it("SAL", 1979, 1992)

sal <- merge(sal.areg.1977, sal.rwar.1979, all = TRUE)

sal <- melt(sal, id = c("sftgcode", "year"))
sal$value <- as.numeric(as.character(sal$value))  # Req'd to get min fun to work in next line
sal <- cast(sal, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
sal[sal == Inf] <- 0

# Equatorial Guinea

eqg.areg.1969 <- areg.it("EQG", 1969, 1969)

eqg <- eqg.areg.1969

# Ethiopia

eti.ewar.1961 <- ewar.it("ETI", 1961, 1991)
eti.ewar.1963 <- ewar.it("ETI", 1963, 1964)
eti.areg.1974 <- areg.it("ETI", 1974, 1975)
eti.rwar.1975 <- rwar.it("ETI", 1975, 1991)
eti.ewar.1977 <- ewar.it("ETI", 1977, 1978)
eti.areg.1991 <- areg.it("ETI", 1991, 1993)
eti.ewar.1999 <- ewar.it("ETH", 1999, 2000) # Note change in country code after 1993
eti.ewar.2007 <- ewar.it("ETH", 2007, lastyear)

eti <- merge(eti.ewar.1961, eti.ewar.1963, all = TRUE)
eti <- merge(eti, eti.areg.1974, all = TRUE)
eti <- merge(eti, eti.rwar.1975, all = TRUE)
eti <- merge(eti, eti.ewar.1977, all = TRUE)
eti <- merge(eti, eti.areg.1991, all = TRUE)
eti <- merge(eti, eti.ewar.1999, all = TRUE)
eti <- merge(eti, eti.ewar.2007, all = TRUE)

eti <- melt(eti, id = c("sftgcode", "year"))
eti$value <- as.numeric(as.character(eti$value))  # Req'd to get min fun to work in next line
eti <- cast(eti, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
eti[eti == Inf] <- 0

# Fiji

fji.areg.1987 <- areg.it("FJI", 1987, 1987)
fji.areg.2006 <- areg.it("FJI", 2006, 2006)

fji <- merge(fji.areg.1987, fji.areg.2006, all = TRUE)

# France

frn.areg.1958 <- areg.it("FRN", 1958, 1958) 

frn <- frn.areg.1958

# The Gambia

gam.areg.1994 <- areg.it("GAM", 1994, 1994) 

gam <- gam.areg.1994

# Georgia

grg.ewar.1991 <- ewar.it("GRG", 1991, 1993)
grg.rwar.1992 <- rwar.it("GRG", 1992, 1993)

grg <- merge(grg.ewar.1991, grg.rwar.1992, all = TRUE)

grg <- melt(grg, id = c("sftgcode", "year"))
grg$value <- as.numeric(as.character(grg$value))  # Req'd to get min fun to work in next line
grg <- cast(grg, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
grg[grg == Inf] <- 0

# Ghana

gha.areg.1972 <- areg.it("GHA", 1972, 1972)
gha.areg.1981 <- areg.it("GHA", 1981, 1981)

gha <- merge(gha.areg.1972, gha.areg.1981, all = TRUE)

# Greece

grc.areg.1967 <- areg.it("GRC", 1967, 1967)

grc <- grc.areg.1967

# Guatemala

gua.rwar.1966 <- rwar.it("GUA", 1966, 1996)
gua.ewar.1975 <- ewar.it("GUA", 1975, 1994)

gua <- merge(gua.rwar.1966, gua.ewar.1975, all = TRUE)

gua <- melt(gua, id = c("sftgcode", "year"))
gua$value <- as.numeric(as.character(gua$value))  # Req'd to get min fun to work in next line
gua <- cast(gua, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
gua[gua == Inf] <- 0

# Guinea

gui.rwar.2000 <- rwar.it("GUI", 2000, 2001)

gui <- gui.rwar.2000

# Guinea-Bissau

gnb.rwar.1998 <- rwar.it("GNB", 1998, 1999)
gnb.areg.1998 <- areg.it("GNB", 1998, 1999)
gnb.areg.2003 <- areg.it("GNB", 2003, 2003)
gnb.areg.2012 <- areg.it("GNB", 2012, 2012)

gnb <- merge(gnb.rwar.1998, gnb.areg.1998, all = TRUE)
gnb <- merge(gnb, gnb.areg.2003, all = TRUE)
gnb <- merge(gnb, gnb.areg.2012, all = TRUE)

gnb <- melt(gnb, id = c("sftgcode", "year"))
gnb$value <- as.numeric(as.character(gnb$value))  # Req'd to get min fun to work in next line
gnb <- cast(gnb, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
gnb[gnb == Inf] <- 0

# Guyana

guy.areg.1978 <- areg.it("GUY", 1978, 1980)

guy <- guy.areg.1978

# Haiti

hai.areg.1991 <- areg.it("HAI", 1991, 1991)
hai.areg.1999 <- areg.it("HAI", 1999, 2000)

hai <- merge(hai.areg.1991, hai.areg.1999, all = TRUE)

# Hungary

hun.areg.1956 <- areg.it("HUN", 1956, 1957)
hun.rwar.1956 <- rwar.it("HUN", 1956, 1956)

hun <- merge(hun.areg.1956, hun.rwar.1956, all = TRUE)

hun <- melt(hun, id = c("sftgcode", "year"))
hun$value <- as.numeric(as.character(hun$value))  # Req'd to get min fun to work in next line
hun <- cast(hun, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
hun[hun == Inf] <- 0

# India

ind.ewar.1956 <- ewar.it("IND", 1956, 1958)
ind.ewar.1967 <- ewar.it("IND", 1967, 1971)
ind.ewar.1983 <- ewar.it("IND", 1983, 1993)
ind.ewar.1990 <- ewar.it("IND", 1990, lastyear)
ind.rwar.2001 <- rwar.it("IND", 2001, lastyear)

ind <- merge(ind.ewar.1956, ind.ewar.1967, all = TRUE)
ind <- merge(ind, ind.ewar.1983, all = TRUE)
ind <- merge(ind, ind.ewar.1990, all = TRUE)
ind <- merge(ind, ind.rwar.2001, all = TRUE)

ind <- melt(ind, id = c("sftgcode", "year"))
ind$value <- as.numeric(as.character(ind$value))  # Req'd to get min fun to work in next line
ind <- cast(ind, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
ind[ind == Inf] <- 0

# Indonesia

ins.rwar.1949 <- rwar.it("INS", 1949, 1961)
ins.areg.1957 <- areg.it("INS", 1957, 1959)
ins.rwar.1958 <- rwar.it("INS", 1958, 1961)
ins.ewar.1967 <- ewar.it("INS", 1967, 1971)
ins.ewar.1975 <- ewar.it("INS", 1975, 1991)
ins.rwar.1981 <- ewar.it("INS", 1981, 1984)
ins.ewar.1997 <- ewar.it("INS", 1997, 1999)
ins.ewar.1998 <- ewar.it("INS", 1998, 2005)
ins.rwar.1998 <- rwar.it("INS", 1998, 1999)

ins <- merge(ins.rwar.1949, ins.areg.1957, all = TRUE)
ins <- merge(ins, ins.rwar.1958, all = TRUE)
ins <- merge(ins, ins.ewar.1967, all = TRUE)
ins <- merge(ins, ins.ewar.1975, all = TRUE)
ins <- merge(ins, ins.rwar.1981, all = TRUE)
ins <- merge(ins, ins.ewar.1997, all = TRUE)
ins <- merge(ins, ins.ewar.1998, all = TRUE)
ins <- merge(ins, ins.rwar.1998, all = TRUE)

ins <- melt(ins, id = c("sftgcode", "year"))
ins$value <- as.numeric(as.character(ins$value))
ins <- cast(ins, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
ins[ins == Inf] <- 0

# Iran

irn.areg.1953 <- areg.it("IRN", 1953, 1955)
irn.rwar.1977 <- rwar.it("IRN", 1977, 1979)
irn.areg.1979 <- areg.it("IRN", 1979, 1982)
irn.ewar.1979 <- ewar.it("IRN", 1979, 1985)
irn.rwar.1981 <- rwar.it("IRN", 1981, 1983)
irn.areg.2004 <- areg.it("IRN", 2004, 2004)

irn <- merge(irn.areg.1953, irn.rwar.1977, all = TRUE)
irn <- merge(irn, irn.areg.1979, all = TRUE)
irn <- merge(irn, irn.ewar.1979, all = TRUE)
irn <- merge(irn, irn.rwar.1981, all = TRUE)
irn <- merge(irn, irn.areg.2004, all = TRUE)

irn <- melt(irn, id = c("sftgcode", "year"))
irn$value <- as.numeric(as.character(irn$value))
irn <- cast(irn, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
irn[irn == Inf] <- 0

# Iraq

irq.rwar.1959 <- rwar.it("IRQ", 1959, 1959)
irq.ewar.1961 <- ewar.it("IRQ", 1961, 1970)
irq.ewar.1974 <- ewar.it("IRQ", 1974, 1975)
irq.ewar.1980 <- ewar.it("IRQ", 1980, 1988)
irq.ewar.1991 <- ewar.it("IRQ", 1991, 1998)
irq.areg.2003 <- areg.it("IRQ", 2003, 2003)
irq.ewar.2003 <- ewar.it("IRQ", 2003, lastyear)

irq <- merge(irq.rwar.1959, irq.ewar.1961, all = TRUE)
irq <- merge(irq, irq.ewar.1974, all = TRUE)
irq <- merge(irq, irq.ewar.1980, all = TRUE)
irq <- merge(irq, irq.ewar.1991, all = TRUE)
irq <- merge(irq, irq.areg.2003, all = TRUE)
irq <- merge(irq, irq.ewar.2003, all = TRUE)

irq <- melt(irq, id = c("sftgcode", "year"))
irq$value <- as.numeric(as.character(irq$value))
irq <- cast(irq, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
irq[irq == Inf] <- 0

# Israel

isr.ewar.1987 <- ewar.it("ISR", 1987, lastyear)

isr <- isr.ewar.1987

# Ivory Coast

ivo.ewar.2002 <- ewar.it("IVO", 2002, 2005)
ivo.areg.2002 <- areg.it("IVO", 2002, 2011)
ivo.rwar.2011 <- rwar.it("IVO", 2011, 2011)

ivo <- merge(ivo.ewar.2002, ivo.areg.2002, all = TRUE)
ivo <- merge(ivo, ivo.rwar.2011, all = TRUE)

ivo <- melt(ivo, id = c("sftgcode", "year"))
ivo$value <- as.numeric(as.character(ivo$value))
ivo <- cast(ivo, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
ivo[ivo == Inf] <- 0

# Jordan

jor.areg.1957 <- areg.it("JOR", 1957, 1957)
jor.rwar.1970 <- rwar.it("JOR", 1970, 1971)

jor <- merge(jor.areg.1957, jor.rwar.1970, all = TRUE)

jor <- melt(jor, id = c("sftgcode", "year"))
jor$value <- as.numeric(as.character(jor$value))
jor <- cast(jor, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
jor[jor == Inf] <- 0

# Kenya

ken.ewar.1964 <- ewar.it("KEN", 1964, 1966)
ken.areg.1969 <- areg.it("KEN", 1969, 1969)
ken.ewar.1991 <- ewar.it("KEN", 1991, 1993)

ken <- merge(ken.ewar.1964, ken.areg.1969, all = TRUE)
ken <- merge(ken, ken.ewar.1991, all = TRUE)

ken <- melt(ken, id = c("sftgcode", "year"))
ken$value <- as.numeric(as.character(ken$value))
ken <- cast(ken, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
ken[ken == Inf] <- 0

# South Korea

rok.areg.1961 <- areg.it("ROK", 1961, 1961)
rok.areg.1972 <- areg.it("ROK", 1972, 1972)

rok <- merge(rok.areg.1961, rok.areg.1972, all = TRUE)

# Laos

lao.areg.1960 <- areg.it("LAO", 1960, 1975)
lao.rwar.1960 <- rwar.it("LAO", 1960, 1962)
lao.ewar.1961 <- ewar.it("LAO", 1961, 1979)
lao.rwar.1963 <- rwar.it("LAO", 1963, 1979)

lao <- merge(lao.areg.1960, lao.rwar.1960, all = TRUE)
lao <- merge(lao, lao.ewar.1961, all = TRUE)
lao <- merge(lao, lao.rwar.1963, all = TRUE)

lao <- melt(lao, id = c("sftgcode", "year"))
lao$value <- as.numeric(as.character(lao$value))
lao <- cast(lao, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
lao[lao == Inf] <- 0

# Lebanon

leb.rwar.1958 <- rwar.it("LEB", 1958, 1958)
leb.areg.1975 <- areg.it("LEB", 1975, 1990)
leb.ewar.1975 <- ewar.it("LEB", 1975, 1991)

leb <- merge(leb.rwar.1958, leb.areg.1975, all = TRUE)
leb <- merge(leb, leb.ewar.1975, all = TRUE)

leb <- melt(leb, id = c("sftgcode", "year"))
leb$value <- as.numeric(as.character(leb$value))
leb <- cast(leb, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
leb[leb == Inf] <- 0

# Lesotho

les.areg.1970 <- areg.it("LES", 1970, 1970)
les.areg.1998 <- areg.it("LES", 1998, 1999)

les <- merge(les.areg.1970, les.areg.1998, all = TRUE)

# Liberia

lbr.rwar.1985 <- rwar.it("LBR", 1985, 1985)
lbr.rwar.1989 <- rwar.it("LBR", 1989, 1993)
lbr.areg.1990 <- areg.it("LBR", 1990, 1996)
lbr.rwar.2000 <- rwar.it("LBR", 2000, 2003)

lbr <- merge(lbr.rwar.1985, lbr.rwar.1989, all = TRUE)
lbr <- merge(lbr, lbr.areg.1990, all = TRUE)
lbr <- merge(lbr, lbr.rwar.2000, all = TRUE)

lbr <- melt(lbr, id = c("sftgcode", "year"))
lbr$value <- as.numeric(as.character(lbr$value))
lbr <- cast(lbr, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
lbr[lbr == Inf] <- 0

# Libya

lib.rwar.2011 <- rwar.it("LIB", 2011, 2011)
lib.areg.2011 <- areg.it("LIB", 2011, 2012)

lib <- merge(lib.areg.2011, lib.rwar.2011, all = TRUE)

lib <- melt(lib, id = c("sftgcode", "year"))
lib$value <- as.numeric(as.character(lib$value))
lib <- cast(lib, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
lib[lib == Inf] <- 0

# Madagascar

mag.areg.2009 <- areg.it("MAG", 2009, 2009)

mag <- mag.areg.2009

# Malaysia

mal.areg.1969 <- areg.it("MAL", 1969, 1969)

mal <- mal.areg.1969

# Mali

mli.ewar.1990 <- ewar.it("MLI", 1990, 1995)
mli.areg.2012 <- areg.it("MLI", 2012, 2012)
mli.rwar.2012 <- rwar.it("MLI", 2012, lastyear)

mli <- merge(mli.ewar.1990, mli.areg.2012, all = TRUE)
mli <- merge(mli, mli.rwar.2012, all = TRUE)

mli <- melt(mli, id = c("sftgcode", "year"))
mli$value <- as.numeric(as.character(mli$value))
mli <- cast(mli, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
mli[mli == Inf] <- 0

# Mauritania

maa.areg.2008 <- areg.it("MAA", 2008, 2008)

maa <- maa.areg.2008

# Mexico

mex.rwar.2007 <- rwar.it("MEX", 2007, lastyear)

mex <- mex.rwar.2007

# Moldova

mld.ewar.1992 <- ewar.it("MLD", 1992, 1992)

mld <- mld.ewar.1992

# Morocco

mor.areg.1965 <- areg.it("MOR", 1965, 1965)
mor.ewar.1975 <- ewar.it("MOR", 1975, 1989)

mor <- merge(mor.areg.1965, mor.ewar.1975, all = TRUE)

mor <- melt(mor, id = c("sftgcode", "year"))
mor$value <- as.numeric(as.character(mor$value))
mor <- cast(mor, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
mor[mor == Inf] <- 0

# Mozambique

mzm.rwar.1976 <- rwar.it("MZM", 1976, 1992)

mzm <- mzm.rwar.1976

# Nepal

nep.areg.1960 <- areg.it("NEP", 1960, 1960)
nep.rwar.1996 <- rwar.it("NEP", 1996, 2006)
nep.areg.2002 <- areg.it("NEP", 2002, 2002)

nep <- merge(nep.areg.1960, nep.rwar.1996, all = TRUE)
nep <- merge(nep, nep.areg.2002, all = TRUE)

nep <- melt(nep, id = c("sftgcode", "year"))
nep$value <- as.numeric(as.character(nep$value))
nep <- cast(nep, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
nep[nep == Inf] <- 0

# Nicaragua

nic.rwar.1978 <- rwar.it("NIC", 1978, 1979)
nic.areg.1979 <- areg.it("NIC", 1979, 1981)
nic.rwar.1981 <- rwar.it("NIC", 1981, 1988)
nic.ewar.1981 <- ewar.it("NIC", 1981, 1984)

nic <- merge(nic.rwar.1978, nic.areg.1979, all = TRUE)
nic <- merge(nic, nic.rwar.1981, all = TRUE)
nic <- merge(nic, nic.ewar.1981, all = TRUE)

nic <- melt(nic, id = c("sftgcode", "year"))
nic$value <- as.numeric(as.character(nic$value))
nic <- cast(nic, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
nic[nic == Inf] <- 0

# Niger

nir.areg.1996 <- areg.it("NIR", 1996, 1996)
nir.areg.2009 <- areg.it("NIR", 2009, 2009)

nir <- merge(nir.areg.1996, nir.areg.2009, all = TRUE)

# nigeria

nig.areg.1964 <- areg.it("NIG", 1964, 1966)
nig.ewar.1966 <- ewar.it("NIG", 1966, 1970)
nig.rwar.1980 <- rwar.it("NIG", 1980, 1985)
nig.areg.1984 <- areg.it("NIG", 1984, 1984)
nig.ewar.2006 <- ewar.it("NIG", 2006, 2010)
nig.rwar.2009 <- rwar.it("NIG", 2009, lastyear)

nig <- merge(nig.areg.1964, nig.ewar.1966, all = TRUE)
nig <- merge(nig, nig.rwar.1980, all = TRUE)
nig <- merge(nig, nig.areg.1984, all = TRUE)
nig <- merge(nig, nig.ewar.2006, all = TRUE)
nig <- merge(nig, nig.rwar.2009, all = TRUE)

nig <- melt(nig, id = c("sftgcode", "year"))
nig$value <- as.numeric(as.character(nig$value))
nig <- cast(nig, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
nig[nig == Inf] <- 0

# Oman

oma.rwar.1970 <- rwar.it("OMA", 1970, 1976)

oma <- oma.rwar.1970

# Pakistan

pak.areg.1958 <- areg.it("PKS", 1958, 1958)
pak.ewar.1971 <- ewar.it("PKS", 1971, 1971)
pak.areg.1971 <- areg.it("PKS", 1971, 1971)
pak.ewar.1973 <- ewar.it("PAK", 1973, 1977) # Note country code change at 1972
pak.areg.1977 <- areg.it("PAK", 1977, 1977)
pak.ewar.1983 <- ewar.it("PAK", 1983, 1998)
pak.areg.1999 <- areg.it("PAK", 1999, 1999)
pak.ewar.2004 <- ewar.it("PAK", 2004, lastyear)

pak <- merge(pak.areg.1958, pak.ewar.1971, all = TRUE)
pak <- merge(pak, pak.areg.1971, all = TRUE)
pak <- merge(pak, pak.ewar.1973, all = TRUE)
pak <- merge(pak, pak.areg.1977, all = TRUE)
pak <- merge(pak, pak.ewar.1983, all = TRUE)
pak <- merge(pak, pak.areg.1999, all = TRUE)
pak <- merge(pak, pak.ewar.2004, all = TRUE)

pak <- melt(pak, id = c("sftgcode", "year"))
pak$value <- as.numeric(as.character(pak$value))
pak <- cast(pak, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
pak[pak == Inf] <- 0

# Panama

pan.areg.1968 <- areg.it("PAN", 1968, 1968)

pan <- pan.areg.1968

# Papua New Guinea

png.ewar.1989 <- ewar.it("PNG", 1989, 1997)

png <- png.ewar.1989

# Peru

per.areg.1962 <- areg.it("PER", 1962, 1962)
per.areg.1968 <- areg.it("PER", 1968, 1968)
per.rwar.1982 <- rwar.it("PER", 1982, 1997)
per.areg.1992 <- areg.it("PER", 1992, 1992)

per <- merge(per.areg.1962, per.areg.1968, all = TRUE)
per <- merge(per, per.rwar.1982, all = TRUE)
per <- merge(per, per.areg.1992, all = TRUE)

per <- melt(per, id = c("sftgcode", "year"))
per$value <- as.numeric(as.character(per$value))
per <- cast(per, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
per[per == Inf] <- 0

# Philippines

phi.areg.1969 <- areg.it("PHI", 1969, 1972)
phi.rwar.1972 <- rwar.it("PHI", 1972, 1996)
phi.ewar.1972 <- ewar.it("PHI", 1972, lastyear)

phi <- merge(phi.areg.1969, phi.rwar.1972, all = TRUE)
phi <- merge(phi, phi.ewar.1972, all = TRUE)

phi <- melt(phi, id = c("sftgcode", "year"))
phi$value <- as.numeric(as.character(phi$value))
phi <- cast(phi, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
phi[phi == Inf] <- 0

# Romania

rum.rwar.1989 <- rwar.it("RUM", 1989, 1989)

rum <- rum.rwar.1989

# Russia

rus.ewar.1994 <- ewar.it("RUS", 1994, 1996)
rus.ewar.1999 <- ewar.it("RUS", 1999, 2006)
rus.ewar.2008 <- ewar.it("RUS", 2008, lastyear)

rus <- merge(rus.ewar.1994, rus.ewar.1999, all = TRUE)
rus <- merge(rus, rus.ewar.2008, all = TRUE)

# Rwanda

rwa.ewar.1963 <- ewar.it("RWA", 1963, 1966)
rwa.ewar.1990 <- ewar.it("RWA", 1990, 1998)
rwa.areg.1994 <- areg.it("RWA", 1994, 1994)
rwa.ewar.2001 <- ewar.it("RWA", 2001, 2001)

rwa <- merge(rwa.ewar.1963, rwa.ewar.1990, all = TRUE)
rwa <- merge(rwa, rwa.areg.1994, all = TRUE)
rwa <- merge(rwa, rwa.ewar.2001, all = TRUE)

rwa <- melt(rwa, id = c("sftgcode", "year"))
rwa$value <- as.numeric(as.character(rwa$value))
rwa <- cast(rwa, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
rwa[rwa == Inf] <- 0

# Senegal

sen.areg.1962 <- areg.it("SEN", 1962, 1963)
sen.ewar.1992 <- ewar.it("SEN", 1992, 1999)

sen <- merge(sen.areg.1962, sen.ewar.1992, all = TRUE)

sen <- melt(sen, id = c("sftgcode", "year"))
sen$value <- as.numeric(as.character(sen$value))
sen <- cast(sen, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
sen[sen == Inf] <- 0

# Sierra Leone

sie.areg.1967 <- areg.it("SIE", 1967, 1967)
sie.areg.1971 <- areg.it("SIE", 1971, 1971)
sie.rwar.1991 <- rwar.it("SIE", 1991, 2001)
sie.areg.1997 <- areg.it("SIE", 1997, 2002)

sie <- merge(sie.areg.1967, sie.areg.1971, all = TRUE)
sie <- merge(sie, sie.rwar.1991, all = TRUE)
sie <- merge(sie, sie.areg.1997, all = TRUE)

sie <- melt(sie, id = c("sftgcode", "year"))
sie$value <- as.numeric(as.character(sie$value))
sie <- cast(sie, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
sie[sie == Inf] <- 0

# Singapore

sin.areg.1963 <- areg.it("SIN", 1963, 1965)

sin <- sin.areg.1963

# Solomon Islands

sol.areg.2000 <- areg.it("SOL", 2000, 2003)

sol <- sol.areg.2000

# Somalia

som.areg.1969 <- areg.it("SOM", 1969, 1969)
som.rwar.1988 <- rwar.it("SOM", 1988, 1994)
som.ewar.1988 <- ewar.it("SOM", 1988, lastyear)
som.areg.1991 <- areg.it("SOM", 1991, 2012)

som <- merge(som.areg.1969, som.rwar.1988, all = TRUE)
som <- merge(som, som.ewar.1988, all = TRUE)
som <- merge(som, som.areg.1991, all = TRUE)

som <- melt(som, id = c("sftgcode", "year"))
som$value <- as.numeric(as.character(som$value))
som <- cast(som, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
som[som == Inf] <- 0

# South Africa

saf.rwar.1984 <- rwar.it("SAF", 1984, 1990)
saf.ewar.1987 <- ewar.it("SAF", 1987, 1996)

saf <- merge(saf.rwar.1984, saf.ewar.1987, all = TRUE)

saf <- melt(saf, id = c("sftgcode", "year"))
saf$value <- as.numeric(as.character(saf$value))
saf <- cast(saf, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
saf[saf == Inf] <- 0

# Sri Lanka

sri.ewar.1983 <- ewar.it("SRI", 1983, 2009)
sri.rwar.1987 <- rwar.it("SRI", 1987, 1989)

sri <- merge(sri.ewar.1983, sri.rwar.1987, all = TRUE)

sri <- melt(sri, id = c("sftgcode", "year"))
sri$value <- as.numeric(as.character(sri$value))
sri <- cast(sri, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
sri[sri == Inf] <- 0

# Sudan

sud.ewar.1956 <- ewar.it("SUD", 1956, 1972)
sud.areg.1958 <- areg.it("SUD", 1958, 1958)
sud.areg.1969 <- areg.it("SUD", 1969, 1971)
sud.ewar.1983 <- ewar.it("SUD", 1983, 2002)
sud.ewar.2003 <- ewar.it("SUD", 2003, lastyear)
sud.ewar.2011 <- ewar.it("SUD", 2011, lastyear)

sud <- merge(sud.ewar.1956, sud.areg.1958, all = TRUE)
sud <- merge(sud, sud.areg.1969, all = TRUE)
sud <- merge(sud, sud.ewar.1983, all = TRUE)
sud <- merge(sud, sud.ewar.2003, all = TRUE)
sud <- merge(sud, sud.ewar.2011, all = TRUE)

sud <- melt(sud, id = c("sftgcode", "year"))
sud$value <- as.numeric(as.character(sud$value))
sud <- cast(sud, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
sud[sud == Inf] <- 0

# Suriname

sur.areg.1980 <- areg.it("SUR", 1980, 1982)

sur <- sur.areg.1980

# Swaziland

swa.areg.1973 <- areg.it("SWA", 1973, 1973)

swa <- swa.areg.1973

# Syria

syr.areg.1961 <- areg.it("SYR", 1961, 1966)
syr.rwar.1979 <- rwar.it("SYR", 1979, 1982)
syr.ewar.2011 <- ewar.it("SYR", 2011, lastyear)

syr <- merge(syr.areg.1961, syr.rwar.1979, all = TRUE)
syr <- merge(syr, syr.ewar.2011, all = TRUE)

syr <- melt(syr, id = c("sftgcode", "year"))
syr$value <- as.numeric(as.character(syr$value))
syr <- cast(syr, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
syr[syr == Inf] <- 0

# Tajikistan

taj.rwar.1992 <- rwar.it("TAJ", 1992, 1998)

taj <- taj.rwar.1992

# Thailand

thi.rwar.1965 <- rwar.it("THI", 1965, 1983)
thi.areg.1971 <- areg.it("THI", 1971, 1971)
thi.areg.1976 <- areg.it("THI", 1976, 1976)
thi.ewar.2004 <- ewar.it("THI", 2004, lastyear)
thi.areg.2006 <- areg.it("THI", 2006, 2006)

thi <- merge(thi.rwar.1965, thi.areg.1971, all = TRUE)
thi <- merge(thi, thi.areg.1976, all = TRUE)
thi <- merge(thi, thi.ewar.2004, all = TRUE)
thi <- merge(thi, thi.areg.2006, all = TRUE)

thi <- melt(thi, id = c("sftgcode", "year"))
thi$value <- as.numeric(as.character(thi$value))
thi <- cast(thi, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
thi[thi == Inf] <- 0

# Turkey

tur.areg.1971 <- areg.it("TUR", 1971, 1971)
tur.areg.1980 <- areg.it("TUR", 1980, 1980)
tur.ewar.1984 <- ewar.it("TUR", 1984, 2000)
tur.ewar.2004 <- ewar.it("TUR", 2004, lastyear)

tur <- merge(tur.areg.1971, tur.areg.1980, all = TRUE)
tur <- merge(tur, tur.ewar.1984, all = TRUE)
tur <- merge(tur, tur.ewar.2004, all = TRUE)

tur <- melt(tur, id = c("sftgcode", "year"))
tur$value <- as.numeric(as.character(tur$value))
tur <- cast(tur, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
tur[tur == Inf] <- 0

# Uganda

uga.areg.1966 <- areg.it("UGA", 1966, 1969)
uga.ewar.1966 <- ewar.it("UGA", 1966, 1966)
uga.ewar.1980 <- ewar.it("UGA", 1980, 2006)
uga.rwar.1983 <- rwar.it("UGA", 1983, 1985)
uga.areg.1985 <- areg.it("UGA", 1985, 1986)

uga <- merge(uga.areg.1966, uga.ewar.1966, all = TRUE)
uga <- merge(uga, uga.ewar.1980, all = TRUE)
uga <- merge(uga, uga.rwar.1983, all = TRUE)
uga <- merge(uga, uga.areg.1985, all = TRUE)

uga <- melt(uga, id = c("sftgcode", "year"))
uga$value <- as.numeric(as.character(uga$value))
uga <- cast(uga, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
uga[uga == Inf] <- 0

# U.K.

uk.ewar.1971 <- ewar.it("UK", 1971, 1982)

uk <- uk.ewar.1971

# Uruguay

uru.areg.1971 <- areg.it("URU", 1971, 1973)

uru <- uru.areg.1971

# U.S.S.R.

uss.areg.1991 <- areg.it("USS", 1991, 1991)

uss <- uss.areg.1991

# South Vietnam

rvn.rwar.1958 <- rwar.it("RVN", 1958, 1975)

rvn <- rvn.rwar.1958

# North Yemen

yar.rwar.1962 <- rwar.it("YAR", 1962, 1970)

yar <- yar.rwar.1962

# South Yemen

ypr.rwar.1986 <- rwar.it("YPR", 1986, 1986)

ypr <- ypr.rwar.1986

# Yemen

yem.rwar.1994 <- rwar.it("YEM", 1994, 1994)
yem.rwar.2004 <- rwar.it("YEM", 2004, lastyear)
yem.rwar.2011 <- rwar.it("YEM", 2011, lastyear)

yem <- merge(yem.rwar.1994, yem.rwar.2004, all = TRUE)
yem <- merge(yem, yem.rwar.2011, all = TRUE)

yem <- melt(yem, id = c("sftgcode", "year"))
yem$value <- as.numeric(as.character(yem$value))
yem <- cast(yem, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
yem[yem == Inf] <- 0

# Yugoslavia

yug.ewar.1991 <- ewar.it("YUG", 1991, 1991)
yug.areg.1991 <- areg.it("YUG", 1991, 1991)

yug <- merge(yug.ewar.1991, yug.areg.1991, all = TRUE)

yug <- melt(yug, id = c("sftgcode", "year"))
yug$value <- as.numeric(as.character(yug$value))
yug <- cast(yug, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
yug[yug == Inf] <- 0

# Former Yugoslavia

ygs.ewar.1998 <- ewar.it("YGS", 1998, 1999)

ygs <- ygs.ewar.1998

# Zambia

zam.rwar.1964 <- rwar.it("ZAM", 1964, 1964)
zam.areg.1968 <- areg.it("ZAM", 1968, 1972)
zam.areg.1996 <- areg.it("ZAM", 1996, 1996)

zam <- merge(zam.rwar.1964, zam.areg.1968, all = TRUE)
zam <- merge(zam, zam.areg.1996, all = TRUE)

zam <- melt(zam, id = c("sftgcode", "year"))
zam$value <- as.numeric(as.character(zam$value))
zam <- cast(zam, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
zam[zam == Inf] <- 0

# Zimbabwe

zim.rwar.1972 <- rwar.it("ZIM", 1972, 1979)
zim.ewar.1981 <- ewar.it("ZIM", 1981, 1987)
zim.areg.1987 <- areg.it("ZIM", 1987, 1987)

zim <- merge(zim.rwar.1972, zim.ewar.1981, all = TRUE)
zim <- merge(zim, zim.areg.1987, all = TRUE)

zim <- melt(zim, id = c("sftgcode", "year"))
zim$value <- as.numeric(as.character(zim$value))
zim <- cast(zim, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
zim[zim == Inf] <- 0

########################################
# All! Together! Now! Alltogethernow!
########################################

all <- merge(afg, alb, all = TRUE)
all <- merge(all, alg, all = TRUE)
all <- merge(all, ang, all = TRUE)
all <- merge(all, arg, all = TRUE)
all <- merge(all, arm, all = TRUE)
all <- merge(all, aze, all = TRUE)
all <- merge(all, bng, all = TRUE)
all <- merge(all, blr, all = TRUE)
all <- merge(all, ben, all = TRUE)
all <- merge(all, bos, all = TRUE)
all <- merge(all, bra, all = TRUE)
all <- merge(all, bfo, all = TRUE)
all <- merge(all, mya, all = TRUE)
all <- merge(all, bui, all = TRUE)
all <- merge(all, cam, all = TRUE)
all <- merge(all, cen, all = TRUE)
all <- merge(all, cha, all = TRUE)
all <- merge(all, chl, all = TRUE)
all <- merge(all, chn, all = TRUE)
all <- merge(all, col, all = TRUE)
all <- merge(all, com, all = TRUE)
all <- merge(all, con, all = TRUE)
all <- merge(all, zai, all = TRUE)
all <- merge(all, cro, all = TRUE)
all <- merge(all, cub, all = TRUE)
all <- merge(all, cyp, all = TRUE)
all <- merge(all, cze, all = TRUE)
all <- merge(all, dji, all = TRUE)
all <- merge(all, dom, all = TRUE)
all <- merge(all, ecu, all = TRUE)
all <- merge(all, egy, all = TRUE)
all <- merge(all, sal, all = TRUE)
all <- merge(all, eqg, all = TRUE)
all <- merge(all, eti, all = TRUE)
all <- merge(all, fji, all = TRUE)
all <- merge(all, frn, all = TRUE)
all <- merge(all, gam, all = TRUE)
all <- merge(all, grg, all = TRUE)
all <- merge(all, gha, all = TRUE)
all <- merge(all, grc, all = TRUE)
all <- merge(all, gua, all = TRUE)
all <- merge(all, gui, all = TRUE)
all <- merge(all, gnb, all = TRUE)
all <- merge(all, guy, all = TRUE)
all <- merge(all, hai, all = TRUE)
all <- merge(all, hun, all = TRUE)
all <- merge(all, ind, all = TRUE)
all <- merge(all, ins, all = TRUE)
all <- merge(all, irn, all = TRUE)
all <- merge(all, irq, all = TRUE)
all <- merge(all, isr, all = TRUE)
all <- merge(all, ivo, all = TRUE)
all <- merge(all, jor, all = TRUE)
all <- merge(all, ken, all = TRUE)
all <- merge(all, rok, all = TRUE)
all <- merge(all, lao, all = TRUE)
all <- merge(all, leb, all = TRUE)
all <- merge(all, les, all = TRUE)
all <- merge(all, lbr, all = TRUE)
all <- merge(all, lib, all = TRUE)
all <- merge(all, mag, all = TRUE)
all <- merge(all, mal, all = TRUE)
all <- merge(all, mli, all = TRUE)
all <- merge(all, maa, all = TRUE)
all <- merge(all, mex, all = TRUE)
all <- merge(all, mld, all = TRUE)
all <- merge(all, mor, all = TRUE)
all <- merge(all, mzm, all = TRUE)
all <- merge(all, nep, all = TRUE)
all <- merge(all, nic, all = TRUE)
all <- merge(all, nir, all = TRUE)
all <- merge(all, nig, all = TRUE)
all <- merge(all, oma, all = TRUE)
all <- merge(all, pak, all = TRUE)
all <- merge(all, pan, all = TRUE)
all <- merge(all, png, all = TRUE)
all <- merge(all, per, all = TRUE)
all <- merge(all, phi, all = TRUE)
all <- merge(all, rum, all = TRUE)
all <- merge(all, rus, all = TRUE)
all <- merge(all, rwa, all = TRUE)
all <- merge(all, sen, all = TRUE)
all <- merge(all, sie, all = TRUE)
all <- merge(all, sin, all = TRUE)
all <- merge(all, sol, all = TRUE)
all <- merge(all, som, all = TRUE)
all <- merge(all, saf, all = TRUE)
all <- merge(all, sri, all = TRUE)
all <- merge(all, sud, all = TRUE)
all <- merge(all, sur, all = TRUE)
all <- merge(all, swa, all = TRUE)
all <- merge(all, syr, all = TRUE)
all <- merge(all, taj, all = TRUE)
all <- merge(all, thi, all = TRUE)
all <- merge(all, tur, all = TRUE)
all <- merge(all, uga, all = TRUE)
all <- merge(all, uk, all = TRUE)
all <- merge(all, uru, all = TRUE)
all <- merge(all, uss, all = TRUE)
all <- merge(all, rvn, all = TRUE)
all <- merge(all, yar, all = TRUE)
all <- merge(all, ypr, all = TRUE)
all <- merge(all, yem, all = TRUE)
all <- merge(all, yug, all = TRUE)
all <- merge(all, ygs, all = TRUE)
all <- merge(all, zam, all = TRUE)
all <- merge(all, zim, all = TRUE)

# Reshape
all <- melt(all, id = c("sftgcode", "year"))
all$value <- as.numeric(as.character(all$value))
all <- cast(all, sftgcode + year ~ variable, function(value) { min(value, na.rm = TRUE) } )
all[all == Inf] <- 0

# Fix types and sort for inspection
all$sftgcode <- as.character(all$sftgcode)
all$year <- as.numeric(as.character(all$year))
all <- all[order(all$sftgcode, all$year),]

# Create rack of names and years, starting in 1945, with codes
source("C:/Documents and Settings/Jay/My Documents/USHMM/statrisk/R/Country Frame Maker.R")

# Rename variables and fix variable type for country name
names(global) <- c("name", "year", "yrborn", "yrdied", "age")
global$name <- as.character(global$name)

# Rename data frame for next couple of steps.
global -> data

# Run PITF code maker.
source("C:/Documents and Settings/Jay/My Documents/USHMM/statrisk/R/pitf_code_maker.R")

# Run COW code maker.
source("C:/Documents and Settings/Jay/My Documents/USHMM/statrisk/R/pitf_code_to_cow_code.R")

# Make copy of results with desired name, fix variable names, & remove original.
data -> pitf
names(pitf) <- c("country", "year", "yrborn", "yrdied", "age", "sftgcode", "ccode")
rm(data, global)

# Merge all into pitf, trimming all to fit country birth & death years
prob <- merge(pitf, all, all.x = TRUE)
prob <- prob[order(prob$country, prob$year),]
prob <- prob[ which(prob$year >= 1955 & prob$year <= 2012),] # Trim to PITF range
prob[is.na(prob) == TRUE] <- 0 # Replace NAs with 0s

#################################
# Create Other Variables
#################################

# Starts of episodes
prob$areg.st <- ifelse(prob$areg.dur==1, 1, 0)
prob$ewar.st <- ifelse(prob$ewar.dur==1, 1, 0)
prob$rwar.st <- ifelse(prob$rwar.dur==1, 1, 0)

# Ongoing episodes
prob$areg.on <- ifelse(prob$areg.dur > 0, 1, 0)
prob$ewar.on <- ifelse(prob$ewar.dur > 0, 1, 0)
prob$rwar.on <- ifelse(prob$rwar.dur > 0, 1, 0)

# Consolidated
prob$pitf.st <- ifelse(prob$areg.st==1 | prob$ewar.st==1 | prob$rwar.st==1, 1, 0)
prob$pitf.on <- ifelse(prob$areg.on==1 | prob$ewar.on==1 | prob$rwar.on==1, 1, 0)
for (i in 1:length(prob$country))
     prob$pitf.dur.min[i] <-
              ifelse(prob$areg.dur[i] > 0 & prob$ewar.dur[i] > 0 & prob$rwar.dur[i] > 0, min(prob$areg.dur[i], prob$ewar.dur[i], prob$rwar.dur[i]),
              ifelse(prob$areg.dur[i] > 0 & prob$ewar.dur[i] > 0 & prob$rwar.dur[i] == 0, min(prob$areg.dur[i], prob$ewar.dur[i]),
              ifelse(prob$areg.dur[i] > 0 & prob$ewar.dur[i] == 0 & prob$rwar.dur[i] > 0, min(prob$areg.dur[i], prob$rwar.dur[i]),
              ifelse(prob$areg.dur[i] == 0 & prob$ewar.dur[i] > 0 & prob$rwar.dur[i] > 0, min(prob$ewar.dur[i], prob$rwar.dur[i]),
              ifelse(prob$areg.dur[i] > 0, prob$areg.dur[i],
              ifelse(prob$ewar.dur[i] > 0, prob$ewar.dur[i],
              ifelse(prob$rwar.dur[i] > 0, prob$rwar.dur[i],
              0  )))))))
for (i in 1:length(prob$country)) prob$pitf.dur.max[i] <- max(prob$areg.dur[i], prob$ewar.dur[i], prob$rwar.dur[i])

# Strip down to id, year, & PITF variables
probsub <- subset(prob, select=c(sftgcode, year,
                                 areg.st, areg.dur, areg.on,
                                 ewar.st, ewar.dur, ewar.on,
                                 rwar.st, rwar.dur, rwar.on,
                                 pitf.st, pitf.on, pitf.dur.min, pitf.dur.max))

# Write out the data set
write.csv(probsub, file = paste("pitfprob", as.character(lastyear), "csv", sep="."),
          quote = FALSE, row.names = FALSE)
rm(prob)
