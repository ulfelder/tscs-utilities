# This script is used to turn the list of coups and coup attempts compiled by 
# the Center for Systemic Peace into merge-able country-year data.

library(downloader)
temp <- tempfile()
download.file("http://www.systemicpeace.org/inscr/CSPCoupsList2012.xls", temp)
library(XLConnect)
couplist <- readWorksheetFromFile(temp, sheet=1)
couplist <- subset(couplist, is.na(scode)==FALSE, select=c(scode, year, success))
names(couplist) <- c("sftgcode", "year", "success")

# Generate dummy vars for each type of coup event.
couplist$successful <- ifelse(couplist$success==1, 1, 0)
couplist$failed <- ifelse(couplist$success==2, 1, 0)
couplist$plot <- ifelse(couplist$success==3, 1, 0)
couplist$rumor <- ifelse(couplist$success==4, 1, 0)

# Create tables of annual sums by type.
coupsum.s <- tapply(couplist$successful, list(couplist$sftgcode, couplist$year), sum)
coupsum.f <- tapply(couplist$failed, list(couplist$sftgcode, couplist$year), sum)
coupsum.p <- tapply(couplist$plot, list(couplist$sftgcode, couplist$year), sum)
coupsum.r <- tapply(couplist$rumor, list(couplist$sftgcode, couplist$year), sum)

# Reshape those tables from wide to long for merging.
library(reshape)
coup.s <- melt(coupsum.s)
coup.f <- melt(coupsum.f)
coup.p <- melt(coupsum.p)
coup.r <- melt(coupsum.r)

# Rename variables for merging.
names(coup.s) <- c("sftgcode", "year", "cou.successful")
names(coup.f) <- c("sftgcode", "year", "cou.failed")
names(coup.p) <- c("sftgcode", "year", "cou.plot")
names(coup.r) <- c("sftgcode", "year", "cou.rumor")

# Create global country-year scaffolding with PITF codes.
source("C:/Documents and Settings/Jay/My Documents/USHMM/statrisk/R/country frame maker.R")
source("C:/Documents and Settings/Jay/My Documents/USHMM/statrisk/R/pitf_code_maker.R")
data <- subset(data, select = c(code, year))
names(data) <- c("sftgcode", "year")

# Merge.
data <- merge(data, coup.s, all.x = TRUE)
data <- merge(data, coup.f, all.x = TRUE)
data <- merge(data, coup.p, all.x = TRUE)
data <- merge(data, coup.r, all.x = TRUE)

# Replace NAs with 0s, conditional on country existence
data$cou.successful <- ifelse(is.na(data$cou.successful)==T, 0, data$cou.successful)
data$cou.failed <- ifelse(is.na(data$cou.failed)==T, 0, data$cou.failed)
data$cou.plot <- ifelse(is.na(data$cou.plot)==T, 0, data$cou.plot)
data$cou.rumor <- ifelse(is.na(data$cou.rumor)==T, 0, data$cou.rumor)

# Create binary versions.
data$cou.s.d <- ifelse(data$cou.successful > 0, 1, 0)
data$cou.f.d <- ifelse(data$cou.failed > 0, 1, 0)
data$cou.p.d <- ifelse(data$cou.plot > 0, 1, 0)
data$cou.r.d <- ifelse(data$cou.rumor > 0, 1, 0)

# Rename and clean up.
data -> coups
rm(couplist, coupsum.s, coupsum.f, coupsum.p, coupsum.r, coup.s, coup.f, coup.p, coup.r)

# Write out a .csv version of the resulting data set.
write.csv(coups, file = "coupdata.csv", row.names = FALSE)
