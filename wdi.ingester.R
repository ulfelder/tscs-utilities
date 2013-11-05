# This script uses the 'WDI' package in R to download selected data from the
# latest version of the World Bank's World Development Indicators and formats 
# them for merging with other country-year data using PITF country codes as
# the case IDs. To modify the variables selected, you need to make changes to
# 'wdilist' and the commands that follow "Subset" and "Rename variables". 

require(WDI)
require(countrycode)

wdilist <- c("NE.TRD.GNFS.ZS",     # Trade (% of GDP)
             "NY.GDP.PCAP.PP.KD",  # GDP per capita, PPP (constant 2005 intl $)
             "NY.GDP.PCAP.KD",     # GDP per capita (constant 2000 US$)
             "NY.GDP.MKTP.KD.ZG",  # GDP growth (annual %)
             "NV.AGR.TOTL.ZS",     # Agriculture, value added (% of GDP)
             "NV.IND.MANF.ZS",     # Manufacturing, value added (% of GDP)
             "NV.IND.TOTL.ZS",     # Industry, value added (% of GDP)
             "NV.SRV.TETC.ZS",     # Services, etc., value added (% of GDP)
             "GC.TAX.TOTL.GD.ZS",  # Tax revenue (% of GDP)
             "GC.DOD.TOTL.GD.ZS",  # Central government debt, total (% of GDP)
             "SP.POP.GROW",        # Population growth (annual %)
             "SP.POP.TOTL",        # Population, total
             "EN.POP.DNST",        # Population density (people per sq. km of land area)
             "SP.URB.TOTL.IN.ZS",  # Urban population (% of total)
             "MS.MIL.TOTL.P1",     # Armed forces personnel, total
             "MS.MIL.TOTL.TF.ZS",  # Armed forces personnel (% of total labor force)
             "NY.ADJ.DFOR.GN.ZS",  # Adjusted savings: forest depletion (% of GNI)
             "NY.ADJ.DMIN.GN.ZS",  # Adjusted savings: mineral depletion (% of GNI)
             "NY.ADJ.DNGY.GN.ZS" ) # Adjusted savings: energy depletion (% of GNI)

# Extract latest version of desired variables from WDI.
wdi <- WDI(country="all", indicator = wdilist, extra = FALSE,
           start = 1960, end = as.numeric(substr(Sys.Date(),1,4)))

# Get PITF country codes via COW numeric codes from iso2c codes using 'countrycode'.
wdi$ccode <- countrycode(wdi$iso2c, "iso2c", "cown")
wdi -> data
source("C:/Documents and Settings/Jay/My Documents/USHMM/statrisk/R/COW_to_PITF_code.R")
data -> wdi
rm(data)

# Subset.
wdi <- subset(wdi, is.na(sftgcode)==FALSE,
              select=c(sftgcode, year,
                       SP.POP.TOTL, SP.URB.TOTL.IN.ZS, NY.GDP.PCAP.PP.KD, NY.GDP.PCAP.KD, NY.GDP.MKTP.KD.ZG,
                       NE.TRD.GNFS.ZS,
                       MS.MIL.TOTL.P1, MS.MIL.TOTL.TF.ZS, 
                       NY.ADJ.DNGY.GN.ZS, NY.ADJ.DMIN.GN.ZS, NY.ADJ.DFOR.GN.ZS ) )

# Rename variables.
names(wdi) <- c("sftgcode", "year",
                "wdi.popsize", "wdi.urban", "wdi.gdppcppp", "wdi.gdppc", "wdi.gdppcgrow",
                "wdi.trade",
                "wdi.miltot", "wdi.milpct",
                "wdi.energy", "wdi.minerals", "wdi.forest" )

# Write out as .csv.
write.csv(wdi, file = paste("wdi.", substr(Sys.Date(),1,4), substr(Sys.Date(),6,7), substr(Sys.Date(),9,10), ".csv", sep=""), row.names=FALSE)
