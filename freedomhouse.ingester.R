# Thanks to GitHub user ramnathv for making an earlier version of this script much cleaner

# download report in xls format
require(downloader)
target <- paste("http:///www.freedomhouse.org/sites/default/files/Country%20Status%20and%20Ratings%2C%201973-",
                as.character(substr(Sys.Date(),1,4)),
                "%20%28FINAL%29_0.xls")
temp <- tempfile()
download(target, temp)

# read data using the xlsx package
require(xlsx)
dat <- read.xlsx2(temp, sheetIndex = 1, startRow = 7)

# format column names as variable_year
nYears = (NCOL(dat) - 1)/3
var_years = expand.grid( x= c('PR', 'CL', 'Status'), y = 1972:(1972 + nYears - 1))
names(dat) = c('country', paste(var_years$x, var_years$y, sep = "_"))

# melt the data, split the variable_year column and voila!
require(reshape2)
dat_m <- melt(dat, id = 'country')
dat_m <- cbind(dat_m, colsplit(dat_m$variable, "_", names = c('indicator', 'year')))
dat_m$variable = NULL

# Write file to wd in .csv format
write.csv(dat_m, paste("fhfiw", as.character(substr(Sys.Date(),1,4)), "csv", sep="."), header = TRUE)
unlink(temp)
