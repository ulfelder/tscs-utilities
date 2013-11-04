# I've grouped these by region to make them a litter easier to browse and edit as required.

# A function to make a rectangle as long as the country has been alive.
kuntry <- function(name, birth, death) {
              year <- seq(birth, ifelse(death < as.numeric(substr(Sys.Date(),1,4)), death,
                                     as.numeric(substr(Sys.Date(),1,4))), 1)
              country <- rep(name, times = length(year))
              yrborn <- rep(birth, times = length(year))
              yrdied <- rep(death, times = length(year))
              z <- cbind(country, year, yrborn, yrdied)
              return(z)
}

# Country list sourced to:
# Wikipedia: http://en.wikipedia.org/wiki/List_of_sovereign_states_by_date_of_formation
# CIA Factbook: https://www.cia.gov/library/publications/the-world-factbook/fields/2088.html

# AFRICA

africa <- rbind(kuntry("Algeria", 1962, 9999),
                kuntry("Angola", 1975, 9999),
                kuntry("Benin", 1960, 9999)
                kuntry("Botswana", 1966, 9999),
                kuntry("Burkina Faso", 1960, 9999),
                kuntry("Burundi", 1962, 9999),
                kuntry("Cameroon", 1960, 9999),
                kuntry("Cape Verde", 1975, 9999),
                kuntry("Central African Republic", 1960, 9999),
                kuntry("Chad", 1960, 9999),
                kuntry("Comoros", 1975, 9999),
                kuntry("Congo-Kinshasa", 1960, 9999),
                kuntry("Congo-Brazzaville", 1960, 9999),
                kuntry("Ivory Coast", 1960, 9999),
                kuntry("Djibouti", 1977, 9999),
                kuntry("Egypt", 1953, 9999),
                kuntry("Equatorial Guinea", 1968, 9999),
                kuntry("Eritrea", 1993, 9999),
                kuntry("Ethiopia", -100, 9999),
                kuntry("Gabon", 1960, 9999),
                kuntry("Gambia", 1965, 9999),
                kuntry("Ghana", 1957, 9999),
                kuntry("Guinea", 1958, 9999),
                kuntry("Guinea-Bissau", 1974, 9999),
                kuntry("Kenya", 1963, 9999),
                kuntry("Lesotho", 1966, 9999),
                kuntry("Liberia", 1847, 9999),
                kuntry("Libya", 1951, 9999),
                kuntry("Madagascar", 1960, 9999),
                kuntry("Malawi", 1964, 9999),
                kuntry("Mali", 1960, 9999),
                kuntry("Mauritania", 1960, 9999),
                kuntry("Mauritius", 1968, 9999),
                kuntry("Morocco", 1956, 9999),
                kuntry("Mozambique", 1975, 9999),
                kuntry("Namibia", 1990, 9999),
                kuntry("Niger", 1960, 9999),
                kuntry("Nigeria", 1960, 9999),
                kuntry("Rwanda", 1962, 9999),
                kuntry("Sao Tome and Principe", 1975, 9999),
                kuntry("Senegal", 1960, 9999),
                kuntry("Seychelles", 1976, 9999),
                kuntry("Sierra Leone", 1961, 9999),
                kuntry("Somalia", 1960, 9999),
                kuntry("South Africa", 1910, 9999),
                kuntry("South Sudan", 2011, 9999),
                kuntry("Sudan", 1956, 9999),
                kuntry("Swaziland", 1968, 9999),
                kuntry("Tanzania", 1961, 9999),
                kuntry("Togo", 1960, 9999),
                kuntry("Tunisia", 1956, 9999),
                kuntry("Uganda", 1962, 9999),
                kuntry("Zambia", 1964, 9999),
                kuntry("Zimbabwe", 1980, 9999))

# AMERICAS

americas <- rbind(kuntry("Antigua and Barbuda", 1981, 9999),
                  kuntry("Argentina", 1816, 9999),
                  kuntry("Bahamas", 1973, 9999),
                  kuntry("Barbados", 1966, 9999),
                  kuntry("Belize", 1981, 9999),
                  kuntry("Bolivia", 1825, 9999),
                  kuntry("Brazil", 1825, 9999),
                  kuntry("Canada", 1867, 9999),
                  kuntry("Chile", 1818, 9999),
                  kuntry("Colombia", 1819, 9999),
                  kuntry("Costa Rica", 1821, 9999),
                  kuntry("Cuba", 1868, 9999),
                  kuntry("Dominica", 1978, 9999),
                  kuntry("Dominican Republic", 1865, 9999),
                  kuntry("Ecuador", 1830, 9999),
                  kuntry("El Salvador", 1841, 9999),
                  kuntry("Grenada", 1974, 9999),
                  kuntry("Guatemala", 1839, 9999),
                  kuntry("Guyana", 1966, 9999),
                  kuntry("Haiti", 1804, 9999),
                  kuntry("Honduras", 1838, 9999),
                  kuntry("Jamaica", 1962, 9999),
                  kuntry("Mexico", 1821, 9999),
                  kuntry("Nicaragua", 1838, 9999),
                  kuntry("Panama", 1903, 9999),
                  kuntry("Paraguay", 1811, 9999),
                  kuntry("Peru", 1879, 9999),
                  kuntry("Saint Kitts and Nevis", 1983, 9999),
                  kuntry("Saint Lucia", 1979, 9999),
                  kuntry("Saint Vincent and the Grenadines", 1979, 9999),
                  kuntry("Suriname", 1975, 9999),
                  kuntry("Trinidad and Tobago", 1962, 9999),
                  kuntry("United States", 1783, 9999),
                  kuntry("Uruguay", 1825, 9999),
                  kuntry("Venezuela", 1830, 9999))

# ASIA AND MIDDLE EAST

asia <- rbind(kuntry("Afghanistan", 1919, 9999),
              kuntry("Bahrain", 1971, 9999),
              kuntry("Bangladesh", 1971, 9999),
              kuntry("Bhutan", 1885, 9999),
              kuntry("Brunei", 1984, 9999),
              kuntry("Cambodia", 1953, 9999),
              kuntry("China", -221, 9999),
              kuntry("India", 1947, 9999),
              kuntry("Indonesia", 1949, 9999),
              kuntry("Iran", 1501, 9999),
              kuntry("Iraq", 1932, 9999),
              kuntry("Israel", 1948, 9999),
              kuntry("Japan", 660, 9999),
              kuntry("Jordan", 1946, 9999),
              kuntry("Kuwait", 1961, 9999),
              kuntry("Laos", 1949, 9999),
              kuntry("Lebanon", 1943, 9999),
              kuntry("Malaysia", 1957, 9999),
              kuntry("Maldives", 1965, 9999),
              kuntry("Mongolia", 1911, 9999),
              kuntry("Myanmar", 1948, 9999),
              kuntry("Nepal", 1768, 9999),
              kuntry("North Korea", 1948, 9999),
              kuntry("Oman", 1650, 9999),
              kuntry("Pakistan", 1947, 9999),
              kuntry("Philippines", 1898, 9999),
              kuntry("Qatar", 1971, 9999),
              kuntry("Saudi Arabia", 1932, 9999),
              kuntry("Singapore", 1965, 9999),
              kuntry("South Korea", 1948, 9999),
              kuntry("Sri Lanka", 1972, 9999),
              kuntry("Syria", 1946, 9999),
              kuntry("Taiwan", 1949, 9999),
              kuntry("Thailand", 1350, 9999),
              kuntry("Timor Leste", 2002, 9999),
              kuntry("United Arab Emirates", 1971, 9999),
              kuntry("Vietnam", 1976, 9999),
              kuntry("Yemen", 1990, 9999))

# EUROPE

eurasia <- rbind(kuntry("Albania", 1912, 9999),
                 kuntry("Andorra", 1813, 9999),
                 kuntry("Austria", 1918, 9999),
                 kuntry("Belarus", 1991, 9999),
                 kuntry("Belgium", 1830, 9999),
                 kuntry("Bosnia and Herzegovina", 1992, 9999),
                 kuntry("Bulgaria", 1878, 9999),
                 kuntry("Croatia", 1991, 9999),
                 kuntry("Cyprus", 1960, 9999),
                 kuntry("Czech Republic", 1993, 9999),
                 kuntry("Denmark", 965, 9999),
                 kuntry("Estonia", 1991, 9999),
                 kuntry("Finland", 1918, 9999),
                 kuntry("France", 843, 9999),
                 kuntry("Germany", 1990, 9999),
                 kuntry("Greece", 1832, 9999),
                 kuntry("Hungary", 1849, 9999),
                 kuntry("Iceland", 1944, 9999),
                 kuntry("Ireland", 1922, 9999),
                 kuntry("Italy", 1861, 9999),
                 kuntry("Kosovo", 2008, 9999),
                 kuntry("Latvia", 1991, 9999),
                 kuntry("Liechtenstein", 1813, 9999),
                 kuntry("Lithuania", 1991, 9999),
                 kuntry("Luxembourg", 1890, 9999),
                 kuntry("Macedonia", 1991, 9999),
                 kuntry("Malta", 1964, 9999),
                 kuntry("Moldova", 1991, 9999),
                 kuntry("Monaco", 1861, 9999),
                 kuntry("Montenegro", 2006, 9999),
                 kuntry("Netherlands", 1568, 9999),
                 kuntry("Norway", 1905, 9999),
                 kuntry("Poland", 1918, 9999),
                 kuntry("Portugal", 1143, 9999),
                 kuntry("Romania", 1877, 9999),
                 kuntry("San Marino", 301, 9999),
                 kuntry("Serbia", 2006, 9999),
                 kuntry("Slovakia", 1993, 9999),
                 kuntry("Slovenia", 1991, 9999),
                 kuntry("Spain", 1492, 9999),
                 kuntry("Sweden", 1523, 9999),
                 kuntry("Switzerland", 1291, 9999),
                 kuntry("Turkey", 1923, 9999),
                 kuntry("Ukraine", 1991, 9999),
                 kuntry("United Kingdom", 1536, 9999),
                 kuntry("Armenia", 1991, 9999),
                 kuntry("Azerbaijan", 1991, 9999),
                 kuntry("Georgia", 1991, 9999),
                 kuntry("Kazakhstan", 1991, 9999),
                 kuntry("Kyrgyzstan", 1991, 9999),
                 kuntry("Russia", 1991, 9999),
                 kuntry("Tajikistan", 1991, 9999),
                 kuntry("Turkmenistan", 1991, 9999),
                 kuntry("Uzbekistan", 1991, 9999))

# OCEANIA

oceania <- rbind(kuntry("Australia", 1901, 9999),
                 kuntry("Fiji", 1970, 9999),
                 kuntry("Kiribati", 1979, 9999),
                 kuntry("Marshall Islands", 1979, 9999),
                 kuntry("Federated States of Micronesia", 1979, 9999),
                 kuntry("Nauru", 1968, 9999),
                 kuntry("New Zealand", 1907, 9999),
                 kuntry("Palau", 1981, 9999),
                 kuntry("Papua New Guinea", 1975, 9999),
                 kuntry("Samoa", 1962, 9999),
                 kuntry("Solomon Islands", 1978, 9999),
                 kuntry("Tonga", 1970, 9999),
                 kuntry("Tuvalu", 1978, 9999),
                 kuntry("Vanuatu", 1980, 9999))

# DEFUNCT

defunct <- rbind(kuntry("Czechoslovakia", 1918, 1992),
                 kuntry("Yugoslavia", 1918, 1992),
                 kuntry("Federal Republic of Yugoslavia", 1992, 2002),
                 kuntry("Serbia and Montenegro", 2003, 2006),
                 kuntry("West Germany", 1945, 1990),
                 kuntry("East Germany", 1945, 1990),
                 kuntry("Soviet Union", 1922, 1991),
                 kuntry("North Yemen", 1918, 1990),
                 kuntry("South Yemen", 1967, 1990),
                 kuntry("North Vietnam", 1954, 1976),
                 kuntry("South Vietnam", 1954, 1976))

# Aggregate and fix types and remove region objects

global <- as.data.frame(rbind(africa, americas, asia, eurasia, oceania, defunct))
global$year <- as.numeric(as.character(global$year))
global$yrborn <- as.numeric(as.character(global$yrborn))
global$yrdied <- as.numeric(as.character(global$yrdied))
rm(africa, americas, asia, eurasia, oceania, defunct)

# Trim at 1945
global <- subset(global, year >= 1945)

# Calculate country age
global$age <- global$year - global$yrborn

# Rename data frame
global -> data

# Add binary variables and a categorical variable indicating U.S. Dept. of State regions
data$reg.eap <- ifelse(data$country=="Australia" | data$country=="Brunei" | data$country=="Burma" | data$country=="Cambodia" |
                       data$country=="China" | data$country=="East Timor" | data$country=="Fiji" | data$country=="Indonesia" |
                       data$country=="Japan" | data$country=="Kiribati" | data$country=="Laos" | data$country=="Malaysia" |
                       data$country=="Marshall Islands" | data$country=="Micronesia" | data$country=="Mongolia" | data$country=="Nauru" |
                       data$country=="New Zealand" | data$country=="North Korea" | data$country=="Palau" | data$country=="Papua New Guinea" |
                       data$country=="Philippines" | data$country=="Samoa" | data$country=="Singapore" | data$country=="Solomon Islands" |
                       data$country=="South Korea" | data$country=="Taiwan" | data$country=="Thailand" | data$country=="Tonga" |
                       data$country=="Tuvalu" | data$country=="Vanuatu" | data$country=="Vietnam" |
                       data$country=="South Vietnam" | data$country=="North Vietnam" | data$country=="Myanmar" | data$country=="Timor Leste",
                       1, 0)
data$reg.afr <- ifelse(data$country=="Angola" | data$country=="Benin" | data$country=="Botswana" | data$country=="Burkina Faso" |
                       data$country=="Burundi" | data$country=="Cameroon" | data$country=="Cape Verde" | data$country=="Central African Republic" |
                       data$country=="Chad" | data$country=="Comoros" | data$country=="Congo-Kinshasa" | data$country=="Congo-Brazzaville" |
                       data$country=="Ivory Coast" | data$country=="Djibouti" | data$country=="Equatorial Guinea" | data$country=="Eritrea" |
                       data$country=="Ethiopia" | data$country=="Gabon" | data$country=="Gambia" | data$country=="Ghana" |
                       data$country=="Guinea" | data$country=="Guinea-Bissau" | data$country=="Kenya" | data$country=="Lesotho" |
                       data$country=="Liberia" | data$country=="Madagascar" | data$country=="Malawi" | data$country=="Mali" |
                       data$country=="Mauritania" | data$country=="Mauritius" | data$country=="Mozambique" | data$country=="Namibia" |
                       data$country=="Niger" | data$country=="Nigeria" | data$country=="Rwanda" | data$country=="Sao Tome and Principe" |
                       data$country=="Senegal" | data$country=="Seychelles" | data$country=="Sierra Leone" | data$country=="Somalia" |
                       data$country=="South Africa" | data$country=="Sudan" | data$country=="South Sudan" | data$country=="Swaziland" |
                       data$country=="Tanzania" | data$country=="Togo" | data$country=="Uganda" | data$country=="Zambia" |
                       data$country=="Zimbabwe",
                       1, 0)
data$reg.eur <- ifelse(data$country=="Albania" | data$country=="Armenia" | data$country=="Austria" | data$country=="Azerbaijan" |
                       data$country=="Belarus" | data$country=="Belgium" | data$country=="Bosnia and Herzegovina" | data$country=="Bulgaria" |
                       data$country=="Croatia" | data$country=="Czech Republic" | data$country=="Cyprus" | data$country=="Denmark" |
                       data$country=="Estonia" | data$country=="Finland" | data$country=="France" | data$country=="Georgia" |
                       data$country=="Germany" | data$country=="Greece" | data$country=="Hungary" | data$country=="Iceland" |
                       data$country=="Ireland" | data$country=="Italy" | data$country=="Kosovo" | data$country=="Latvia" |
                       data$country=="Lithuania" | data$country=="Liechtenstein" | data$country=="Macedonia" | data$country=="Malta" |
                       data$country=="Moldova" | data$country=="Monaco" | data$country=="Montenegro" | data$country=="Netherlands" |
                       data$country=="Norway" | data$country=="Poland" | data$country=="Portugal" | data$country=="Romania" |
                       data$country=="Russia" | data$country=="San Marino" | data$country=="Serbia" | data$country=="Slovakia" |
                       data$country=="Slovenia" | data$country=="Spain" | data$country=="Sweden" | data$country=="Switzerland" |
                       data$country=="Turkey" | data$country=="Ukraine" | data$country=="United Kingdom" |
                       data$country=="East Germany" | data$country=="West Germany" | data$country=="Soviet Union" | data$country=="Yugoslavia" |
                       data$country=="Federal Republic of Yugoslavia" | data$country=="Serbia and Montenegro" | data$country=="Czechoslovakia",
                       1, 0)
data$reg.mna <- ifelse(data$country=="Algeria" | data$country=="Bahrain" | data$country=="Egypt" | data$country=="Iran" |
                       data$country=="Iraq" | data$country=="Israel" | data$country=="Jordan" | data$country=="Kuwait" |
                       data$country=="Lebanon" | data$country=="Libya" | data$country=="Morocco" | data$country=="Oman" |
                       data$country=="Palestinian Territories" | data$country=="Qatar" | data$country=="Saudi Arabia" | data$country=="Syria" |
                       data$country=="Tunisia" | data$country=="United Arab Emirates" | data$country=="Yemen" |
                       data$country=="North Yemen" | data$country=="South Yemen",
                       1, 0)
data$reg.sca <- ifelse(data$country=="Afghanistan" | data$country=="Bangladesh" | data$country=="Bhutan" | data$country=="India" |
                       data$country=="Kazakhstan" | data$country=="Kyrgyzstan" | data$country=="Maldives" | data$country=="Nepal" |
                       data$country=="Pakistan" | data$country=="Sri Lanka" | data$country=="Tajikistan" | data$country=="Turkmenistan" |
                       data$country=="Uzbekistan",
                       1, 0)
data$reg.amr <- ifelse(data$country=="Antigua and Barbuda" | data$country=="Argentina" | data$country=="Bahamas" | data$country=="Barbados" |
                       data$country=="Belize" | data$country=="Bolivia" | data$country=="Brazil" | data$country=="Canada" |
                       data$country=="Cayman Islands" | data$country=="Chile" | data$country=="Colombia" | data$country=="Costa Rica" |
                       data$country=="Cuba" | data$country=="Dominica" | data$country=="Dominican Republic" | data$country=="Ecuador" |
                       data$country=="El Salvador" | data$country=="Grenada" | data$country=="Guatemala" | data$country=="Guyana" |
                       data$country=="Haiti" | data$country=="Honduras" | data$country=="Jamaica" | data$country=="Mexico" |
                       data$country=="Nicaragua" | data$country=="Panama" | data$country=="Paraguay" | data$country=="Peru" |
                       data$country=="Saint Kitts and Nevis" | data$country=="Saint Lucia" | data$country=="Saint Vincent and the Grenadines" | data$country=="Suriname" |
                       data$country=="Trinidad and Tobago" | data$country=="United States" | data$country=="Uruguay" | data$country=="Venezuela",
                       1, 0)
data$region <- NA
data$region[data$reg.afr==1] <- "Africa"
data$region[data$reg.eap==1] <- "East Asia & Pacific"
data$region[data$reg.eur==1] <- "Europe & Eurasia"
data$region[data$reg.mna==1] <- "Middle East & North Africa"
data$region[data$reg.sca==1] <- "South & Central Asia"
data$region[data$reg.amr==1] <- "Americas"

# Rename country name variable to play better with other utilities
require(reshape)
data <- rename(data, c(country="name"))
