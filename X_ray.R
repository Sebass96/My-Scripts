### X Ray ###

# Install packages

install.packages("xray")

library(xray)

# Anomaly deteccion

data(longley)

badLongley=longley

badLongley$GNP=NA

xray::anomalies(badLongley)

# Distributions

distrLongley=longley

distrLongley$testCategorical=c(rep('One',7), rep('Two', 9))

xray::distributions(distrLongley)

x11()

# Distributions along a time axis

dateLongley=longley

dateLongley$Year=as.Date(paste0(dateLongley$Year,'-01-01'))

dateLongley$Data='Original'

ndateLongley=dateLongley

ndateLongley$GNP=dateLongley$GNP+10

ndateLongley$Data='Offseted'

xray::timebased(rbind(dateLongley, ndateLongley), 'Year')
