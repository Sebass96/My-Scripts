#### DataMaid ###

# Load Packages

install.packages("dataReporter")
install.packages("dataMaid")

# Generate Report

library("dataMaid")
data(trees)
makeDataReport(trees)

# Check Data

data(toyData)
check(toyData$events)  # Individual check of events
check(toyData) # Check all variables at once

check(toyData$events, checks = setChecks(numeric = "identifyMissing"))

# Visualization

#Visualize a variable

visualize(toyData$events)

#Visualize a dataset

visualize(toyData)

#Summarize a variable with default settings:
summarize(toyData$events) 

#Summarize a variable with user-specified settings:
summarize(toyData$events, summaries = setSummaries(all =  c("centralValue", "minMax"))
          

vignette("extending_dataMaid")          
          
library(shiny)
runUrl("https://github.com/ekstroem/dataMaid/raw/master/app/app.zip")
