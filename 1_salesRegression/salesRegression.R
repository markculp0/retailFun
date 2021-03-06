
# ===========================================
# R Version of Excel Sales Regression Example
# -------------------------------------------
# Instructor Santiago Gallino's Excel Data
# from EDX Retail Fundamental's Course
# ===========================================

options(scipen = 999)

library(dplyr)
library(scales)
library(readxl)
library(ggplot2)
library(reshape2)
library(lubridate)

# Read in Sales Regression data
sr <- read_xlsx("RFundX_2017_M1_2_Sales_Regression.xlsx", "All Data", "A2:C166")

# ==================== Regression Example ====================

# Create trend column
sr$Trend <- 1:164

# Create matrix columns for months
m <- matrix(0L,nrow = 164, ncol = 12)

# Convert to DF
m <- as.data.frame(m)

# Get vector of column numbers with 1's
one <- as.numeric(format(sr$Week,"%m"))

# Place 1 in col from matching month
for (i in 1:164) {
  m[i, one[i]] <- 1
}
rm(i,one)

# Combine month data
sr <- cbind(sr, m)
rm(m)  

# Fit the Linear Model Using all Variables
fitAllModel <- lm(sr$Sales ~ sr$`Total Promos` + sr$Trend + 
            factor(sr$V1) + factor(sr$V2) + factor(sr$V3) + factor(sr$V4) +
            factor(sr$V5) + factor(sr$V6) + factor(sr$V7) + factor(sr$V8) +
            factor(sr$V9) + factor(sr$V10) + factor(sr$V11) + factor(sr$V12))

# Summarize Linear model
summary(fitAllModel)

# Generate predicted values fm lm
All_Model <- predict(fitAllModel)

# Get Week and Actual Demand/Sales 
# data for plot
srPlot <- sr[,1:2]

# Bind Forecast to Actual data
srPlot<- cbind(srPlot, All_Model)
rm(All_Model)

# Melt data w reshape2 lib
# or "gather" fm tidyr
srPlotLong <- melt(srPlot, id="Week")
rm(srPlot)

# Create plot
g = ggplot(srPlotLong, aes(x = Week, y=value/1000, color=variable))
# g = ggplot(sr2long, aes(x = Week, y=value/1000, linetype=variable))

g = g + ggtitle("Sales vs. Model: All Data")

g = g + geom_line()

#g = g + geom_line(mapping = aes(y = newY/1000, colour = newY/1000))
#g = g + geom_line(aes(y = Sales/1000, colour=Sales/1000))

# g = g + scale_colour_manual(values=c("black", "orange")) 

# label = comma | dollar_format()
g = g + scale_y_continuous(label = dollar_format(),
                           limits = c(min(srPlotLong$value),max(srPlotLong$value))/1000)

g = g + labs(x="Week", y="Sales  (000s)")

g
rm(g, srPlotLong)

# ================ Regression Forecast Example ===============

# Now use only historical data through Week 125
# to generate a forecast for the remaining weeks.

# Remove Week column from data as it is 
# not used as a regression variable
srVars <- sr[,2:16]

# Fit the model using only the first
# 125 weeks of data
fit125 <- lm(Sales ~ ., srVars[1:125,])

# Summarize Linear model (lm)
summary(fit125)

# Generate predicted values from
# the lm for the next 39 weeks
Forecast_All <- predict(fit125, srVars[126:164,])
rm(srVars)

# Get Week 126-164 and Actual Demand/Sales data
srPlot <- sr[126:164,1:2]

# Bind Forecast to Actual data
srPlot <- cbind(srPlot, Forecast_All)

# Melt data w reshape2 lib
srPlotLong <- melt(srPlot, id="Week")
rm(srPlot)

# Create plot
g = ggplot(srPlotLong, aes(x = Week, y=value/1000, color=variable))

g = g + ggtitle("Sales vs. Forecast: Week 125")

g = g + geom_line()

g = g + scale_y_continuous(label = dollar_format(),
                           limits = c(min(srPlotLong$value),max(srPlotLong$value))/1000)

g = g + labs(x="Week", y="Sales  (000s)")

g
rm(g, srPlotLong)
