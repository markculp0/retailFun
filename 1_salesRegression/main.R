
# ============================
# Analysis of 2012 Sales Data
# ============================

options(scipen = 999)

library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

# Read in 2012 Weekly Sales ====================

# Get 2012 weekly sales
sales <- read_xlsx("RFundX_2017_M1_2_Sales_Regression.xlsx",
                  sheet = "All Data",
                  range = "a102:b154",
                  col_names = F)

# Rename columns
names(sales) <- c("Date", "Sales")

# Convert to Date format
sales$Date <- as.Date(sales$Date)

# Add month column
sales <- sales %>%
  mutate(Month = month(Date))

# Monthly sales
salesMonth <-sales %>%
  select(Month, Sales) %>%
  group_by(Month) %>%
  summarise(Monthly_Sales = sum(Sales))

# Add month Names
Month_Name <- with(salesMonth, month.abb[Month])
salesMonth <- cbind(salesMonth, Month_Name)

# Plot 2012 Monthly Sales
salesMonth %>%
  ggplot(aes(x = Month, y = Monthly_Sales/1000000)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Monthly Sales 2012",
       x = "Month",
       y = "Sales (millions $)") +
  scale_x_continuous(breaks = salesMonth$Month,
                     labels = salesMonth$Month_Name)



