library('dplyr')
library('tidyr')
library('lubridate')
library('readr')
library('readxl')
library('ggplot2')
# 
# The College Board publishes the 
# [Trends in College Pricing](http://trends.collegeboard.org/college-pricing) report 
# every year. Get the data in Excel; save it in the data folder.
xlsfile <- "data/2015-trends-college-pricing-source-data-12_16_15.xls"
# 
# Get data from Excel the way it is, so 
# you can spot check against spreadsheets.
# You will tidy it up later.
source('code/rawinput.R')

# 
# OK, now tidy up. Two rules:
# 1. All tables will be turned to long tbl_df objects
# 2. Academic years will be YYYY of the Fall year
source('code/tidyup.R')

# here's a possible plot. set knobs in ui.R for 
# private vs. public, current vs. 2015 dollars.
ggplot(data = filter(tidytabs$tab2, Sector == 'Public.Four.Year' & 
                        Dollars == 'Current Dollars'), 
       aes(x = Year, y = Cost, color = Type)) + geom_point()
# by region
ggplot(data = filter(tidytabs$tab4, Sector == 'Public Four-Year' & 
                        Dollars == 'Current Dollars'), 
       aes(x = Year, y = Cost, color = Region)) + geom_line(size = 2)