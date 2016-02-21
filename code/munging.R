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
# by kind of expense
ggplot(data = filter(tidytabs$tab7, Dollars == 'Public Four-Year In-State'), 
       aes(x = Year, y = Cost, color = Type)) + geom_line(size = 2)

# try a simple linear predictor: looks 
# like the slope changes every decade. 
# so put in a decade dummy interacted 
# with year.
mydata <- filter(tidytabs$tab2, 
                 Sector == 'Public.Four.Year' & 
                    Dollars == 'Current Dollars') %>% 
   mutate(Decade = ceiling((Year - 1970)/10))
mod <- lm(Cost ~ Year * factor(Decade) * factor(Type), data = mydata)

# now extrapolate under each of the
# 5 slope scenario, one per decade.
years <- 2015 + c(0:12)
types <- unique(factor(mydata$Type))
decades <- unique(factor(mydata$Decade))
newdata <- expand.grid(years,types,decades) %>%
   rename(Year = Var1, Type = Var2, Decade = Var3)
pcost <- predict(mod, newdata = newdata)
newdata$Cost <- pcost
newdata <- tbl_df(newdata) %>% 
   mutate(Decade = as.integer(Decade))

# adjust the intercept.
observed.start <- filter(mydata, Year == 2015) %>% 
   select(-Decade) %>% 
   rename(Observed.Cost = Cost)
predicted.start <- filter(newdata, Year == 2015) %>% 
   rename(Predicted.Cost = Cost)
adjust <- inner_join(observed.start, predicted.start) %>%
   arrange(Decade, Type, Year)
adjustby <- filter(adjust, Decade == 5L) %>% 
   select(-c(Observed.Cost, Year, Decade)) %>% 
   rename(Adjust.By = Predicted.Cost) %>%
   inner_join(adjust) %>%
   mutate(Adjust.By = Adjust.By - Predicted.Cost) %>%
   arrange(Decade, Type, Year) %>%
   select(Year, Type, Decade, Observed.Cost, Predicted.Cost, Adjust.By)

# now plot the forecast under each slope scenario
forecast <- select(adjustby, Type, Decade, Adjust.By) %>% 
   inner_join(newdata) %>% 
   mutate(Cost = Cost + Adjust.By, 
          Year = as.integer(Year), 
          Decade = factor(Decade, levels = c(1L:5L), 
                          labels = c("1970's", 
                                     "1980's", 
                                     "1990's", 
                                     "2000's",
                                     "2010's")))
ggplot(data = filter(forecast, Type == 'Tuition and Fees'), 
       aes(x = Year, y = Cost, color = Decade)) + 
   geom_line() + 
   ylab('Predicted Cost') + 
   scale_colour_discrete(name = "Using growth rate of:")


