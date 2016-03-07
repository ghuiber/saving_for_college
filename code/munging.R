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

# try a simple linear predictor: for 
# 4-year public colleges, the slope  
# seems to change at the start of every 
# decade and is more or less linear 
# within each decade. this may look 
# like it calls for a spline regression, 
# but a decade dummy interacted with
# year will work well enough.
mydata <- filter(tidytabs$tab2, 
                 Sector == 'Public.Four.Year' & 
                    Dollars == 'Current Dollars') %>% 
   mutate(Decade = ceiling((Year - 1970)/10)) %>%
   select(-c(Dollars, Sector))
mod <- lm(Cost ~ Year * factor(Decade) * factor(Type), data = mydata)

# now extrapolate under each of the
# 5 slope scenarios, one per decade.
years <- 2015 + c(0:12)
types <- unique(factor(mydata$Type))
decades <- unique(factor(mydata$Decade))
newdata <- expand.grid(years,types,decades) %>%
   rename(Year = Var1, Type = Var2, Decade = Var3)
pcost <- predict(mod, newdata = newdata)
newdata$Cost <- pcost
newdata <- tbl_df(newdata) %>% 
   mutate(Decade = as.integer(Decade))

# adjust the intercept: there has to be a
# jump at year 2015 so forecasts under 
# each decade's own slope start from the 
# same point
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

# get extrapolations under each slope scenario.
# this will help explain what the Shiny app is doing.
lilextra <- do.call(rbind, 
                    sapply(c(1:5), 
                           function(x) cbind(x, 
                                             c((x * 10 + 1960):2015)))) %>%
   data.frame() %>%
   rename(Decade = x, 
          Year = V2)
lilextra$Type <- types[1]
lilextra2 <- select(lilextra, -Type) %>%
   mutate(Type = types[2])
lilextra <- rbind(lilextra, lilextra2) %>%
   tbl_df()
lilextra$Cost <- predict(mod, newdata = lilextra)
rm(observed.start, predicted.start, adjust, lilextra2)

# put predicted and observed together.
# there's gotta be a way to ggplot both,
# observed as scatter and predicted as line.
mycast <- rbind(mutate(mydata, Observed = TRUE), 
                mutate(lilextra, Observed = FALSE)) %>%
   mutate(Year = as.integer(Year), 
          Decade = factor(Decade, levels = c(1L:5L), 
                          labels = c("1970's", 
                                     "1980's", 
                                     "1990's", 
                                     "2000's",
                                     "2010's")))

# now plot the forecast under each slope scenario.
# add the jump at year 2015 so forecasts start out
# from the same point.
forecast <- select(adjustby, Type, Decade, Adjust.By) %>% 
   inner_join(newdata) %>% 
   mutate(Cost = Cost + Adjust.By, 
          Year = as.integer(Year), 
          Decade = factor(Decade, levels = c(1L:5L), 
                          labels = c("1970's", 
                                     "1980's", 
                                     "1990's", 
                                     "2000's",
                                     "2010's"))) %>%
   select(-Adjust.By) %>%
   mutate(Observed = FALSE)
mycast <- rbind(mycast, forecast) %>%
   mutate(Jump = as.integer(!(Year == 2015)))

ggplot(data = filter(mycast, Observed == TRUE), 
                  aes(x = Year, y = Cost)) + 
   geom_line(data = filter(mycast, Observed == FALSE), 
             aes(x = Year, y = Cost, color = Decade, 
             size = Jump)) + 
   facet_grid(.~Type) + 
   scale_colour_discrete(name = "Growth rate of:") + 
   geom_point() + 
   scale_size(guide = 'none', range = c(.2, 1.5)) 



