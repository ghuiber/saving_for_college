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

# Regions, states, DC and PR
# Classification according to Table 4 Notes here: 
# http://trends.collegeboard.org/college-pricing/figures-tables/published-tuition-fees-region-over-time
regions <- list()
regions$`Middle States` <- c('DC', 'DE', 'MD', 'NJ', 'NY', 'PA', 'PR')
regions$`Midwest` <- c('IA', 'IL', 'IN', 'KS', 'MI', 'MN', 'MO', 'NE', 'ND', 'OH', 'SD', 'WI', 'WV')
regions$`New England` <- c('CT', 'MA', 'ME', 'NH', 'RI', 'VT')
regions$`South` <- c('AL', 'FL', 'GA', 'KY', 'LA', 'MS', 'NC', 'SC', 'TN', 'VA')
regions$`Southwest` <- c('AR', 'NM', 'OK', 'TX')
regions$`West` <- c('AK', 'AZ', 'CA', 'CO', 'HI', 'ID', 'MT', 'NV', 'OR', 'UT', 'WA', 'WY')
states <- cbind(state.name, state.abb)
states <- rbind(states, c('District of Columbia', 'DC'))
states <- rbind(states, c('Puerto Rico', 'PR'))

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
ggplot(data = filter(tidytabs$tab7, Sector == 'Public Four-Year In-State'), 
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
<<<<<<< Updated upstream
=======
rm(adjustby, forecast, newdata, lilextra)
>>>>>>> Stashed changes

ggplot(data = filter(mycast, Observed == TRUE), 
                  aes(x = Year, y = Cost)) + 
   geom_line(data = filter(mycast, Observed == FALSE), 
             aes(x = Year, y = Cost, color = Decade, 
             size = Jump)) + 
   facet_grid(.~Type) + 
   scale_colour_discrete(name = "Growth rate of:") + 
   geom_point() + 
   scale_size(guide = 'none', range = c(.2, 1.5)) 

# plot this region: x is region as in names(regions),
# default is current dollars, fs is font size and la
# is line alpha; flagship FALSE means tab5 data, average 
# in-state cost across the states' systems; TRUE means cost 
# at the states' flagship universities.
plotRegion <- function(x = 'Southwest', 
                       dollars = 'Current Dollars', 
                       fs = 5, la = .5, 
                       flagship = FALSE) {
   rstates <- states[,'state.name'][states[,'state.abb'] %in% regions[[x]]]
   rst <- states[,'state.abb'][states[,'state.abb'] %in% regions[[x]]]
   df <- tidytabs$tab5 %>% 
      filter(Dollars == dollars & State %in% rstates)
   if(flagship == TRUE) {
      df <- tidytabs$tab6 %>% 
         filter(Dollars == dollars & State %in% rst)
   }
   labz <- filter(df, Year == max(Year))
   ggplot(df, aes(x = Year, y = Cost, colour = State)) + 
      geom_line(size = 1, alpha = la) + 
      scale_colour_discrete(name = 'State') + 
      geom_text(labz, 
                mapping=aes(Year, Cost, label=State), 
                size = fs) + 
      theme(legend.position="none")
}
# if average = TRUE, then this is in and out of state combined.
# otherwise, it's in-state only.
plotNetVsPublished <- function(sector = 'Public Four-Year In-State', 
                               average = TRUE,
                               la = .5) {
   stopifnot(sector %in% c('Private Nonprofit Four-Year', 'Public Four-Year In-State'))
   df <- tidytabs$tab7 %>% 
      filter(Sector == sector) 
   if(average == FALSE) {
      df <- tidytabs$fig12
      if(sector == 'Private Nonprofit Four-Year') df <- tidytabs$fig13
   }
   df <- filter(df, Type %in% c('Published Tuition and Fees', 
                                'Net Tuition and Fees'))
   ggplot(df, aes(x = Year, y = Cost, colour = Type)) + 
      geom_line(size = 1, alpha = la) + 
      scale_colour_discrete(name = 'Type')
}
