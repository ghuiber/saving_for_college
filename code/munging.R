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
# OK, now tidy up. Two rules (for now):
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
# year will work well enough. Use glm 
# instead of lm because the object returned
# has more things in it, including data.
# Arguments:
# - training: tidy data frame to train forecast on
# - sector: one of c('Public.Four.Year', 'Private.Nonprofit.Four.Year')
# - dollars: as of 2016, one of c('2015 Dollars', 'Current Dollars')
# - howfar: integer years into the future
# Returns:
# - a list of 2 elements: the data frame to plot the trend lines,
#   and the decades with the slowest and fastest growth rate that
#   I will use later to shade in the forecast range, which will 
#   look like a kind of uncertainty cone.
makeForecast <- function(training = tidytabs$tab2, 
                         sector = 'Public.Four.Year',
                         dollars = 'Current Dollars',
                         howfar = 12) {
   stopifnot(sector %in% c('Public.Four.Year',
                           'Private.Nonprofit.Four.Year'))
   stopifnot(dollars %in% c('2015 Dollars', 
                            'Current Dollars'))  
   # last year in training data
   maxy <- max(training$Year)
   # base year
   basey <- min(training$Year) - 1
   
   # linear model
   mydata <- filter(training, 
                    Sector == sector & 
                       Dollars == dollars) %>% 
      mutate(Decade = ceiling((Year - basey)/10)) %>%
      select(-c(Dollars, Sector))
   mod <- glm(Cost ~ Year * factor(Decade) * factor(Type), 
              data = mydata)
   
   # Collect slope coefficients. Find steepest, smallest
   est <- summary(mod)$coefficients[,'Estimate']
   est <- est[grep('(^Year$|^Year:factor\\(Decade\\)[0-9]$)', 
                   names(est))]
   names(est) <- paste('Decade', c(1:length(est)), sep = '.')
   steepest <- as.integer(gsub('Decade.', 
                               '', 
                               names(est)[est == max(est)]))
   smallest <- as.integer(gsub('Decade.', 
                               '', 
                               names(est)[est == min(est)]))
   
   # now extrapolate under each of the
   # 5 slope scenarios, one per decade.
   years <- maxy + c(0:howfar)
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
   observed.start <- filter(mydata, Year == maxy) %>% 
      select(-Decade) %>% 
      rename(Observed.Cost = Cost)
   predicted.start <- filter(newdata, Year == maxy) %>% 
      rename(Predicted.Cost = Cost)
   adjust <- inner_join(observed.start, predicted.start) %>%
      arrange(Decade, Type, Year)
   
   adjustby <- filter(adjust, Decade == max(Decade)) %>% 
      select(-c(Observed.Cost, Year, Decade)) %>% 
      rename(Adjust.By = Predicted.Cost) %>%
      inner_join(adjust) %>%
      mutate(Adjust.By = Adjust.By - Predicted.Cost) %>%
      arrange(Decade, Type, Year) %>%
      select(Year, Type, Decade, Observed.Cost, Predicted.Cost, Adjust.By)
   
   decades <- sort(unique(adjustby$Decade))
   dlabels <- paste((decades - 1) * 10 + basey, "s", sep = "'")
   # get extrapolations under each slope scenario.
   # this will help explain what the Shiny app is doing.
   lilextra <- do.call(rbind, 
                       sapply(decades, 
                              function(x) cbind(x, 
                                                c((x * 10 + 
                                                      (basey - 10)):maxy)))) %>%
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
             Decade = factor(Decade, levels = decades, 
                             labels = dlabels, 
                             ordered = TRUE))
   
   # now plot the forecast under each slope scenario.
   # add the jump at year 2015 so forecasts start out
   # from the same point.
   forecast <- select(adjustby, Type, Decade, Adjust.By) %>% 
      inner_join(newdata) %>% 
      mutate(Cost = Cost + Adjust.By, 
             Year = as.integer(Year), 
             Decade = factor(Decade, levels = decades, 
                             labels = dlabels, 
                             ordered = TRUE)) %>%
      select(-Adjust.By) %>%
      mutate(Observed = FALSE)
   out <- list()
   out$df <- rbind(mycast, forecast) %>% 
      mutate(Jump = as.integer(!(Year == maxy)), 
             Forecast = (Year >= maxy)) %>%
      mutate(Highest = Forecast * 
                (as.integer(Decade) == steepest), 
             Lowest = Forecast * 
                (as.integer(Decade) == smallest)) %>%
      arrange(Type, Year)
   out$bounds <- c(smallest, steepest)
   out
}

# Plot the linear predictor `mycast`, returned by makeForecast()
# - type: 'Tuition and Fees' or 'Tuition and Fees and Room and Board'
plotForecast <- function(mycast, 
                         type = 'Tuition and Fees') {
   stopifnot(type %in% c('Tuition and Fees', 
                         'Tuition and Fees and Room and Board'))
   df <- filter(mycast$df, Type == type)
   
   # This is how we'll plot the uncertaity cone
   fmean <- filter(df, Jump == 1 & Forecast == TRUE) %>%
      group_by(Type, Year) %>%
      summarise(Cost = mean(Cost)) %>%
      ungroup()
   cone <- filter(df, Jump == 1 & (Lowest == 1 | Highest == 1)) %>%
      dplyr::select(Year, Type, Cost, Decade) %>% 
      tidyr::spread(key = Decade, value = Cost)
   boundaries <- levels(df$Decade)[mycast$bounds]
   renamethem <- sapply(boundaries, gsub, pattern ="'", replacement ="")
   renamethem <- paste('d', renamethem, sep = '')
   names(cone)[names(cone) == boundaries] <- renamethem
   cone <- inner_join(fmean, cone)
   
   # And here's the full plot
   ggplot(data = filter(df, Observed == TRUE), 
             aes(x = Year, y = Cost)) + 
      geom_ribbon(data = cone, 
                  aes_string(x = 'Year', 
                             ymin = renamethem[1], 
                             ymax = renamethem[2]), 
                  alpha = 0.2) +
      geom_line(data = filter(df, Observed == FALSE), 
                aes(x = Year, y = Cost, color = Decade, 
                    size = Jump)) + 
      scale_colour_discrete(name = "Growth rate of:") + 
      geom_point() + 
      scale_size(guide = 'none', range = c(.2, 1.5)) + 
      ggtitle(type)
}   

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
   stopifnot(sector %in% c('Private Nonprofit Four-Year', 
                           'Public Four-Year In-State'))
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

mycast <- makeForecast()
plotFees <- plotForecast(mycast)
plotAll <- plotForecast(mycast, type = 'Tuition and Fees and Room and Board')

