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
# The data at this stage are in the mytabs list.

# 
# OK, now tidy up. Two rules (for now):
# 1. All tables will be turned to long tbl_df objects
# 2. Academic years will be YYYY of the Fall year
source('code/tidyup.R')
# The data at this stage are in the tidytabs list.

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
      tbl_df() %>%
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
   rstates <- states[,'state.name'][states[,'state.abb'] %in% 
                                       regions[[x]]]
   rst <- states[,'state.abb'][states[,'state.abb'] %in% 
                                  regions[[x]]]
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

# Ratio of net to published prices in 2015 dollars for
# 4-year public colleges, across all students and then 
# in-state only, as far back as available for both
plotNetToStickerRatio <- function() {
   both <- list()
   both$all <- plotNetVsPublished()$data %>% 
      mutate(Which = 'All') %>% tbl_df()
   both$instate <- plotNetVsPublished(average = FALSE)$data %>% 
      mutate(Which = 'In-State')
   both <- lapply(both, function(x) { 
      dplyr::select(x, Year, Which, Type, Cost) %>% 
         tidyr::spread(key = Type, value = Cost) %>%
         mutate(Ratio = `Net Tuition and Fees`/`Published Tuition and Fees`) %>% 
         dplyr::select(Year, Which, Ratio)
      })
   foo <- do.call(rbind, both)
   part1 <- 'Average net tuition is less than half of the sticker price'
   part2 <- 'at 4-year public colleges, whether in-state or all combined'
   out <- ggplot(data = foo, 
                 aes(x = Year, 
                     y = Ratio, 
                     group = Which, 
                     colour = Which)) + 
      geom_line() + 
      geom_hline(yintercept = .5, alpha = .4) + 
      ggtitle(paste(part1, part2, sep = '\n'))
   out
}

# How about same ratio by income quartile: you want to 
# see the share of the posted price that you'll have to 
# cover, given the income quartile q you expect to be in
# at the time the kid goes to college. On the non-tuition
# expenses, this share is 1 by default, b/c The College 
# Board has no idea what discounts are available by income 
# quartile. Probably none. But colleges make net price 
# decisions based on income, so there are differences in 
# net price visible by quartile to The College Board. 
# So in the table below the MustCover column gives you an 
# idea of how much you should save based on the tuition 
# deal you expect to get, and then you're on your own in 
# deciding how much of the expected living expenses you 
# want to save ahead of time: 100% may be too paranoid. 
# You may want to let the kid take a side gig and have to 
# watch his or her budget; both build life skills.
willNeedToCover <- function(q) {
   stopifnot(q %in% c('Lowest', 'Second', 'Third', 'Highest'))
   tbl_df(tidytabs$fig2014_14a) %>% 
      filter(Residency == 'In-State' & Type %in% Type[grep(q, Type)]) %>%
      mutate(Type = gsub(paste("Dependent Students' Family Income Quartile", 
                               q, sep = " "), 
                         "", Type)) %>% 
      mutate(Type = trimws(gsub("For|Paid If", "", Type))) %>% 
      tidyr::spread(key = Type, value = Cost) %>% 
      mutate(MustCover = round(Net / Posted, digits = 2))
}

# Blended rate given willNeedToCover above:
getBlendedForecast <- function(mycast, q) {
   mustCover <- willNeedToCover(q)
   foo <- mycast$df %>% 
      filter(Forecast == FALSE & Observed == TRUE) %>% 
      dplyr::select(Year, Type, Cost) %>% 
      tidyr::spread(key = Type, value = Cost) %>% 
      mutate(`Room and Board` = 
                `Tuition and Fees and Room and Board` - 
                `Tuition and Fees`)
   # aside: tuition and fees grow faster than room & board.
   # their ratio changes by about 1.3% per year
   foo <- mutate(foo, Ratio = `Tuition and Fees`/`Room and Board`)
   summary(lm(Ratio ~ Year, data = foo))
   ggplot(data = foo, aes(x = Year, y = Ratio)) + geom_point()
}

# So if posted (Tuition and Fees) + (Room and Board) go up
# to a total of x by year t, then based on their expected 
# ratio as of t you can forecast how much of x you should 
# expect to have to cover, between the discount on tuition 
# based on income quartile (higher quartiles get lower 
# discount) and the discount on Room and Board based on 
# income quartile (which is 0, because the private sector 
# that feeds and houses you won't give you an income-based 
# scholarship). The savings will be small, but growing over 
# time, because as long as sticker price tuition and fees 
# continue to grow faster than the cost of room and board, 
# the portion of the total cost on which you do get a discount 
# grows over time.

# A conservative way to go, the, is to keep it fixed at the 2010's
# observed level, which is about .93. This means that the blended
# discount rate on the total posted at year t is roughly half the 
# tuition discount rate for a given income quartile. If so, you can
# just take the simple mean of the MustCover column.

# So, full recipe:
myt <- 11
myq <- 'Third'
myshare <- mean(willNeedToCover(q = myq)$MustCover)
mycast <- makeForecast(howfar = myt)$df %>% 
   filter(Year == max(Year) & 
             Type == 'Tuition and Fees and Room and Board') %>% 
   mutate(Cost = Cost * myshare) %>% 
   dplyr::select(Year, Cost, Decade, Highest, Lowest)

# How about a simple linear trend for one flagship university?
mydf <- plotRegion(x = 'South', flagship = TRUE)$data %>% 
   tbl_df() %>% filter(University == 'University of North Carolina at Chapel Hill' & 
                          Dollars == 'Current Dollars')
mod <- lm(Cost ~ I(Year - min(Year)), data = mydf)


