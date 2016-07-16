# Where were we
source('code/munging.R')

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
      rename(Type = Income) %>%
      mutate(MustCover = pmax(0, round(Net / Posted, digits = 2)))
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

# One easy way to go, then, is to keep it fixed at the 2010's
# observed level, which is about .93. This means that the blended
# discount rate on the total posted at year t is roughly half the 
# tuition discount rate for a given income quartile. If so, you can
# just take the simple mean of the MustCover column.

# Another way to go is to just forecast room and board cost 
# like tuition cost -- with a linear trend and a compount rate.

# Here's a recipe for estimating yearly cost of attendance

# myt: years after 2015 when child expects to start college
# myq: income quartile you expect to be in at application time
# myschool: where you want to go.
getExpectedCostPerYear <- function(myt, 
                                   myq, 
                                   myschool) {
   # Share I will need to cover
   cover <- willNeedToCover(q = myq)
   myshare.all <- mean(cover$MustCover)
   myshare.fees <- filter(cover, 
                          Type == 'Tuition.and.Fees')$MustCover
   
   # Expected range based on series of linear models by decade
   # (this covers the whole country, but goes furthest back)
   mycast <- makeForecast(howfar = myt)
   
   # Expected range of cost. Say we want the total, 
   # not just tuition and fees. This may serve as a 
   # sanity check on the two forecasts -- linear should
   # fall somewhere inside the range, and compound 
   # growth should fall somewhere above it.
   mycost <- 'Tuition and Fees and Room and Board'
   myrange <- mycast$df %>% 
      filter(Year == max(Year) & 
                Type == mycost) %>% 
      dplyr::select(Year, Cost, Decade, Highest, Lowest)
   if(mycost == 'Tuition and Fees and Room and Board') {
      myrange <- mutate(myrange, Cost = Cost * myshare.all)
   } else {
      myrange <- mutate(myrange, Cost = Cost * myshare.fees)
   }
   
   # Room and board: national figures. No others 
   # available in current dollars.
   rbdf <- mycast$df %>% 
      filter(Observed == TRUE) %>% 
      select(Year, Cost, Type) %>% 
      tidyr::spread(key = Type, value = Cost) %>% 
      mutate(`Room and Board` = `Tuition and Fees and Room and Board` - `Tuition and Fees`) %>% 
      select(Year, `Room and Board`)
   
   # Tuition and fees at this flagship university
   
   # Simple linear trend based on flagship universities only
   # (covers schools of interest specifically, but only goes
   # back so far)
   mydf <- tbl_df(tidytabs$tab6) %>% 
      filter(Dollars == 'Current Dollars' & 
                University == myschool) %>% 
      rename(Tuition = Cost) %>%
      inner_join(rbdf)

   # Linear forecast of tuition and fees
   tcoefs <- summary(lm(Tuition ~ I(Year - min(Year)), 
                        data = mydf))$coefficients
   # Linear forecast of room and board
   rcoefs <- summary(lm(`Room and Board` ~ I(Year - min(Year)), 
                        data = mydf))$coefficients
   
   # Linear forecast adjusted by share expected to have to cover
   lincast <- (tcoefs['(Intercept)', 'Estimate'] + 
                  (max(mydf$Year) + myt - min(mydf$Year)) * 
                  tcoefs['I(Year - min(Year))', 'Estimate']) * 
      myshare.fees + 
      rcoefs['(Intercept)', 'Estimate'] + 
      (max(mydf$Year) + myt - min(mydf$Year)) * 
      rcoefs['I(Year - min(Year))', 'Estimate']
   
   # Compound annual growth rate forecast adjusted by share
   # expected to have to cover. First, the log-linear model:
   tlmod <- lm(log(Tuition) ~ I(Year - min(Year)), 
               data = mydf)
   rlmod <- lm(log(`Room and Board`) ~ I(Year - min(Year)), 
               data = mydf)
   tcagr <- exp(summary(tlmod)$coefficients['I(Year - min(Year))', 'Estimate']) - 1
   rcagr <- exp(summary(rlmod)$coefficients['I(Year - min(Year))', 'Estimate']) - 1
   
   # so then tuition expected at myt, adjusted by 
   # share expected to have to cover, should be
   tcgrcast <- filter(mydf, Year == max(Year))$Tuition * (1 + tcagr)^myt * 
      myshare.fees
   # and there's no adjustment for room and board cost
   rcgrcast <- filter(mydf, Year == max(Year))$`Room and Board` * (1 + rcagr)^myt
   
   # now return this stuff
   out <- list()
   out$range <- myrange
   out$lincast <- lincast
   out$cgrcast <- tcgrcast + rcgrcast
   out$myshare <- c(myshare.all, myshare.fees)
   out$tcagr <- tcagr
   out$rcagr <- rcagr
   names(out$myshare) <- c('All Cost', 'Tuition and Fees')
   out
}

# The function below computes what you'll have at the end
# of t years given a an amount pa saved every year, assuming
# an appreciation rate t and some starting balance
# pa -- per annum savings (divide by 12 for per-paycheck; close enough)
# time -- years until college starts
# growth -- rate at which assets appreciate over time
# starting balance -- what you have saved so far
getFutureBalance <- function(pa, 
                             time, 
                             growth = r, 
                             starting.balance = 0) {
   savingstream <- rep(pa, time) * (1 + growth)^c(1:time)
   sum(savingstream) + 
      starting.balance * (1 + growth)^time
}

# Now suppose you have two kids you want to send to UNC Chapel Hill.
# One starts 12 years from 2015, the other 15 years from 2015:
kids <- c('Anna', 'Belle')
years <- c(12, 15)

# Say each child has some money saved already, and you plan to save
# more for each of them every month until they each start, enough so
# that at the start time they have all the money you expect they need
# to cover tuition, fees, room and board for four years.
balances <- c(1500, 1000)
names(years) <- kids
names(balances) <- kids

# To use the recipe above, you will also want to set:
myschool <- 'University of North Carolina at Chapel Hill'
incquart <- c('Lowest', 'Second', 'Third', 'Highest')

# What you need to have on hand for each child in yearly total 
# costs to attend is calculated here. This function returns both 
# a forecast cost based on a linear trend (lincast) and one based
# on an estimated compound annual growth rate (cgrcast).
costPerYear <- lapply(incquart, 
                      function(myq) {
                         out <- lapply(years, 
                                function(x) {
                                   out <- getExpectedCostPerYear(x, 
                                                                 myq, 
                                                                 myschool)
                                   c(out$lincast, out$cgrcast)
                                })
                         out <- do.call(rbind, out)
                         colnames(out) <- c('linear trend', 'compound growth')
                         rownames(out) <- kids
                         out
                       })
names(costPerYear) <- incquart
# This is nice to know, but I care more about how much I need to 
# save per monthly paycheck, assuming a compound annual growth rate r.

# Objective function: find x that mimizes the absolute 
# difference between the future balance if saving x per 
# year and what you need to have on hand, assuming that 
# you want all 4 years' worth of cost ready upfront. 
# x/12 = amount needed to save per month. Will return 
# 2 figures: one assuming linear growth in cost of college, 
# the other assuming compound growth.
getMonthlySaving <- function(myt = years[1],
                             starting.balance = balances[1], 
                             myq = 'Highest',
                             asset.growth = .04) {
   tnames <- c('linear trend', 'compound growth')
   # Estimate cost to attend school of choice starting
   # at time t, given expected income quartile
   cost.estimates <- getExpectedCostPerYear(myt, 
                                            myq, 
                                            myschool)
   # In the lowest income quartile net tuition paid is
   # negative, so bind it at zero and save only the 
   # expected room and board
   cost <- c(cost.estimates$lincast, 
             cost.estimates$cgrcast)
   names(cost) <- tnames
   cost[cost == 0] <- mean(cost.estimates$range$Cost)
   cost <- cost * 4
   obj <- function(par, expected.cost) {
      abs(getFutureBalance(par, 
                           time = myt, 
                           growth = asset.growth, 
                           starting.balance = starting.balance) - 
             expected.cost)
   }
   lin <- optimx::optimx(par = 1, 
                         fn = obj, 
                         method = 'BFGS', 
                         expected.cost = cost['linear trend'])
   cgr <- optimx::optimx(par = 1, 
                         fn = obj, 
                         method = 'BFGS', 
                         expected.cost = cost['compound growth'])
   out <- c(lin$p1/12, cgr$p1/12)
   names(out) <- tnames
   round(out)
}

# Set up the data over which you want forecasts: 
# the times until college, starting balances, and 
# expected income quartiles of the parents.
getMyForecastInputs <- function(kids, 
                                years, 
                                balances, 
                                incquart, 
                                growth) {
   yset <- expand.grid(years, incquart) %>% 
      rename(years = Var1, incquart = Var2)
   cbind(years, balances) %>% 
      tbl_df() %>% 
      mutate(kid = kids) %>%
      inner_join(yset) %>% 
      mutate(r = growth)
}

# And now, here are the expected bounds on what you should
# save per month given the parameters above. A linear trend 
# is more forgiving. A compound growth trend is more common 
# in use by financial planners, but the evidence to date 
# shows that a linear trend is not a silly thing to expect.
# That, and the fact that few people pay sticker price, means 
# that there's hope: you don't need to set aside so much 
# every month that you'll get totally discouraged from even 
# trying. Saving for four years at a flagship university in 
# your state is probably not as hard as it looks.
myset <- getMyForecastInputs(kids = c('Anna', 'Belle'), 
                             years = c(11, 14), 
                             balances = c(8000, 5000), 
                             incquart = c('Third', 'Highest'), 
                             growth = .04)
saving <- mapply(getMonthlySaving, 
                 myt = myset$years, 
                 starting.balance = myset$balances, 
                 myq = myset$incquart, 
                 asset.growth = myset$r,
                 SIMPLIFY = FALSE)
fullset <- cbind(myset, do.call(rbind, saving))

