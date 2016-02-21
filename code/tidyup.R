# must call from code/munging.R after sourcing code/rawinput.R. 
tidytabs <- vector("list", length(sheets))
names(tidytabs) <- names(sheets)
tab1a <- mytabs$tab1a
tab1a <- lapply(tab1a, function(x) {
   x <- cbind(as.integer(substr(rownames(mytabs$tab1a$`Tuition and Fees`),1,4)), x);
   colnames(x)[1] <- 'Year'; 
   data.frame(x)})
tab1a <- lapply(tab1a, gather, Sector, Cost, Public.Four.Year.In.State:Private.Nonprofit.Four.Year)
tab1a <- lapply(names(tab1a), function(x) mutate(tab1a[[x]], Type = x))
tidytabs$tab1a <- bind_rows(tab1a[[1]], tab1a[[2]]) %>% 
   select(Year, Type, Sector, Cost)
rm(tab1a)

tab2 <- lapply(mytabs$tab2, data.frame)
tab2 <- lapply(tab2, function(x) mutate(x, Year = 1970 + row_number()))
tab2 <- lapply(tab2, gather, Sector, Cost, Private.Nonprofit.Four.Year:Public.Four.Year)
tab2 <- lapply(names(tab2), function(x) mutate(tab2[[x]], Type = x))
tab2 <- do.call(bind_rows, tab2) %>% 
   mutate(Dollars = trimws(gsub('^.+\\sin', '', Type))) %>%
   mutate(Type = trimws(gsub('\\sin.*', '', Type))) %>%
   select(Year, Dollars, Type, Sector, Cost) 
tidytabs$tab2 <- tab2 
rm(tab2)

tab4 <- lapply(mytabs$tab4, data.frame)
tab4 <- lapply(tab4, function(x) {x$Sector <- rownames(x); x})
tab4 <- lapply(tab4, gather, Year, Cost, starts_with('Year'))
tab4 <- lapply(tab4, function(x) mutate(x, Year = 1900 + as.integer(substr(gsub('Year\\.','',Year),1,2))))
tab4 <- lapply(tab4, function(x) mutate(x, Year = ifelse(Year >= 1990, Year, Year + 100)))
tab4 <- lapply(names(tab4), function(x) mutate(tab4[[x]], Dollars = x))
tab4 <- do.call(bind_rows, tab4) %>% 
   mutate(Dollars = gsub('In\\s', '', Dollars)) %>%
   mutate(Region = trimws(gsub('Private Nonprofit Four-Year|Public Four-Year', 
                        '', Sector))) %>%
   mutate(Sector = trimws(gsub(paste(unique(Region), collapse = '|'),
                               '', Sector))) %>%
   select(Region, Year, Dollars, Sector, Cost)
tidytabs$tab4 <- tab4
rm(tab4)

tab5 <- lapply(mytabs$tab5, data.frame)
tab5 <- lapply(tab5, function(x) {x$State <- rownames(x); x})
tab5 <- lapply(tab5, gather, Year, Cost, starts_with('Year'))
tab5 <- lapply(tab5, function(x) mutate(x, Year = as.integer(substr(gsub('Year\\.','',Year),1,4))))
tab5 <- lapply(names(tab5), function(x) mutate(tab5[[x]], Dollars = x))
tab5 <- do.call(bind_rows, tab5) %>% 
   select(Year, Dollars, State, Cost) %>%
   mutate(Dollars = gsub('Public Four-Year In-State Tuition and Fees', 
                         '', 
                         Dollars)) %>%
   rename(Public.Four.Year.In.State.Tuition.and.Fees = Dollars)
tidytabs$tab5 <- tab5
rm(tab5)

tab6 <- lapply(mytabs$tab6, data.frame)
tab6 <- lapply(tab6, function(x) {x$University <- rownames(x); x})
tab6 <- lapply(tab6, gather, Year, Cost, starts_with('Year'))
tab6 <- lapply(tab6, function(x) mutate(x, Year = as.integer(substr(gsub('Year\\.','',Year),1,4))))
tab6 <- lapply(names(tab6), function(x) mutate(tab6[[x]], Dollars = x))
tidytabs$tab6 <- do.call(bind_rows, tab6) %>% 
   select(Year, Dollars, University, Cost)
rm(tab6)

tab7 <- lapply(mytabs$tab7, data.frame)
tab7 <- lapply(tab7, function(x) {x$Type <- rownames(x); x})
tab7 <- lapply(tab7, gather, Year, Cost, starts_with('Year'))
tab7 <- lapply(tab7, function(x) mutate(x, Year = 1900 + as.integer(substr(gsub('Year\\.','',Year),1,2))))
tab7 <- lapply(tab7, function(x) mutate(x, Year = ifelse(Year > 1989, Year, Year + 100)))
tab7 <- lapply(names(tab7), function(x) mutate(tab7[[x]], Dollars = x))
tidytabs$tab7 <- do.call(bind_rows, tab7) %>% 
   select(Year, Dollars, Type, Cost)
rm(tab7)

tidytabs$taba2 <- data.frame(mytabs$taba2)

tidytabs$fig1 <- data.frame(mytabs$fig1)
tidytabs$fig1$Sector <- rownames(tidytabs$fig1)
tidytabs$fig1 <- gather(tidytabs$fig1, Type, Cost, Tuition.and.Fees:Total.Expenses.)  %>% 
   filter(!(Sector %in% Sector[grep('Two-Year', Sector)]))

tidytabs$fig2 <- data.frame(mytabs$fig2)
tidytabs$fig2$Region <- rownames(tidytabs$fig2)
tidytabs$fig2 <- gather(tidytabs$fig2, Type, Cost, 
                        Public.Four.Year.In.State.Tuition.and.Fees:
                           Private.Nonprofit.Four.Year.Room.and.Board)

fig3 <- mytabs$fig3
Sector <- colnames(fig3)
Median <- as.numeric(gsub('^.*\\$|)|,','',Sector))
Sector <- sapply(gsub('\\(.*','',Sector), trimws)
colnames(fig3) <- Sector
fig3 <- data.frame(fig3)
names(Median) <- names(fig3)
fig3$Bracket <- rownames(fig3)
fig3 <- gather(fig3, Sector, Share, Public.and.Private.Nonprofit.Four.Year.Combined:
                  Private.Nonprofit.Four.Year)
Median <- data.frame(Median)
Median$Sector <- rownames(Median)
tidytabs$fig3 <- merge(fig3, Median)
rm(fig3, Median, Sector)

fig5 <- lapply(mytabs$fig5, data.frame)
fig5 <- lapply(fig5, function(x) {x$Decade <- rownames(x); x})
fig5 <- lapply(fig5, gather, Sector, Increase, Private.Nonprofit.Four.Year:Public.Four.Year)
fig5 <- lapply(names(fig5), function(x) mutate(fig5[[x]], Type = x))
tidytabs$fig5 <- do.call(bind_rows, fig5) %>% 
   select(Type, Sector, Decade, Increase)
rm(fig5)

fig6 <- data.frame(mytabs$fig6) 
fig6$Year <- rownames(fig6)
fig6 <- mutate(fig6, Year = as.integer(substr(Year, 1, 2)) + 1900) %>% 
   mutate(Year = ifelse(Year > 1984, Year, Year + 100)) %>%
   gather(Sector, Multiplier, Private.Nonprofit.Four.Year:Public.Four.Year)
tidytabs$fig6 <- fig6
rm(fig6)

tidyUp89 <- function(x = mytabs$fig8) {
   fig8 <- lapply(x, data.frame)
   fig8 <- lapply(fig8, function(x) {x$State <- rownames(x); x})
   fig8 <- do.call(cbind, fig8)
   names(fig8) <- c('X2015.16.In.State.Tuition.and.Fees', 'St', 'Five.Year.Pct.Change', 'State') 
   fig8 <- select(fig8, St, State, X2015.16.In.State.Tuition.and.Fees, Five.Year.Pct.Change)
   tbl_df(fig8)
}
tidytabs$fig8 <- tidyUp89(mytabs$fig8)
tidytabs$fig9 <- tidyUp89(mytabs$fig9)

fig10 <- data.frame(mytabs$fig10) 
fig10$School <- rownames(fig10)
fig10 <- select(fig10, 
                School, 
                X2015.16.In.State.Tuition.and.Fees, 
                X2015.16.Out.of.State.Tuition.and.Fees, 
                X5.Year...Change.in.In.State.TF)
names(fig10)[4] <- 'X5.Year.Pct.Change.in.In.State.TF'
tidytabs$fig10 <- tbl_df(fig10)
rm(fig10)

tidyUp1213 <- function(x = mytabs$fig12) {
   fig12 <- data.frame(x) 
   fig12$Type <- rownames(fig12)
   fig12 <- gather(fig12, Year, Cost, starts_with('X'))
   fig12 <- mutate(fig12, Year = as.integer(substr(Year, 2, 4)) + 1900) %>% 
      mutate(Year = ifelse(Year > 1984, Year, Year + 100))
   tbl_df(fig12)
}
tidytabs$fig12 <- tidyUp1213(mytabs$fig12)
tidytabs$fig13 <- tidyUp1213(mytabs$fig13)

fig14 <- cbind(mytabs$fig14$avg, mytabs$fig14$distro[,-1])
colnames(fig14)[1:2] <- c('Average.Net.Tuition.Paid', 'Income.Bracket.Share')
net_tuition_brackets <- colnames(fig14)[3:ncol(fig14)]   
fig14 <- data.frame(fig14)
names(net_tuition_brackets) <- names(fig14)[3:ncol(fig14)]
fig14$Income.Bracket <- rownames(fig14)
fig14 <- gather(fig14, Net.Tuition.Bracket, Share, Exactly..0:X.10.000.or.More) %>%
   mutate(Net.Tuition.Bracket = factor(Net.Tuition.Bracket, 
                                       levels = names(net_tuition_brackets), 
                                       labels = net_tuition_brackets))
tidytabs$fig14 <- tbl_df(fig14)
rm(fig14)

fig15 <- cbind(mytabs$fig15$avg, mytabs$fig15$distro[,-1])
colnames(fig15)[1:2] <- c('Average.Net.Tuition.Paid', 'Income.Bracket.Share')
net_tuition_brackets <- colnames(fig15)[3:ncol(fig15)]   
fig15 <- data.frame(fig15)
names(net_tuition_brackets) <- names(fig15)[3:ncol(fig15)]
fig15$Income.Bracket <- rownames(fig15)
fig15 <- gather(fig15, Net.Tuition.Bracket, Share, Exactly..0:X.30.000.or.More) %>%
   mutate(Net.Tuition.Bracket = factor(Net.Tuition.Bracket, 
                                       levels = names(net_tuition_brackets), 
                                       labels = net_tuition_brackets))
tidytabs$fig15 <- tbl_df(fig15)
rm(fig15)

tidyUp2013 <- function(x = mytabs$fig2013_12) {
   fig2013 <- lapply(x, data.frame)
   fig2013 <- lapply(fig2013, function(x) { 
      x$Type <- rownames(x) 
      x <- gather(x, Year, Cost, starts_with('Year')) %>%
         mutate(Year = gsub('Year.','',Year)) %>%
         mutate(Year = as.integer(Year, 1, 4))
      x
   })
   fig2013 <- lapply(names(fig2013), function(x) {fig2013[[x]]$Quartile <- x; fig2013[[x]]})
   fig2013 <- do.call(rbind, fig2013) %>%
      select(Quartile, Year, Type, Cost)
   tbl_df(fig2013)
}
tidytabs$fig2013_12 <- tidyUp2013(mytabs$fig2013_12)
tidytabs$fig2013_13 <- tidyUp2013(mytabs$fig2013_13)

tidyUp2014 <- function(x = mytabs$fig2014_14a, y = 'Residency') {
   fig2013 <- lapply(x, data.frame)
   fig2013 <- lapply(fig2013, function(x) { 
      x$Type <- rownames(x) 
      gather(x, Income, Cost, Nontuition.Expenses:Tuition.and.Fees)
   })
   fig2013 <- lapply(names(fig2013), function(x) {fig2013[[x]][[y]] <- x; fig2013[[x]]})
   fig2013 <- do.call(rbind, fig2013)
   fig2013[,c(y, 'Income', 'Type', 'Cost')] %>% 
      tbl_df()
}
tidytabs$fig2014_14a <- tidyUp2014()
tidytabs$fig2014_15a <- tidyUp2014(x = mytabs$fig2014_15a, y = 'Quartile')
