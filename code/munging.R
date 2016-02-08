library('dplyr')
library('tidyr')
library('lubridate')
library('readr')
library('readxl')
library('ggplot2')
# 
# The College Board publishes the 
# [Trends in College Pricing](http://trends.collegeboard.org/college-pricing) report 
# every year. Get the data in Excel; save it on Google Drive in the collegecost folder.
xlsfile <- "data/2015-trends-college-pricing-source-data-12_16_15.xls"
# 
# Get data from Excel the way it is, so 
# you can spot check against spreadsheets.
# You will tidy it up later.
sheets <- c("Table 1A", "Table 2", "Table 4", "Table 5", "Table 6", "Table 7", 
            "Table A2", "Fig 1", "Fig 2", "Fig 3", "Fig 5", "Fig 6", "Fig 8",
            "Fig 9", "Fig 10", "Fig 12", "Fig 13", "Fig 14", "Fig 15",
            "Fig 2013_12", "Fig 2013_13", "Fig 2014_14A", "Fig 2014_15A")
names(sheets) <- c('tab1a', 'tab2', 'tab4', 'tab5', 'tab6', 'tab7', 'taba2', 
                   'fig1', 'fig2', 'fig3', 'fig5', 'fig6', 'fig8', 'fig9', 
                   'fig10', 'fig12', 'fig13', 'fig14', 'fig15', 
                   'fig2013_12', 'fig2013_13', 'fig2014_14a', 'fig2014_15a')
tabnames <- sapply(sheets, function(x) names(read_excel(xlsfile, sheet = x)[1,1]))

mytabs <- vector("list", length(sheets))
names(mytabs) <- names(sheets)

x <- read_excel(xlsfile, sheet = sheets[1])[,1:5]
tf <- apply(x[3:4,2:5], 2, unlist) %>% apply(2, as.numeric)
rb <- apply(x[8:9,2:5], 2, unlist) %>% apply(2, as.numeric)
tab1a <- list(tf, rb)
names(tab1a) <- c('Tuition and Fees', 'Room and Board')
colz <- unlist(x[1,2:5])
rowz <- unlist(x[3:4,1])
tab1a <- lapply(tab1a, function(x) {colnames(x) <- colz; rownames(x) <- rowz; x})
mytabs$tab1a <- lapply(tab1a, function(x) x[,-1])
rm(x, tf, rb, colz, rowz, tab1a)

x <- read_excel(xlsfile, sheet = sheets[2])[,c(1,2,4,8,10)]
constant2015 <- apply(x[3:47,2:5], 2, unlist) %>% apply(2, as.numeric)
currentdollars <- apply(x[50:94,2:5], 2, unlist) %>% apply(2, as.numeric)
tab2 <- list(constant2015, currentdollars)
firsttab <- unlist(x[1,c(2,4)])
secondtab <- unlist(x[48,c(2,4)])
names(tab2) <- c('constant2015', 'current')
rowz <- unlist(x[3:47,1])
colz <- unlist(x[2,2:5])
tab2 <- lapply(tab2, function(x) {rownames(x) <- rowz; colnames(x) <- colz; x})
tab2 <- list(tab2[[1]][,1:2], tab2[[1]][,3:4],tab2[[2]][,1:2], tab2[[2]][,3:4])
names(tab2) <- c(firsttab, secondtab)
mytabs$tab2 <- tab2
rm(x, constant2015, currentdollars, firsttab, secondtab, rowz, colz, tab2)

x <- read_excel(xlsfile, sheet = sheets[3])[,c(1:28)]
constant2015 <- apply(x[2:22,3:28], 2, unlist) %>% apply(2, as.numeric)
currentdollars <- apply(x[24:44,3:28], 2, unlist) %>% apply(2, as.numeric)
setnames <- unlist(x[,1])
setnames <- setnames[!is.na(setnames)]
names(setnames) <- NULL
setnames <- setnames[2:8]
rowz <- unlist(x[,2])
rowz <- rowz[!is.na(rowz)] %>% setdiff("Sector")
rowz <- apply(expand.grid(rowz, setnames),1,function(x) paste(x,collapse=" "))
colz <- paste('Year', sapply(unlist(x[1, 3:28]), trimws), sep = ' ')
tab4 <- list(constant2015, currentdollars)
tablabs <- c(unlist(x[1,1]), unlist(x[23,1]))
names(tab4) <- tablabs
tab4 <- lapply(tab4, function(x) { 
   rownames(x) <- rowz; 
   colnames(x) <- colz; 
   x[setdiff(rowz, rowz[grep('Two-Year', rowz)]),]})
mytabs$tab4 <- tab4
rm(x, constant2015, currentdollars, tablabs, rowz, colz, tab4)

x <- read_excel(xlsfile, sheet = sheets[4])[,c(1,16:30)]
tablabs <- c(paste(unlist(x[1,2]), unlist(x[2,1]), sep = ' '), 
             paste(unlist(x[55,2]), unlist(x[56,1]), sep = ' '))
rowz <- x[3:54,1] %>% unlist()
colz <- paste('Year', sapply(unlist(x[2, 3:14]), trimws), sep = ' ')
constant2015 <- apply(x[3:54,3:14], 2, unlist) %>% apply(2, as.numeric)
currentdollars <- apply(x[57:108,3:14], 2, unlist) %>% apply(2, as.numeric)
tab5 <- list(constant2015, currentdollars)
names(tab5) <- tablabs
tab5 <- lapply(tab5, function(x) {rownames(x) <- rowz; colnames(x) <- colz; x})
mytabs$tab5 <- tab5
rm(x, constant2015, currentdollars, tablabs, rowz, colz, tab5)

x <- read_excel(xlsfile, sheet = sheets[5])[,c(1:11)]
tablabs <- c(paste(unlist(x[1,3]), unlist(x[2,1]), sep = ' '), 
             paste(unlist(x[53,3]), unlist(x[54,1]), sep = ' '))
rowz <- x[3:52,c(2,1)] %>% apply(1, function(x) paste(x, collapse = ' '))
colz <- paste('Year', sapply(unlist(x[2, 3:11]), trimws), sep = ' ')
constant2015 <- apply(x[3:52,3:11], 2, unlist) %>% apply(2, as.numeric)
currentdollars <- apply(x[55:104,3:11], 2, unlist) %>% apply(2, as.numeric)
tab6 <- list(constant2015, currentdollars)
names(tab6) <- tablabs
tab6 <- lapply(tab6, function(x) {rownames(x) <- rowz; colnames(x) <- colz; x})
mytabs$tab6 <- tab6
rm(x, constant2015, currentdollars, tablabs, rowz, colz, tab6)

x <- read_excel(xlsfile, sheet = sheets[6])[1:21,]
tablabs <- c(unlist(x[1,1]), unlist(x[8,1]), unlist(x[15,1]))
tablabs <- sapply(tablabs, gsub, pattern = 'Prviate', replacement = 'Private')
rowz <- x[2:7,1] %>% unlist()
colz <- paste('Year', sapply(unlist(x[1, 2:27]), trimws), sep = ' ')
t1 <- apply(x[2:7,2:27], 2, unlist) %>% apply(2, as.numeric)
t2 <- apply(x[9:14,2:27], 2, unlist) %>% apply(2, as.numeric)
t3 <- apply(x[16:21,2:27], 2, unlist) %>% apply(2, as.numeric)
tab7 <- list(t1,t2,t3)
names(tab7) <- tablabs
tab7 <- lapply(tab7, function(x) {rownames(x) <- rowz; colnames(x) <- colz; x})
tab7[[1]] <- NULL
mytabs$tab7 <- tab7
rm(x, t1, t2, t3, tablabs, rowz, colz, tab7)

x <- read_excel(xlsfile, sheet = sheets[7])[2:56,]
taba2 <- apply(x[2:55,], 2, unlist) %>% apply(2, as.numeric)
colnames(taba2) <- unlist(x[1,])
mytabs$taba2 <- taba2
rm(x, taba2)

x <- read_excel(xlsfile, sheet = sheets[8])
foo <- apply(x[,2:7],2,function(x) x[!is.na(x)])
foo <- apply(foo, 2, unlist)
cost <- rbind(as.integer(foo[2,]), 
              as.integer(foo[3,]), 
              as.integer(foo[4,]), 
              as.integer(foo[5,]))
colnames(cost) <- foo[1,]
rownames(cost) <- unlist(x[,1][2:5,])
fig1 <- cost
mytabs$fig1 <- fig1
rm(x, foo, cost, fig1)

x <- read_excel(xlsfile, sheet = sheets[9])[,1:4]
fig2 <- list(apply(x[8:13,3:4], 2, unlist) %>% apply(2, as.numeric), 
             apply(x[14:19,3:4], 2, unlist) %>% apply(2, as.numeric))
fig2 <- lapply(fig2, function(y) { 
   rownames(y) <- x[2:7,2][[1]]; 
   colnames(y) <- unlist(x[1,3:4][1,]);
   y})
names(fig2) <- sapply(c(x[8,1], x[14,1]), unlist)
mytabs$fig2 <- fig2
rm(x, fig2)

x <- read_excel(xlsfile, sheet = sheets[10])
fig3 <- apply(x[2:16,2:4], 2, unlist) %>% apply(2, as.numeric)
rownames(fig3) <- x[2:16,1][[1]]
colnames(fig3) <- unlist(x[1,2:4][1,])
mytabs$fig3 <- fig3
rm(x, fig3)

x <- read_excel(xlsfile, sheet = sheets[11])
fig5 <- list(apply(x[3:5,2:3], 2, unlist) %>% apply(2, as.numeric), 
             apply(x[3:5,5:6], 2, unlist) %>% apply(2, as.numeric))
fig5 <- lapply(fig5, function(y) { 
   rownames(y) <- x[3:5,1][[1]]; 
   colnames(y) <- unlist(x[2,2:3][1,]);
   y})
names(fig5) <- sapply(c(x[1,2], x[1,5]), unlist)
mytabs$fig5 <- fig5
rm(x, fig5)

x <- read_excel(xlsfile, sheet = sheets[12])
fig6 <- apply(x[2:32,2:3], 2, unlist) %>% apply(2, as.numeric)
rownames(fig6) <- x[2:32,1][[1]]
colnames(fig6) <- unlist(x[1,2:3][1,])
mytabs$fig6 <- fig6
rm(x, fig6)

getFigs89 <- function(x) {
   fig8 <- list(apply(x[2:52,2], 2, unlist) %>% apply(2, as.numeric), 
                apply(x[2:52,4], 2, unlist) %>% apply(2, as.numeric))
   rownames(fig8[[1]]) <- x[2:52,1][[1]]
   rownames(fig8[[2]]) <- x[2:52,3][[1]]
   colnames(fig8[[1]]) <- unlist(x[1,2][1,])
   colnames(fig8[[2]]) <- unlist(x[1,4][1,])
   fig8
}
mytabs$fig8 <- getFigs89(read_excel(xlsfile, sheet = sheets[13]))
mytabs$fig9 <- getFigs89(read_excel(xlsfile, sheet = sheets[14]))

x <- read_excel(xlsfile, sheet = sheets[15])
fig10 <- apply(x[2:51,2:5], 2, unlist) %>% apply(2, as.numeric)
rownames(fig10) <- x[2:51,1][[1]]
colnames(fig10) <- unlist(x[1,2:5][1,])
mytabs$fig10 <- fig10
rm(x, fig10)

getFigs1213 <- function(x) {
   fig12 <- apply(x[2:5,2:22], 2, unlist) %>% apply(2, as.numeric)
   rownames(fig12) <- x[2:5,1][[1]]
   colnames(fig12) <- unlist(x[1,2:22][1,])
   fig12
}
mytabs$fig12 <- getFigs1213(read_excel(xlsfile, sheet = sheets[16]))
mytabs$fig13 <- getFigs1213(read_excel(xlsfile, sheet = sheets[17]))

x <- read_excel(xlsfile, sheet = sheets[18])
fig14 <- list()
fig14$distro <- apply(x[9:12,3:7], 2, unlist) %>% apply(2, as.numeric)
rowz <- x[9:12,2]
colz <- sapply(unlist(x[8, 3:7]), trimws)
colz[1] <- paste('Exactly $', colz[1], sep = '')
colnames(fig14$distro) <- colz
rowz$share <- sapply(rowz[[1]], function(x) as.integer(gsub('^.*\\(|%\\)$','',x))/100)
names(rowz)[1] <- 'rowname'
fig14$distro <- cbind(rowz, fig14$distro)
fig14$avg <- apply(x[5:8,10], 2, unlist) %>% apply(2, as.numeric)
rownames(fig14$avg) <- x[5:8,8][[1]]
mytabs$fig14 <- fig14
rm(x, rowz, colz, fig14)

x <- read_excel(xlsfile, sheet = sheets[19])
fig15 <- list()
fig15$distro <- apply(x[2:5,3:7], 2, unlist) %>% apply(2, as.numeric)
rowz <- x[2:5,2]
colz <- sapply(unlist(x[1, 3:7]), trimws)
colz[1] <- paste('Exactly $', colz[1], sep = '')
colnames(fig15$distro) <- colz
rowz$share <- sapply(rowz[[1]], function(x) as.integer(gsub('^.*\\(|%\\)$','',x))/100)
names(rowz)[1] <- 'rowname'
fig15$distro <- cbind(rowz, fig15$distro)
fig15$avg <- apply(x[5:8,10], 2, unlist) %>% apply(2, as.numeric)
rownames(fig15$avg) <- x[5:8,8][[1]]
mytabs$fig15 <- fig15
rm(x, rowz, colz, fig15)

getFigs2013 <- function(x, rng = c(12:17)) {
   qs <- sapply(unlist(x[1, 2:17]), trimws)
   rowz <- x[3:8,1][[1]]
   colz <- paste('Year', sapply(unlist(x[2, 2:5]), trimws), sep = ' ')
   qs <- qs[!is.na(qs)]
   fig2013_12 <- vector("list", length(qs))
   names(fig2013_12) <- qs
   fig2013_12[[1]] <- apply(x[rng,2:5], 2, unlist) %>% apply(2, as.numeric)
   fig2013_12[[2]] <- apply(x[rng,6:9], 2, unlist) %>% apply(2, as.numeric)
   fig2013_12[[3]] <- apply(x[rng,10:13], 2, unlist) %>% apply(2, as.numeric)
   fig2013_12[[4]] <- apply(x[rng,14:17], 2, unlist) %>% apply(2, as.numeric)
   fig2013_12 <- lapply(fig2013_12, function(x) {rownames(x) <- rowz; colnames(x) <- colz; x})
   fig2013_12
}
mytabs$fig2013_12 <- getFigs2013(x = read_excel(xlsfile, sheet = sheets[20]), rng = c(12:17))
mytabs$fig2013_13 <- getFigs2013(x = read_excel(xlsfile, sheet = sheets[21]), rng = c(3:8))

x <- read_excel(xlsfile, sheet = sheets[22])
fig2014_14a <- vector("list", 2)
names(fig2014_14a) <- x[c(2,12),1][[1]]
fig2014_14a$`In-State` <- apply(x[2:9,4:5], 2, unlist) %>% apply(2, as.numeric)
fig2014_14a$`Out-of-State` <- apply(x[12:19,4:5], 2, unlist) %>% apply(2, as.numeric)
colz <- sapply(unlist(x[1, 4:5]), trimws)
rowz <- x[c(2:9),3][[1]][!is.na(x[c(2:9),3][[1]])]
rowz <- apply(expand.grid(x[2,2][[1]], rowz),1,function(x) paste(x,collapse=" "))
rowz <- apply(expand.grid(c('Posted For', 'Net Paid If'), rowz),1,function(x) paste(x,collapse=" "))
fig2014_14a <- lapply(fig2014_14a, function(x) {rownames(x) <- rowz; colnames(x) <- colz; x})
mytabs$fig2014_14a <- fig2014_14a
rm(x, rowz, colz, fig2014_14a)

x <- read_excel(xlsfile, sheet = sheets[23])
fig2014_15a <- vector("list", 4)
names(fig2014_15a) <- x[c(2,10,18,26),1][[1]]
fig2014_15a[[1]] <- apply(x[2:9,4:5], 2, unlist) %>% apply(2, as.numeric)
fig2014_15a[[2]] <- apply(x[10:17,4:5], 2, unlist) %>% apply(2, as.numeric)
fig2014_15a[[3]] <- apply(x[18:25,4:5], 2, unlist) %>% apply(2, as.numeric)
fig2014_15a[[4]] <- apply(x[26:33,4:5], 2, unlist) %>% apply(2, as.numeric)
colz <- sapply(unlist(x[1, 4:5]), trimws)
rowz <- x[c(2:9),3][[1]][!is.na(x[c(2:9),3][[1]])]
rowz <- apply(expand.grid(x[2,2][[1]], rowz),1,function(x) paste(x,collapse=" "))
rowz <- apply(expand.grid(c('Posted For', 'Net Paid If'), rowz),1,function(x) paste(x,collapse=" "))
fig2014_15a <- lapply(fig2014_15a, function(x) {rownames(x) <- rowz; colnames(x) <- colz; x})
mytabs$fig2014_15a <- fig2014_15a
rm(x, rowz, colz, fig2014_15a)

# 
# OK, now tidy up. Two rules:
# 1. All tables will be turned to long tbl_df objects
# 2. Academic years will be YYYY of the Fall year
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
tidytabs$tab2 <- do.call(bind_rows, tab2) %>% 
   select(Year, Type, Sector, Cost)
rm(tab2)

tab4 <- lapply(mytabs$tab4, data.frame)
tab4 <- lapply(tab4, function(x) {x$Sector <- rownames(x); x})
tab4 <- lapply(tab4, gather, Year, Cost, Year.90.91:Year.15.16)
tab4 <- lapply(tab4, function(x) mutate(x, Year = 1900 + as.integer(substr(gsub('Year\\.','',Year),1,2))))
tab4 <- lapply(tab4, function(x) mutate(x, Year = ifelse(Year > 1990, Year, Year + 100)))
tab4 <- lapply(names(tab4), function(x) mutate(tab4[[x]], Dollars = x))
tidytabs$tab4 <- do.call(bind_rows, tab4) %>% 
   select(Year, Dollars, Sector, Cost)
rm(tab4)