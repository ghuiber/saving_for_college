# saving_for_college

A look at 2015 data from The College Board. 

The original plan was to make some kind of Shiny app so people could put in their own time until the kids go to college and the kind of college they have in mind -- 4-year public in-state, out-of-state, or private nonprofit, and I was going to pitch it at [Analytics Forward](http://www.meetup.com/Research-Triangle-Analysts/events/228455037/?rv=ea1).

The current minimum viable product is a function -- `getMonthlySaving()` -- that will tell you how much you need to set aside per month, per kid, given some assumption about the asset return, time until college starts, and your expected income quartile then, assuming a linear or compound growth rate of the cost of attendance. If you `apply` it to more than one child, income quartile as shown in `forecast.R` you get a table like the one below, with dollar figures in the last two columns:

|  years | balances | kid  | incquart | r     | linear trend | compound growth |
|:------:|:--------:|:----:|:--------:|:-----:|:------------:|:---------------:|
|  11   |   8000   | Anna |   Third  |  0.04 |     478      |       585       |
|  11   |   8000   | Anna | Highest  | 0.04  |    502      |        615  |   
|  14   |   5000   | Belle|   Third|    0.04|      403|              530|       
|  14   |   5000   | Belle| Highest|   0.04|      422|              557|

__This is not financial advice. Duh.__
