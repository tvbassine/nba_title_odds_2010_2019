# This R code produces the total PPEC from 2010-2019 used in this 
# Nylon Calculus post:
# https://fansided.com/2020/06/01/nylon-calculus-nba-expected-titles-decade/

# The second section also contains code to make a pretty plot
# of the championship race each year.
##############################################################################
# Read in Data:

x <- read.csv('~/Downloads/NBA Pre-Playoff Championship Odds - All Years.csv',
              stringsAsFactors = F)
x <- read.csv('https://raw.githubusercontent.com/tvbassine/nba_title_odds_2010_2019/master/NBA%20Pre-Playoff%20Championship%20Odds%20-%20All%20Years.csv',
              stringsAsFactors = F)

# Adjust team names:
x$Team[x$Team == 'Charlotte Bobcats'] = 'Charlotte Hornets'
x$Team[x$Team == 'New Orleans Hornets'] = 'New Orleans Pelicans'

x$Title_Odds_star <- 0

for(i in unique(x$Year)){
  ind <- which(x$Year == i)
  sum_odds <- sum(x$Implied_Odds[ind])
  x$Title_Odds_star[ind] <- x$Implied_Odds[ind] / sum_odds
}

library(dplyr)
y <- x %>%
     group_by(Team) %>%
     summarise(exp_title = sum(Title_Odds_star))
y <- y[order(y$exp_title, decreasing = T),]


y$pct_win_1 <- 0

# Get odds of at least 1 title for each team:
for(i in 1:nrow(y)){
  tm <- y$Team[i]
  temp <- 1 - x$Title_Odds_star[x$Team == tm]
  y$pct_win_1[i] = 1 - prod(temp)
}

# write.csv(y, '~/Desktop/Misc NBA/ppec.csv')

#######################################################
# Make a pretty plot of implied title odds by year:

library(ggplot2)
library(ggrepel)
library(RColorBrewer)


x$yr_rank <- 0
for(i in unique(x$Year)){
  ind <- which(x$Year == i)
  x$yr_rank[ind] <- 1:length(ind)
}

p = ggplot(aes(fill = factor(yr_rank), y=Title_Odds_star, x=Year),
           data = x) + 
  geom_bar(stat = 'identity', colour = 'black', show.legend = F)  +
  scale_x_continuous(breaks = 2010:2019)
p + scale_fill_manual(values=rep('White',16)) +
  ylab('Title Odds') +
  ggtitle('NBA title odds immediately before the playoffs, last 10 seasons', subtitle = 'Source = sportsoddshistory.com') + 
  annotate(geom="text", x=2010, y=.875, label="CLE",
           color="red") +
  annotate(geom="text", x=2011, y=.9, label="LAL",
           color="purple") +
  annotate(geom="text", x=2012, y=.9, label="MIA",
           color="orange") +
  annotate(geom="text", x=2013, y=.8, label="MIA",
           color="orange") +
  annotate(geom="text", x=2014, y=.9, label="MIA",
           color="orange") +
  annotate(geom="text", x=2015, y=.9, label="GSW",
           color="yellow") +
  annotate(geom="text", x=2016, y=.75, label="GSW",
           color="yellow") +
  annotate(geom="text", x=2017, y=.75, label="GSW",
           color="yellow") +
  annotate(geom="text", x=2018, y=.825, label="GSW",
           color="yellow") +
  annotate(geom="text", x=2019, y=.75, label="GSW",
           color="yellow") +
  annotate(geom="text", x=2010, y=.58, label="LAL",
         color="purple") +
  annotate(geom="text", x=2010, y=.4, label="ORL",
         color="blue") +
  annotate(geom="text", x=2011, y=.7, label="MIA",
         color="orange")  +
  annotate(geom="text", x=2011, y=.52, label="CHI",
           color="darkred")  +
  annotate(geom="text", x=2011, y=.37, label="BOS",
           color="green")  +
  annotate(geom="text", x=2011, y=.25, label="SAS",
           color="black") +
  annotate(geom="text", x=2012, y=.65, label="CHI",
           color="darkred")  +
  annotate(geom="text", x=2012, y=.48, label="OKC",
           color="lightblue")  +
  annotate(geom="text", x=2012, y=.32, label="SAS",
           color="black")  +
  annotate(geom="text", x=2013, y=.33, label="SAS",
           color="black")  +
  annotate(geom="text", x=2013, y=.47, label="OKC",
           color="lightblue")  +
  annotate(geom="text", x=2014, y=.65, label="SAS",
         color="black")  +
  annotate(geom="text", x=2014, y=.48, label="OKC",
           color="lightblue")  +
  annotate(geom="text", x=2014, y=.33, label="IND",
           color="gold") + 
  annotate(geom="text", x=2015, y=.6, label="CLE",
           color="red")  +
  annotate(geom="text", x=2015, y=.4, label="SAS",
           color="black")  +
  annotate(geom="text", x=2016, y=.39, label="SAS",
           color="black")  +
  annotate(geom="text", x=2016, y=.22, label="CLE",
           color="red")  +
  annotate(geom="text", x=2017, y=.33, label="CLE",
         color="red")  +
  annotate(geom="text", x=2018, y=.49, label="HOU",
             color="red")  +
  annotate(geom="text", x=2018, y=.27, label="CLE",
             color="red")  +
  annotate(geom="text", x=2019, y=.38, label="MIL",
         color="darkgreen")  +
  annotate(geom="text", x=2019, y=.29, label="HOU",
           color="red")  +
  annotate(geom="text", x=2019, y=.2, label="TOR",
           color="red") +
  annotate(geom="text", x=2011, y=.125, label="DAL",
         color="darkblue")
