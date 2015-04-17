# written by Joe Hilgard March 2014
# borrowing liberally from Hadley Wickham's 2007 presentation and code
# Powerpoint at http://ggplot2.org/resources/2007-vanderbilt.pdf
# Code at http://ggplot2.org/resources/the-grammar.r

# Please find the documentation at http://docs.ggplot2.org/current/
# for further instructions, functions, and arguments.

# install & load ggplot2
install.packages("ggplot2")
require(ggplot2)
# store the "diamonds" dataset as object "dat"
# <- or = takes the stuff on the right and stores it to the object on the left
dat <- diamonds
# what are the column names of "diamonds"?
colnames(diamonds)

# what are the levels of the factor "cut"?
levels(dat$cut)
dat[1,c("carat", "cut")]
# for speed's sake we'll restrict it to just two levels of "cut"
# make a dataset of just those levels.
dat=diamonds[diamonds$cut %in% c("Fair", "Very Good"),]
# lm() fits a model & gets the t-test
# note that you can store models to objects
model = lm(price ~ carat * cut, data=dat)
summary(model)

# if you wanted to make the dopey categorical binned interaction plot,
# it's actually a fair bit of work making the +/- 1 SD
dat$caratBin = NULL
dat$caratBin[dat$carat > mean(dat$carat) + sd(dat$carat)] = "high"
dat$caratBin[dat$carat < mean(dat$carat) - sd(dat$carat)] = "low"
dat2 = na.omit(dat)
table = tapply(dat2$price, list(dat2$cut, dat2$caratBin), FUN=mean)
table = table[complete.cases(table),]
barplot(table, beside=T)
# 68% of your data is within +/-1SD of the mean.
# When you go to plot, you're gonna throw 68% of your data in
# the garbage? I'd rather you didn't.

# GGPLOT has two main tools: qplot() "quick plot" and ggplot() "grammar of graphics plot"
# qplot() is simple and effective for most casual applications.
  # It has reasonable defaults.
# ggplot() is more precise and rigorous, requiring you to specify more options.


qplot(dat$carat, dat$price) # supply two continuous values and qplot intuits a scatterplot

# if you get sick of typing dat$ over and over again, just name the data frame in data=
# and just tell it what column you want mapped to what feature of the graph
qplot(x=carat, y=price, col=clarity, data=dat)

# for regression lines we ask for multiple geoms
# to ask for multiple geoms, use c() to combine them into a vector
# this will get both point geoms and regression line geoms.
# geom "smooth" defaults to locally-weighted regresion curves
# such as loess or GAM as appropriate given N
qplot(x=carat, y=price, data=dat, 
      geom=c("point", "smooth"))

# You can specify method=lm for the strictly linear regression line (instead of locally-weighted)
qplot(x=carat, y=price, col=cut,
      data=dat, geom=c("point", "smooth"),
      method=lm)
# you can see the model holds great until about 3 carats,
# after which it goes all pear-shaped.

# you can request histograms too
qplot(carat, data=dat, geom="histogram")
# modify the binwidth for narrower or wider bins
qplot(carat, data=dat, geom="histogram", binwidth=.05)
# map a feature onto "fill" to get stacked histogram bars
qplot(carat, data=dat, 
      geom="histogram", fill=cut)
# NB: like in powerpoint, "fill" refers to the inside of shapes,
#   while "color" refers to the outline of shapes. Compare:
qplot(carat, data=dat, 
      geom="histogram", col=cut)


# ggplot gives you the power to change anything about your graph,
# just using + to add additional objects and arguments.
# the aes() wrapper is what tells ggplot what column of data corresponds
# to which element or feature of the graph.
# arguments outside the aes() wrapper are set as constants, as for 
# alpha transparancy of points here.
ggplot(dat, aes(x=carat, y=price, col=cut)) + # the base line sets the defaults
  geom_point(alpha=.5) +
  geom_smooth(method=lm) +
  scale_y_continuous(limits=c(0, 20000))

# you can store ggplots to objects!
d <- ggplot(dat,
            aes(x=carat, y=price))
# then call them, with additional arguments if you want.
d + geom_point()
d + geom_point(aes(colour = carat))

ggplot(dat, aes(x=clarity, y=price)) +
  geom_point(position="jitter", aes(alpha=carat, color=cut))

ggplot(data=dat, aes(x=carat, y=price, col=cut)) +
  geom_point() +
  geom_smooth(method="lm")

# now to really show y'all some shit
# suppose you have a 3-way interaction
# that can be kinda difficult to look at, right?
# with facet wrapping, you can plot the interaction at each
# level of a 3rd factor.

plot2 = ggplot(data=dat, aes(x=carat, y=price, col=clarity)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~cut)
plot2

# facet_wrap takes arguments as though they were a prediction formula
# so you put in a tilde and then the factor level you want to condition on
# you could even do products of factor levels for bigger interactions,
# or facet wrap by subject for time-series data, etc.
# note that you can also specify the number of rows and columns for the
# resulting matrix of graphs.

# we can plot a stat with a variety of geoms
ex = ggplot(data=dat, aes(x=carat))
# Histogram is stat_bin with geom_bar
ex + geom_histogram()
ex + stat_bin(geom="bar")
# We could always change the geom, representing the same stat
# with a different geometric object
ex + stat_bin(geom="point")
ex + stat_bin(geom="area")
ex + stat_bin(geom="line")
# this showcases the remarkable modularity and flexibility of ggplot2
ex + stat_bin(geom="line", aes(x=carat, col=cut))

# What about prettying things up, e.g.:
  # Changing font size
  # Removing grey background
  # Converting to B&W
# All this presentational stuff is mostly handled with "themes"
# See http://docs.ggplot2.org/current/theme.html for the
  # full list of all possible theme elements
# See http://docs.ggplot2.org/dev/vignettes/themes.html for vignette

ex + 
  stat_bin(geom="line", aes(x=carat, lty=cut)) +
  theme_bw()

require(grid) # for specifying units in inches, cm, etc.
ex + 
  stat_bin(geom="line", aes(x=carat, lty=cut)) +
  theme(axis.title = element_text(size=24) # change size of axis title
        , legend.key.size = unit(.7, "inch") # make legend bigger
        )

# if you don't like dead space outside range, use expand_limits()
  # set things to zero (or whatever other vector you want)
ex +
  stat_bin(geom="line", aes(x=carat, lty=cut)) +
  expand_limits(x=0, y=0) 

ex +
  stat_bin(geom="line", aes(x=carat, lty=cut)) +
  expand_limits(x=c(-1, 10), y=0) 

# You can save a theme to an object for rapid use.
mytheme = theme(axis.title = element_text(size=24) # change size of axis title
                , legend.key.size = unit(.7, "inch") # make legend bigger
)

plot2 + mytheme

require(tidyr)
mtcars$car = row.names(mtcars)
mt = gather(mtcars, key=car)
ggplot(mt, aes(x=value)) +
  facet_wrap(~name) +
  geom_histogram()

# There's plenty more on the internet at docs.ggplot2.org
# and feel free to email me at jhilgard@gmail.com with your data & code
# if you need a hand.

