# Brief dplyr example
library(dplyr)

# combining rows from different spreadsheets
dat1 = data.frame(
  "Subject" = c(1:3, 5),
  "a" = c(3,3,2,1),
  "b" = c(4, 1, 4, 3))
dat2 = data.frame(
  "Subject" = c(6:10),
  "a" = c(2, 1, 3, 1, 3),
  "c" = c(5, 5, 2, 2, 3))
dat1
dat2
# This can be challenging with the base rbind() function
rbind(dat1, dat2)

# It's a snap with dplyr::bind_rows()!
  # Note that bind_rows() automatically imputes NA sensibly.
dat = bind_rows(dat1, dat2)
dat

# Note also that dplyr::full_join() and etc. 
  # may be preferable to cbind 
dat3 = data.frame("Subject" = 9:1, "d" = sample(1:10, 9))
dat3

full_join(dat, dat3) # returns rows for anybody in either tbl
# see ?full_join for more details on the 'by' argument
  # which the join() family uses to match rows.
inner_join(dat, dat3) # returns rows only for obs in both tbls
left_join(dat, dat3) # returns rows only for obs in left tbl 
anti_join(dat, dat3) # returns rows in left tbl that are missing in right tbl

# Let's return to the mtcars example
# Let's make a dplyr chain to get a table of mean MPGs by automatic vs. manual
  # but only for 6-cylinder cars
mtcars %>%
  filter(cyl == 6) %>%
  select(mpg, am, wt) %>%
  group_by(am) %>%
  summarise("Mean_Weight" = mean(wt),
            "Mean_MPG" = mean(mpg)) %>%
  mutate(am = as.factor(am)) %>%
  ggplot(aes(x=am, y=Mean_MPG, fill = am)) +
  geom_bar(stat="identity")

# Sometimes a violin plot can be more informative
mtcars %>%
  mutate(cyl = as.factor(cyl),
         am = as.factor(am)) %>%
  ggplot(aes(x=am, y=mpg, fill = am)) +
  geom_violin() +
  facet_wrap(~cyl)

