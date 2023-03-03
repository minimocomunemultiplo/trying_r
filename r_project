# First we download and load the packages needed.
install.packages("tidyverse")
install.packages("GGally")
install.packages("ggplot2")
library(tidyverse)
library(GGally)
library(readr)
library(ggplot2)

# This R-project is about the dataset we downloaded from the website https://www.kaggle.com/datasets/parasharmanas/world-forest-cover-trends?select=The+Dataset+of+World+Forest+Cover+in+2020.csv . 
# It is a study over the course of 40 years divided in 4 datasets, each one corresponding to a year (1990, 2000, 2010, 2020). Each dataset is 
# made of 4 columns, the first shows country names, the second shows the land area, the third shows the forest cover area, and the fourth 
# shows the ratio forest cover/land area. Rows are, of course, the single state names and their values for each parameter.
# We decided to use the datasets of years 1990 and 2020 and set our null hypothesis (h0) to "the level of forest cover has changed over the 
# course of these 40 years, leading to a significantly lower mean of the forest cover/land area between 1990 and 2020". In an opposite scenario, if 
# h0 is not proved to be true, then (h1) "there isn't any evidence for a change in forest cover/land area ratio from 1990 to 2020".

# Importing datasets and addressing them to variables 

dat20 <- read_csv("data/dataset_2020.csv")
print(dat20)
dat90<-read_csv("data/dataset_1990.csv")
print(dat90)

#inspect dataset
dim(dat20)
dim(dat90)
names(dat20)
names(dat90)
nrow(dat20)
nrow(dat90)
ncol(dat20)
ncol(dat90)

# We can see that the column "land area", gives results of land area in "km x 10". Each value is to be considered x10. The same happens for
# the third column showing the forest covered area. Given these mistakes found in the dataset, and given the fact that they are not very clear,
# we decided to remove the last column (the ratio) and calculate it anew.

# Removing rows containing NA, or irrelevant rows and the last column. 
glimpse(dat20)
dat20<-dat20[-c(1),-4]
dat20
glimpse(dat90)
dat90<-dat90[-c(1),-4]
dat90

# we also want to remove the codes before the country name, those "\xa0". We will do it later, when we join the datasets in one.

# changing columns names not to make them overlay.

dat90<-dat90 %>% 
  rename(land_area_90=`Land area`,
         cover_90=`1990`,
         country=`Country`) 
dat90
dat20<-dat20 %>% 
  rename(land_area_20=`Land area`,
         cover_20=`2020`,
         country=`Country`) 
dat20

# changing reading type, from chr to dbl for last col

dat90<-dat90%>% 
  mutate(cover_90=as.double(cover_90),
         land_area_90=as.double(land_area_90))
dat90
dat20<-dat20%>% 
  mutate(cover_20=as.double(cover_20),
         land_area_20=as.double(land_area_20))
dat20

# now we can finally merge the data frames and stock everything under new variable dat_total, and inspect it, in order to check if everything is fine.
dat_total<-merge(dat90,dat20)
dat_total
dim(dat_total)
tail(dat_total)
head(dat_total)
glimpse(dat_total)

# now we want to sort them in alphabetical order withouth "\xa0" and we want the numbers to be multiplied by a factor of 10, in order to have
# the real surfaces in square km.
# we create a new vector containing the good country names and replace it on the existing one, then we sort alphabetically
country_names<-gsub("\xa0","",as.character(dat_total$country))
country_names
dat_total$country<-country_names
dat_total<-dat_total[order(dat_total$country),]
dat_total
dim(dat_total)

# now we want to change the values for land area of each year and forest cover for each year. all of them x10.
dat_total$land_area_90 #here are the wrong values 
dat_total$land_area_90[1:215]*10 #this is the vector containing correct values
dat_total$land_area_90<-dat_total$land_area_90[1:215]*10 #here we replace it
#the same will be done for the next 3 columns
dat_total$cover_90[1:215]*10 
dat_total$cover_90<-dat_total$cover_90[1:215]*10
dat_total$land_area_20[1:215]*10 
dat_total$land_area_20<-dat_total$land_area_20[1:215]*10
dat_total$cover_20[1:215]*10 
dat_total$cover_20<-dat_total$cover_20[1:215]*10
#here we correct some last names kept mis-spelled
dat_total$country[161]<-"Sao Tome e Principe"
dat_total$country[156]<-"Reunion"
dim(dat_total)

# then, finally, we calculate the new percentage of forest cover for each state, for both years, and stock them in new variables. 

rapp_90<-dat_total$cover_90/dat_total$land_area_90
rapp_20<-dat_total$cover_20/dat_total$land_area_20
rapp_90
rapp_20
dat_total$col6<-rapp_90
dat_total$col7<-rapp_20
dat_total
#changing col names for col 6 and 7.
dat_total<-dat_total %>% 
  rename(rapp_90=`col6`,
         rapp_20=`col7`) 
dat_total

# here I want a new variable containing the updated data frame with both ratios, in order not to make mistakes.
# The function summary gave us a useful glimpse of data. We can already take a look to the most important data such  as max and min
# forest cover, or mean, median etc. 
dat_total_rapp<-dat_total
dat_total_rapp
print(summary(dat_total_rapp))

# here we use a boxplot to show some results
ggplot()+
  geom_boxplot(mapping = aes(x="1990",y=rapp_90))+
  geom_boxplot(mapping = aes(x="2020",y=rapp_20))+
  labs(title = "Forest cover ratio 1990 vs 2020",
       subtitle = NULL,
       x = "Year",
       y = "Forest cover/Area")
# the boxplot shows 1st Qu. and 3rd Qu. for both years, slightly higher for 1990 (0.528) than 2020 (0.510). The median is almost
# equal, the max values are higher in 1990 (0.986 vs 0.974)

# here below we draw cover trends. One for 1990 and one for 2020. On Y-axis we visualize the forest cover on each year, and on 
# X-axis we show the total land area in both years. It is easy to see that for big states, with land area >750000km2 the forest 
# cover is way lower than the land area in 2020 compared to 1990.

trend_plot20 <- dat_total_rapp %>% 
  ggplot(aes(x = land_area_20,
             y = cover_20)) +
  geom_point() +
  labs(title = "Cover trends anno 2020",
       subtitle = NULL,
       x = "Land area 2020",
       y = "Forest cover 2020")
trend_plot20

trend_plot90 <- dat_total_rapp %>% 
  ggplot(aes(x = land_area_90,
             y = cover_90)) +
  geom_point() +
  labs(title = "Cover trends anno 1990",
       subtitle = NULL,
       x = "Land area 1990",
       y = "Forest cover 1990")
trend_plot90

# There are few states with a high forest cover compared to their extension. This is what we can see 
# through the histograms below.
h_plot_20<-ggplot(data = dat_total_rapp)+ 
  geom_histogram(aes(x = rapp_20))
h_plot_20

h_plot_90<-ggplot(data = dat_total_rapp)+ 
  geom_histogram(aes(x = rapp_90))
h_plot_90

# the graphics don't look very good in the first histograms, so we calculated them again, using
# different aesthetics, together with larger classes (0,1). It is possible to notice that in some
# states with lower ratio in 1990 (classes from 0.0 to 0.6) there was an increase in 2020, while in 
# states with higher ratio in 1990 (classes from 0.6 to 1.0) there has been a decrease in 2020.

hist_90<-hist(dat_total_rapp$rapp_90,
              xlab = "Forest cover/Land area",
              col = "green",
              border = "blue", 
              main = "Distribution of the forest cover ratio 1990")
hist_20<-hist(dat_total_rapp$rapp_20,
              xlab = "Forest cover/Land area",
              col = "green",
              border = "blue",
              main = "Distribution of the forest cover ratio 2020")


# in addition we execute a t.test comparing the means for values of forest cover/land area of 1990 and 2020. This test gives us a 
# feedback.
# The null hypothesis (H0) is that the true difference between these group means is zero, thus forest cover hasn't changed.
# The alternate hypothesis (H1) is that the true difference is different from zero, thus forest cover has changed.

t.test(rapp_90,rapp_20)
# In conclusion, we see the p value is higher than 0.05, so we can't say the means are different and we have to accept H0, which 
# tells us that there has been no significant change between 1990 and 2020 in terms of forest cover. 
# since the change is not significant, it is irrelevant to check if the forest cover ratio has decreased, and our research stops here.
# We could think that probably the high usage of forests for shelter wood or crops, initiated over the past century, has surely lowered
# the forest areas in all countries, but the western world has recently compensated this decrease by introducing forestry laws that aim to 
# protect the vegetation. So the western world protected forests, increasing their extension, but the industrial expansion in the 
# poor countries of the third world or even South America contributed to a forest destruction, balancing the growth produced in the west.
# this gives no change in overall global forest cover.
