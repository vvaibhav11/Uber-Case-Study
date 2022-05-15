install.packages("readxl")
library("readxl")

library(dplyr)
library(ggplot2)
library(tidyr)

#loading artea dataset
uber <- read_excel("uber_dataset.xlsx", sheet = "Switchbacks")
uber$year <- format(uber$period_start, format="%Y")
uber$month <- format(uber$period_start, format="%m")
uber$day <- format(uber$period_start, format="%d")
uber$hour <- format(uber$period_start, format="%H")
uber$minute <- format(uber$period_start, format="%M")
uber$weekday_no <- as.POSIXlt(uber$period_start)$wday
uber$weekdays <- weekdays(as.Date(uber$period_start))

attach(uber)

############################# exploratory data analysis and graphical insights #################################
# Relevant metrics were used for analysis like hours, commute, weekdays etc.

df = uber %>% group_by(wait_time)  %>%
  summarise(TripsPool = sum(trips_pool),
            TripsExpress = sum(trips_express),
            Cancel = sum(rider_cancellations),
            Matches = sum(total_matches),
            DoubleMatch = sum(total_double_matches),
            .groups = 'drop')

df = gather(df, condition, measurement, TripsPool, TripsExpress, Cancel, Matches, DoubleMatch)

ggplot(df, aes(fill = wait_time, y=measurement, x=condition)) + 
  geom_bar(position="dodge",stat="identity") + theme_bw() +
  ylab("Per Unit") + 
  xlab("Metric") + 
  ggtitle("Metric vs unit across wait times") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title="Wait Time"))

df2 = uber %>% group_by(wait_time)  %>%
  summarise(DriverPayout = sum(total_driver_payout),
            .groups = 'drop')

ggplot(df2, aes(y=DriverPayout, x=wait_time)) + 
  geom_bar(position="dodge",stat="identity", fill = "chocolate2") + 
  theme_bw() + 
  ylab("Driver Payout (in $)") + 
  xlab("Wait Time") + 
  ggtitle("Driver payout vs Wait time") +
  theme(plot.title = element_text(hjust = 0.5))

######################################################################################

df3 = uber %>% group_by(wait_time,hour)  %>%
  summarise(TripsPool = sum(trips_pool),
            TripsExpress = sum(trips_express),
            Cancel = sum(rider_cancellations),
            DriverPayout = sum(total_driver_payout),
            .groups = 'drop')

ggplot(data=df3, aes(x=hour, y=TripsPool, group=wait_time, color=wait_time)) +
  geom_line()+
  ggtitle("Pool rides spead across day hours") +
  ylab("Number of pool rides")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Wait Time"))

ggplot(data=df3, aes(x=hour, y=TripsExpress, group=wait_time, color=wait_time)) +
  geom_line()+
  ggtitle("Express rides spead across day hours") +
  ylab("Number of Express rides")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Wait Time"))

ggplot(data=df3, aes(x=hour, y=Cancel, group=wait_time, color=wait_time)) +
  geom_line()+
  ggtitle("Cancellation across day hours") +
  ylab("Number of Cancellations")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Wait Time"))

ggplot(data=df3, aes(x=hour, y=DriverPayout, group=wait_time, color=wait_time)) +
  geom_line()+
  ggtitle("Driver payout across day hours") +
  ylab("Driver Payout")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Wait Time"))

######################################################################################

df4 = uber %>% group_by(wait_time,weekdays)  %>%
  summarise(TripsPool = sum(trips_pool),
            TripsExpress = sum(trips_express),
            Cancel = sum(rider_cancellations),
            DriverPayout = sum(total_driver_payout),
            .groups = 'drop')

df4$weekdays <- factor(df4$weekdays, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

df4 <- df4[order(df4$weekdays), ]

ggplot(data=df4, aes(x=weekdays, y=TripsPool, group=wait_time, color=wait_time)) +
  geom_line()+
  ggtitle("Pool rides spead across weekdays") +
  ylab("Number of pool rides")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Wait Time"))

ggplot(data=df4, aes(x=weekdays, y=TripsExpress, group=wait_time, color=wait_time)) +
  geom_line()+
  ggtitle("Express rides spead across weekdays") +
  ylab("Number of Express rides")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Wait Time"))

ggplot(data=df4, aes(x=weekdays, y=Cancel, group=wait_time, color=wait_time)) +
  geom_line()+
  ggtitle("Cancellation across weekdays") +
  ylab("Number of Cancellations")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Wait Time"))

ggplot(data=df4, aes(x=weekdays, y=DriverPayout, group=wait_time, color=wait_time)) +
  geom_line()+
  ggtitle("Driver payout across weekdays") +
  ylab("Driver Payout")+ theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(title="Wait Time"))

##########################################################################################

df5 = uber %>% group_by(wait_time, commute)  %>%
  summarise(TripsPool = sum(trips_pool),
            TripsExpress = sum(trips_express),
            Cancel = sum(rider_cancellations),
            DriverPayout = sum(total_driver_payout),
            total_matches = sum(total_matches),
            total_double_matches = sum(total_double_matches),
            .groups = 'drop')

df5$trip_acceptance = (1-(df5$Cancel/(df5$TripsPool+df5$TripsExpress)))*100
df5$avg_earning = df5$DriverPayout/((df5$TripsPool+df5$TripsExpress)-df5$Cancel)
df5$seat_fill_rate = ((df5$total_matches*2)+df5$total_double_matches*3)/(df5$TripsPool+df5$TripsExpress-df5$Cancel)
df5$pool_ratio = df5$TripsPool/(df5$TripsPool + df5$TripsExpress) * 100
df5$express_ratio = df5$TripsExpress/(df5$TripsPool + df5$TripsExpress) * 100

colnames(df5)[1] <- "Wait time"
colnames(df5)[2] <- "Rush hour"
colnames(df5)[9] <- "Trip acceptance"
colnames(df5)[10] <- "Average earning"
colnames(df5)[11] <- "Seat fill rate"
colnames(df5)[12] <- "Pool ratio"
colnames(df5)[13] <- "Express ratio"

df5 = df5[c("Wait time", "Rush hour","Trip acceptance","Average earning","Seat fill rate","Pool ratio","Express ratio")]

##########################################################################################

df6 = uber %>% group_by(commute)  %>%
  summarise(TripsPool = sum(trips_pool),
            TripsExpress = sum(trips_express),
            Cancel = sum(rider_cancellations),
            DriverPayout = sum(total_driver_payout),
            total_matches = sum(total_matches),
            total_double_matches = sum(total_double_matches),
            .groups = 'drop')

df6$trip_acceptance = (1-(df6$Cancel/(df6$TripsPool+df6$TripsExpress)))*100
df6$avg_earning = df6$DriverPayout/((df6$TripsPool+df6$TripsExpress)-df6$Cancel)
df6$seat_fill_rate = ((df6$total_matches*2)+df6$total_double_matches*3)/(df6$TripsPool+df6$TripsExpress-df6$Cancel)
df6$pool_ratio = df6$TripsPool/(df6$TripsPool + df6$TripsExpress) * 100
df6$express_ratio = df6$TripsExpress/(df6$TripsPool + df6$TripsExpress) * 100

colnames(df6)[1] <- "Wait time"
colnames(df6)[8] <- "Trip acceptance"
colnames(df6)[9] <- "Average earning"
colnames(df6)[10] <- "Seat fill rate"
colnames(df6)[11] <- "Pool ratio"
colnames(df6)[12] <- "Express ratio"

df6 = df6[c("Wait time","Trip acceptance","Average earning","Seat fill rate","Pool ratio","Express ratio")]

##########################################################################################
df4 = uber
df4$wait_time = ifelse(df4$wait_time=="2 mins",0,1)

t.test(uber$wait_time[uber$wait_time=="0"], uber$trips_pool)
t.test(uber$wait_time[uber$wait_time=="1"], uber$trips_pool)

t.test(uber$wait_time[uber$wait_time=="2 mins"], uber$trips_express)
t.test(uber$wait_time[uber$wait_time=="5 mins"], uber$trips_express)

t.test(uber$wait_time[uber$wait_time=="2 mins"], uber$rider_cancellations)
t.test(uber$wait_time[uber$wait_time=="5 mins"], uber$rider_cancellations)

t.test(uber$wait_time[uber$wait_time=="2 mins"], uber$total_matches)
t.test(uber$wait_time[uber$wait_time=="5 mins"], uber$total_matches)

t.test(uber$wait_time[uber$wait_time=="2 mins"], uber$total_double_matches)
t.test(uber$wait_time[uber$wait_time=="5 mins"], uber$total_double_matches)

t.test(uber$wait_time[uber$wait_time=="2 mins"], uber$total_driver_payout)
t.test(uber$wait_time[uber$wait_time=="5 mins"], uber$total_driver_payout)

##########################################################################################
# linear model

model1 = lm(trips_express ~ treat + rider_cancellations + total_driver_payout, 
           data = uber)

summary(model)

##########################################################################################
##########################################################################################
# Further analysis and predictions

###### Imports 
library("readxl")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(car)
library(stargazer)
library(tidyverse)

###### Simple Data exploration
uberdata <- read_excel("619702-XLS-ENG.xlsx", sheet = 3)

summary(uberdata)
attach(uberdata)

#Numeric to numeric and factor to factor
uberdata = uberdata %>% 
  mutate_at(vars(trips_pool, trips_express, rider_cancellations, 
                 total_driver_payout, total_matches, total_double_matches),as.numeric) %>% 
  mutate_at(vars(treat, commute),
            as.factor)

#Numeric columns
vec.num <- c(6:11)
###Histogram plots of continuous variables
histplot = function (data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(fill = "blue") +
    xlab(column) 
  
}
list.histplots <- lapply(colnames(uberdata[vec.num]), histplot, data = uberdata[vec.num])
names(list.histplots) <- colnames(uberdata[vec.num])

#Arrange in grid
n <- length(list.histplots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.histplots, ncol=nCol))


##### Scatter plots every variable
pairs(uberdata[vec.num], pch=10, lower.panel=NULL)

##### Correlation matrix
cor(uberdata[vec.num])

###### Basic Linear Regressions
base_linreg.rcancel <- lm(rider_cancellations ~ treat + commute + trips_pool + trips_express + 
                            total_matches + total_double_matches)
summary(base_linreg.rcancel)

base_linreg.payout <- lm(total_driver_payout ~ treat + commute +trips_pool + trips_express + 
                           total_matches + total_double_matches)
summary(base_linreg.payout)

#Resdidual plot
residualPlots(base_linreg.rcancel)
residualPlots(base_linreg.payout)

#Trying polynomial regression for rider cancellations
#Residual plots indicate non-linearity for trips_express, total_matches and total_double_matches
poly_linreg.rcancel <- lm(rider_cancellations ~ treat + commute + trips_pool + poly(trips_express,2) + 
                            poly(total_matches,2) + poly(total_double_matches,2))
summary(poly_linreg.rcancel)

poly_linreg3.rcancel <- lm(rider_cancellations ~ treat + commute + trips_pool + poly(trips_express,3) + 
                             poly(total_matches,3) + poly(total_double_matches,3))
summary(poly_linreg.rcancel)

anova(base_linreg.rcancel,poly_linreg.rcancel, poly_linreg3.rcancel)
#Anvova says stick with quadratic


#Trying polynomial regression for driver payout
#Residual plots indicate non-linearity for total_matches 
poly_linreg.payout <- lm(total_driver_payout ~ treat + commute + trips_pool + poly(trips_express,2) + 
                           poly(total_matches,2) + poly(total_double_matches,2))
summary(poly_linreg.payout)


###Effect of wait time on total shared rides - pool + express
uberdata$total_shared = uberdata$trips_pool + uberdata$trips_express
attach(uberdata)


#Linear Regression
base_linreg.total <- lm(total_shared ~ treat, data=uberdata)
summary(base_linreg.total)
#With just treat, not significant

#Add total_matches, total_double_matches, rider_cancellations, total_driver_payout 
base_linreg.total <- lm(total_shared ~ treat + commute + total_matches + total_double_matches 
                        + total_driver_payout + rider_cancellations , data=uberdata)
summary(base_linreg.total)
#Still not significant

#What about an interaction between commute and treatment?
#Logic: rush hour has an effect on market dynamics
interac_linreg.total <- lm(total_shared ~ treat + commute + treat*commute + total_matches + total_double_matches 
                           + total_driver_payout + rider_cancellations  , data=uberdata)
summary(interac_linreg.total)


#Interaction is significant, decrease in total number of shared rides
#Decrease doesn't occur when commute=TRUE and treat=FALSE, suggesting longer express wait time has a
#negative effect

#Check same features but for trips_pool and trips_express to understand effects
interac_linreg.trips_pool <- lm(trips_pool ~ treat + commute + treat*commute + total_matches +  total_double_matches 
                                + total_driver_payout + rider_cancellations , data=uberdata)
summary(interac_linreg.trips_pool)


interac_linreg.trips_express <- lm(trips_express ~ treat + commute + treat*commute + total_matches + total_double_matches 
                                   + total_driver_payout + rider_cancellations, data=uberdata)
summary(interac_linreg.trips_express)

stargazer(interac_linreg.total, interac_linreg.trips_pool, interac_linreg.trips_express , type="html", 
          title="Effect of Treatment on Total Shared Rides", single.row=TRUE,
          digits = 1, report = "vc*")

#Average effects
avg.total_shared <- aggregate(total_shared ~ treat + commute, uberdata, FUN=mean)
View(avg.total_shared)

uberdata.commute <- subset(uberdata, commute == FALSE)
wilcox.total_shared.commute_false <- wilcox.test(total_shared ~ treat, data = uberdata.commute)
uberdata.commute <- subset(uberdata, commute == TRUE)
wilcox.total_shared.commute_true <- wilcox.test(total_shared ~ treat, data = uberdata.commute)

###Effect of wait time on proportion of matched shared rides
# Prop shared rides that were matched = total_matches/total_shared (*100?)
uberdata$prop_matched = uberdata$total_matches / uberdata$total_shared
attach(uberdata)

# Same Interactive lin reg as above
interac_linreg.prop_matched <- lm(prop_matched ~ treat + commute + treat*commute 
                                  , data=uberdata)
summary(interac_linreg.prop_matched)


stargazer(interac_linreg.prop_matched , type="html", 
          title="Effect of Treatment on Proportion of Matched Shared Rides", single.row=TRUE,
          digits = 1, report = "vc*")


###Effect of wait time on driver payout per trip
#Driver payout per trip = total_driver_payout / total_shared
uberdata$payout_per_trip = uberdata$total_driver_payout / uberdata$total_shared
attach(uberdata)



# Same Interactive lin reg as above
interac_linreg.payout_per_trip <- lm(payout_per_trip ~ treat + commute + treat*commute + rider_cancellations 
                                     + total_matches + total_double_matches, data=uberdata)
summary(interac_linreg.payout_per_trip)

# For total driver payout
interac_linreg.total_payout <- lm(total_driver_payout ~ treat + commute + treat*commute + rider_cancellations 
                                  + total_matches + total_double_matches, data=uberdata)
summary(interac_linreg.total_payout)

stargazer(interac_linreg.payout_per_trip, interac_linreg.total_payout, type="html", 
          title="Effect of Treatment on Driver Payout per Trip", single.row=TRUE,
          digits = 1, report = "vc*")


### Effect of wait time on rider cancellation
interac_linreg.cancellation <- lm(rider_cancellations ~ treat + commute + treat*commute + total_matches + total_driver_payout)
summary(interac_linreg.cancellation)


## Average total driver payout grouped by treat and commute
avg.total_payout <- aggregate(total_driver_payout ~ treat + commute, uberdata, mean)
View(avg.total_payout)


###### Graphs

# Trips pool, express and rider cancellation distrib curves grouped by treat
gg.trips_pool = ggplot(data= uberdata, 
                       aes(x=trips_pool, group=treat, fill=treat)) +
  geom_density(adjust=1.5, alpha=0.5) +
  labs(x="Total Pool Trips") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  facet_wrap(~commute, labeller = label_both)

gg.trips_express = ggplot(data= uberdata, 
                          aes(x=trips_express, group=treat, fill=treat)) +
  geom_density(adjust=1.5, alpha=0.5) +
  labs(x="Total Express Trips")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(~commute, labeller = label_both)

gg.rider_cancellations = ggplot(data= uberdata, 
                                aes(x=rider_cancellations, group=treat, fill=treat)) +
  geom_density(adjust=1.5, alpha=0.5) +
  labs(x="Total Rider Cancellations")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(~commute, labeller = label_both)

grid.arrange(gg.trips_express, gg.trips_pool, gg.rider_cancellations)

# Mean total shared rides grouped by treat and commute
gg.total_shared <- ggplot(avg.total_shared, 
                          aes(fill=treat, y=total_shared, x=commute)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Mean Total Shared Rides", x = "Commute") +
  scale_x_discrete(labels= c("No Rush Hour", "Rush Hour")) 




# Mean total driver payout grouped by treat and commute
gg.total_driver_payout <- ggplot(avg.total_payout, 
                                 aes(fill=treat, y=total_driver_payout, x=commute)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Mean Total Driver Payout", x = "Commute") +
  scale_x_discrete(labels= c("No Rush Hour", "Rush Hour"))
