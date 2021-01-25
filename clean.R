
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(RColorBrewer)


#raw <- read_excel("Master.xlsx",sheet="DATA") # %>% clean_names() #this made everyhting lowercase and better

# read in the data
raw <- read_csv("Master.csv", col_names = TRUE, col_types = NULL) %>% clean_names()
?read_csv

dim(raw)
str(raw)
names(raw)
# new data set for cleaning
pam <- raw

# remove asterisk in frags id
pam$frags_id <- gsub("\\*", "", pam$frags_id)

# coerce to correct class
pam <- mutate(pam,
              tank=as.factor(tank), 
              frags_id=as.numeric(frags_id),
              colony_id=as.factor(colony_id), 
              temp=as.factor(temp),
              phenotype=as.factor(phenotype),
              bleaching_phenotype=as.factor(bleaching_phenotype), 
              treatment=as.factor(treatment))

str(pam)
spec(pam)
# clean factor variables
levels(pam$tank)
levels(pam$bleaching_phenotype)

# recode misspelling
pam$bleaching_phenotype <- fct_recode(pam$bleaching_phenotype, 
                                      "yellow"="yelllow")
# reorder treatments
levels(pam$treatment)
pam$treatment <- fct_relevel(pam$treatment, 
                             "Control", "POC Juice", "POR Juice", "POC + POR")
summary(pam) # check again - 6/30 is not numeric

pam$x6_30_20 # one has letters 
pam$x6_30_20[91] <- "0.616" # fixed

as.numeric(pam$x6_30_20) # still throwing errors
which(!is.na(pam$x6_30_20) == is.na(as.numeric(pam$x6_30_20)))

pam$x6_30_20[23] <- "0.642" #fix two decimals and zeroes
pam$x6_30_20 <- as.numeric(pam$x6_30_20) # no errors now!

# subset for timeseries
timeseries <- select(pam, tank, treatment, temp, x6_30_20:x8_13_20)

# get means
timeseries_mean <- timeseries %>% group_by(tank, treatment, temp) %>% 
  summarize_at(c("x6_30_20","x7_13_20","x7_16_20", 
                 "x7_20_20", "x7_23_20", "x7_28_20", 
                 "x7_30_20","x8_10_20", "x8_13_20"), mean, na.rm=TRUE)
#?summarize_at

timeseries_sd <- timeseries %>% group_by(tank, treatment, temp) %>% 
  summarize_at(c("x6_30_20","x7_13_20","x7_16_20", 
                 "x7_20_20", "x7_23_20", "x7_28_20", 
                 "x7_30_20","x8_10_20", "x8_13_20"), sd, na.rm=TRUE)
#?summarize_at


# wide to long
timeseries_mean <- gather(timeseries_mean, x6_30_20:x8_13_20, key = "date", value="fv_fm")
timeseries_sd <- gather(timeseries_sd, x6_30_20:x8_13_20, key = "date", value="fv_fm")

names(timeseries_mean)[5] <- "mean"
names(timeseries_sd)[5] <- "sd"

timeseries <- left_join(timeseries_mean, timeseries_sd, by=names(timeseries_mean)[1:4])

# clean dates
timeseries$date <- gsub("x", "", timeseries$date)
timeseries$date <- gsub("\\_", "-", timeseries$date)
timeseries$date <- ymd(timeseries$date)

# drop extra tank 24 - check this later
timeseries <- subset(timeseries, !is.na(temp))

############################Graphing PAM vs. treatement########################

# plot - tanks as colors
ggplot(timeseries, aes(x=date, y=mean, color=tank)) + 
  geom_point() + geom_line()

# plot - line type temp, tanks as colors
timeseries$temp <- fct_relevel(timeseries$temp, "High", "Ambient")
ggplot(timeseries, aes(x=date, y=mean, color=tank)) + 
  geom_point() + geom_line(aes(linetype=temp))

# plot colors as temps, line type for each tank
ggplot(timeseries, aes(x=date, y=mean, color=temp)) + 
  geom_point() + geom_line(aes(linetype=tank))

# plot colors as temps, line for each tank
ggplot(timeseries, aes(x=date, y=mean, color=temp, group=tank)) + 
  geom_point() + geom_line()

# plot facet by treatment, line type by temp
ggplot(timeseries, aes(x=date, y=mean, color=tank)) + 
  geom_point() + geom_line(aes(linetype=temp)) + facet_wrap(~treatment)

# plot facet by temp, linetype treatment, color tank
ggplot(timeseries, aes(x=date, y=mean, color=tank)) + 
  geom_point() + geom_line(aes(linetype=treatment)) + facet_wrap(~temp)

# plot facet by temp, linetype tank, color treatment
ggplot(timeseries, aes(x=date, y=mean, color=treatment)) + 
  geom_point() + geom_line(aes(linetype=tank)) + facet_wrap(~temp) +
  labs(title = "Ray's smart science graph", 
       subtitle="She said something about pociloptiies", 
       x="Date", y="FVFM???", 
       caption="Note; there was a freakin' hurricane dude", 
       color="Treatment", linetype="Tank ID") 
 # theme(plot.title = element_text(face="bold"), 
  #      legend.position = "bottom")
 # scale_color_manual(values=brewer.pal(8, "Set2")[c(2,4,6,8)])

ggplot(timeseries, aes(x=date, y=mean, color=temp)) + 
  geom_point() + geom_line(aes(linetype=tank)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=2) +
  facet_wrap(~treatment) 

ggplot(timeseries, aes(x=date, y=mean, color=temp)) + 
  geom_point() + geom_line(aes(linetype=tank)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=2) +
  facet_wrap(~treatment) 


ggplot(timeseries, aes(x=date, y=mean, color=treatment)) + 
  geom_point() + geom_line(aes()) + facet_wrap(~temp) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=2) +
  labs(title = "Photosynthetic Efficiency Over Time", 
       subtitle="Ambient vs. High temperatures colored by treatment", 
       x="Date", y="Fv/Fm Yield", 
       caption="Note: Hurricane Douglas distruption on July 26th", 
       color="Treatment", linetype="Tank ID") +
  geom_vline(xintercept=ymd("2020-07-26"), linetype=3, color="black")+
  annotate("text", x=ymd("2020-07-15"), y=0.3, label="hurricane here")


ggplot(timeseries, aes(x=date, y=mean, color=treatment)) + 
  geom_point() + geom_line(aes()) + facet_wrap(~temp) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=2) +
  labs(title = "Photosynthetic Efficiency Over Time", 
       subtitle="Ambient vs. High temperatures colored by treatment", 
       x="Date", y="Fv/Fm Yield", 
       caption="Note: Hurricane Douglas distruption on July 26th", 
       color="Treatment", linetype="Tank ID") + 
  scale_x_date(breaks=ymd(c("2020-07-01","2020-07-15","2020-07-27", "2020-08-01", "2020-08-15")))

               
timeseries$daynum <- as.numeric(difftime(timeseries$date, ymd("2020-06-30"), units = "days"))

ggplot(timeseries, aes(x=daynum, y=mean, color=treatment)) + 
  geom_point() + geom_line(aes()) + facet_wrap(~temp) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=2) +
  labs(title = "Photosynthetic Efficiency Over Time", 
       subtitle="Ambient vs. High temperatures colored by treatment", 
       x="Day of Experiment", y="Fv/Fm Yield", 
       caption="Note: Hurricane Douglas distruption on Day 26", 
       color="Treatment", linetype="Tank ID") 

difftime(ymd("2020-07-26"), ymd("2020-06-30"), units = "days")

ggplot(timeseries, aes(x=daynum, y=mean, color=treatment)) + 
  geom_point() + geom_line(aes()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=1.5) +
  labs(title = "Photosynthetic Efficiency Over Time", 
       subtitle="High temperatures colored by treatment", 
       x="Day of Experiment", y="Fv/Fm Yield", 
       caption="Note: Hurricane Douglas distruption on Day 26", 
       color="Treatment", linetype="Tank ID") 

ggplot(timeseries, aes(x=date, y=mean, color=tank)) + 
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=1.5) +
  labs(title = "Photosynthetic Efficiency Over Time", 
       subtitle="", 
       x="Day of Experiment", y="Fv/Fm Yield", 
       caption="Note: Hurricane Douglas distruption on July 26", 
       color="Treatment", linetype="Tank ID") 


############################Bleaching Phenotype############################
# subset for bleahcing phenotype
colormorph <- select(pam, tank, treatment, temp, phenotype, bleaching_phenotype, x2020_06_30:x2020_08_13)

# get means
colormorph_mean <- colormorph %>% group_by(tank, treatment, temp, phenotype, bleaching_phenotype) %>% 
  summarize_at(c("x2020_06_30","x2020_07_13","x2020_07_16", 
                 "x2020_07_20", "x2020_07_23", "x2020_07_28", 
                 "x2020_07_30","x2020_08_10", "x2020_08_13"), mean, na.rm=TRUE)
#?summarize_at

colormorph_sd <- colormorph %>% group_by(tank, treatment, temp, phenotype, bleaching_phenotype) %>% 
  summarize_at(c("x2020_06_30","x2020_07_13","x2020_07_16", 
                 "x2020_07_20", "x2020_07_23", "x2020_07_28", 
                 "x2020_07_30","x2020_08_10", "x2020_08_13"), sd, na.rm=TRUE)
#?summarize_at


# wide to long
colormorph_mean <- gather(colormorph_mean, x2020_06_30:x2020_08_13, key = "date", value="fv_fm")
colormorph_sd <- gather(colormorph_sd, x2020_06_30:x2020_08_13, key = "date", value="fv_fm")

names(colormorph_mean)[7] <- "mean"
names(colormorph_sd)[7] <- "sd"

colormorph <- left_join(colormorph_mean, colormorph_sd, by=names(colormorph_mean)[1:6])

# clean dates
colormorph$date <- gsub("x", "", colormorph$date)
colormorph$date <- gsub("\\_", "-", colormorph$date)
colormorph$date <- ymd(colormorph$date)

# drop extra tank 24 - check this later
colormorph <- subset(colormorph, !is.na(temp))

# plot - tanks as colors
ggplot(colormorph, aes(x=date, y=mean, color=bleaching_phenotype)) + 
  geom_point() + geom_line()

# plot facet by treatment, line type by temp
ggplot(colormorph, aes(x=date, y=mean, color=bleaching_phenotype)) + 
  geom_point() + geom_line(aes(linetype=treatment)) + facet_wrap(~phenotype)
