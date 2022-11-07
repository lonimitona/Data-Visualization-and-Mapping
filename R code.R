# -----------------------------------------------------------------------------
# <502 Assignment code.R>
# <PMIM502 Assignment>/<Health Data Visualization>
# <R Script>/<26/05/2022>
# <Student ID>/<2149508>
# <Rcode/RStudio Project>
# <Copyright/2149508>
# -----------------------------------------------------------------------------

#Load libraries
library(tidyverse) #set of packages for performing statistical transformations with data
library(dplyr) #for manipulating data frames
library(ggplot2) #for building and modifying graphs and charts
library(scales) #for manipulating scales on graphs and charts
library(lubridate) #for parsing date-time data
library(reshape2)  #To merge two different columns into one for easy plotting on 1 graph

#Load the csv files for line graph
#Graph will compare Total Deaths in Care Homes vs. 5 year average
death_regs_2022 <- read.csv('weekly_regs_2022.csv')

all_and_covid_deaths_2021 <- read.csv('weeklydeaths2021.csv')

all_and_covid_deaths_2020 <- read.csv('weeklydeaths2020.csv')

#Sort 2020 death data
dth_wales_20 <- all_and_covid_deaths_2020 %>% select(month = Week.ending, 
  five_yr_avg = X5.year.average.comparison..Wales, tot_dth = All.deaths..Wales)
glimpse(dth_wales_20)
#Convert character to date
dth_wales_20$month <- mdy(dth_wales_20$month)
#Group days to months
dth_wales_20 <- dth_wales_20 %>% 
  group_by(month = lubridate::floor_date(month, "month")) %>%
  summarize(five_yr_avg = sum(five_yr_avg), tot_dth = sum(tot_dth))

#Sort 2021 death data
dth_wales_21 <- all_and_covid_deaths_2021 %>% select(month = Week.ended, 
  five_yr_avg = X5.year.average, tot_dth = All.deaths)
glimpse(dth_wales_21)
#Convert character to date
dth_wales_21$month <- dmy(dth_wales_21$month)
#Group days to months
dth_wales_21 <- dth_wales_21 %>% 
  group_by(month = lubridate::floor_date(month, "month")) %>%
  summarize(five_yr_avg = sum(five_yr_avg), tot_dth = sum(tot_dth))

#Sort 2022 death data
dth_wales_22 <- death_regs_2022 %>% select(month = Week.ended,
  five_yr_avg = Five.year.average.Wales, tot_dth = Total.deaths.Wales) 
glimpse(dth_wales_22)
#Convert character to date
dth_wales_22$month <- dmy(dth_wales_22$month)
#Group days to months
dth_wales_22 <- dth_wales_22 %>% 
  group_by(month = lubridate::floor_date(month, "month")) %>%
  summarize(five_yr_avg = sum(five_yr_avg), tot_dth = sum(tot_dth))
#Omit empty cells
dth_wales_22 <- dth_wales_22[!is.na(dth_wales_22$tot_dth), ] 

#Merge the three files
dth_wales_all <-rbind(dth_wales_20, dth_wales_21, dth_wales_22)

#melt data frame into long format using reshape2 package
df <- melt(dth_wales_all,  id.vars = 'month', variable.name = 'series')
df <- df %>% mutate(year = year(month)) %>% group_by(year)
glimpse(df)

#create line plot for each column in data frame
ggplot(df, aes(month, value)) +
  geom_line(aes(colour = series)) + geom_point(color = "rosybrown4") + theme_bw() + 
  facet_grid(~year, scales="free_x", space="free_x") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%B") + 
  scale_y_continuous(limits=c(100,1200), breaks=seq(100,1200,by=100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12), axis.title = element_text(face = "bold", size = 15),
        plot.title = element_text(size = rel(2.0), face = "bold", hjust = 0.5,                                                                                           margin = margin(t = 10, b = 20, unit = "pt")), 
        plot.subtitle = element_text(size = rel(1.0), face = "bold", hjust = 0.5), 
        strip.text = element_text(face = "bold", color = "rosybrown4",
                                  hjust = 0.5, size = 15),
        strip.background = element_rect(fill = "gainsboro", linetype = "dotted"), 
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ) +
  labs(y = "Number of Deaths", x = "Month", title = "Number of Deaths Registered in Care Homes in Wales (March 2020 - April 2022)",
       subtitle = "Total Deaths Vs. 5 Year Average \n(The 5 year average is based on the actual number of death registrations recorded
for each corresponding month in the previous five years. In calculating the average
for the years 2021 and 2022, the year 2020 has been excluded because 
of the impact of the COVID-19 pandemic on deaths registered in this year)",
       caption = "Source: UK Office for National Statistics. Accessed on: 26th May, 2022") + 
  guides(colour = guide_legend(title="Deaths", override.aes = list(size=10))) +
  scale_color_manual(values = c(five_yr_avg= "green4", tot_dth = "cadetblue2"), 
                     labels = c("5 Year Average", "Total deaths"))

#Save the chart
ggsave("linechart502.jpg", width = 40, height = 30, units = c("cm"), dpi = 300)

#Sort Data for overlapping bar chart
#Graph to compare total Deaths and Deaths with COVID-19
dth_age_grp <- read.csv('dth_age_grp_mar20_jan22.csv')
dth_age_grp <- dth_age_grp %>% select(age_group, all_deaths, all_covid_deaths, percent_covid_deaths)
#Convert character to integer
dth_age_grp$all_covid_deaths <- as.integer(dth_age_grp$all_covid_deaths)
glimpse(dth_age_grp)
#Delete the first row showing sum of deaths
dth_age_grp <- dth_age_grp[-1,]
df2 <- dth_age_grp %>% select(age_group, all_deaths, all_covid_deaths)
df2 <- melt(df2,  id.vars = 'age_group', variable.name = 'series')

#Plot graph
ggplot(df2 %>% arrange(series), 
       aes(x = age_group, y = value, fill = series)) + 
  geom_col(position = "identity") + 
  geom_label(data = df2, aes(label = value, fontface = "bold", alpha = 0.4), 
        position = position_dodge(width = 0.9), stat = "sum", show.legend = FALSE) +
  scale_y_continuous(limits=c(0,6000), breaks=seq(0, 6000, by=500)) +
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5, vjust = 0.5, size = 12), 
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"), 
        axis.title = element_text(face = "bold", size = 15),
        plot.title = element_text(size = rel(2.0), face = "bold", hjust = -0.2, 
        margin = margin(t = 10, b = 20, unit = "pt")), 
        plot.subtitle = element_text(size = rel(1.0), face = "bold", hjust = 0.5), 
        panel.background = element_blank()) + 
  labs(y = "Number of Deaths", x = "Age Groups", 
       title = "Deaths in each Age Group of Care Home residents in Wales (March 2020 - April 2022)",
       subtitle = "Total Deaths Vs. Deaths Involving COVID-19 \n(COVID-19 Deaths include all deaths where COVID-19 was mentioned anywhere on the death certificate, as a main cause of death or a contributory cause.)",
       caption = "Source: UK Office for National Statistics. Accessed on: 26th May, 2022") + 
  guides(fill=guide_legend(title= NULL)) + 
  scale_fill_manual(values = c("cadetblue2","darkolivegreen3"), 
                    labels= c("Total Deaths", "Deaths Involving COVID-19"))

#Save the chart
ggsave("barchart502.jpg", width = 40, height = 30, units = c("cm"), dpi = 300)

#Show percentage of COVID-19 Deaths compared to other deaths
df3 <- dth_age_grp %>% mutate(other_deaths = all_deaths - all_covid_deaths)
df3 <- df3 %>% mutate(percent_other_deaths = (other_deaths / all_deaths) * 100) 
df3$percent_other_deaths <- round(df3$percent_other_deaths, digits = 1)
df3 <- df3 %>% select(age_group, percent_other_deaths, percent_covid_deaths)
df3 <- melt(df3,  id.vars = 'age_group', variable.name = 'series')

#Plot Stacked area chart
library(viridis) #for creating colour palettes for graphs
library(hrbrthemes) #additional themes for graphs
library(ggrepel) #to label only a subset of the data
ggplot(df3, aes(x = age_group, y = value, fill= series, group = series, label = value)) + 
  geom_area(alpha=0.8 , size=0.5, colour="white") + scale_fill_viridis(discrete = T) + 
  geom_label_repel(data = subset(df3, value < 20, size = 4, box.padding   = 0.5,
       point.padding = 0.5, force = 100, segment.size  = 0.2, segment.color = "grey50",
       direction = "x"), show.legend = FALSE) +
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, by=10), labels = function(x) paste0(x, "%")) + 
  theme_ipsum(subtitle_size = 13, axis_title_size = 15, axis_title_just = "cc", grid = FALSE, ticks = TRUE, axis = TRUE) + 
  labs(y = "Percentage of Deaths", x = "Age Groups", 
       title = "Percentage of Deaths in each Age Group of Care Home residents in Wales (March 2020 - April 2022)",
       subtitle = "Percentage of Deaths Involving COVID-19  Vs. Percentage of Other Deaths ",
       caption = "Source: UK Office for National Statistics. Accessed on: 26th May, 2022") +
  guides(fill=guide_legend(title= NULL)) + 
  scale_fill_manual(values = c("cadetblue2","darkolivegreen3"),labels= c("Other Deaths", "Deaths Involving COVID-19"))

#Save the chart
ggsave("areachart502.jpg", width = 40, height = 30, units = c("cm"), dpi = 300)

# -----------------------------------------------------------------------------

