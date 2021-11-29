#library
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(maps)
library(mapproj)
library(patchwork)
library(usdata)


#Summarise Chart
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
race_jail_pop <- incarceration_trends %>%
group_by(year) %>%
summarise(total_aapi = sum(aapi_jail_pop, na.rm = TRUE), 
          total_black = sum(black_jail_pop, na.rm = TRUE), 
          total_white = sum(white_jail_pop, na.rm = TRUE),
          total_native = sum(native_jail_pop, na.rm = TRUE),
          total_other = sum(other_race_jail_pop, na.rm = TRUE),
          total_latinx = sum(latinx_jail_pop, na.rm = TRUE)) %>%
gather(race, population, -year)


options(scipen = 999)

# Plot
line_chart <- race_jail_pop %>% ggplot(aes(x=year, y=population, group=race, color=race)) +
  geom_line() +
  ggtitle("Population of Races in Jail by Year") +
  xlab("Years")+
  ylab("Population")+
  scale_color_discrete(
    limits = c("total_aapi", "total_black", "total_latinx", "total_native", "total_other", "total_white"),
    labels = c("Total AAPI population in jail", "Total Black population in jail", "Total Latinx population in jail", "Total Native population in jail", "Total other race population in jail", "Total White population in jail"))



#Variable Comparison
comparison_points <- incarceration_trends %>%
group_by(year) %>%
summarise(total_black = sum(black_jail_pop, na.rm = TRUE), 
          total_white = sum(white_jail_pop, na.rm = TRUE)) %>%
           gather(race, population, -year)

scatter_plot <- comparison_points %>% ggplot(aes(x = year, y=population, group=race, color=race)) +
  geom_point() +
  ggtitle("Population of White vs Black people in Jail by Year") +
  xlab("Years")+
  ylab("Population")+
  scale_color_discrete(
      labels = c("The total Black population in jail", "The total White population in jail"))

#Incarceration Map
incarceration_map <- incarceration_trends %>%
  mutate(state = (abbr2state(state))) %>%
  mutate(state = tolower(state))%>%
  group_by(state) %>%
 filter(year == 2018) %>% 
  summarize(total_jail_black = sum(black_jail_pop, na.rm = TRUE)) %>%
  select(state, total_jail_black)
 


state_shapes <- map_data("state")%>% 
  rename(state = region)%>% 
 left_join(incarceration_map, by="state")  


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),    
    axis.ticks = element_blank(),
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()   
  )

total_black_pop_map <- ggplot(state_shapes) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_black),
    color = "gray", 
    size = .1        
  ) +
 coord_map() + 
 scale_fill_continuous (limits = c(0, max(incarceration_map$total_jail_black)), low = "#132B43", high = "Red") +
  ggtitle("Sum of Total Black Population in Jail (2018)") +
 labs(fill = "Black population in Jail per State") +
 blank_theme 



#Summary of Information

#What was the ratio between the black vs white population in 2012?
black_pop_2012 <- average_race_pop %>%
  filter(year == "2012") %>%
  filter(race == "total_black") %>%
  pull(population)

white_pop_2012 <- average_race_pop %>%
  filter(year == "2012") %>%
  filter(race == "total_white") %>%
  pull(population)


ratio_in_2017 <- (black_pop_2017 / white_pop_2017)

#What was the race by Black population and White Population
highest_total_race <- race_jail_pop %>%
  filter(population == max(population)) %>%
  pull(race)


Year_of_highest_black_pop <- comparison_points %>%
  filter(race == "total_black") %>%
   filter(population == max(population)) %>%
  pull(year)

# What was the highest average for the black and white population?

average_race_pop <- incarceration_trends %>%
  group_by(year) %>%
  summarise(total_aapi = mean(aapi_jail_pop, na.rm = TRUE), 
            total_black = mean(black_jail_pop, na.rm = TRUE), 
            total_white = mean(white_jail_pop, na.rm = TRUE),
            total_native = mean(native_jail_pop, na.rm = TRUE),
            total_other = mean(other_race_jail_pop, na.rm = TRUE),
            total_latinx = mean(latinx_jail_pop, na.rm = TRUE)) %>%
  gather(race, population, -year)

highest_avg_white_pop <- average_race_pop %>%
  filter(race == "total_white") %>%
  filter(population == max(population)) %>%
  pull(population, year)

highest_avg_black_pop <- average_race_pop %>%
  filter(race == "total_black") %>%
  filter(population == max(population)) %>%
  pull(population, year)






 
