library(readr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(dplyr)
library(grid)

#Data import and formatting
flu <- melt(read_delim("flu_AB_global_2015.txt",
                        "\t", escape_double = FALSE, trim_ws = TRUE))
colnames(flu) <- c("Country","Week","Cases")
flu$Week <- as.integer(sub("2015 week: ", "", flu$Week))

#We're going to focus on 10 countries for simplicity's sake
countries=c("Canada","United States of America","Germany","Turkey","South Africa","Bangaldesh","New Zealand","Venezuela","Jordan","China")
flu_subset <- flu[flu$Country %in% countries, ]

#here's a bad idea
ggplot(flu_subset) +
  geom_line(aes(Week,Cases,color=Country))

#sparklines
#Find max/min/endpoint/quartiles
mins <- group_by(flu_subset, Country) %>% slice(which.min(Cases))
maxes <- group_by(flu_subset, Country) %>% slice(which.max(Cases))
#labeling the endpoint of the line can be useful, but not so relevant here
#ends <- group_by(flu_subset, Country) %>% filter(Week == max(Week))
quarts <- flu_subset %>% group_by(Country) %>%
  summarize(quart1 = quantile(Cases, 0.25,na.rm = TRUE),
            quart2 = quantile(Cases, 0.75,na.rm = TRUE)) %>% right_join(flu_subset)


#sparklines
tiny_flu <- ggplot(flu_subset,aes(Week,Cases))+
  scale_color_discrete(guide=FALSE) +
  geom_ribbon(data = quarts, aes(Week,ymin = quart1, max = quart2), fill = 'grey90') +
  geom_line(aes(color=Country)) +
  facet_grid(Country ~ .,scales="free_y",labeller = label_wrap_gen()) +
  geom_point(data = mins, color = 'red') +
  geom_point(data = maxes, color = 'blue') +
  geom_text(data = mins, aes(label = Cases),size=3,vjust=-1) +
  geom_text(data = maxes, aes(x=Week+5.5,y=0.85*Cases, label = Cases),size=3) +
  theme_tufte() +
  xlab("Week of 2015") +
  theme(plot.background = element_blank(),axis.title.y=element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(),strip.background = element_rect(fill=NA,color = NA),
        strip.text.y = element_text(angle = 360,hjust = 0,vjust=0.25,size=12))

#mostly solves clipping issues
gt <- ggplot_gtable(ggplot_build(tiny_flu))
gt$layout$clip[grep("panel",gt$layout$name)] <- "off"
grid.draw(gt)

#Chinese weather data
chinese_weather <- read_delim("~/UCI/tufte/chinese_weather.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
chinese_weather$City <- factor(chinese_weather$City, levels=c("China","Beijing", "Shanghai", "Guangzhou"))
better_names=c("Rain"="Precip. (mm)","Temp"="Temp. (C)","Flu"="Flu","China"="China","Beijing"="Beijing","Shanghai"="Shanghai","Guangzhou"="Guangzhou")
wmins <- group_by(chinese_weather, .dots=c("Type","City")) %>% slice(which.min(Value))
wmaxes <- group_by(chinese_weather, .dots=c("Type","City")) %>% slice(which.max(Value))


cw <- ggplot(data=chinese_weather,aes(as.factor(Month),Value)) +
  geom_line(aes(group=Type,linetype=Type)) +
  facet_grid(Type+City ~ .,scales="free_y",labeller = as_labeller(better_names)) +
  geom_point(data = wmins, color = 'red') +
  geom_point(data = wmaxes, color = 'blue') +
  geom_text(data = wmins, aes(label = round(Value,0)),size=3,vjust=-1) +
  geom_text(data = wmaxes, aes(x=Month+0.5,y=1.2*Value, label = round(Value,0)),size=3) +
  theme_tufte() +
  scale_linetype_discrete(guide=FALSE) +
  xlab("Month") +
  theme(plot.background = element_blank(),axis.title.y=element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(),strip.background = element_rect(fill=NA,color = NA),
        strip.text.y = element_text(angle = 360,hjust = 0,vjust=0.25,size=12))

gt2 <- ggplot_gtable(ggplot_build(cw))
gt2$layout$clip[grep("panel",gt2$layout$name)] <- "off"
grid.draw(gt2)



