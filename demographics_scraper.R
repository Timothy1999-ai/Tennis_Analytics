## Timothy Stubblefield

## Acquire demographic data on tennis players

library(tidyverse)
library(stringr)
library(plotly)
library(scales)

demographics <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv")


## Perform some early visualizations just to practice

country_plot <- demographics %>% 
  count(ioc) %>%
  mutate(Prop = n/sum(n),
         Percentage = round(prop*100,2)) %>%
  ggplot(aes(x = ioc, y = percentage, fill = ioc)) + 
  geom_bar(stat = "identity") +
  coord_cartesian(clip = "off") +
 # scale_y_continuous(labels = percent_format()) +
  #theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1),
  #      axis.title = element_blank()
  #) +
  ggtitle("Country breakdown of Players via Country affiliation") +
  labs(x = "Country", y = "Percentage")

ggplotly(country_plot)
  

country_plot <- demographics %>%
  count(ioc) %>%
  mutate(prop = n/sum(n)) %>%
  plot_ly(x = "ioc", y = "prop", type = "bar")
