library(tidyverse)
library(data.table)
library(readxl)
rm(list = ls())

#### All the users ####

setwd("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data/influencers")
OnlineActivities <- read_xlsx("Nodes_JX_OnlineActivities.xlsx") %>%
  mutate(event = 1)
DomesticPolicy <- read_xlsx("Nodes_JX_DomesticPolicy.xlsx") %>%
  mutate(event = 4)
InternationalNews <- read_xlsx("Nodes_JX_InternationalNews.xlsx") %>%
  mutate(event = 5)
InternationalConferences <- read_xlsx("Nodes_JX_InternationalConferences.xlsx") %>%
  mutate(event = 2)
ExtremeWeather <- read_xlsx("Nodes_JX_ExtremeWeather.xlsx") %>%
  mutate(event = 3)

df_user <- rbind(OnlineActivities, InternationalConferences, ExtremeWeather,
                 DomesticPolicy, InternationalNews) %>%
  rename(subgroup = "Sub Group", maingroup = "Main Group")

distinct_name <- unique(df_user$name)  

#### All the topics ####
setwd("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data/topics")
filenames <- list.files(pattern="event.*.xlsx", full.names=F)
#si_fig_path <- "E:/独立研究/2023年/2023_气候变化微博/Figures/SI_figures"
df_topic <- do.call(rbind, lapply(filenames, function(x) cbind(read_xlsx(x), 
                                                    event_group = strsplit(x,'\\.')[[1]][1])))

df_topic <- df_topic %>%
  mutate(topic = case_when(event_group == "event1" & topic == 1 ~ "Macro-control measures",
                           event_group == "event1" & topic == 2 ~ "Disastrous impacts",
                           event_group == "event1" & topic == 3 ~ "Activity content",
                           event_group == "event1" & topic == 4 ~ "International collaboration",
                           event_group == "event1" & topic == 5 ~ "Micro-mitigation actions",
                           event_group == "event2" & topic == 1 ~ "Weather change adaptation",
                           event_group == "event2" & topic == 2 ~ "Disastrous impacts",
                           event_group == "event2" & topic == 3 ~ "International collaboration",
                           event_group == "event2" & topic == 4 ~ "Conference content",
                           event_group == "event2" & topic == 5 ~ "Energy transition",
                           event_group == "event3" & topic == 1 ~ "Weather change adaptation",
                           event_group == "event3" & topic == 2 ~ "China-US relations",
                           event_group == "event3" & topic == 3 ~ "Macro-control measures",
                           event_group == "event3" & topic == 4 ~ "Causes of extreme Weather"
                           
                           ))
  
df_topic %>%
  filter(发布者的姓名 %in% distinct_name) -> match_user_topic

################################################################################
# Fig.SI (1)

df_topic %>%
  group_by(event_group, topic) %>%
  summarise(count = n()) %>%
  mutate(percent = prop.table(count)) -> topic_event

ggplot(data = topic_event) +
  geom_bar(aes(x = event_group, y = count, 
               fill = topic),
           stat="identity",position="fill") 
  geom_text(aes(x = event_type, y = percent, 
                label = paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5),size = 3) +

match_user_topic%>%
  group_by(event_group, topic) %>%
  summarise(count = n()) %>%
  mutate(percent = prop.table(count))



