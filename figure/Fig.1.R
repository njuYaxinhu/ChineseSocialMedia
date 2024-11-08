library(tidyverse)
library(data.table)
library(readxl)
library(ggrepel)

#### Fig.1A Time series ####
main_fig_path <- "E:/独立研究/2023年/2023_气候变化微博/Figures/Main_figures"

setwd("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data")
df <- read_excel("Event_tag_Fig1.xlsx")
ggplot(data = df)+
  geom_line(aes(x = date, y = oam), linewidth = 1)+
  geom_smooth(aes(x = date, y = oam),color = "#b2182b",se = F,linewidth = 1)+
  geom_text_repel(
                  aes(x = date, y = oam, label = event),
                  #box.padding = 0.6, 
                  force = 1,
                  vjust= -3,
                  #arrow = arrow(length = unit(0.015, "npc")),
                  segment.curvature = 0.2,
                  point.padding = 0.2,
                  size = 5
                    ) +
  scale_y_continuous(name = "weekly climate change Weibo OAM")+
  scale_x_datetime(date_breaks = "1 year",# date_minor_breaks = "3 month",
               date_labels = "%Y", name = NULL)+
  theme_bw()+
  theme(#panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0"),
    axis.text = element_text(size = 12, color = "black", face = "bold"),
    #axis.text.x = element_text(vjust = 1, hjust=1),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)) 

ggsave(filename = file.path(main_fig_path,"Fig1_TimeSeries.jpg"),
       dpi = 1000, height = 6, width = 12)

#### Fig.2A Time series ####
timeline <- read_excel("TimeLine.xlsx") %>%
  rename(event_type = tag) %>%
  mutate(event_name = paste0("#",event," ",NAME))

ggplot(data = timeline, 
       aes(x = date, y = factor(event_type,levels = c("5","4","3","2","1")))) +
  geom_line()+
  geom_point(size=2, color = "#b2182b") +
  geom_point(size=4, color = "#b2182b", alpha=.5) +
  geom_label_repel(aes(label = stringr::str_wrap(event_name, 20), 
                       fill = factor(event_type)),
                   size = 4,
                   force = 3,
                   max.overlaps = Inf,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.75, "lines"))+
  scale_fill_manual(values = c("1" = "#66c2a5", "2" = "#fc8d62", "3" = "#8da0cb",
                               "4" = "#e78ac3", "5" = "#a6d854"))+
  scale_y_discrete(labels = c(
    "1" = "Online\nActivities",
    "2" = "International\nConferences",
    "3" = "Extreme\nWeather",
    "4" = "Domestic\nPolicies",
    "5" = "International\nNews"), name = "")+
  scale_x_datetime(name = "")+
  theme_bw()+
  theme(#panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    #strip.background = element_rect(fill = "#f0f0f0"),
    axis.text = element_text(size = 12, color = "black", face = "bold"),
    #axis.text.x = element_text(vjust = 1, hjust=1),
    axis.title = element_text(size = 15),
    legend.position="none") +
  guides(fill = guide_legend(override.aes = aes(label = "")))

ggsave(filename = file.path(main_fig_path,"Fig1_TimeLines.jpg"),
       dpi = 1000, height = 10, width = 12)

