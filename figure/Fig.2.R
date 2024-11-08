library(tidyverse)
library(data.table)
library(readxl)

#### 1. Proportion by event ####
# READ DATA FOR PREPROCESSING
setwd("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data/influencers")
OnlineActivities <- read_xlsx("Nodes_JX_OnlineActivities.xlsx") %>%
  mutate(event = "OnlineActivities")
DomesticPolicy <- read_xlsx("Nodes_JX_DomesticPolicy.xlsx") %>%
  mutate(event = "DomesticPolicy")
InternationalNews <- read_xlsx("Nodes_JX_InternationalNews.xlsx") %>%
  mutate(event = "InternationalNews")
InternationalConferences <- read_xlsx("Nodes_JX_InternationalConferences.xlsx") %>%
  mutate(event = "InternationalConferences")
ExtremeWeather <- read_xlsx("Nodes_JX_ExtremeWeather.xlsx") %>%
  mutate(event = "ExtremeWeather")

df_user <- rbind(OnlineActivities, InternationalConferences, ExtremeWeather,
            DomesticPolicy, InternationalNews) %>%
  rename(subgroup = "Sub Group", maingroup = "Main Group")

df_user %>%
  group_by(subgroup, maingroup, event) %>%
  summarise(totalnumber = n()) %>%
  mutate(subgroup = factor(subgroup, levels = c("Central official media",                                                 "Local media",
                                       "Unofficial media", "Universities",
                                       "Government agencies of China", 
                                       "International institutions", "Personal & business accounts",
                                       "Enterprises")),
         event = factor(event, levels = c("OnlineActivities", "InternationalConferences",
                                          "ExtremeWeather", "DomesticPolicy",
                                          "InternationalNews"))) -> sum_event
  
ggplot()+
  geom_bar(data = sum_event,
           aes(x = event, y = totalnumber, fill = subgroup),
           stat="identity", position="fill") +
  scale_y_continuous(labels = scales::percent,
                     name = "Proportion") +
  scale_x_discrete(labels = c(
    "OnlineActivities" = "Online Activities",
    "InternationalConferences" = "International Conferences",
    "ExtremeWeather" = "Extreme Weather",
    "DomesticPolicy" = "Domestic Policies",
    "InternationalNews" = "International News"), name = NULL) +
   scale_fill_manual(name = "Types", 
                     values = c("Central official media" = "#a50f15",
                                "Local media" = "#fb6a4a",
                                "Unofficial media" = "#fee5d9",
                                "Universities" = "#08519c",
                                "Government agencies of China" = "#6baed6",
                                "International institutions" = "#deebf7",
                                "Personal & business accounts" = "#74c476"))+
  
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))

si_fig_path <- "E:/独立研究/2023年/2023_气候变化微博/Figures/SI_figures"
ggsave(filename = file.path(si_fig_path,"SI_Fig_InfluncersbyTopic.jpg"),
       dpi = 600, height = 4, width = 5)




#### 2. Connections by event ####

setwd("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data/edges")
filenames <- list.files(pattern="*.xlsx", full.names=F)
#si_fig_path <- "E:/独立研究/2023年/2023_气候变化微博/Figures/SI_figures"
df_edge <- do.call(rbind, lapply(filenames, function(x) cbind(read_xlsx(x), 
                                                         event_group = strsplit(x,'\\.')[[1]][1])))

df_user %>%
  distinct(name,.keep_all = TRUE) %>%
  select(-id, -event) -> distinct_user

df_edge %>%
  left_join(distinct_user, by = c("Source" = "name")) %>%
  rename(source_subgroup = subgroup, 
         source_maingroup = maingroup) %>%
  left_join(distinct_user, by = c("Target" = "name")) %>%
  rename(target_subgroup = subgroup, 
         target_maingroup = maingroup) %>% 
  filter(!is.na(target_subgroup)) %>%
  mutate(ifsame_source_target = ifelse(target_subgroup == source_subgroup, 1, 0)) -> edge_event
  
edge_event %>%
  group_by(event_group) %>%
  summarise(n_edge = n(),
            mean_weight = mean(weight),
            sd_weight = sd(weight),
            same_source_target = sum(ifsame_source_target,na.rm = T)/n_edge,
            unique_source = length(unique(Source)),
            unique_target = length(unique(Target))) -> node_event

##### Fig.A #####
ggplot()+
  geom_bar(data = node_event, 
           aes(x = event_group, y = n_edge, fill = event_group),
           color = "gray50", stat = "identity")+
  scale_x_discrete(labels = c(
    "edge_1" = "Online Activities",
    "edge_2" = "International Conferences",
    "edge_3" = "Extreme Weather",
    "edge_4" = "Domestic Policies",
    "edge_5" = "International News"), 
    name = "Event group") +
  scale_y_continuous(name = "Total edges for the selected event") +
  scale_fill_manual(name = "Types", 
                    values = c("edge_1" = "#f0f0f0",
                               "edge_2" = "#f0f0f0",
                               "edge_3" = "#f0f0f0",
                               "edge_4" = "#f0f0f0",
                               "edge_5" = "#f0f0f0"))+
  
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 10),
        legend.position = "none")

ggsave(filename = file.path(si_fig_path,"SI_Fig_TotalEdgesbyTopic.jpg"),
       dpi = 600, height = 4, width = 4)

##### Fig.B #####
ggplot(data = node_event)+
  geom_bar(aes(x = event_group, y = mean_weight, fill = event_group), stat = "identity")+
  geom_errorbar(aes(x = event_group, 
                    ymax = mean_weight + sd_weight, 
                    ymin = mean_weight - sd_weight), width = 0.5)+
  scale_x_discrete(labels = c(
    "edge_1" = "Online Activities",
    "edge_2" = "International Conferences",
    "edge_3" = "Extreme Weather",
    "edge_4" = "Domestic Policies",
    "edge_5" = "International News"), 
    name = "Event group") +
  scale_fill_manual(name = "Types", 
                    values = c("edge_1" = "#f0f0f0",
                               "edge_2" = "#f0f0f0",
                               "edge_3" = "#f0f0f0",
                               "edge_4" = "#f0f0f0",
                               "edge_5" = "#f0f0f0"))+
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 10),
        legend.position = "none")+
  scale_y_continuous(name = "Mean text similarity between edges")

ggsave(filename = file.path(si_fig_path,"SI_Fig_WeightsandErrorbars.jpg"),
       dpi = 600, height = 4, width = 4)


##### Fig.C #####
ggplot(data = node_event)+
  geom_bar(aes(x = event_group, y = same_source_target, fill = event_group),
           color = "gray50", stat = "identity")+
  scale_x_discrete(labels = c(
    "edge_1" = "Online Activities",
    "edge_2" = "International Conferences",
    "edge_3" = "Extreme Weather",
    "edge_4" = "Domestic Policies",
    "edge_5" = "International News"), 
    name = "Event group") +
  scale_fill_manual(name = "Types", 
                    values = c("edge_1" = "#f0f0f0",
                               "edge_2" = "#f0f0f0",
                               "edge_3" = "#f0f0f0",
                               "edge_4" = "#f0f0f0",
                               "edge_5" = "#f0f0f0"))+
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title = element_text(size = 10),
        legend.position = "none")+
  scale_y_continuous(name = "The proportion of edges with the\nsame type of source and target")

ggsave(filename = file.path(si_fig_path,"SI_Rateof_Same_Source&Target.jpg"),
       dpi = 600, height = 4, width = 4)
