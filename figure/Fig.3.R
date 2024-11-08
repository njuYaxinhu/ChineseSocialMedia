library(tidyverse)
library(data.table)
library(readxl)
library(fuzzyjoin)

rm(list = ls())

setwd("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data/sentiment")
filenames <- list.files(pattern="*.xlsx", full.names=F)
#si_fig_path <- "E:/独立研究/2023年/2023_气候变化微博/Figures/SI_figures"
df_sentiment <- do.call(rbind, lapply(filenames, function(x) cbind(read_xlsx(x), 
                                                              event_group = strsplit(x,'\\.')[[1]][1])))
event_mean_score <- df_sentiment %>%
  group_by(event) %>%
  summarise(mean_sentiment = mean(sentiment,na.rm = T)) %>%
  mutate(bin = cut(mean_sentiment, 
                   breaks = c(40,45,50,55,60,65,70,75,
                              80,85,90,95,100)))
annual_mean_score <- df_sentiment %>%
  group_by(Year,event_group) %>%
  summarise(annual_mean_sentiment = median(sentiment, na.rm = T))

df_sentiment %>%
  left_join(event_mean_score, by = "event") %>% 
  left_join(annual_mean_score, by = c("Year","event_group")) %>%
  mutate(Year = factor(Year)) %>%
  ggplot()+
  annotate("rect", xmin = "2020", xmax = Inf, ymin = 0, ymax = 100, 
           fill="#fee0d2", size=0.5)+
  geom_boxplot(aes(x = Year, y = sentiment, group = event, fill = bin),
             width = 0.4,  position = position_dodge(1), outlier.shape = NA) +
  geom_line(aes(x = Year, y = annual_mean_sentiment, group = event_group),
            color = "#3366FF", linewidth = 0.5)+
  scale_fill_brewer(palette = "Purples", direction = -1,
                    name = "Mean sentiment score")+
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "sentiment score")+
  facet_wrap(~event_group, 
             labeller = as_labeller(c(
               "event1" = "Online Activities",
               "event2" = "International Conferences",
               "event3" = "Extreme Weather",
               "event4" = "Domestic Policies",
               "event5" = "International News")))+
  theme_bw() +
  theme(#panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = c(.8,.15), legend.direction = "horizontal") +
  guides(fill = guide_legend(title.position = "top",
                             ncol=2))



main_fig_path <- "E:/独立研究/2023年/2023_气候变化微博/Figures/Main_figures"
ggsave(filename = file.path(main_fig_path,"Fig4_Sentiment.jpg"),
       dpi = 1000, height = 4, width = 8)


# timeline <- read_excel("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data/TimeLine.xlsx") %>%
#   rename(event_type = tag)



####################################################################################
event <- read_excel("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data/Event_tag.xlsx")
peak_event <- event %>%
  filter(!is.na(event)) %>%
  select(start, end, event_id = event, event_type = tag) %>%
  mutate(start = as.Date(start),
         end   = as.Date(end))

# sentiment_event <- fuzzy_inner_join(df_sentiment, peak_event,
#                                 by = c("发布时间" = "start", "发布时间" = "end"),
#                                 match_fun = list(`>=`, `<=`)
# )
# unique(sentiment_event$event_id) %>%length()

####################################################################################
