library(ggalluvial)
library(tidyverse)

setwd("E:/独立研究/2023年/2023_气候变化微博/data_analysis/data/sangshen")

si_fig_path <- "E:/独立研究/2023年/2023_气候变化微博/Figures/SI_figures"

dat <- readxl::read_excel("link(1).xlsx")

ggplot(dat,
       aes(y = weight, axis1 = Type, axis2 = Framework)) +
  geom_alluvium(aes(fill = Type), width = 1/6) +
  geom_stratum(width = 1/6, fill = "lightblue", color = "black") +
  #geom_text(stat = "stratum") +
  scale_x_discrete(limits = c("Type", "Framework"), expand = c(.08, .08)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"))

ggsave('outall.png',pp6,height=14,width=14,dpi=1500)
