library(ggalluvial)
library(ggplot2)
library(tidyverse)

getwd()
wf11 <- read_xlsx('C:/Users/ASUS/Desktop/wf11.xlsx')
wf22 <- read_xlsx('C:/Users/ASUS/Desktop/wf22.xlsx')
wf33 <- read_xlsx('C:/Users/ASUS/Desktop/wf33.xlsx')
wf44 <- read_xlsx('C:/Users/ASUS/Desktop/wf44.xlsx')
wf55 <- read_xlsx('C:/Users/ASUS/Desktop/wf55.xlsx')

translation <- data.frame(

  frameEn = c("Personal action", "International diplomacy", "Economic and socia", "Scientific research","Climate measures","National strategy","China-US diplomacy")  
)

# 
wf55$alluvium <- translation$frameEn[match(wf44$frame, translation$frame)]



pp5 <- ggplot(wf55,
              aes(y = value, axis1 = stratum, axis2 = alluvium)) +
  geom_alluvium(aes(fill = stratum), width = 1/12,) +
  geom_stratum(width = 1/12, aes(fill = stratum), color = "black") +
  geom_label(stat = "stratum", size = 10, aes(label = str_wrap(after_stat(stratum),12))) +
  scale_x_discrete(limits = c("Type", "Frame"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Pastel1")+
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) 
pp5
ggsave('tu5.png',pp5,bg = "white",height=20,width=20,dpi=1000)