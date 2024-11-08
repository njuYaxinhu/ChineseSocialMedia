library(ggplot2)
library(readxl)
library(writexl)
library(tidyverse)
dat<- read_xlsx("C:/Users/ASUS/Desktop/cc.xlsx")

dat$Date <- as.Date(dat1$发布时间)
dat$inf <- dat$帖子点赞数+dat$帖子转发数+dat$帖子评论数
dat <- dat %>%
  group_by(Date) %>%
  sum  marise(count = n(),eng = sum(inf))

dat$eng <- log(dat$eng)
dat$eng[is.infinite(dat$eng)] <- 0
dat$OAM <- dat$count * dat$eng

dat2<- dat %>%
  group_by(id = ceiling(row_number() / 7)) %>%
  summarise(OAM = sum(OAM, na.rm = TRUE),
            count = sum(count),
            eng = sum(eng),
            start = min(Date),
            end = max(Date))

p1 <- ggplot(dat2, aes(x = id, y = OAM)) +
  geom_line(size = 0.6, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "DATE", y = "OAM", title = "Index") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))
p1

ggsave("time1.png", plot = p1,width = 6,height = 3, dpi = 2000)
