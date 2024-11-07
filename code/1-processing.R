library(tidyverse)
library(quanteda)
library(jiebaR)
library(quanteda.textplots)
library(quanteda.textstats)
library(pheatmap)
library(topicmodels)
library(furrr)
library(LDAvis)
library(slam)
library(readxl)
library(dplyr)
library(stringr)
dat=read_xlsx("C:/Users/ASUS/Desktop/cc.xlsx")

keywords <- c("气候变化")
pattern <- paste(keywords,collapse = "|")
dat <- dat %>% 
  mutate(key=str_detect(dat$帖子内容,pattern)) %>% 
  filter(key==TRUE) %>% 
  select(-key)
dat <- dat[!duplicated(dat[, c("帖子内容", "发布时间","发布者的姓名","帖子点赞数","帖子评论数")]), ]

setwd('C:/Users/hyx/Desktop/1')
dat = read_xlsx('C:/Users/hyx/Desktop/1/event.xlsx')
dat = dat %>%
  mutate(text = str_replace_all(帖子内容, "(?<=[^。])\n", ""),
         year = 发布时间)
corp = corpus(dat, text_field = "text")
head(corp, 2)
wk = worker(stop_word = '百度停用词表.txt')
toks = map(dat$text, ~ segment(.x, wk)) %>%
  set_names(dat$year) %>%
  tokens()
zh_stop = stopwords("zh", source = "misc") 
toks = tokens_remove(toks, zh_stop)
toks <- tokens_select(toks, pattern = "[\u4e00-\u9fff]", selection = "keep", valuetype = "regex")
toks <- tokens_remove(toks, pattern = "[A-Za-z0-9]", valuetype = "regex")
map_int(toks, length) |> `names<-`(NULL)
map_int(toks, length) |> `names<-`(NULL)


doc.dfm = dfm(toks) %>%
  dfm_trim(min_docfreq = 2)
ndoc(doc.dfm)
nfeat(doc.dfm)
featnames(doc.dfm) %>% head(10)
topfeatures(doc.dfm, 20)                             
fcmat = fcm(toks, context = "window", window = 5)
feat = names(topfeatures(fcmat, 50))
fcmat = fcm_select(fcmat, pattern = feat)
