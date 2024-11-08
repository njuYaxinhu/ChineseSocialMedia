library(readxl)
library(data.table)
library(stringr)
library(tidyverse)
library(writexl)

'data/词.xlsx'->f1
read_xlsx(f1)->ww
setDT(ww)
ww[,.N,type][order(type)]

'data/用户名.xlsx'->f2
read_xlsx(f2)->pp
setDT(pp)
pp[,.N,`Main Group`]
pp[,.N,`Sub Group`]
pp[,!'id']->pp

'data/微博.xlsx'->f3
read_xlsx(f3)->tt
setDT(tt)

tt[,.(
c1=帖子点赞数,	
c2=帖子转发数,	
c3=帖子评论数,
name=发布者的姓名,
content=帖子内容)]->tt

tt[name %in% pp$name]->tt

split(tt,tt[,name])->fen

lapply(fen,function(x)
{
sum(x[,c1+c2+c3])->influ

lapply(setNames(,ww$名字),function(y)
{
sapply(x$content, function(zz) str_count(zz,y)) |> unname()
}
)->shu

shu<-sapply(shu,sum)
shu<-shu[shu>0]
shu<-rep(names(shu),shu)
dtt<-data.table(name=unique(x$name),group=pp[match(unique(x$name),name),`Sub Group`],word=shu)

lapply(1:influ,function(x) dtt)->ok
ok<-rbindlist(ok)

}
)->resu

resu<-rbindlist(resu)

library(ggalluvial)
library(ggplot2)
library(ggforce)
resu[,.(Freq=.N),.(name,group,word)]->bb
unique(bb[,.(group,name)])[order(group)][,name]->nnn
#bb[,name:=factor(name,nnn)]
bb1 <- read_xlsx("C:/Users/ASUS/Desktop/sangshen1.xlsx")

bb1 <- bb %>%
  group_by(group, word) %>%
  summarise(Freq = sum(Freq))
pp<-ggplot(bb1,
       aes(y = Freq, axis1 = group,axis2=type, axis3=word)) +
  geom_alluvium(aes(fill = group), width = 1/12) +
  geom_stratum(width = 0.2, fill = "pink", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Group", "Account","Word"), expand = c(.05, .05)) +
  scale_y_continuous(trans='log10')+
  scale_fill_brewer(type = "qual", palette = "Set3")+
  theme_minimal()
  
pp
ggsave('out2.png',pp,height=28,width=15,dpi=1000)
getwd()

ggplot(bb1,
       aes(x = group, stratum = type, alluvium = word,
           y = Freq,
           fill = type, label = type)) +
  geom_alluvium(aes(fill = group), width = 1/12, separation = 0.5) +
  geom_stratum(width = 0.2, fill = "pink", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Group", "Account","Word"), expand = c(.05, .05)) +
  scale_y_continuous(trans='log10')+
  scale_fill_brewer(type = "qual", palette = "Set3") +
  theme_minimal()
library(dplyr)

df_summarized <- bb1 %>%
  group_by(group, type) %>%
  summarize(freq_group_type = sum(Freq))

df2_summarized <- bb1 %>%
  group_by(type,word) %>%
  summarize(freq_type_word = sum(Freq))
df_summarized <- df_summarized %>%
  rename(source = group, target = type, freq = freq_group_type)
df2_summarized <- df2_summarized %>%
  rename(source = type, target = word, freq = freq_type_word)
df2_summarized <- df2_summarized %>%
  mutate(source = as.factor(source),
         target = as.factor(target))
newbb <- rbind(df_summarized,df2_summarized)

newbb <- newbb %>%
  rename(source = group, target = word, freq = freq_type_word)

print(df_summarized)