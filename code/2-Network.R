model_Gephi.2.graph<-function (igraph,method = "cluster_fast_greedy", seed = 2) 
{
	nn<-igraph::as_data_frame(igraph,'vertices')
		
    if (method == "cluster_walktrap") {
        fc <- cluster_walktrap(igraph, weights = abs(E(igraph)$weight))
    }
    if (method == "cluster_edge_betweenness") {
        fc <- cluster_edge_betweenness(igraph, weights = abs(E(igraph)$weight))
    }
    if (method == "cluster_fast_greedy") {
        fc <- cluster_fast_greedy(igraph, weights = abs(E(igraph)$weight))
    }
    if (method == "cluster_spinglass") {
        fc <- cluster_spinglass(igraph, weights = abs(E(igraph)$weight))
    }
    modularity <- modularity(igraph, membership(fc))
    modularity
    netClu = data.frame(ID = names(membership(fc)), group = as.vector(membership(fc)))
    igraph.degree <- igraph::degree(igraph) %>% as.data.frame()
    colnames(igraph.degree) = "degree"
    igraph.degree$ID = row.names(igraph.degree)
    netClu <- netClu %>% dplyr::left_join(igraph.degree, na_matches = "never")
    netClu$degree[is.na(netClu$degree)] = 0
    netClu <- netClu %>% dplyr::arrange(desc(degree))
    sumtav <- netClu %>% dplyr::group_by(group) %>% dplyr::summarise(sum(degree))
    colnames(sumtav) = c("group", "degree")
    tab0 <- sumtav %>% dplyr::arrange(desc(degree))
    tab0$group = as.character(tab0$group)
    tab1 = as.data.frame(table(netClu$group)) %>% dplyr::arrange(desc(Freq))
    colnames(tab1)[1] = "group"
    tab1$group = as.character(tab1$group)
    tab3 <- tab0 %>% dplyr::left_join(tab1, by = "group")
    num.node <- nrow(nn)
    for (N in 1:num.node) {
        A = 1 + (7 * (N + 1) * N)/2 - N
        if (A >= num.node) {
            break
        }
        n = N - 1
        print(n)
    }
    wai.mode = num.node - (1 + (7 * (n + 1) * n)/2 - n)
    dat = data.frame(x = 0, y = 0)
    i = 1
    for (i in 1:n) {
        t <- seq(0, 2 * pi, length.out = 7 * i)
        t = t[-1]
        x <- sin(t) * i
        y <- cos(t) * i
        add = data.frame(x = x, y = y)
        dat = rbind(dat, add)
        if (i == n) {
            i = i + 1
            t <- seq(0, 2 * pi, length.out = (wai.mode + 1))
            t = t[-1]
            x <- sin(t) * i
            y <- cos(t) * i
            add = data.frame(x = x, y = y)
            dat = rbind(dat, add)
        }
    }
    row.names(dat) = nn$name
    dat$elements = nn$name
    colnames(dat)[1:2] = c("X1", "X2")
    dat0 = dat
    axis.node = c()
    for (j in 1:dim(tab3)[1]) {
        if (dim(dat)[1] <= 2 | tab3$Freq[j] == tab3$Freq[dim(tab3)[1]]) {
            lacat = row.names(dat)[1:tab3$Freq[j]]
        }
        if (dim(dat)[1] > 2 & tab3$Freq[j] != tab3$Freq[dim(tab3)[1]]) {
            set.seed(seed)
            axis_mod = sample(1:dim(dat)[1], 100, replace = TRUE) %>% 
                sort()
            d <- dist(dat[, -3]) %>% as.matrix()
            id = dat[axis_mod[1], ]$elements
            id2 = d[id, ] %>% sort()
            lacat = c(names(id2[1:(tab3$Freq[j])]))
        }
        new.dat = dat[lacat, ]
        New.locat = netClu$ID[netClu$group == as.numeric(tab3$group[j])]
        row.names(new.dat) = New.locat
        new.dat$elements = New.locat
        if (j == 1) {
            axis.node = new.dat
        }
        if (j != 1) {
            axis.node = rbind(axis.node, new.dat)
        }
        dat = dat[dat$elements %in% lacat == FALSE, ]
    }
    return(list(axis.node, dat0, netClu))
}
#<bytecode: 0x00000116a2d02a08>
#<environment: namespace:ggClusterNet>
environment(model_Gephi.2.graph)<-getNamespace('igraph')
library(readxl)
library(data.table)
library(quanteda)
library(stringr)
library(showtext)
showtext_auto()
font_add('kai','simkai.ttf')

info<-read_xlsx('demo1.xlsx')
setDT(info)

c('发布者的姓名',
'帖子内容',
'帖子点赞数',
'帖子转发数',
'帖子评论数')->colss

info<-info[,..colss]

names(info)<-c('user','content','dianzan','zhuanfa','pinglun')


info[,total:=dianzan+zhuanfa+pinglun]
info<-info[,.(user,total,content)]
dat<-info[,.(content=paste(content,collapse=' '),total=sum(total)),.(user)
dat

doc<-setNames(dat[,content],dat[,user])

lapply(doc,function(x)
{
gsub('[A-Za-z0-9|[:punct:]]|　','',x)->x
gsub('\\s+','',x)
})->doc

library(jiebaR)
library(quanteda.textstats)

engine<-worker(stop_word = '百度停用词表.txt') 
vecs<-lapply(doc,function(x) segment(x,engine))
vecs<-lapply(vecs,function(x) x[nchar(x)>1])
vecs<-vecs[sapply(vecs,length)>=30]
my<-tokens(vecs)
mydfm<-dfm(my)
topfeatures(mydfm, 20)

textstat_simil(mydfm,margin='document',method='cosine')->sim
sim.mat<-as.matrix(sim)
isSymmetric(sim.mat)
sim.mat[upper.tri(sim.mat,T)]<-NA
ee<-reshape2::melt(sim.mat)
setDT(ee)
ee<-ee[!is.na(value)]
ee<-ee[value>=0.7]

names(ee)<-c('user1','user2','similarity')
ee[,user1:=as.character(user1)]
ee[,user2:=as.character(user2)]

ee[,as.character(unique(c(user1,user2)))]->ns
dat[,.(user,total)][ns,on=.(user)]->nn
ee[,weight:=similarity]
nn[,userLAB:=user]


library(igraph)
library(tidygraph)
library(ggraph)

source('aaa.R')
graph_from_data_frame(ee,F,vertices=nn)->gg
model_Gephi.2.graph(gg)->lay
lay[[1]]->lay
lay[nn$user,]->lay
lay<-as.matrix(lay[,c('X1','X2')])


tbl_graph(nodes = nn, edges = ee, directed = F)->gg


ggraph(gg,layout=lay)+
        geom_edge_link(aes(color=similarity,width=similarity))+
        geom_node_point(size=25,shape=21,colour='transparent',fill='#0C090A')+
        labs(size='Influence',colour='Similarity')+
        coord_equal()+
        theme_void()+
        guides(fill=guide_legend(override.aes=list(size=3)))+
        geom_node_text(aes(label=userLAB),size=3,colour='white',family='kai')+
    	scale_edge_width_continuous(range=c(0.1,1.2),limits=c(0,1))+
    	#scale_size_continuous(range=c(5,25))+
		theme(plot.title=element_text(hjust=0.5,size=rel(1.5)))+
		#scale_edge_colour_distiller(palette='YlOrRd',direction = 1)
		scale_edge_colour_distiller(palette='Blues',direction = 1)
		
ggsave('out.svg',width=12,height=12,dpi=1000)
