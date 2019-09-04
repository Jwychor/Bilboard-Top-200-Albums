###Packages####
require('dplyr')
require("reshape2")
require('tidyr')
require('ggplot2')
require('rvest')
require('ggrepel')
require('plotly')
###Importing/Cleaning####
##Website
url<-'https://www.billboard.com/charts/billboard-200'
wp<-read_html(url)

#Rankings
ra<-html_nodes(wp,'.chart-list-item__rank')
rank<-html_text(ra)

#Title
t<-html_nodes(wp,'.chart-list-item__title-text')
title<-html_text(t)

#Artist
a<-html_nodes(wp,'.chart-list-item__artist')
artist<-html_text(a)

#Last Weeks Rank
lw<-html_nodes(wp,'.chart-list-item__last-week')
lastweek<-html_text(lw)

#Peak position
pp<-html_nodes(wp,'.chart-list-item__weeks-at-one')
peak<-html_text(pp)

#Weeks on chart
wc<-html_nodes(wp,'.chart-list-item__weeks-on-chart')
weeks<-html_text(wc)
length(weeks)

#Cleanup
rm(ra,lw,pp,wc,a,t)

#Combine them together
m<-data.frame(rank,lastweek,peak,artist,title,weeks)

m<-as.data.frame(apply(m,2,function(y) as.character(gsub("\n","",y))))
m<-as.data.frame(apply(m,2,as.character),stringsAsFactors = F)

for(i in 1:ncol(m)){
  if(is.na(as.numeric(m[1,i]))==F)
    m[,i]<-as.numeric(m[,i])
}

m<-m[1:200,]

###Graphing####

gg<-ggplot(m,aes(x=rank,y=1/peak,size=weeks))
gg+geom_point()+geom_text_repel(aes(label=ifelse(weeks>141,paste(title,"-","Years:",round(weeks/52,2),"Peak:",peak,sep=' '),"")),vjust=-.9,size=3.6)

p<- plot_ly(m,x=~rank,y=~peak,z=weeks,color=~factor(weeks),
            text=~paste("Artist: ",artist,'<br>Title: ',title,'<br>Current Rank: ',rank,'<br>Peak Rank: ',peak,
                        '<br>Weeks on the Chart: ',weeks)) %>%
  add_markers() %>%
  layout(showlegend=F,scene=list(xaxis=list(title='Current Rank',
                               gridcolor='rgb(255,255,255)'),
                    yaxis=list(title='Peak Rank',
                               gridcolor='rgb(230,200,230)'),
                    zaxis=list(title='Weeks on the Chart',
                               gridcolor='rgb(230,230,200)',
                               ticklen = 2,
                               griwith = 1,
                               type = 'log')))
p

###Only works with a plotly account####

#chart_link = api_create(p, filename="jwychor-Top 200 Albums 8/30/2019")
#chart_link
