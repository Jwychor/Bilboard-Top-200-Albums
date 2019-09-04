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
url<-'https://www.billboard.com/charts/hot-100'
wp<-read_html(url)

#Rankings
ra<-html_nodes(wp,'.chart-list-item__rank')
rank<-html_text(ra)

#Peak
ra<-html_nodes(wp,'.chart-list-item__weeks-at-one')
peak<-html_text(ra)

#Weeks on the chart
ra<-html_nodes(wp,'.chart-list-item__weeks-on-chart')
weeks<-html_text(ra)

#Title
ra<-html_nodes(wp,'.chart-list-item__title-text')
title<-html_text(ra)

#Artist
ra<-html_nodes(wp,'.chart-list-item__artist')
artist<-html_text(ra)

#Lastweek
ra<-html_nodes(wp,'.chart-list-item__last-week')
lastweek<-html_text(ra)

a<-as.data.frame(matrix(ncol=1,nrow=100))
for(i in 1:200){
  if(i%%2==0){
    a[i,1]<-i
  }
}
a<-a[!is.na(a)]
lastweek<-lastweek[a]
length(lastweek)

lastweek[lastweek=="-"]<-101

m<-data.frame(rank,peak,weeks,title,artist,lastweek)

str(m)

m<-as.data.frame(apply(m,2,function(y) as.character(gsub("\n","",y))))
m<-as.data.frame(apply(m,2,as.character),stringsAsFactors = F)

for(i in 1:ncol(m)){
  if(is.na(as.numeric(m[1,i]))==F)
    m[,i]<-as.numeric(m[,i])
}

m[,7]<-m$lastweek-m$rank
colnames(m)[7]<-'Trend'

p<- plot_ly(m,x=~rank,y=~peak,z=~weeks,color=~Trend,
            text=~paste("Artist: ",artist,'<br>Title: ',title,'<br>Current Rank: ',rank,'<br>Peak Rank: ',peak,
                        '<br>Weeks on the Chart: ',weeks,'<br>1-Week Trend: ',Trend),
            hoverinfo='text',title='Bilboard Top 100 Songs',
            colorscale='viridis') %>%
  add_markers() %>%
  layout(showlegend=F,scene=list(xaxis=list(title='Current Rank',
                                            gridcolor='rgb(255,255,255)'),
                                 yaxis=list(title='Peak Rank',
                                            gridcolor='rgb(230,200,230)'),
                                 zaxis=list(title='Weeks on the Chart',
                                            gridcolor='rgb(230,230,200)',
                                            ticklen = 2,
                                            griwith = 1)))
p
