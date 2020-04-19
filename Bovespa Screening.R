##Loading Libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(quantmod)
library(xts)
library(ggpubr)
library(zoo)
library(alphavantager)
library(quantmod)

#Directory: use this directly within the same directory as the Assets.csv file.
Assets<-read.csv('Assets.csv',sep=';',encoding = 'UTF-8',header = TRUE,stringsAsFactors = FALSE)[,1]

#Getting Data: now, we are going to request for chunks of 5 assets per time in Yahoo Finance server.
Initial<-1
Final<-Initial+4

while(Final<length(Assets)){

getSymbols(Assets[Initial:Final],src='yahoo',output.size="full",from='2018-01-01',to=Sys.Date())

Initial<<-Final+1
Final<<-ifelse(Initial+4<length(Assets),Initial+4,length(Assets))
  print(paste0("Sleeeping. Next iteration is from ",Initial,' to ',Final))
  }

#Closing Prices and Return Data: use this wrapper to verify whether there is some Asset with less observations.
#As we are going to use a data frame, we must keep assets with the same number of observartions. However,
# one can modify this code and simply treat each asset as a component of a list.

Filter<-which(Assets %in% ls())

Exclude<<-vector()

for (i in Assets[Filter]){
  assign(i,na.locf(get(i)))
  assign(i,window(get(i),start='2019-01-01',end=Sys.Date()))
  
  if(nrow(get(i))<nrow(ABEV3.SA)){
    print(paste0(i,' must be excluded.'))
    Exclude<<-rbind(Exclude,as.character(i))
  }
}



# List of assets to be excluded given financial information was not available to calculate the Technical Indicators: use this
# only if the above wrapper has indicated some discrepancies between the number of observations of some stocks.

#rm(KLBN11.SA,NTCO3.SA,YDUQ3.SA,BPAC11.SA,ALSO3.SA,ALUP11.SA,CEAB3.SA,FJTA3.SA,CNTO3.SA)

#Now, we are going to collapse the close prices into a single dataframe and calculate technical indicators.
ClosePrices <- do.call(merge, lapply(Assets[Filter], function(x) Cl(get(x))))
ClosePrices<-na.locf(ClosePrices)

#Create separate dataframes to accomodate technical indicator calculations for each stock
Bands <- lapply(Assets[Filter], function(x) merge(BBands(Cl(get(x)))[,c(1,3)]))
RSI <- lapply(Assets[Filter], function(x) merge(RSI(Cl(get(x)))[,1]))

#Rules: now, we define the thresholds for the parameters that we are going to use.

#Specifically, we are going to define:

#1: Bollinger Bands: must cross the Bollinger Bands calculated for the period.
#2: Relative Strength Index: must be above 65 or below 35

# Each condition will count as an index: 1(-1) if the conditions is achieved and zero otherwise. Finally, we are going to sum over all conditions
# in order to calculate the Index for each stock, where 2(-2) means that both RSI and BB are favorable, 1(-2) means that only one T.A is favorable
# and 0 means that no technical indicator is favorable.


for (i in 1:length(Assets[Filter])){
  
  
  assign(paste0('Rule',i),
         
         ifelse(ClosePrices[,i]>Bands[[i]][,2],-1,
                
                ifelse(ClosePrices[,i]<Bands[[i]][,1],1,0)))
  
  
  assign(paste0('Rule',i),
         
         cbind(get(paste0('Rule',i)),ifelse(RSI[[i]]>65,-1,
                                            
                                            ifelse(RSI[[i]]<35,1,0))))
  
  assign(paste0('Rule',i), xts(rowSums(get(paste0('Rule',i)),na.rm = TRUE),order.by =index(ClosePrices)))
  
  
}


#Collapse into one dataframe
Rule<-lapply(ls()[grep('Rule',ls())][order(as.numeric(gsub('Rule','',ls()[grep('Rule',ls())])))], get)
Decision<-as.data.frame(lapply(Rule,unlist))
names(Decision)<-Assets[Filter]
Decision<-tail(Decision,1)%>%as.xts()%>%t()
Decision<-as.data.frame(cbind(Decision,Assets[Filter]))
names(Decision)<-c('Signal','Ticker')
Decision$Signal<-as.numeric(as.character(Decision$Signal))
Decision$Type<-ifelse(Decision$Signal<0,'Short',ifelse(Decision$Signal>0,'Long','Neutral'))


#Which assets have shown trading signals according to our rules?
Short_Assets<-which(Decision$Type=='Short')
Long_Assets<-which(Decision$Type=='Long')
Decision[Short_Assets,]
Decision[Long_Assets,]


#MACD Diffence: we can also calculate a fully customized T.A and include it into the charts for each asset that has been marked as Long/Short: 
#Here, we simply apply a differenced MACD from 2 periods to include in each chart

MACD_D<-function(Asset,i,j){
  
  getSymbols(Asset,from='2019-01-01',to=Sys.Date())
  MACD11<-MACD(na.locf(Cl(get(Asset))),nFast=i,nSlow=j)[,1]
  MACD12<-MACD(na.locf(Cl(get(Asset))),nFast=i,nSlow=j)[,2]
  MACD21<-Lag(MACD11,1)
  MACD22<-Lag(MACD12,1)
  MACD31<-Lag(MACD11,2)
  MACD32<-Lag(MACD12,2)
  
  ifelse(((MACD11-MACD12)-(MACD21-MACD22))
         -((MACD21-MACD22)-(MACD31-MACD32))<0,1,0)
  
}



#How to choose assets that have the desired signs altoghether? Let's create a directory and store the files there.

dir.create(format(as.Date(Sys.Date()),format='%d.%m'))
setwd(paste0(getwd(),'/',format(as.Date(Sys.Date()),format='%d.%m'),'/'))

#Short Assets

for (i in Short_Assets){
  
  svg(filename=paste0('Plot',as.character(Decision$Ticker[i]),'.svg'),
      width=10, 
      height=6, 
      pointsize=12)

  
  symbol <- gsub('.Close','',as.character(Decision$Ticker[i]))
  
  TA_MACD<-MACD_D(symbol,13,26)%>%as.xts()
  index(TA_MACD)<-as.Date(index(TA_MACD))
  Diff<-TA_MACD[,1]
  chartSeries(get(symbol),subset=paste0("2019-01-1::",Sys.Date()),theme='white',
              name=NULL)
  plot(addTA(BBands(Cl(get(symbol)),maType='SMA',n=20,sd=2)[,c(1,3)],
             on=1,col=c('blue','blue'),type=c('l','l'),lty=c(2,2)))
  plot(addSMA(50,col="blue"))
  plot(addSMA(100,col="red"))
  plot(addRSI())
  plot(addTA(Diff,type = "o"))
  plot(addMACD())
  plot(addLines(h=c(0.5), on=3,col='red'))
  
  title(as.character(Decision$Ticker[i]))
  dev.off()
} 


#Long Assets

for (i in Long_Assets){
  
  svg(filename=paste0('Plot',as.character(Decision$Ticker[i]),'.svg'),
      width=10, 
      height=6, 
      pointsize=12)
  
  
  symbol <- gsub('.Close','',as.character(Decision$Ticker[i]))
  
  TA_MACD<-MACD_D(symbol,13,26)%>%as.xts()
  index(TA_MACD)<-as.Date(index(TA_MACD))
  Diff<-TA_MACD[,1]
  chartSeries(get(symbol),subset=paste0("2019-01-1::",Sys.Date()),theme='white',
              name=NULL)
  plot(addTA(BBands(Cl(get(symbol)),maType='SMA',n=20,sd=2)[,c(1,3)],
             on=1,col=c('blue','blue'),type=c('l','l'),lty=c(2,2)))
  plot(addSMA(50,col="blue"))
  plot(addSMA(100,col="red"))
  plot(addRSI())
  plot(addTA(Diff,type = "o"))
  plot(addMACD())
  plot(addLines(h=c(0.5), on=3,col='red'))
  
  title(as.character(Decision$Ticker[i]))
  dev.off()
} 


