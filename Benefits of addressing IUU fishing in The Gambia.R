#Benefits of addressing IUU fishing in The Gambia
#Author: Reniel Cabral
#Last updated: 5 Dec 2017

#load libraries and data
library(ggplot2)
library(grid)
library(gridExtra)
library(psych)#for geometric mean
library(dplyr)
library(data.table)
library(ggthemes)

#load catch data
catchdata<-fread("C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/SAU EEZ 270 v46-0.txt")
head(catchdata)
table(catchdata$scientific_name)

#get catch of Octopus vulgaris
catch_octo<-catchdata[catchdata$scientific_name=="Octopus vulgaris",]
head(catch_octo)
table(catch_octo$data_layer)

#add harvest and harvest value per year
catch_octo_peryear<-catch_octo %>%
  group_by(year,data_layer,reporting_status) %>%
  summarize(
    catch=sum(tonnes),
    value=sum(landed_value)
  )

#get price by getting the mean of value/catch 
octoprice<-mean(catch_octo_peryear$value/catch_octo_peryear$catch)



#rename categories
catch_octo_peryear$Categories<-1

catch_octo_peryear<-catch_octo_peryear %>%
  mutate(Categories=replace(Categories, data_layer=="Reconstructed domestic catch" , "Domestic")) %>%  
  as.data.frame()
catch_octo_peryear<-catch_octo_peryear %>%
  mutate(Categories=replace(Categories, data_layer=="Inferred foreign catch" & reporting_status=="Reported", "Foreign-Reported")) %>%
  as.data.frame()
catch_octo_peryear<-catch_octo_peryear %>%
  mutate(Categories=replace(Categories, data_layer=="Inferred foreign catch" & reporting_status=="Unreported", "Foreign-Unreported")) %>%
  as.data.frame()

#merge domestic catch and value data
catch_octo_peryear<-catch_octo_peryear %>%
  group_by(year,Categories) %>%
  summarize(
    catch=sum(catch),
    value=sum(value)
  )


alldates<-data.frame(unique(catch_octo_peryear$year))
colnames(alldates)<-"year"

catch_octo_domestic<-catch_octo_peryear[catch_octo_peryear$Categories=="Domestic",]
catch_octo_domestic_fill <- merge(catch_octo_domestic, alldates, by="year", all=TRUE)
catch_octo_domestic_fill$Categories<-"Domestic"

catch_octo_foreignRep<-catch_octo_peryear[catch_octo_peryear$Categories=="Foreign-Reported",]
catch_octo_foreignRep_fill <- merge(catch_octo_foreignRep, alldates, by="year", all=TRUE)
catch_octo_foreignRep_fill$Categories<-"Foreign-Reported"

catch_octo_foreignUnrep<-catch_octo_peryear[catch_octo_peryear$Categories=="Foreign-Unreported",]
catch_octo_foreignUnrep_fill <- merge(catch_octo_foreignUnrep, alldates, by="year", all=TRUE)
catch_octo_foreignUnrep_fill$Categories<-"Foreign-Unreported"

catch_octo_peryear_FILL<-rbind(catch_octo_domestic_fill,catch_octo_foreignRep_fill,catch_octo_foreignUnrep_fill)
catch_octo_peryear_FILL[is.na(catch_octo_peryear_FILL)] <- 0

table(catch_octo_peryear_FILL$Categories)

domestic2014<-catch_octo_peryear_FILL$catch[catch_octo_peryear_FILL$year=="2014" & catch_octo_peryear_FILL$Categories=="Domestic"]
ForeignRep2014<-catch_octo_peryear_FILL$catch[catch_octo_peryear_FILL$year=="2014" & catch_octo_peryear_FILL$Categories=="Foreign-Reported"]
ForeignUnrep2014<-catch_octo_peryear_FILL$catch[catch_octo_peryear_FILL$year=="2014" & catch_octo_peryear_FILL$Categories=="Foreign-Unreported"]
PercentReduce<-(ForeignRep2014+ForeignUnrep2014)*100/(ForeignRep2014+ForeignUnrep2014+domestic2014)
#95% reduction as output

#PLOT different data
tiff("C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/Fig. S6_GambiaCatchProfile.tiff",width=7,height=7,units="in",res=350)
catchprofile <- ggplot(catch_octo_peryear_FILL, aes(year, y = catch, fill = Categories, order = Categories)) + labs(x="Year", y="Harvest (MT)") +
  geom_area(position = 'stack') + theme_minimal() + scale_fill_tableau("tableau20") 
catchprofile
dev.off()


#Save catch data for catch-msy
catch_octo_CMSY <-catch_octo %>%
  group_by(year) %>%
  summarize(
    catch=sum(tonnes)
  )

octodata<-data.frame(catch_octo_CMSY)
write.csv(octodata, file = "C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/OctoCatchdata_TheGambia.csv")

ggplot(catch_octo_CMSY, aes(x=year,y=catch))+
  geom_point()+geom_line()+labs(x="Year", y="Catch (MT)")


###Do the projection here

#Load the libraries needed for plotting. If these libraries are not yet installed in your computer, you can install them by
# using the code: install.packages(c("ggplot2","gridExtra","grid"))
library(ggplot2)
library(grid)
library(gridExtra)
library(psych)#for geometric mean
library(dplyr)
library(data.table)
library(ggthemes)

#Function simulating the effects of reduction in fishing effort due to IUU policies under different management regimes
MyFunction<-function(hreduce,lambda,params,RunNumber){
  b_curr<-params$b #0.7
  f_curr<-params$f #2
  f_reduce<-f_curr*(1-hreduce)
  g<- params$r #0.033
  MSY<-params$MSY#1200
  
  phi<-0.188
  p<-2945
  beta<-1.33
  YearLast = 2014 #year of last catch data
  Time = 100 #number of years
  
  #c<-(0.67*(p*f_curr*b_curr*MSY))/((g*f_curr)^beta)
  c<-(0.21*(p*f_curr*b_curr*MSY))/((g*f_curr)^beta)
  
  #last year biomass, local catch, and local profit of Indonesia
  biomFmsy<-b_curr
  biomFopen<-b_curr
  harvestFmsy<-(MSY*f_curr*b_curr)*(1-hreduce)
  harvestFopen<-harvestFmsy  
  profitFmsy<-((p*f_curr*b_curr*MSY)-(c*((g*f_curr)^beta)))*(1-hreduce)  
  profitFopen<-profitFmsy
  fFmsy<-f_reduce
  fFopen<-f_reduce
  
  #Fmsy
  b<-b_curr
  for (iter in 1:Time){
    f<-1
    b<-b+ ( ((phi+1)/phi)*g*b*(1-(b^phi/(phi+1))) ) -(g*f*b)
    biomFmsy<-append(biomFmsy,b)
    profitFmsy<-append(profitFmsy,(p*f*b*MSY)-(c*((g*f)^beta)))
    harvestFmsy<-append(harvestFmsy,MSY*f*b)
    fFmsy<-append(fFmsy,f)
  }
  
  #open access
  b<-b_curr
  f<-f_reduce
  profitMSY<- (p*MSY)-(c*(g^beta)) #p should be different here? I just used current price
  for (iter in 1:Time){
    lastyearprofit<-tail(profitFopen,1)
    f<-f+(lambda*lastyearprofit/profitMSY)
    b<-b+ ( ((phi+1)/phi)*g*b*(1-(b^phi/(phi+1))) ) -(g*f*b)
    biomFopen<-append(biomFopen,b)
    profitFopen<-append(profitFopen,(p*f*b*MSY)-(c*((g*f)^beta)))
    harvestFopen<-append(harvestFopen,MSY*f*b)
    fFopen<-append(fFopen,f)
  }
  
  EndTime<- YearLast+Time
  FmsyResult<-cbind(c(YearLast:EndTime),biomFmsy,profitFmsy,harvestFmsy,fFmsy,1,RunNumber)
  colnames(FmsyResult) <- c("Year","BvBmsy","Profit","Harvest","f","Policy","RunNumber")
  FopenResult<-cbind(c(YearLast:EndTime),biomFopen,profitFopen,harvestFopen,fFopen,2,RunNumber)
  colnames(FopenResult) <- c("Year","BvBmsy","Profit","Harvest","f","Policy","RunNumber")
  output<-data.frame(rbind(FmsyResult,FopenResult))
  
  output$Policy <- as.factor(output$Policy)
  levels(output$Policy)[levels(output$Policy)=="1"] <- "Fmsy"
  levels(output$Policy)[levels(output$Policy)=="2"] <- "Open Access"
  
  return(output)
}

#25% of fishing effort reduced due to IUU policy
hreduce<-0.9
lambda<-0.5
endyearproj<-2050#2035

#Load parameters
sensitivityparams<-fread("C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/bioeconParams_Gambia.txt", sep="\t")
sensitivityparams<-data.frame(sensitivityparams)
colnames(sensitivityparams)<-c("x","r","MSY","f","b")
head(sensitivityparams)

rdist<-ggplot(sensitivityparams, aes(x=r)) + geom_density()
MSYdist<-ggplot(sensitivityparams, aes(x=MSY)) + geom_density()
fdist<-ggplot(sensitivityparams, aes(x=f)) + geom_density()
bdist<-ggplot(sensitivityparams, aes(x=b)) + geom_density()
grid.arrange(rdist,MSYdist,fdist,bdist,ncol=2)

nsample<-1000 #number of means
sampsize<-100
MeanParamsSAMPLE <- vector("list",nsample)
for (i in 1:nsample){
  ParamsSAMPLE_r<-sample(sensitivityparams$r, sampsize, replace=T)
  ParamsSAMPLE_MSY<-sample(sensitivityparams$MSY, sampsize, replace=T)
  ParamsSAMPLE_f<-sample(sensitivityparams$f, sampsize, replace=T)
  ParamsSAMPLE_b<-sample(sensitivityparams$b, sampsize, replace=T)
  MeanParamsSAMPLE[[i]]<-as.list(c(geometric.mean(ParamsSAMPLE_r),geometric.mean(ParamsSAMPLE_MSY),geometric.mean(ParamsSAMPLE_f),geometric.mean(ParamsSAMPLE_b)))
}
sensitivityparamsSAMPLE<-rbindlist(MeanParamsSAMPLE)
colnames(sensitivityparamsSAMPLE) <- c("r","MSY","f","b")

#Multi-run output_IUU_multiple<-MyFunction(hreduce=hreduce,lambda=lambda,params=sensitivityparamsSAMPLE[1,])
MultiRun_IUU <- vector("list",nsample)
for (i in 1:nsample){
  MultiRun_IUU[[i]]<-MyFunction(hreduce=hreduce,lambda=lambda,params=sensitivityparamsSAMPLE[i,],RunNumber=i)
}

#Multi-run Business-as-usual scenario (open access)
MultiRun_BAU <- vector("list",nsample)
for (i in 1:nsample){
  MultiRun_BAU[[i]]<-MyFunction(hreduce=0,lambda=lambda,params=sensitivityparamsSAMPLE[i,],RunNumber=i)
}

#bind the list
MultiRun_IUU<-rbindlist(MultiRun_IUU)
MultiRun_BAU<-rbindlist(MultiRun_BAU)

##extracting the output of the IUU policy scenarios only (IUU Policy + MSY and IUU policy + Open Access)
MultiRun_IUUi<-MultiRun_IUU[which(MultiRun_IUU$Year<=endyearproj),]

##extracting the output of the open access scenario only and renaming the policy into Business-as-usual or BAU
# with no IUU policy, catch and profit are shared by Indonesia and foreign fishing fleets
MultiRun_BAUi<-MultiRun_BAU[which(MultiRun_BAU$Policy=="Open Access" & MultiRun_BAU$Year<=endyearproj),]
MultiRun_BAUi$Policy<-"BAU"
MultiRun_BAUi$Profit<-MultiRun_BAUi$Profit*(1-hreduce)
MultiRun_BAUi$Harvest<-MultiRun_BAUi$Harvest*(1-hreduce)
MultiRun_BAUi$f<-MultiRun_BAUi$f*(1-hreduce)

#merge results
mergedresult<-rbind(MultiRun_IUUi,MultiRun_BAUi)#,MultiRun_Fmsyi)

##aestetic purpose only. y-axes labels will be in unit of 1e5 for harvest and 1e8 for profit
mergedresult$Harvest<-mergedresult$Harvest/1e3
mergedresult$Profit<-mergedresult$Profit/1e6

mean_harvest <- mergedresult %>%
  group_by(Year,Policy) %>%
  summarize(mean.val = mean(Harvest), sd.val=sd(Harvest))%>%
  mutate(lower.val = mean.val - sd.val, 
         upper.val = mean.val + sd.val)

mean_profit <- mergedresult %>%
  group_by(Year,Policy) %>%
  summarize(mean.val = mean(Profit), sd.val=sd(Profit))%>%
  mutate(lower.val = mean.val - sd.val,
         upper.val = mean.val + sd.val)


#plot harvest and profit
plotharvest<-  ggplot()+
  geom_line(data = mergedresult[Policy=="Fmsy"], aes(x = Year, y = Harvest, group = RunNumber), color = "#F8766D",size=1,alpha=0.01)+  
  geom_line(data = mergedresult[Policy=="Open Access"], aes(x = Year, y = Harvest, group = RunNumber), color = "#00BFC4",size=1,alpha=0.01)+
  geom_line(data = mergedresult[Policy=="BAU"], aes(x = Year, y = Harvest, group = RunNumber), color = "darkviolet", size=1,alpha=0.01)+
  geom_line(data = mean_harvest, aes(x = Year, y = mean.val,group=Policy, colour=Policy),size=0.5)+
  geom_line(data = mean_harvest, aes(x = Year, y = lower.val,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  geom_line(data = mean_harvest, aes(x = Year, y = upper.val,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  theme_minimal()+theme(legend.position="none") + scale_colour_manual(values = c("#F8766D", "#00BFC4", "darkviolet"))+
  labs(y=expression("Harvest (10"^"3"*" MT)"),x="Year")+
  annotate("text", x = 2040, y = 1.2, label = "IUU Policy + MSY")+
  annotate("text", x = 2040, y = 0.2, label = "Open Access")+
  annotate("text", x = 2040, y = 2.1, label = "IUU Policy +")+
  annotate("text", x = 2040, y = 2, label = "Open Access")

plotprofit<-  ggplot()+
  geom_line(data = mergedresult[Policy=="Fmsy"], aes(x = Year, y = Profit, group = RunNumber), color = "#F8766D",size=1,alpha=0.01)+  
  geom_line(data = mergedresult[Policy=="Open Access"], aes(x = Year, y = Profit, group = RunNumber), color = "#00BFC4",size=1,alpha=0.01)+
  geom_line(data = mergedresult[Policy=="BAU"], aes(x = Year, y = Profit, group = RunNumber),color = "darkviolet", size=1,alpha=0.01)+
  geom_line(data = mean_profit, aes(x = Year, y = mean.val,group=Policy, colour=Policy),size=0.5)+
  geom_line(data = mean_profit, aes(x = Year, y = lower.val,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  geom_line(data = mean_profit, aes(x = Year, y = upper.val,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  theme_minimal()+theme(legend.position="none")+ scale_colour_manual(values = c("#F8766D", "#00BFC4", "darkviolet"))+
  labs(y=expression("Profit (10"^"6"*" USD)"),x="Year")+
  annotate("text", x = 2040, y = 2, label = "IUU Policy + MSY")+
  annotate("text", x = 2040, y = 0.2, label = "Open Access")+  
  annotate("text", x = 2040, y = -1, label = "IUU Policy +")+
  annotate("text", x = 2040, y = -1.25, label = "Open Access")


PlotFig4<-grid.arrange(plotharvest,plotprofit,ncol=2)

#save results
tiff("C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/FigS7_GambiaProj.tiff",width=7,height=7,units="in",res=350)
PlotFig4<-grid.arrange(plotharvest,plotprofit,ncol=2)
PlotFig4
dev.off()