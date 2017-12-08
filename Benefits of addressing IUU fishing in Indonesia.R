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
  p<-1177
  c<-133957649#74039270 #variable cost per unit of fishing mortality
  beta<-1.33
  YearLast = 2014 #year of last catch data
  Time = 100 #number of years
    
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
hreduce<-0.25
lambda<-0.1
endyearproj<-2035

#load sensitivity analysis parameters
sensitivityparams<-fread("C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/bioeconParams_skipjack.txt", sep="\t")
sensitivityparams<-data.frame(sensitivityparams)
colnames(sensitivityparams)<-c("x","r","MSY","f","b")
head(sensitivityparams)

rdist<-ggplot(sensitivityparams, aes(x=r)) + geom_density()
MSYdist<-ggplot(sensitivityparams, aes(x=MSY)) + geom_density()
fdist<-ggplot(sensitivityparams, aes(x=f)) + geom_density()
bdist<-ggplot(sensitivityparams, aes(x=b)) + geom_density()
grid.arrange(rdist,MSYdist,fdist,bdist,ncol=2)


# #generate a random number then (e.g. 2)
# #nrow(sensitivityparams)
# #sample 100 or 1000
# nsample<-1000
# #head(sensitivityparams)
# 
# #bootstrap here, get 1000 points (~10% of population with replacement)
# MeanParamsSAMPLE <- vector("list",nsample)
# for (i in 1:nsample){
# #ParamsSAMPLE<-sensitivityparams[sample(nrow(sensitivityparams), 1000, replace=T), ] #round(0.1*nrow(sensitivityparams))
# ParamsSAMPLE<-sensitivityparams[sample(nrow(sensitivityparams), 1000, replace=T), ] 
# MeanParamsSAMPLE[[i]]<-as.list(c(geometric.mean(ParamsSAMPLE$r),geometric.mean(ParamsSAMPLE$MSY),geometric.mean(ParamsSAMPLE$f),geometric.mean(ParamsSAMPLE$b)))
# #MeanParamsSAMPLE[[i]]<-as.list(c(mean(ParamsSAMPLE$r),mean(ParamsSAMPLE$MSY),mean(ParamsSAMPLE$f),mean(ParamsSAMPLE$b)))
# }
# sensitivityparamsSAMPLE<-rbindlist(MeanParamsSAMPLE)
# colnames(sensitivityparamsSAMPLE) <- c("r","MSY","f","b")
# #sensitivityparamsSAMPLE
# #sensitivityparamsSAMPLE<-sensitivityparams[sample(nrow(sensitivityparams), nsample), ]

#round(0.1*nrow(sensitivityparams))

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
#output_IUUi<-output_IUU[which(output_IUU$Policy!="Fcurrent" & output_IUU$Year<=2035),]
MultiRun_IUUi<-MultiRun_IUU[which(MultiRun_IUU$Year<=endyearproj),]

# extracting the output of the open access scenario only and renaming the policy into Business-as-usual or BAU
# with no IUU policy, catch and profit are shared by Indonesia and foreign fishing fleets
MultiRun_BAUi<-MultiRun_BAU[which(MultiRun_BAU$Policy=="Open Access" & MultiRun_BAU$Year<=endyearproj),]
MultiRun_BAUi$Policy<-"BAU"
MultiRun_BAUi$Profit<-MultiRun_BAUi$Profit*(1-hreduce)
MultiRun_BAUi$Harvest<-MultiRun_BAUi$Harvest*(1-hreduce)
MultiRun_BAUi$f<-MultiRun_BAUi$f*(1-hreduce)

# ##Extract Fmsy with no IUU policy
# MultiRun_Fmsyi<-MultiRun_BAU[which(MultiRun_BAU$Policy=="Fmsy" & MultiRun_BAU$Year<=endyearproj),]
# MultiRun_Fmsyi$Policy<-"BAUFmsy"
# MultiRun_Fmsyi$Profit<-MultiRun_Fmsyi$Profit*(1-hreduce)
# MultiRun_Fmsyi$Harvest<-MultiRun_Fmsyi$Harvest*(1-hreduce)
# MultiRun_Fmsyi$f<-MultiRun_Fmsyi$f*(1-hreduce)

#merge results
mergedresult<-rbind(MultiRun_IUUi,MultiRun_BAUi)#,MultiRun_Fmsyi)
##aestetic purpose only. y-axes labels will be in unit of 1e5 for harvest and 1e8 for profit
mergedresult$Harvest<-mergedresult$Harvest/1e5
mergedresult$Profit<-mergedresult$Profit/1e8

# mean_harvest <- mergedresult %>%
#   group_by(Year,Policy) %>%
#   summarize(mean.val = mean(Harvest), sd.val=sd(Harvest),n.val=n())%>%
#   mutate(se.val = sd.val / sqrt(n.val),
#          lower.ci.val = mean.val - qt(1 - (0.05 / 2), n.val - 1) * se.val,
#          upper.ci.val = mean.val + qt(1 - (0.05 / 2), n.val - 1) * se.val)
# head(mean_harvest)

mean_harvest <- mergedresult %>%
  group_by(Year,Policy) %>%
  summarize(mean.val = mean(Harvest), sd.val=sd(Harvest))%>%
  mutate(lower.val = mean.val - sd.val, 
         upper.val = mean.val + sd.val)

data.frame(mean_harvest)
# mean_profit <- mergedresult %>%
#   group_by(Year,Policy) %>%
#   summarize(mean.val = mean(Profit), sd.val=sd(Profit),n.val=n())%>%
#   mutate(se.val = sd.val / sqrt(n.val),
#          lower.ci.val = mean.val - sd.val, #qt(1 - (0.05 / 2), n.val - 1) * se.val,
#          upper.ci.val = mean.val + sd.val)#qt(1 - (0.05 / 2), n.val - 1) * se.val)


mean_profit <- mergedresult %>%
  group_by(Year,Policy) %>%
  summarize(mean.val = mean(Profit), sd.val=sd(Profit))%>%
  mutate(lower.val = mean.val - sd.val,
         upper.val = mean.val + sd.val)

data.frame(mean_profit)

plotharvest<-  ggplot()+
  geom_line(data = mergedresult[Policy=="Fmsy"], aes(x = Year, y = Harvest, group = RunNumber), color = "#F8766D",size=1,alpha=0.01)+  
  geom_line(data = mergedresult[Policy=="Open Access"], aes(x = Year, y = Harvest, group = RunNumber), color = "#00BFC4",size=1,alpha=0.01)+
  geom_line(data = mergedresult[Policy=="BAU"], aes(x = Year, y = Harvest, group = RunNumber), color = "darkviolet", size=1,alpha=0.01)+
  geom_line(data = mean_harvest, aes(x = Year, y = mean.val,group=Policy, colour=Policy),size=0.5)+
#  geom_line(data = mean_harvest, aes(x = Year, y = lower.q,group=Policy, colour=Policy),size=0.5,linetype = 2)+
#  geom_line(data = mean_harvest, aes(x = Year, y = upper.q,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  geom_line(data = mean_harvest, aes(x = Year, y = lower.val,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  geom_line(data = mean_harvest, aes(x = Year, y = upper.val,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  theme_minimal()+theme(legend.position="none") + scale_colour_manual(values = c("#F8766D", "#00BFC4", "darkviolet"))+
  labs(y=expression("Harvest (10"^"5"*" MT)"),x="Year")+ expand_limits(y=0)+
  annotate("text", x = 2030, y = 4.75, label = "IUU Policy + MSY")+
  annotate("text", x = 2025, y = 2, label = "Open Access")+
  annotate("text", x = 2031, y = 3.8, label = "IUU Policy +")+
  annotate("text", x = 2031, y = 3.6, label = "Open Access") 
#plotharvest

plotprofit<-  ggplot()+
  geom_line(data = mergedresult[Policy=="Fmsy"], aes(x = Year, y = Profit, group = RunNumber), color = "#F8766D",size=1,alpha=0.01)+  
  geom_line(data = mergedresult[Policy=="Open Access"], aes(x = Year, y = Profit, group = RunNumber), color = "#00BFC4",size=1,alpha=0.01)+
  geom_line(data = mergedresult[Policy=="BAU"], aes(x = Year, y = Profit, group = RunNumber),color = "darkviolet", size=1,alpha=0.01)+
  geom_line(data = mean_profit, aes(x = Year, y = mean.val,group=Policy, colour=Policy),size=0.5)+
#  geom_line(data = mean_profit, aes(x = Year, y = lower.q,group=Policy, colour=Policy),size=0.5,linetype = 2)+
#  geom_line(data = mean_profit, aes(x = Year, y = upper.q,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  geom_line(data = mean_profit, aes(x = Year, y = lower.val,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  geom_line(data = mean_profit, aes(x = Year, y = upper.val,group=Policy, colour=Policy),size=0.5,linetype = 2)+
  theme_minimal()+theme(legend.position="none")+ scale_colour_manual(values = c("#F8766D", "#00BFC4", "darkviolet"))+
  labs(y=expression("Profit (10"^"8"*" USD)"),x="Year")+ expand_limits(y=0)+
  annotate("text", x = 2030, y = 5.3, label = "IUU Policy + MSY")+
  annotate("text", x = 2023, y = 2, label = "Open Access")+  
  annotate("text", x = 2030, y = 4.5, label = "IUU Policy +")+
  annotate("text", x = 2030, y = 4.3, label = "Open Access")
#plotprofit
#ggplot_build(plotme)


write.csv(mean_harvest, file = "C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/skipjackprojectionharvest.csv")
write.csv(mean_profit, file = "C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/skipjackprojectionprofit.csv")

PlotFig3<-grid.arrange(plotharvest,plotprofit,ncol=2)

#saving results
tiff("C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/Fig4_skipjackbioecon.tiff",width=7,height=7,units="in",res=350)
PlotFig3<-grid.arrange(plotharvest,plotprofit,ncol=2)
PlotFig3
dev.off()


# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 
# sample(sensitivityparams,1,replace=FALSE)
# 
# sample(1:10,9,replace=FALSE) 
# 
# sensitivityparams[2,]
# 
# #IUU policy scenario
# output_IUU<-MyFunction(hreduce=hreduce,lambda=lambda)
# #Business-as-usual scenario (open access)
# output_BAU<-MyFunction(hreduce=0,lambda=lambda)
# 
# ##extracting the output of the IUU policy scenarios only (IUU Policy + MSY and IUU policy + Open Access)
# #output_IUUi<-output_IUU[which(output_IUU$Policy!="Fcurrent" & output_IUU$Year<=2035),]
# output_IUUi<-output_IUU[which(output_IUU$Year<=endyearproj),]
# 
# # extracting the output of the open access scenario only and renaming the policy into Business-as-usual or BAU
# # with no IUU policy, catch and profit are shared by Indonesia and foreign fishing fleets
# output_BAUi<-output_BAU[which(output_BAU$Policy=="Open Access" & output_BAU$Year<=endyearproj),]
# output_BAUi$Policy<-"BAU"
# output_BAUi$Profit<-output_BAUi$Profit*(1-hreduce)
# output_BAUi$Harvest<-output_BAUi$Harvest*(1-hreduce)
# output_BAUi$f<-output_BAUi$f*(1-hreduce)
# 
# ##Extract Fmsy with no IUU policy
# output_Fmsyi<-output_BAU[which(output_BAU$Policy=="Fmsy" & output_BAU$Year<=endyearproj),]
# output_Fmsyi$Policy<-"BAUFmsy"
# output_Fmsyi$Profit<-output_Fmsyi$Profit*(1-hreduce)
# output_Fmsyi$Harvest<-output_Fmsyi$Harvest*(1-hreduce)
# output_Fmsyi$f<-output_Fmsyi$f*(1-hreduce)
# 
# #merge results
# mergedresult<-rbind(output_IUUi,output_BAUi,output_Fmsyi)
# #aestetic purpose only. y-axes labels will be in unit of 1e5 for harvest and 1e8 for profit
# mergedresult$Harvest<-mergedresult$Harvest
# mergedresult$Profit<-mergedresult$Profit
# 
# #plotting results
# harvestplot<-ggplot(data=mergedresult, aes(x=Year,y=Harvest, color=Policy,group=Policy, shape=Policy)) +
#   geom_line(size=1.3)+labs(y=expression("Harvest (10"^"5"*" MT)"),x="Year")+theme_minimal()+theme(legend.position="none")
# harvestplot
# # +ylim(0,6)+
# #   annotate("text", x = 2029, y = 4.75, label = "IUU Policy + MSY")+
# #   annotate("text", x = 2025, y = 2, label = "Open Access")+
# #   annotate("text", x = 2031, y = 4, label = "IUU Policy +")+
# #   annotate("text", x = 2031, y = 3.8, label = "Open Access")
# 
# profitplot<-ggplot(data=mergedresult, aes(x=Year,y=Profit, color=Policy,group=Policy, shape=Policy)) +
#   geom_line(size=1.3)+labs(y=expression("Profit (10"^"8"*" USD)"),x="Year")+theme_minimal()+theme(legend.position="none")
# profitplot
# 
# 
# biomplot<-ggplot(data=mergedresult, aes(x=Year,y=BvBmsy, color=Policy,group=Policy, shape=Policy)) +
#   geom_line(size=1.3)+labs(y=expression("Biomass (10"^"5"*" MT)"),x="Year")+theme_minimal()+theme(legend.position="none")
# biomplot
# 
# fplot<-ggplot(data=mergedresult, aes(x=Year,y=f, color=Policy,group=Policy, shape=Policy)) +
#   geom_line(size=1.3)+labs(y=expression("f (10"^"5"*" MT)"),x="Year")+theme_minimal()+theme(legend.position="none")
# fplot
# 
# 
# # +ylim(0,6)+
# #   annotate("text", x = 2029, y = 5.3, label = "IUU Policy + MSY")+
# #   annotate("text", x = 2023, y = 2, label = "Open Access")+  
# #   annotate("text", x = 2030, y = 4.7, label = "IUU Policy +")+
# #   annotate("text", x = 2030, y = 4.5, label = "Open Access")
# 
# PlotFig3<-grid.arrange(harvestplot,profitplot,biomplot,fplot,ncol=2)
# 
# 
# #--------------------------
# # Indonesian Skipjack Tuna Projection using P-T model
# # Author: Reniel B. Cabral
# # August 31, 2017
# #--------------------------
# 
# #Load the libraries needed for plotting. If these libraries are not yet installed in your computer, you can install them by
# # using the code: install.packages(c("ggplot2","gridExtra","grid"))
# library(ggplot2)
# library(grid)
# library(gridExtra)
# 
# #Function simulating the effects of reduction in fishing effort due to IUU policies under different management regimes
# MyFunction<-function(freduce){
# 
#   b_orig<-1.433
#   f_orig<-0.818*(1-freduce)
#   g<- 0.237
#   MSY<-423586
#   
#   ##parameter values (see Extended Data Table 3)
#   #b_orig<-1.359
#   #f_orig<-0.804*(1-freduce)
#   #g<- 0.472
#   #MSY<-454729
#   
#   phi<-0.188
#   lambda<-0.1
#   p<-1177
#   c<-74039270 #variable cost per unit of fishing mortality
#   beta<-1.33
#   YearLast = 2014 #year of last catch data
#   Time = 30 #number of years
#   
#   biomFcurr<-b_orig
#   biomFmsy<-b_orig
#   biomFopen<-b_orig
#   
#   #Fcurrent forever
#   b<-b_orig
#   f<-f_orig
#   profitFcurr<-(p*f*b*MSY)-(c*((g*f)^beta))
#   harvestFcurr<-MSY*f*b
#   
#   profit_ini<-profitFcurr
#   harvest_ini<-harvestFcurr
#   
#   for (iter in 1:Time){
#     b<-b+ ( ((phi+1)/phi)*g*b*(1-(b^phi/(phi+1))) ) -(g*f*b)
#     biomFcurr<-append(biomFcurr,b)
#     profitFcurr<-append(profitFcurr,(p*f*b*MSY)-(c*((g*f)^beta)))
#     harvestFcurr<-append(harvestFcurr,MSY*f*b)
#   }
#   
#   #Fmsy
#   b<-b_orig
#   f<-f_orig
#   fFmsy<-f
#   profitFmsy<-(p*fFmsy*b*MSY)-(c*((g*fFmsy)^beta))
#   harvestFmsy<-MSY*fFmsy*b #this is just f because we are getting t=0
#   for (iter in 1:Time){
#     f<-1
#     b<-b+ ( ((phi+1)/phi)*g*b*(1-(b^phi/(phi+1))) ) -(g*f*b)
#     biomFmsy<-append(biomFmsy,b)
#     profitFmsy<-append(profitFmsy,(p*f*b*MSY)-(c*((g*f)^beta)))
#     harvestFmsy<-append(harvestFmsy,MSY*f*b)
#     fFmsy<-append(fFmsy,f)
#   }
#   
#   #open access
#   b<-b_orig
#   f<-f_orig
#   fFopen<-f
#   profitMSY<- (p*MSY)-(c*(g^beta)) #p should be different here? I just used current price
#   profitFopen<-(p*f*b*MSY)-(c*((g*f)^beta)) #profit previous time
#   harvestFopen<-MSY*f*b
#   for (iter in 1:Time){
#     profitdummy<-(p*f*b*MSY)-(c*((g*f)^beta)) #profit previous time
#     f<-f+(lambda*profitdummy/profitMSY)
#     b<-b+ ( ((phi+1)/phi)*g*b*(1-(b^phi/(phi+1))) ) -(g*f*b)
#     biomFopen<-append(biomFopen,b)
#     profitFopen<-append(profitFopen,(p*f*b*MSY)-(c*((g*f)^beta)))
#     harvestFopen<-append(harvestFopen,MSY*f*b)
#     fFopen<-append(fFopen,f)
#   }
#   
#   EndTime<- YearLast+Time
#   FcurrResult<-cbind(c(YearLast:EndTime),biomFcurr,profitFcurr,harvestFcurr,f_orig,1)
#   colnames(FcurrResult) <- c("Year","BvBmsy","Profit","Harvest","f","Policy")
#   FmsyResult<-cbind(c(YearLast:EndTime),biomFmsy,profitFmsy,harvestFmsy,fFmsy,2)
#   colnames(FmsyResult) <- c("Year","BvBmsy","Profit","Harvest","f","Policy")
#   FopenResult<-cbind(c(YearLast:EndTime),biomFopen,profitFopen,harvestFopen,fFopen,3)
#   colnames(FopenResult) <- c("Year","BvBmsy","Profit","Harvest","f","Policy")
#   output<-data.frame(rbind(FcurrResult,FmsyResult,FopenResult))
#   
#   output$Policy <- as.factor(output$Policy)
#   levels(output$Policy)[levels(output$Policy)=="1"] <- "Fcurrent"
#   levels(output$Policy)[levels(output$Policy)=="2"] <- "Fmsy"
#   levels(output$Policy)[levels(output$Policy)=="3"] <- "Open Access"
#   
#   return(output)
# }
# 
# 
# #25% of fishing effort reduced due to IUU policy
# reducedF<-0.25
# 
# #IUU policy scenario
# output_IUU<-MyFunction(freduce=reducedF)
# #Business-as-usual scenario (open access)
# output_BAU<-MyFunction(freduce=0)
# 
# #extracting the output of the IUU policy scenarios only (IUU Policy + MSY and IUU policy + Open Access)
# output_IUUi<-output_IUU[which(output_IUU$Policy!="Fcurrent" & output_IUU$Year<=2035),]
# 
# # extracting the output of the open access scenario only and renaming the policy into Business-as-usual or BAU
# # with no IUU policy, catch and profit are shared by Indonesia and foreign fishing fleets
# output_BAUi<-output_BAU[which(output_BAU$Policy=="Open Access" & output_BAU$Year<=2035),]
# output_BAUi$Policy<-"BAU"
# output_BAUi$Profit<-output_BAUi$Profit*(1-reducedF)
# output_BAUi$Harvest<-output_BAUi$Harvest*(1-reducedF)
# 
# ##Extract Fmsy with no IUU policy
# output_Fmsyi<-output_BAU[which(output_BAU$Policy=="Fmsy" & output_BAU$Year<=2035),]
# output_Fmsyi$Policy<-"BAUFmsy"
# output_Fmsyi$Profit<-output_Fmsyi$Profit*(1-reducedF)
# output_Fmsyi$Harvest<-output_Fmsyi$Harvest*(1-reducedF)
# 
# 
# #merge results
# mergedresult<-rbind(output_IUUi,output_BAUi,output_Fmsyi)
# #aestetic purpose only. y-axes labels will be in unit of 1e5 for harvest and 1e8 for profit
# mergedresult$Harvest<-mergedresult$Harvest/1e5
# mergedresult$Profit<-mergedresult$Profit/1e8
# 
# #plotting results
# 
# harvestplot<-ggplot(data=mergedresult, aes(x=Year,y=Harvest, color=Policy,group=Policy, shape=Policy)) +
#   geom_line(size=1.3)+labs(y=expression("Harvest (10"^"5"*" MT)"),x="Year")+theme_minimal()+theme(legend.position="none")+ylim(0,6)+
#   annotate("text", x = 2029, y = 4.75, label = "IUU Policy + MSY")+
#   annotate("text", x = 2025, y = 2, label = "Open Access")+
#   annotate("text", x = 2031, y = 4, label = "IUU Policy +")+
#   annotate("text", x = 2031, y = 3.8, label = "Open Access")
# harvestplot
# 
# profitplot<-ggplot(data=mergedresult, aes(x=Year,y=Profit, color=Policy,group=Policy, shape=Policy)) +
#   geom_line(size=1.3)+labs(y=expression("Profit (10"^"8"*" USD)"),x="Year")+theme_minimal()+theme(legend.position="none")+#ylim(0,6)+
#   annotate("text", x = 2029, y = 5.3, label = "IUU Policy + MSY")+
#   annotate("text", x = 2023, y = 2, label = "Open Access")+  
#   annotate("text", x = 2030, y = 4.7, label = "IUU Policy +")+
#   annotate("text", x = 2030, y = 4.5, label = "Open Access")
# profitplot
# 
# 
# 
# 
# PlotFig3<-grid.arrange(harvestplot,profitplot,ncol=2)
# 
# #saving results
# tiff("C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/Fig4_skipjackbioecon.tiff",width=7,height=7,units="in",res=350)
# PlotFig3<-grid.arrange(biomplot,profitplot,ncol=2)
# PlotFig3
# dev.off()