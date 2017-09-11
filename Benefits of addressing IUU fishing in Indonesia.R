#--------------------------
# Indonesian Skipjack Tuna Projection using P-T model
# Author: Reniel B. Cabral
# August 31, 2017
#--------------------------

#Load the libraries needed for plotting. If these libraries are not yet installed in your computer, you can install them by
# using the code: install.packages(c("ggplot2","gridExtra","grid"))
library(ggplot2)
library(grid)
library(gridExtra)

#Function simulating the effects of reduction in fishing effort due to IUU policies under different management regimes
MyFunction<-function(freduce){
  
  #parameter values (see Extended Data Table 3)
  b_orig<-1.359
  f_orig<-0.804*(1-freduce)
  phi<-0.188
  g<- 0.472
  lambda<-0.1
  p<-1177
  c<-74039270 #variable cost per unit of fishing mortality
  MSY<-454729
  beta<-1.33
  YearLast = 2014 #year of last catch data
  Time = 30 #number of years
  
  biomFcurr<-b_orig
  biomFmsy<-b_orig
  biomFopen<-b_orig
  
  #Fcurrent forever
  b<-b_orig
  f<-f_orig
  profitFcurr<-(p*f*b*MSY)-(c*((g*f)^beta))
  harvestFcurr<-MSY*f*b
  
  profit_ini<-profitFcurr
  harvest_ini<-harvestFcurr
  
  for (iter in 1:Time){
    b<-b+ ( ((phi+1)/phi)*g*b*(1-(b^phi/(phi+1))) ) -(g*f*b)
    biomFcurr<-append(biomFcurr,b)
    profitFcurr<-append(profitFcurr,(p*f*b*MSY)-(c*((g*f)^beta)))
    harvestFcurr<-append(harvestFcurr,MSY*f*b)
  }
  
  #Fmsy
  b<-b_orig
  f<-f_orig
  fFmsy<-f
  profitFmsy<-(p*fFmsy*b*MSY)-(c*((g*fFmsy)^beta))
  harvestFmsy<-MSY*fFmsy*b #this is just f because we are getting t=0
  for (iter in 1:Time){
    f<-1
    b<-b+ ( ((phi+1)/phi)*g*b*(1-(b^phi/(phi+1))) ) -(g*f*b)
    biomFmsy<-append(biomFmsy,b)
    profitFmsy<-append(profitFmsy,(p*f*b*MSY)-(c*((g*f)^beta)))
    harvestFmsy<-append(harvestFmsy,MSY*f*b)
    fFmsy<-append(fFmsy,f)
  }
  
  #open access
  b<-b_orig
  f<-f_orig
  fFopen<-f
  profitMSY<- (p*MSY)-(c*(g^beta)) #p should be different here? I just used current price
  profitFopen<-(p*f*b*MSY)-(c*((g*f)^beta)) #profit previous time
  harvestFopen<-MSY*f*b
  for (iter in 1:Time){
    profitdummy<-(p*f*b*MSY)-(c*((g*f)^beta)) #profit previous time
    f<-f+(lambda*profitdummy/profitMSY)
    b<-b+ ( ((phi+1)/phi)*g*b*(1-(b^phi/(phi+1))) ) -(g*f*b)
    biomFopen<-append(biomFopen,b)
    profitFopen<-append(profitFopen,(p*f*b*MSY)-(c*((g*f)^beta)))
    harvestFopen<-append(harvestFopen,MSY*f*b)
    fFopen<-append(fFopen,f)
  }
  
  EndTime<- YearLast+Time
  FcurrResult<-cbind(c(YearLast:EndTime),biomFcurr,profitFcurr,harvestFcurr,f_orig,1)
  colnames(FcurrResult) <- c("Year","BvBmsy","Profit","Harvest","f","Policy")
  FmsyResult<-cbind(c(YearLast:EndTime),biomFmsy,profitFmsy,harvestFmsy,fFmsy,2)
  colnames(FmsyResult) <- c("Year","BvBmsy","Profit","Harvest","f","Policy")
  FopenResult<-cbind(c(YearLast:EndTime),biomFopen,profitFopen,harvestFopen,fFopen,3)
  colnames(FopenResult) <- c("Year","BvBmsy","Profit","Harvest","f","Policy")
  output<-data.frame(rbind(FcurrResult,FmsyResult,FopenResult))
  
  output$Policy <- as.factor(output$Policy)
  levels(output$Policy)[levels(output$Policy)=="1"] <- "Fcurrent"
  levels(output$Policy)[levels(output$Policy)=="2"] <- "Fmsy"
  levels(output$Policy)[levels(output$Policy)=="3"] <- "Open Access"
  
  return(output)
}


#25% of fishing effort reduced due to IUU policy
reducedF<-0.25

#IUU policy scenario
output_IUU<-MyFunction(freduce=reducedF)
#Business-as-usual scenario (open access)
output_BAU<-MyFunction(freduce=0)

#extracting the output of the IUU policy scenarios only (IUU Policy + MSY and IUU policy + Open Access)
output_IUUi<-output_IUU[which(output_IUU$Policy!="Fcurrent" & output_IUU$Year<=2035),]

# extracting the output of the open access scenario only and renaming the policy into Business-as-usual or BAU
# with no IUU policy, catch and profit are shared by Indonesia and foreign fishing fleets
output_BAUi<-output_BAU[which(output_BAU$Policy=="Open Access" & output_BAU$Year<=2035),]
output_BAUi$Policy<-"BAU"
output_BAUi$Profit<-output_BAUi$Profit*(1-reducedF)
output_BAUi$Harvest<-output_BAUi$Harvest*(1-reducedF)

#merge results
mergedresult<-rbind(output_IUUi,output_BAUi)
#aestetic purpose only. y-axes labels will be in unit of 1e5 for harvest and 1e8 for profit
mergedresult$Harvest<-mergedresult$Harvest/1e5
mergedresult$Profit<-mergedresult$Profit/1e8

#plotting results
biomplot<-ggplot(data=mergedresult, aes(x=Year,y=Harvest, color=Policy,group=Policy, shape=Policy)) +
  geom_line(size=1.3)+labs(y=expression("Harvest (10"^"5"*" MT)"),x="Year")+theme_minimal()+theme(legend.position="none")+ylim(0,6)+
  annotate("text", x = 2029, y = 4.75, label = "IUU Policy + MSY")+
  annotate("text", x = 2025, y = 2, label = "Open Access")+
  annotate("text", x = 2031, y = 4, label = "IUU Policy +")+
  annotate("text", x = 2031, y = 3.8, label = "Open Access")

profitplot<-ggplot(data=mergedresult, aes(x=Year,y=Profit, color=Policy,group=Policy, shape=Policy)) +
  geom_line(size=1.3)+labs(y=expression("Profit (10"^"8"*" USD)"),x="Year")+theme_minimal()+theme(legend.position="none")+ylim(0,6)+
  annotate("text", x = 2029, y = 5.3, label = "IUU Policy + MSY")+
  annotate("text", x = 2023, y = 2, label = "Open Access")+  
  annotate("text", x = 2030, y = 4.7, label = "IUU Policy +")+
  annotate("text", x = 2030, y = 4.5, label = "Open Access")

PlotFig3<-grid.arrange(biomplot,profitplot,ncol=2)

#saving results
tiff("C:/Users/Ren/Documents/GitHub/Rapid-and-lasting-gains-from-solving-illegal-fishing/Fig3.tiff",width=7,height=7,units="in",res=350)
PlotFig3<-grid.arrange(biomplot,profitplot,ncol=2)
PlotFig3
dev.off()