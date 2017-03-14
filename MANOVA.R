###  MANOVA design for EPMS Vendor quote data   ###
##  I modified this data set to match the requirements of MANOVA examples online - not sure if needed  ##
vendata <- read.csv("/Users/eshelman/Documents/EPMS Cost Comparisons_MANOVAset.txt", header=TRUE, sep="\t", stringsAsFactors = TRUE)
strpdata <- vendata[-c(1:3)]
trial1 <- matrix(data=strpdata, byrow=TRUE, ncol=19, dimnames = list(NULL,c("Elec_Lab","Elec_Mats","Elec_P","Elec_Sub","Elec_Tot",
                                                                            "Eng_Cost","Eng_Hrs","Eng_Rate","Eqip_Cost","Field_Tech_Cost",
                                                                            "Field_Tech_Hrs","Field_Tech_Rt","Freight","Hrly_Rt","Other",
                                                                            "PM_Cost", "PM_Hrs", "Total_Cost","Warranty")))
mantrial1 <- manova(trial1)

mantrial <- manova(cbind(vendata$Electrical.Sub.Quote,vendata$Electrical.Total.Cost,vendata$Engineering.Cost,vendata$Engineering.Hrs,
                         vendata$Engineering.Rate,vendata$Equipment.Cost,vendata$Field.Tech.Cost)~
                     vendata$Vendor*vendata$Loc*vendata$Phase, data=vendata)

anova1 <- aov(vendata$Electrical.Sub.Quote~vendata$Vendor, data=vendata)
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(anova1) # diagnostic plots
summary(anova1) # display Type I ANOVA table
drop1(anova1,~.,test="F") # type III SS and F Tests 
# Tukey Honestly Significant Differences
TukeyHSD(anova1) # where fit comes from aov() 
library(gplots)
plotmeans(vendata$Electrical.Sub.Quote~vendata$Vendor,xlab="Vendor Providing Quote",
          ylab="Avg. Elec. Sub. Quote (all sites/all phases)", main="Mean Plot\nwith 95% CI") 
plotmeans(vendata$Electrical.Sub.Quote~vendata$Loc,xlab="Cluster",
          ylab="Avg. Elec. Sub. Quote (all sites/all phases)", main="Mean Plot\nwith 95% CI") 

interaction.plot(vendata$Phase, vendata$Loc, vendata$Electrical.Sub.Quote, type="b", col=c(1:6),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Phase",
                 ylab="Cost Approximation",
                 main="Interaction Plot \nElectrical Sub. Quotes by Location/Phase")

interaction.plot(vendata$Loc, vendata$Vendor, vendata$Electrical.Sub.Quote, type="b", col=c(1:6),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Location",
                 ylab="Cost Approximation",
                 main="Interaction Plot \nElectrical Sub. Quotes by Location/Vendor")

####    This is the set that is usable for EPMS/BMS at this stage!    ####
###   Secondary set using original data set from JJ   ###
epms.df <- read.csv("/Users/eshelman/Documents/EPMS Cost Comparisons Data.txt", header=TRUE, sep="\t", stringsAsFactors = TRUE)
  #quote.df$Value <- as.numeric(quote.df$Value)
bms.df <- read.csv("/Users/eshelman/Documents/BMS BaselineSummary.txt", header=TRUE, sep="\t", stringsAsFactors = TRUE)

for (i in unique(epms.df$Description)) {
   print(i)
  j <- gsub("\\s","",paste(i))
  datset<-subset(epms.df, Description==i)

  datset
  anova <- aov(datset$Value~datset$Vendor*datset$Loc, data=datset)
  layout(matrix(c(1,2,3,4),2,2)) # optional layout
  plot(anova) # diagnostic plots
  summary(anova) # display Type I ANOVA table
  drop1(anova,~.,test="F") # type III SS and F Tests 
  # Tukey Honestly Significant Differences
  TukeyHSD(anova) # where fit comes from aov() 
  library(gplots)
  filename <- paste("/Users/eshelman/Documents/ReportCharts/EPMS/", j," Vendor Quotes - ", Sys.Date(), ".png", sep="")
  png(file=filename, width=720, height=480)
  vdn <- plotmeans(datset$Value~datset$Vendor,xlab="Vendor Providing Quote",
            ylab=paste(unique(datset$Description), "(all sites/all phases)", sep=" "), 
            main=paste("EPMS - ", unique(datset$Description), " Vendor\nMean Plot with 95% CI")) 
  dev.off()
  
  filename2 <- paste("/Users/eshelman/Documents/ReportCharts/EPMS/", j," Cluster Quotes - ", Sys.Date(), ".png", sep="")
  png(file=filename2, width=720, height=480)
  clu <- plotmeans(datset$Value~datset$Loc,xlab="Cluster",
            ylab=paste(unique(datset$Description), "(all sites/all phases)", sep=" "), 
            main=paste("EPMS - ", unique(datset$Description), " Cluster\nMean Plot with 95% CI")) 
  dev.off()
  
  filename3 <- paste("/Users/eshelman/Documents/ReportCharts/EPMS/", j," Region Quotes - ", Sys.Date(), ".png", sep="") 
  png(file=filename3, width=720, height=480)
  int <- interaction.plot(datset$Region, datset$Vendor, datset$Value, type="b", col=c(1:length(unique(datset$Vendor))),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Location",
                 ylab="Cost Approximation",
                 main=paste("EPMS - ", unique(datset$Description)," \nInteraction Plot by Region/Vendor", sep=""))
  dev.off()
}


for (i in unique(bms.df$Description)) {
  print(i)
  j <- gsub("\\s","",paste(i))
  datset<-subset(bms.df, Description==i)
  
  datset
  anova <- aov(datset$Value~datset$Vendor*datset$Region.1, data=datset)
  layout(matrix(c(1,2,3,4),2,2)) # optional layout
  plot(anova) # diagnostic plots
  summary(anova) # display Type I ANOVA table
  drop1(anova,~.,test="F") # type III SS and F Tests 
  # Tukey Honestly Significant Differences
  TukeyHSD(anova) # where fit comes from aov() 
  library(gplots)
  filename <- paste("/Users/eshelman/Documents/ReportCharts/BMS/", j," Vendor Quotes - ", Sys.Date(), ".png", sep="")
  png(file=filename, width=720, height=480)
  vdn <- plotmeans(datset$Value~datset$Vendor,xlab="Vendor Providing Quote",
                   ylab=paste(unique(datset$Description), "(all sites/all phases)", sep=" "), 
                   main=paste("BMS - ", unique(datset$Description), " Cluster\nMean Plot with 95% CI")) 
  dev.off()
  
  filename2 <- paste("/Users/eshelman/Documents/ReportCharts/BMS/", j," Cluster Quotes - ", Sys.Date(), ".png", sep="")
  png(file=filename2, width=720, height=480)
  clu <- plotmeans(datset$Value~datset$Region.1,xlab="Cluster",
                   ylab=paste(unique(datset$Description), "(all sites/all phases)", sep=" "), 
                   main=paste("BMS - ", unique(datset$Description), " Cluster\nMean Plot with 95% CI"))
  dev.off()
  
  filename3 <- paste("/Users/eshelman/Documents/ReportCharts/BMS/", j," Region Quotes - ", Sys.Date(), ".png", sep="") 
  png(file=filename3, width=720, height=480)
  int <- interaction.plot(datset$Region, datset$Vendor, datset$Value, type="b", col=c(1:length(unique(datset$Vendor))),
                          leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                          xlab="Location",
                          ylab="Cost Approximation",
                          main=paste("BMS - ", unique(datset$Description)," \nInteraction Plot by Region/Vendor", sep=""))
  dev.off()
}
