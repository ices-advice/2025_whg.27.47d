## Extract results of interest, write TAF output tables

## Before: SAM results (fit, RETRO, forcast, forecast_options )
## After:  output tables (parameters, summary,catchtab, natage, fatage, sdtab, model fit,  mohn' rho, advice table, Foption table)

rm(list=ls())


library(FLRef)
library(icesTAF)
library(stockassessment)
library(icesSAG)
library(icesAdvice)
library(EnvStats)

# update for forecast
#season<-"spring"  # advice timing/ autumn
#propc4<-0.81      # proportion HC catches in area 4 in final year of assessment
#TAC<-17158          # TAC in intermediate year
#advice<-22082       # total catch advice given for intermediate year


load("model/allmodel.Rdata")
source("utilities_output.R")
source("output_stockweights.R")
source("output_Intercatch.R")
source("output_maturity.R")
#source("output_forecast.R")


# Model Parameters
ptab   <- partable(fit)
ptab<-data.frame(rownames(ptab), ptab)
colnames(ptab)[1]<-"variable"

# F at age
faytab <- faytable(fit)
faytab<-data.frame(rownames(faytab), faytab)
colnames(faytab)<-c("Year", "Age 0", "Age 1", "Age 2", "Age 3", "Age 4", "Age 5", "Age 6")

# Ns
ntab   <- ntable(fit)
ntab<-data.frame(rownames(ntab), ntab)
colnames(ntab)<-c("Year", "Age 0", "Age 1", "Age 2", "Age 3", "Age 4", "Age 5", "Age 6")

# Catch
catab  <- catchtable(fit)
colnames(catab) <- c("Catch","Low", "High")
catab<-data.frame(rownames(ntab)[-(length(rownames(ntab)))], catab) # catch has no rownames for some reason, ans is only until datayear
colnames(catab)[1]<-c("Year")

# TSB, Summary Table
tsb    <- round(tsbtable(fit))
colnames(tsb) <- c("TSB","Low", "High")

tabsummary<- data.frame(summary(fit), tsb)
tsb<-data.frame(rownames(tsb), tsb)

tabsummary<-data.frame(rownames(tabsummary), tabsummary)
colnames(tabsummary)<-c("Year", "R.age.0", "R_Low","R_High","SSB","SSB_Low","SSB_High","Fbar.2.5","Fbar.2.5_Low","Fbar.2.5_High","TSB","TSB_Low","TSB_High")


# model summary
mtab <- modeltable(c(Current=fit))
mtab<-data.frame(rownames(mtab), mtab)
colnames(mtab)[1]<-"run"


# SD

sdState<-function(fit, y=max(fit$data$years)-1:0){
  idx <- names(fit$sdrep$value) == "logR"
  sdLogR<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logssb"
  sdLogSSB<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logfbar"
  sdLogF<-fit$sdrep$sd[idx][fit$data$years%in%y]
  ret<-cbind(sdLogR, sdLogSSB, sdLogF)
  rownames(ret)<-y
  colnames(ret)<-c("sd(log(R))", "sd(log(SSB))", "sd(log(Fbar))")
  return(ret)
}

sdtab <-sdState(fit)
sdtab<-data.frame(rownames(sdtab), sdtab)

#plot bio, sel variables
load("model\\whg.27.47d_FLStock_input_data.Rdata")
stk<-stk_dat
ages <- 0:6
meanFages <- c(2:5)
years<-1983:datayear

#sel plots
taf.png("output/Sel_years.png")
if(min(ages)==0) {meanFages_ <- meanFages+1} else {meanFages_ <- meanFages}
meanF <- apply(harvest(stk)[meanFages_,],2, "mean")
sel <- sweep(harvest(stk),2,meanF,"/")
plot(ages,sel[,ac(max(years)-1)], type="l", ylim=c(0,max(sel)), xlab="Age", ylab="Selectivity", main="Selectivity at age")
for (i in ac((datayear-19):datayear)) lines(ages,sel[,i], col=i)
lines(ages,apply(sel[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
lines(ages,apply(sel[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
lines(ages,apply(sel[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
lines(ages,apply(sel[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
dev.off()

#biological var
taf.png("output/StWeights_years.png")
plot(ages,stock.wt(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(stock.wt(stk))), xlab="Age", ylab="Weight (kg)", main="Stock Weights at Age")
for (i in ac((datayear-19):datayear)) lines(ages,stock.wt(stk)[,i], col=i)
lines(ages,apply(stock.wt(stk)[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
dev.off()


#biological var
taf.png("output/Mat_years.png")
plot(ages,mat(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(mat(stk))), xlab="Age", ylab="Probability mature", main="Maturity at Age")
for (i in ac((datayear-19):datayear)) lines(ages,mat(stk)[,i], col=i)
lines(ages,apply(mat(stk)[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
lines(ages,apply(mat(stk)[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
lines(ages,apply(mat(stk)[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
lines(ages,apply(mat(stk)[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
dev.off()


#biological var
taf.png("output/Mort_years.png")
plot(ages,m(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(m(stk))), xlab="Age", ylab="Natural mortality", main="Natural mortality at Age")
for (i in ac((datayear-19):datayear)) lines(ages,m(stk)[,i], col=i)
lines(ages,apply(m(stk)[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
lines(ages,apply(m(stk)[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
lines(ages,apply(m(stk)[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
lines(ages,apply(m(stk)[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
legend("topright", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
dev.off()

#biological var
taf.png("output/catchweights_years.png")
plot(ages,catch.wt(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(catch.wt(stk)[,ac(max(years)-1)])), xlab="Age", ylab="Catch weight (kg)", main="Catch weights at Age")
for (i in ac((datayear-19):datayear)) lines(ages,catch.wt(stk)[,i], col=i)
lines(ages,apply(catch.wt(stk)[,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
lines(ages,apply(catch.wt(stk)[,ac((datayear-4):datayear)],1,mean), col=2, lwd=5)
lines(ages,apply(catch.wt(stk)[,ac((datayear-9):datayear)],1,mean), col=3, lwd=5)
lines(ages,apply(catch.wt(stk)[,ac((datayear-19):datayear)],1,mean), col=4, lwd=5)
legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
dev.off()

s<-as.data.frame(ssb(stk))
r<-as.data.frame(rec(stk))

ds <- dim(stk)
rec <- r[,"data"] 
ssb <- s[,"data"]
yr  <- s[,"year"] # 1 year lag


taf.png("output/Rec_per_SSB.png")
plot(yr,log(rec/ssb),type='b',xlab="Year",ylab="ln(Recruits/SSB) ",cex.lab=1.5)
dev.off()



# Mohns rho

mon<-t(as.data.frame(stockassessment::mohn(RETRO, what = NULL)))

## Write tables to output directory
write.taf(ptab, "output/partab.csv")  
write.taf(tabsummary, "output/summary.csv")  
write.taf(ntab, "output/natage.csv")    
write.taf(faytab, "output/fatage.csv") 
write.taf(mtab, "output/modelfit.csv") 
write.taf(sdtab, "output/sdtab.csv") 
write.taf(mon, "output/mohn.csv") 
write.taf(catab, "output/catchtab.csv") 

mtab <- modeltable(c(Current=fit)) #, base=basefit))
write.taf(mtab,"SAM_modelfitting.csv", dir="output")

# create SAG xml file
stockinfo <-stockInfo(
  StockCode = "whg.27.47d",
            AssessmentYear = year, 
            StockCategory = "1",
            ModelType="A",
            ModelName ="SAM",
            ConfidenceIntervalDefinition="95%",
            ContactPerson = "tanja.miethe@gov.scot",
            MSYBtrigger=167419,
            Bpa=167419,
            FMSY=0.68,
            Blim=119585,
            Fpa=0.68,
            RecruitmentAge=0)

#bms<-read.table("bootstrap/data/whg47d_bms.dat",skip=5)
d<-read.table("bootstrap/data/whg47d_di.dat",skip=5)#-bms
l<-read.table("bootstrap/data/whg47d_la.dat",skip=5)
i<-read.table("bootstrap/data/whg47d_iby.dat",skip=5)
c<-d+i+l#+bms



fishdata <- stockFishdata(Year = tabsummary$Year)
# For the standard graphs, recruitment in intermediate year = geomatric mean(2003:data year), used in forecast
fishdata$Recruitment <- c(tabsummary$R.age.0[1:(length(tabsummary$R.age.0)-1)],geoMean(tabsummary$R.age.0[26:(length(tabsummary$R.age.0)-1)])) 
# Forecast is deterministic, so we don't show the CIs
fishdata$Low_Recruitment<-c(tabsummary$R_Low[1:(length(tabsummary$R_Low)-1)],NA)
fishdata$High_Recruitment<-c(tabsummary$R_High[1:(length(tabsummary$R_High)-1)],NA)
fishdata$Low_StockSize<-tabsummary$SSB_Low#/1000
fishdata$High_StockSize<-tabsummary$SSB_High#/1000
fishdata$StockSize<-tabsummary$SSB#/1000
fishdata$TBiomass<-tabsummary$TSB
fishdata$Low_TBiomass<-tabsummary$TSB_Low
fishdata$High_TBiomass<-tabsummary$TSB_High
fishdata$FishingPressure <-c(tabsummary$Fbar.2.5[1:(length(tabsummary$Fbar.2.5)-1)],NA)
fishdata$Low_FishingPressure<-c(tabsummary$Fbar.2.5_Low[1:(length(tabsummary$Fbar.2.5_Low)-1)],NA)
fishdata$High_FishingPressure<-c(tabsummary$Fbar.2.5_High[1:(length(tabsummary$Fbar.2.5_High)-1)],NA)
fishdata$Catches<-rbind(c,NA)$V1
fishdata$Landings<-rbind(l,NA)$V1
fishdata$Discards<-rbind(d,NA)$V1
#fishdata$LandingsBMS<-rbind(bms,NA)$V1
fishdata$IBC<-rbind(i,NA)$V1

# Add ref points
# Check IBC column name
# Recruitment for 2020 to add

xmlfile <- createSAGxml(stockinfo, fishdata)
cat(xmlfile, file = "output/sag.xml")


