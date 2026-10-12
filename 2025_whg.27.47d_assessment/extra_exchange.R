rm(list=ls())

library(DATRAS)

year <- 2025
exch_name <- "Unaggregated trawl and biological information_2025-04-02 13_17_31.zip"


# load new data
d <- readExchange(paste0("N:\\1_Tanja\\2_MARLAB\\ICES\\WGNSSK",year,"\\WHITING_",year,"\\ICES_whg.27.47d\\DATRAS\\",exch_name), strict=FALSE)

# load old data next year use previous year exchange data and check variable names match
load(paste0("N:\\1_Tanja\\2_MARLAB\\ICES\\WGNSSK",year,"\\WHITING_",year,"\\ICES_whg.27.47d\\DATRAS\\exchange_",2024,".Rdata"))


# keep whiting
# species specific parameters
genus = "Merlangius"
bfamily = "merlangus"

# Q1 whiting data (in areas 1-7), valid hauls; update recent maturity data for corrections
whiting_new <- subset(d, Species==paste(genus,bfamily), Quarter==1  ,Year%in%c(2021:2025), HaulVal=="V")
whiting <- subset(whiting, Species==paste(genus,bfamily), Quarter==1  ,Year%in%c(1978:2020), HaulVal=="V")


#check variable names for each frame 
#setdiff(colnames(whiting_new[[3]]), colnames(whiting[[3]]))
#setdiff(colnames(whiting[[3]]), colnames(whiting_new[[3]]))

# rename so that it matches old data
whiting_new[["CA"]]$NoAtLngt <- whiting_new[["CA"]]$NoAtALK
whiting_new[["CA"]]$GearExp <- whiting_new[["CA"]]$GearEx
whiting_new[["HH"]]$GearExp <- whiting_new[["HH"]]$GearEx
whiting_new[["HL"]]$GearExp <- whiting_new[["HL"]]$GearEx
whiting_new[["HH"]]$Stratum <- whiting_new[["HH"]]$DepthStratum
whiting_new[["HH"]]$BycSpecRecCode <- whiting_new[["HH"]]$BySpecRecCode

whiting[["CA"]]$FishID <- NA

### The new data has extra columns. To merge the old and new data we keep only the columns matching the old data

for (zz in c("HH","HL","CA")) { # zz<- "CA"

# create dataframe with new data in old format
xx <- data.frame(array(NA,dim=c(nrow(whiting_new[[zz]]),ncol(whiting[[zz]]))))
colnames(xx) <- colnames(whiting[[zz]])

# fill it with new data
for (i in 1:ncol(whiting[[zz]])) { # i<-1
  if(colnames(xx[i]) %in% colnames(whiting_new[[zz]])) {
    xx[,colnames(whiting[[zz]][i])] <- whiting_new[[zz]][,colnames(whiting[[zz]][i])]
  }
}

# add it to the old data
whiting[[zz]] <- rbind(whiting[[zz]],xx)

rm(xx,i)

}

# save data
save(whiting,file=paste("bootstrap\\initial\\data\\exchange.Rdata",sep=''))


#check duplicates
#x<- as_tibble(whiting[[1]])
#which(duplicated(x))
#unique(whiting[[1]]$Year)
#unique(whiting[[1]][which(duplicated(x)),"Year"])

     