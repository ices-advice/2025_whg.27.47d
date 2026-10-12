
############################### compare SAM and SURBAR ###############################

wk.stock<-s.results$s.stock
wk.psim<-s.results$s.psim
wk.y1 <- s.results$s.y1
wk.y2 <- s.results$s.y2

# meanZ
wk.stock.meanz <- do.call(rbind, lapply(wk.psim, function(wk){wk$meanz}))
wk.stock.meanz.quantile <- array(NA, dim = c(dim(wk.stock.meanz)[2],5))
wk.stock.meanz.mean <- rep(NA, dim(wk.stock.meanz)[2])
rownames(wk.stock.meanz.quantile) <- wk.y1:wk.y2
colnames(wk.stock.meanz.quantile) <- c("5%","25%","50%","75%","95%")

for (wk.i in 1:dim(wk.stock.meanz)[2])
{
  wk.stock.meanz.quantile[wk.i,] <- quantile(wk.stock.meanz[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
  wk.stock.meanz.mean[wk.i] <- mean(wk.stock.meanz[,wk.i])
}

# SSB
wk.stock.ssb <- do.call(rbind, lapply(wk.psim, function(wk){wk$ssb}))
wk.stock.ssb.quantile <- array(NA, dim = c(dim(wk.stock.ssb)[2],5))
wk.stock.ssb.mean <- rep(NA, dim(wk.stock.ssb)[2])
rownames(wk.stock.ssb.quantile) <- wk.y1:wk.y2
colnames(wk.stock.ssb.quantile) <- c("5%","25%","50%","75%","95%")
for (wk.i in 1:dim(wk.stock.ssb)[2])
{
  wk.stock.ssb.quantile[wk.i,] <- quantile(wk.stock.ssb[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
  wk.stock.ssb.mean[wk.i] <- mean(wk.stock.ssb[,wk.i])
}


# Recruitment
wk.stock.rec <- do.call(rbind, lapply(wk.psim, function(wk){wk$rec}))
wk.stock.rec.quantile <- array(NA, dim = c(dim(wk.stock.rec)[2],5))
wk.stock.rec.mean <- rep(NA, dim(wk.stock.rec)[2])
rownames(wk.stock.rec.quantile) <- wk.y1:wk.y2
colnames(wk.stock.rec.quantile) <- c("5%","25%","50%","75%","95%")
for (wk.i in 1:dim(wk.stock.rec)[2])
{
  wk.stock.rec.quantile[wk.i,] <- quantile(wk.stock.rec[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
  wk.stock.rec.mean[wk.i] <- mean(wk.stock.rec[,wk.i])
}

tab12<-summary(fit)
tabN<-ntable(fit)


png('report\\Fig_32_plot_SSB_SAMSURBAR.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "SSB", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.ssb.quantile), max(wk.stock.ssb.quantile[,3],tab12[,4]/mean(tab12[,4])))
)
lines(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(rownames(tab12),tab12[,4]/mean(tab12[,4]), col="black", lwd=2)

legend(legend = c("SAM","SURBAR"),x="top",
       lty = c(1,1), lwd = c(2,2), bty = "n", col = c("black","darkorange"))
dev.off()



png('report\\Fig_32_plot_Rec_SAMSURBAR_age1.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.rec.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Recruitment", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.rec.quantile), max(wk.stock.rec.quantile[,4])))

lines(wk.y1:wk.y2, wk.stock.rec.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(rownames(tabN),tabN[,2]/mean(tabN[,2]), col="black", lwd=2)

legend(legend = c("SAM (age 1)","SURBAR (age 1)"), x = "topright",
       lty = c(1,1), lwd = c(2,2), bty = "n", col = c("black","darkorange"))

dev.off()

natmor<- rowMeans(fit$data$natMor[,3:5])
fmor<- rowMeans(faytable(fit)[,3:5])


png('report\\Fig_32_plot_Z_SAMSURBAR_2_4.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Mortality", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.meanz.quantile), max(wk.stock.meanz.quantile[,4])))

lines(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(rownames(tab12),fmor+natmor, col="black", lwd=2)

legend(legend = c("SAM Z(2-4)","SURBAR Z(2-4)"), x = "topright",
       lty = c(1,1), lwd = c(2,2), bty = "n", col = c("black","darkorange"))

dev.off()



natmor<- rowMeans(fit$data$natMor[,3:5])
fmor<- rowMeans(faytable(fit)[,3:5])


png('report\\Fig_32_plot_Z_SAMSURBAR_2_4.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Mortality", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.meanz.quantile), max(wk.stock.meanz.quantile[,4])))

lines(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(rownames(tab12),fmor+natmor, col="black", lwd=2)

legend(legend = c("SAM Z(2-4)","SURBAR Z(2-4)"), x = "topright",
       lty = c(1,1), lwd = c(2,2), bty = "n", col = c("black","darkorange"))

dev.off()




#compare assessments


load("model/model_2024.Rdata")
fit2024<-fit
load("model/model.Rdata")
fit2025<-fit
load("model/model_2024_newCatch21.Rdata")
fit2024_new<-fit
load("model/model_2024_altmat.Rdata")
fit2024_altmat<-fit

load("model/model.Rdata")
fit2025<-fit

rm(fit)

tab2025<-summary(fit2025)
tab2024<-summary(fit2024)
tab2024_new<-summary(fit2024_new)
tab2024_altmat<-summary(fit2024_altmat)

ctab2025<-catchtable(fit2025)
ctab2024<-catchtable(fit2024)
ctab2024_new<-catchtable(fit2024_new)

png('output\\Fig_plot_catch_SAM compare.png', width=1200, height=1000,res=200)
plot(rownames(ctab2025),ctab2025[,1],  col="royalblue",type = "l", lwd=3,lty = 1,bty="L",
     xlab = "Year", ylab = "Modelled catch (t)", xlim=c(1978,year))
lines(rownames(ctab2024_new),ctab2024_new[,1], col="orange", lwd=3)
lines(rownames(ctab2024),ctab2024[,1], col="black", lwd=3, lty="dotted")


dev.off()

png('output\\Fig_plot_SSB_SAM compare.png', width=1200, height=1000,res=200)
plot(rownames(tab2025),tab2025[,4],  col="royalblue",type = "l", lwd=3,lty = 1,bty="L",
     xlab = "Year", ylab = "SSB", xlim=c(1978,year))
lines(rownames(tab2024_new),tab2024_new[,4], col="orange", lwd=3)
lines(rownames(tab2024),tab2024[,4], col="black", lwd=3, lty="dotted")


dev.off()


png('output\\Fig_plot_Rec_SAM compare.png', width=1200, height=1000,res=200)
plot(rownames(tab2025)[1:nrow(tab2025)-1],tab2025[1:nrow(tab2025)-1,1],  col="royalblue",type = "l", lwd=3,lty = 1,bty="L",
     xlab = "Year", ylab = "Rec (age 0)", xlim=c(1978,year))
lines(rownames(tab2024_new)[1:nrow(tab2024_new)-1],tab2024_new[1:nrow(tab2024_new)-1,1], col="orange", lwd=3)
lines(rownames(tab2024)[1:nrow(tab2024)-1],tab2024[1:nrow(tab2024)-1,1], col="black", lwd=3, lty="dotted")


dev.off()


png('output\\Fig_plot_F_SAM compare.png', width=1200, height=1000,res=200)
plot(rownames(tab2025)[1:nrow(tab2025)-1],tab2025[1:nrow(tab2025)-1,7],  col="royalblue",type = "l", lwd=3,lty = 1,bty="L",
     xlab = "Year", ylab = "F (2-5)", xlim=c(1978,year))
lines(rownames(tab2024_new)[1:nrow(tab2024_new)-1],tab2024_new[1:nrow(tab2024_new)-1,7], col="orange", lwd=3)
lines(rownames(tab2024)[1:nrow(tab2024)-1],tab2024[1:nrow(tab2024)-1,7], col="black", lwd=3, lty="dotted")

legend(legend = c("WGNSSK 2025","WGNSSK 2024", "WGNSSK 2024 update"),x="topright",
       lty = c(1,3,1), lwd = c(3,3,3), cex=0.9,bty = "n", col = c("royalblue","black","orange"))
dev.off()


png('output\\Fig_plot_SSB_SAM compare mat.png', width=1200, height=1000,res=200)
plot(rownames(tab2025),tab2025[,4],  col="transparent",type = "l", lwd=3,lty = 1,bty="L",
     xlab = "Year", ylab = "SSB", xlim=c(1978,year))
lines(rownames(tab2024_altmat),tab2024_altmat[,4], col="orange", lwd=3)
lines(rownames(tab2024),tab2024[,4], col="black", lwd=3, lty="dotted")
legend(legend = c("WGNSSK 2024", "WGNSSK 2024 altern. maturity"),x="topright",
       lty = c(3,1), lwd = c(3,3), cex=0.9,bty = "n", col = c("black","orange"))

dev.off()

geom<-exp(mean(log(tab2025[26:(nrow(tab2025)-1),1])))/1000000
geom_all<-exp(mean(log(tab2025[1:(nrow(tab2025)-1),1])))/1000000


png('output\\Fig_plot_Rec_SAM geom mean.png', width=1200, height=1000,res=200)
plot(rownames(tab2025)[1:nrow(tab2025)-1],tab2025[1:nrow(tab2025)-1,1]/1000000,  col="black",type = "l", lwd=3,bty="L",
     xlab = "Year", ylab = "Rec (age 0, billions)", xlim=c(1978,year))

points(c(2003:year),rep(geom,length(2003:year)), col="black", lwd=2, type="l",lty="dotted")
points(c(1978:year),rep(geom_all,length(1978:year)), col="royalblue", lwd=2, type="l",lty="dotted")
legend("topright", legend=c("geom. mean since 1978","geom. mean since 2003"),col=c("royalblue","black"), lwd=2,lty="dotted", bty="n")

dev.off()

