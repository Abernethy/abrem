objectlist <- ls() # for removing unused demo objects later
abrem.defaults   <- options.abrem()
# +-----------------------------------------------------------------------------
options.abrem(sub=paste0(
    "R package abrem ",packageVersion("abrem"),
    " (https://r-forge.r-project.org/projects/abernethy)"),
    method.fit=c("rr","xony"),method.conf.blives="mcpivotals")
# +-----------------------------------------------------------------------------
lto <- runif(8,100,1000)
da <- list(Abrem(lto,ppos="Benard",col="black",pch=1),
           Abrem(lto,ppos="beta",col="blue",pch=2),
           Abrem(lto,ppos="mean",col="green",pch=3),
           Abrem(lto,ppos="KM",col="yellow3",pch=4),
           Abrem(lto,ppos="Hazen",col="orange",pch=5),
           Abrem(lto,ppos="Blom",col="red",pch=6))
da <- abrem.fit(da)
plot.abrem(da,main='Comparing different ranking methods.')
#readline(prompt = "Hit <RETURN> to see next plot.\n")
# +-----------------------------------------------------------------------------
da <- Abrem(runif(8,100,1000))
da <- abrem.fit(da)
da <- abrem.conf(da)
da <- abrem.conf(da,method.conf.blives="bbb",lty=2)
plot(da)
#readline(prompt = "Hit <RETURN> to see next plot.\n")
# +-----------------------------------------------------------------------------
if(require(boot)){
    data(aircondit,aircondit7,package="boot")
    da1  <- abrem.fit(
        Abrem(aircondit$hours,label='\"aircondit\" dataset (package \"boot\")'))
    da2  <- abrem.fit(
        Abrem(aircondit7$hours,label='\"aircondit7\" dataset',col="red",pch=4))
    plot.abrem(list(da1,da2),xlim=c(0.01,1e5),
        main='\"aircondit\" dataset (package \"boot\")',
        xlab="Time To Failure (hours)")
}else message("Recommended package \"boot\" is not available, skipping this demo...")
# +-----------------------------------------------------------------------------
if(require(boot)){
    data(hirose,package="boot")
    names(hirose)  <- c("volt","time","event")
    da <- list()
    hc <- rev(rainbow(4,end=4/6))
    # create temperature-like colors
    da[[1]] <- abrem.fit(Abrem(subset(hirose,volt==5),
        label="Voltage= 5 [V]",col=hc[1],pch=0))
    da[[2]] <- abrem.fit(Abrem(subset(hirose,volt==7),
        label="Voltage= 7 [V]",col=hc[2],pch=1))
    da[[3]] <- abrem.fit(Abrem(subset(hirose,volt==10),
        label="Voltage= 10 [V]",col=hc[3],pch=2))
    da[[4]] <- abrem.fit(Abrem(subset(hirose,volt==15),
        label="Voltage= 15 [V]",col=hc[4],pch=5))

    def <- options.abrem()
    options.abrem(legend.text.size=0.5,xlim=NULL)

    #lapply(da,plot.abrem)
    plot.abrem(da,xlim=c(1,1e6),main='\"Hirose\" dataset (package \"boot\")')
#    legend("topright",c("5 [V]","7 [V]","10 [V]","15 [V]"),
#        title="Test voltages:",bg="white",inset=0.05,
#        cex=0.7,col=hc,lty=1,lwd=2)
    invisible(options.abrem(def))
}else message("Recommended package \"boot\" is not available, skipping this demo...")

# +-----------------------------------------------------------------------------
if(require(MASS)){
    def <- options.abrem()
    data(motors,package="MASS")
    names(motors) <- c("temp","time","event")
    mo <-  list()
    mo[[1]] <- abrem.fit(Abrem(subset(motors,temp==150),
        label="Temp = 150"))
        # note that subset(motors,temp==150) has no failures,
        # so it cannot be analysed by this package yet.
    hc <- rev(rainbow(3,end=4/6))
        # create temperature-like colors
    mo[[2]] <- abrem.fit(Abrem(subset(motors,temp==170),
        label="Temp = 170",col=hc[1],pch=0))
    mo[[3]] <- abrem.fit(Abrem(subset(motors,temp==190),
        label="Temp = 190",col=hc[2],pch=1))
    mo[[4]] <- abrem.fit(Abrem(subset(motors,temp==220),
        label="Temp = 220",col=hc[3],pch=2))

    mo <- lapply(mo,abrem.conf)
#    mo[[2]] <- abrem.conf(abrem.fit(Abrem(mo[[2]]$time,event=mo[[2]]$status),
#        col=hc[1],pch=0))
#    mo[[3]] <- abrem.conf(abrem.fit(Abrem(mo[[3]]$time,event=mo[[3]]$status),
#        col=hc[2],pch=1))
#    mo[[4]] <- abrem.conf(abrem.fit(Abrem(mo[[4]]$time,event=mo[[4]]$status),
#        col=hc[3],pch=2))
    plot.abrem(mo,xlim=c(5,1e6),main='\"Motors\" dataset (package \"MASS\")')
    options.abrem(def)
}else message("Recommended package \"MASS\" is not available, skipping this demo...")

# +-----------------------------------------------------------------------------
da <- Abrem(rweibull(8,1.5,1000)+600)
    # store any threshold value for later use while plotting
da <- abrem.fit(da,dist="weibull2p",col="blue")
da <- abrem.fit(da,dist="weibull3p",col="steelblue")
da <- abrem.fit(da,dist="lognormal2p",col="red")
da <- abrem.fit(da,dist="lognormal3p",col="orange")

plot(da,main="Comparison between Weibull and lognormal, two and three parameter")
legend("topleft",legend="Weibull plotting canvas",bg="white")
plot(da,main="Comparison between Weibull and lognormal, two and three parameter",
    canvas="lognormal")
legend("topleft",legend="Lognormal plotting canvas",bg="white")
# +-----------------------------------------------------------------------------
da <- abrem.fit(Abrem(rweibull(5,3,1000)),dist="weibull")
message("This might take some time...")
for(i in 1:20){
    da <- abrem.conf(da,col="blue",S=100,lwd=1,in.legend=i==TRUE)
    # just allow the first confidence bound in the legend, drop the rest
}
for(i in 1:20){
    da <- abrem.conf(da,col="red",S=1e4,lwd=1,in.legend=i==TRUE)
    # just allow the first confidence bound in the legend, drop the rest
}

plot(da,main='Variability in \"mcpivotals\" B-life confidence for S=100 and 1e4')
# +----------------------------------------------------------------------------
data(abrem_mix1)
earlyda <-abrem_mix1[1:10]
midda   <-abrem_mix1[11:131]
endda   <-abrem_mix1[132:200]

for(th in c(FALSE,TRUE)){
    da       <-Abrem(abrem_mix1,col="gray",pch=1,
        label=" abrem_mix1 (Complete, unaltered dataset) ")
    da21     <-Abrem(earlyda,susp=c(midda,endda),col="black",pch=19)
    da22     <-Abrem(midda,susp=c(earlyda,endda),col="blue",pch=3)
    da23     <-Abrem(endda,susp=c(earlyda,midda),col="green3",pch=4)
    
    ### without threshold parameter corrected plotting ###
    da21 <- abrem.fit(da21,
        label='\"Early\" data',dist="weibull2p",threshold=th)
    da22 <- abrem.fit(da22,
        label='\"Mid-\" data',dist="weibull3p",threshold=th)
    da23 <- abrem.fit(da23,
        label='\"End\" data',dist="weibull3p",threshold=th)
    plot.abrem(list(da,da21,da22,da23),xlim=c(0.5,1e5),
        main=paste0("Diaphragm life data of acid gas compressor, ",
        ifelse(th,"with","without"),
        " threshold parameter corrected plotting"))
}

# +----------------------------------------------------------------------------
data(abrem_mix1)
earlyda <-abrem_mix1[1:10]
midda   <-abrem_mix1[11:131]
endda   <-abrem_mix1[132:200]
da       <-Abrem(abrem_mix1,col="gray",pch=1,
            label=" abrem_mix1 (Complete, unaltered dataset) ")
da21     <-Abrem(endda,susp=c(earlyda,midda),col="green2",pch=19)
da22     <-Abrem(endda,susp=c(earlyda,midda),col="green3",pch=3)
da23     <-Abrem(endda,susp=c(earlyda,midda),col="green4",pch=4)
da21 <- abrem.fit(da21,
    label='\"End\" data, threshold=FALSE',dist="weibull3p",threshold=FALSE)
da22 <- abrem.fit(da22,
    label='\"End\" data, threshold=TRUE',dist="weibull3p",threshold=TRUE)
da23 <- abrem.fit(da23,
    label='\"End\" data, threshold=5000',dist="weibull3p",threshold=1800)
plot.abrem(list(da,da21,da22,da23),xlim=c(0.5,1e5),
    main="Diaphragm life data of acid gas compressor, threshold parameter usage")

## +----------------------------------------------------------------------------
fail1 <- c(535,269,1066,788,288)
fail2 <- c(1043,843,1303,1847,2590)
da1 <- abrem.fit(Abrem(fail1,pch=0,col="blue"))
da2 <- abrem.fit(Abrem(fail2,pch=4,col="red"))

da1 <- abrem.conf(da1,method.conf.blives="lrb",cl=0.5,
    in.legend.blives=F,is.plot.cb=F,lty=5)
da1 <- abrem.conf(da1,method.conf.blives="lrb",cl=0.9)
da1 <- abrem.conf(da1,method.conf.blives="lrb",cl=0.95,
    in.legend.blives=F,is.plot.cb=F,lty=5)
da2 <- abrem.conf(da2,method.conf.blives="lrb",cl=0.5,
    in.legend.blives=F,is.plot.cb=F,lty=5)
da2 <- abrem.conf(da2,method.conf.blives="lrb",cl=0.9)
da2 <- abrem.conf(da2,method.conf.blives="lrb",cl=0.95,
    in.legend.blives=F,is.plot.cb=F,lty=5)
par(mfrow=c(1,2))
dataset <- list(da1,da2)
plot.abrem(dataset,main="Weibull 2p")
contour.abrem(dataset)
# +----------------------------------------------------------------------------

invisible(options.abrem(abrem.defaults))
   # resetting the all abrem options.
rm(list = ls()[!(ls() %in% objectlist)])
   # removing demo objects
