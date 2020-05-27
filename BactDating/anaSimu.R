rm(list=ls())
dir='~/simuBactDating'

if (Sys.info()["nodename"]=='server1b') {
  #SERVER SIDE
  library(BactDating)
  store=matrix(NA,400,16)
  for (ind in 1:200) {
    tryCatch({
      myind=ind
      load(sprintf('%s/run%d.RData',dir,ind))
      ind=myind
      for (j in 1:2) {
        mind=2*ind+j-2
        if (j==1) res=res1 else res=res2
        store[mind,1]=mu
        v=res$record[,'mu']
        v=sort(v[floor(length(v)/2):length(v)])
        store[mind,c(2,3,4)]=v[floor(length(v)*c(0.025,0.5,0.975))]
        store[mind,5]=alpha
        v=res$record[,'alpha']
        v=sort(v[floor(length(v)/2):length(v)])
        store[mind,c(6,7,8)]=v[floor(length(v)*c(0.025,0.5,0.975))]
        store[mind,9]=sigma
        v=res$record[,'sigma']
        v=sort(v[floor(length(v)/2):length(v)])
        store[mind,c(10,11,12)]=v[floor(length(v)*c(0.025,0.5,0.975))]
        v=res$record[,length(dates)+1]-phy$root.time
        v=sort(v[floor(length(v)/2):length(v)])
        store[mind,c(13,14,15)]=v[floor(length(v)*c(0.025,0.5,0.975))]
        store[mind,16]=res$dic
        #store[ind,13]=res$pstrict
      }
      ind=myind
    },error=function(e) {},warning=function(w) {})
  }
  rm('res')
  save.image(sprintf('%s/all.RData',dir))
} else {

  #LAPTOP SIDE
  #system(sprintf('scp ubuntu@137.205.69.104:%s/all.RData .',dir))
  load('all.RData')
  allstore=store

  pdf('/tmp/fig.pdf',9,12)
  allstore=allstore[201:400,]#FOCUS ON NEW SIMULATIONS
  
  #INFERENCE WITH OLD MODEL
  store=allstore[seq(1,200,2),]
  par(fig=c(0.25,0.6,0.6,0.85), new=F,mar=c(0,0,0,0),xpd=T)
  plot(store[,9],store[,14],xlim=c(0,10),ylim=c(-25,25),pch=16,xlab='',ylab='',axes=F)
  box()
  axis(1,at=c(0,2,4,6,8,10),labels =NA)
  axis(2,at=c(-20,-10,0,10,20))
  segments(store[,9],store[,13],store[,9],store[,15])
  lines(c(0,10),c(0,0))
  par(xpd=NA)
  text(-2.5,5,expression(paste('Inferred ','TMRCA')),srt=90)
  par(fig=c(0.25,0.6,0.35,0.6), new=T,mar=c(0,0,0,0),xpd=T)
  plot(store[,9],store[,3],xlim=c(0,10),ylim=c(0,11),pch=16,xlab='',ylab='',axes=F)
  box()
  axis(1,at=c(0,2,4,6,8,10),labels =NA)
  axis(2,at=c(0,2,4,6,8,10))
  segments(store[,9],store[,2],store[,9],store[,4])
  lines(c(0,10),c(5,5))
  par(xpd=NA)
  text(-2.5,5,expression(paste('Inferred ',mu)),srt=90)
  par(fig=c(0.25,0.6,0.1,0.35), new=T,mar=c(0,0,0,0),xpd=T)
  store[,c(9,10,11)]=NA#NOT FAIR COMPARISON
  plot(store[,9],store[,11],xlim=c(0,10),ylim=c(0,15),pch=16,xlab='',ylab='',axes=F)
  box()
  axis(1,at=c(0,2,4,6,8,10))
  axis(2,at=c(0,2,4,6,8,10,12,14))
  segments(store[,9],store[,10],store[,9],store[,12])
  #lines(c(0,10),c(0,10))
  par(xpd=NA)
  text(-2.5,8,expression(paste('Inferred ',omega)),srt=90)
  text(5,-4.5,expression(paste('Correct ',omega)))

  #INFERENCE WITH NEW MODEL
  store=allstore[seq(2,200,2),]
  par(fig=c(0.6,0.95,0.6,0.85), new=T,mar=c(0,0,0,0),xpd=T)
  plot(store[,9],store[,14],xlim=c(0,10),ylim=c(-25,25),pch=16,xlab='',ylab='',axes=F)
  box()
  axis(1,at=c(0,2,4,6,8,10),labels =NA)
#  axis(2,at=c(0,2,4,6,8,10))
  segments(store[,9],store[,13],store[,9],store[,15])
  lines(c(0,10),c(0,0))
  par(xpd=NA)
#  text(-2.5,5,expression(paste('Inferred ',alpha)),srt=90)
  par(fig=c(0.6,0.95,0.35,0.6), new=T,mar=c(0,0,0,0),xpd=T)
  plot(store[,9],store[,3],xlim=c(0,10),ylim=c(0,11),pch=16,xlab='',ylab='',axes=F)
  box()
  axis(1,at=c(0,2,4,6,8,10),labels =NA)
#  axis(2,at=c(0,2,4,6,8,10))
  segments(store[,9],store[,2],store[,9],store[,4])
  lines(c(0,10),c(5,5))
  par(xpd=NA)
#  text(-2.5,5,expression(paste('Inferred ',mu)),srt=90)
  par(fig=c(0.6,0.95,0.1,0.35), new=T,mar=c(0,0,0,0),xpd=T)
  plot(store[,9],store[,11],xlim=c(0,10),ylim=c(0,15),pch=16,xlab='',ylab='',axes=F)
  box()
  axis(1,at=c(0,2,4,6,8,10))
#  axis(2,at=c(0,2,4,6,8,10,12,14))
  segments(store[,9],store[,10],store[,9],store[,12])
  lines(c(0,10),c(0,10))
  par(xpd=NA)
#  text(-2.5,8,expression(paste('Inferred ',sigma)),srt=90)
  text(5,-4.5,expression(paste('Correct ',omega)))  
  
  text(-5.5,50,'RC model',cex=1.5)
  text(5,50,'ARC model',cex=1.5)
  
  dev.off()
  system('open /tmp/fig.pdf')

#DIC COMPARISON
  allstore[seq(2,200,2),16]-allstore[seq(1,200,2),16]

}

c(mean(allstore[seq(1,200,2),15]-allstore[seq(1,200,2),13]),
  mean(allstore[seq(2,200,2),15]-allstore[seq(2,200,2),13]))

c(mean(allstore[seq(1,200,2),4]-allstore[seq(1,200,2),2]),
  mean(allstore[seq(2,200,2),4]-allstore[seq(2,200,2),2]))
