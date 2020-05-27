library(BactDating)
library(ape)
system('rm -rf simu.nwk')
truths=c()
for (ind in 101:200) {
  set.seed(ind)
  dates=seq(2010,2020,0.1)

  if (ind<=100) {
    alpha=5
    mu=5
    sigma=ind/100*10
    phy=simcoaltree(dates,alpha=alpha)
    #obsphy=simobsphy(phy,mu=mu,sigma=sigma,model='relaxedgamma')
  } else {
    alpha=5
    mu=5
    sigma=(ind-100)/100*10
    phy=simcoaltree(dates,alpha=alpha)
    #obsphy=simobsphy(phy,mu=mu,sigma=sigma,model='carc')
  }
  #write.tree(obsphy,'simu.nwk',append = T)
  truths=rbind(truths,phy$root.time)
}
write.table(truths,'tmrca.tab',row.names = F,col.names = F)
