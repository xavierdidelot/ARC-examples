library(BactDating)
library(ape)
rm(list=setdiff(ls(),'ind'))
if (!exists('ind')) ind=101
set.seed(ind)
dates=seq(2010,2020,0.1)

if (ind<=100) {
  alpha=5
  mu=5
  sigma=ind/100*10
  phy=simcoaltree(dates,alpha=alpha)
  obsphy=simobsphy(phy,mu=mu,sigma=sigma,model='relaxedgamma')
} else {
  alpha=5
  mu=5
  sigma=(ind-100)/100*10
  phy=simcoaltree(dates,alpha=alpha)
#  obsphy=simobsphy(phy,mu=mu,sigma=sigma,model='carc')
  obsphy=phy
  obsphy$root.time=NULL
  for (i in 1:length(obsphy$edge.length)) {
    obsphy$edge.length[i]=rgamma(1,shape=mu*obsphy$edge.length[i]/sigma,scale=sigma)
  }
}

#create alignment using seq-gen, run phyml and load tree
nsites=100000
write.tree(obsphy,sprintf('tree%d.nwk',ind))
system(sprintf('seq-gen -l %d -s %f -m HKY -z %d < tree%d.nwk > seqs%d.nex',nsites,1/nsites,ind,ind,ind))
system(sprintf('phyml --no_memory_check -b 0 -c 1 -t 1 -q -i seqs%d.nex',ind))
input=read.tree(sprintf('seqs%d.nex_phyml_tree.txt',ind))
input$edge.length=input$edge.length*nsites

##obsphy=unroot(obsphy)
names(dates)=phy$tip.label
res1=bactdate(input,dates,showProgress=T,nbIts=1e6,model='negbin')#1e6
res2=bactdate(input,dates,showProgress=T,nbIts=1e6,model='arc')
save.image(sprintf('run%d.RData',ind))