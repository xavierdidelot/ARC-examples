#devtools::install_github('emvolz-phylodynamics/treedater-dev',ref='xdidelotAdditiveModel0')
rm(list=ls())
set.seed(0)

library( treedater  )
library( magrittr )
library( ape )
library( Hmisc )

tmrcas <- read.table( 'tmrca.tab' )[,1]
tres <- read.tree( 'simu.nwk' )

sts <- seq( 2010 , 2020, by = .1 ) %>% setNames(., as.character(1:101))

arctds <- lapply( 1:length(tres), function(itre ){
	tr <- tres[[itre]]
	#tr$edge.length <- tr$edge.length / 1000 
	dater( unroot(tr), sts = sts, s = 1e4 , clock = 'additive', quiet = FALSE , ncpu = 8)
})
#~ plot( tmrcas, sapply( arctds, '[[', 'timeOfMRCA' ), ylim = range( tmrcas ) )

# par bootstrap 
ind=0
ptm <- proc.time()
arcpbs <- lapply( arctds, function(td) tryCatch( {
  ind<<-ind+1
    done=F
    while (done==F) {
    R.utils::withTimeout({
    print(sprintf("This is index %d !",ind))
    pb = parboot( td, ncpu = 8,  overrideTempConstraint=F , overrideSearchRoot=F,quiet=F)
    done=T},onTimeout='silent',timeout=600)
    }
		print( pb )
		list( tmrca  = c( pb$td$timeOf, pb$timeOfMRCA_CI )
		 , meanrate = c( pb$td$mean.rate , pb$meanRate_CI )
		 , sp = c( pb$td$sp, sapply( pb$trees, '[[', 'sp') %>% quantile( ., c(.025, .975)) )
		 )
	 }, error = function(e) { print( 'Oh noes, got an Error');  NA}  )
) #  overrideTempConstraint? normal? 
print(proc.time() - ptm)

# arc results 
arc_tmrca_resid <- matrix( NA, nrow = 100, ncol = 3)
arc_meanrate <- matrix( NA, nrow = 100, ncol = 3)
arc_sp <- matrix( NA, nrow = 100, ncol = 3)

for ( j in 1:100){
	if ( !is.na( arcpbs[[j]][1] ) ){
		arc_tmrca_resid[j, ] <- arcpbs[[j]]$tmrca - tmrcas[j]
				
		arc_meanrate[j, ] <- arcpbs[[j]]$meanrate*10000 
		
		arc_sp[j, ] <- arcpbs[[j]]$sp
	}
}

pdf('treedater-arc-simunwk.pdf', width = 4.5, height = 9)
par( mfrow = c(3, 1 ))
errbar (1:100, arc_tmrca_resid[,1],  arc_tmrca_resid[, 3], arc_tmrca_resid[,2] , ylim = c(-25, 25 ) 
 , xlab = 'Simulation number'
 , ylab = 'TMRCA residual'); abline ( h = 0)
#~ X11();
 errbar( 1:100, arc_meanrate[, 1], arc_meanrate[,3], arc_meanrate[,2], ylim = c( 0,10 ) 
 , xlab = 'Simulation number' 
 , ylab= expression(paste('Mean clock rate ',mu) )); abline ( h = 5 )
#~ X11(); 
errbar( 1:100, arc_sp[, 1], arc_sp[,3], arc_sp[,2] , ylim = c(0, 15)
	, xlab = 'Simulation number'
	, ylab = expression(paste('Relaxation parameter ',omega))); abline ( a =0 , b= 8 / 100  )
dev.off()
system('open treedater-arc-simunwk.pdf')


