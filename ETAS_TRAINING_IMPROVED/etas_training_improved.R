############################################################
# etas_training_improve.R, rewrite by Qi Zhang             #
# Changes                                                  #
################## 1st improvements ########################
# 1. Clean up unnecessary code. Replace the original       #
#    "placeholder"+"for loop" with sapply functions.       #
# 2. Rewrite the internal loop of error estimate in the    #
#    Mag-dependent automatic model to improve run time     # 
#    performance.                                          #
# 3. Re-organize the code and add comments to make it more #
#    readable.                                             #
# 4. Add grids to the plot to make it more readable.       #
################## 2nd improvements ########################
# 5. Rewrite the etas.CI function to improve run-time      #
#    performance by reducing duplicated power operations.  #
############################################################

# Load test catalog
socal.dat = read.table("socal.txt",header=T)
times     = socal.dat$t
mags      = socal.dat$mg

# Specify period of training and test
start      = ISOdate(1984,1,1,0,0,0)
test.start = ISOdate(2004,6,18,0,0,0)
finish     = ISOdate(2010,1,1,0,0,0)
# Specify the training period
training.period = as.numeric(test.start - start     )        
# Specify the test period
test.period     = as.numeric(finish     - test.start)        
# Specify the overall period
period          = as.numeric(finish     - start     )        

# Re-Definition of the CI function, improve run-time performance by reducing duplicated power operations
etas.CI2 <- function(time_vec, events_idx, m0, mu, K, alpha, c, p) {
  min_mag=min(c(mags,m0))
  mag_power=rep(0,(max(mags)-min_mag)*100+1)
  for(i in unique(sort(mags[1:max(events_idx)]))-min_mag) {
    mag_power[100*i+1.5]=10^(alpha*(i+min_mag-m0))
  }
  sapply(c(1:length(time_vec)), function(x) {
    mu+K*sum(mag_power[100*(mags[1:events_idx[x]]-min_mag)+1.5]/
             (time_vec[x]-times[1:events_idx[x]]+c)^p)
  })
}

# Start from the 2nd day to the end of the training period.
timelist=2:training.period  

# Get number of events within the training period
n.training = sum(times<training.period)

# Get number of events in the training period for each day.
n.events = sapply(timelist,function(x){sum(times<x)}) 

# Calculate the Error for Temporal ETAS
CI.dist=etas.CI2(timelist, n.events[1:length(timelist)], 
                 m0=3,mu=.1687,K=.04225,alpha=1.034/log(10),c=.01922,p=1.222)
# Find values at event times
CI.list=etas.CI2(times[1+c(1:n.training)], c(1:n.training), 
                 m0=3,mu=.1687,K=.04225,alpha=1.034/log(10),c=.01922,p=1.222)
nu.CI=sort(sapply(CI.dist,function(x){mean(CI.list<x)}),decreasing=T)

# Calculate the Error for Space-time ETAS
# Estimates parameter for Space-time ETAS calculation
mu.hat = .329505837595229
K.hat = .0224702963795154
alpha.hat = 1.5839343640414
c.hat = .037651249192514
p.hat = 1.38508560377488

CI2.dist=etas.CI2(timelist, n.events[1:length(timelist)], 
                  m0=3,mu=mu.hat,K=K.hat,alpha=alpha.hat/log(10),c=c.hat,p=p.hat)
# Find values at event times
CI2.list=etas.CI2(times[1+c(1:n.training)], c(1:n.training), 
                  m0=3,mu=mu.hat,K=K.hat,alpha=alpha.hat/log(10),c=c.hat,p=p.hat)
nu.CI2=sort(sapply(CI2.dist,function(x){mean(CI2.list<x)}),decreasing=T)

# Calculate the Error for Automatic alarms
w.dist = timelist-times[n.events]
w.list=diff(times[1:n.training])
nu.auto=sort(sapply(w.dist,function(x){mean(w.list>x)}),decreasing=T)

# Calculate the Error for Mag-dependent automatic
# Improve run time performance by avoiding calculation of 5.8^mags in each loop.
power_mags=5.8^mags
w58.dist=sapply(c(1:length(timelist)), function(x) {
  min((timelist[x]-times[1:n.events[x]])/(power_mags[1:n.events[x]]))
})

w58.list=sapply(c(1:n.training), function(x) {
  min((times[1+x]-times[1:x])/(power_mags[1:x]))
})

nu.w58=sort(sapply(w58.dist,function(x){mean(w58.list>x)}),decreasing=T)

# Plot the Error diagrams
xx=seq(3,length(timelist),10)
yy=length(xx)

plot(seq(0,1,length.out=yy),nu.CI2[xx],col="green",lty=2,
     xlab=expression(tau),ylab=expression(nu),type="l",
     main="Training Set Error Diagrams",ylim=c(0,1))
lines(seq(0,1,length.out=yy),nu.CI[xx],col="blue",lty=3)
lines(seq(0,1,length.out=yy),nu.auto[xx],col="red",lty=4)
lines(seq(0,1,length.out=yy),nu.w58[xx],col="black",lty=1)
abline(v=seq(0,1,0.2), lty="dotted")
abline(h=seq(0,1,0.2), lty="dotted")
legend(.6,1,c("Temporal ETAS","Space-time ETAS","Automatic alarms",
               "Mag-dependent automatic"),
       lty=c(2,3,4,1),col=c("green","blue","red","black"))