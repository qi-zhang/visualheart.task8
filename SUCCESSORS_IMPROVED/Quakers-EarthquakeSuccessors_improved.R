#######################################################################################
# This file contains code for plotting aftershock interval .vs. mainshock magnitude.  #
# Please download datafile DataFrame.csv from following URL to your working directory #
# https://www.dropbox.com/s/tzx4qqxhh9u9iz2/DataFrame.csv                             #
# prior to running the code. The code will generate 3 plots in current directory,     #
# based on information from the data loaded. they are:                                #
# 1. successorsALL.jpeg  : aftershock intervals of all quakes                         #
# 2. successorsMean.jpeg : mean aftershock intervals of quakes of identical magnitude #
# 3. successorsBinp1.jpeg: mean aftershock intervals of quakes of similar magnitude   #                                                             #
# It takes a couple of minutes to generate the first plot, and the delay depends on   #
# available resource of your computer. It takes much shortter time to plot the other  #
# two plots. (Comments added by Qi Zhang)                                             #
#######################################################################################

# Install & load library if necessary
# install.packages("scales")
library(scales)

# Load data
DataFrame = read.csv(file="DataFrame.csv", header =T)

# Extract information needed in following code
mags=DataFrame$magnitude
times=DataFrame$time

## Plot the intevals between shocks vs magnitude of the first shock
jpeg(file="successorsALL.jpeg",1400,800)
cols=c("pink","deeppink",'red',"deeppink4","blue")

# Add title to the plot and tune the y-lab to make it straight forward (by Qi Zhang).
plot(mags,rep(0,length.out=length(mags)),pch=20, ylim=c(0,max(diff(times,lag=5))),
     xlim=c(0,8),ylab="Interval",xlab="Magnitude", 
     main="Aftershock Intervals vs. Magnitude of the Mainshock")

# Plot interval for aftershocks
for (i in 1:length(mags)){
  # Re-write the internal loop to fix incorrect indexing (by Qi Zhang)
  max_j=min(c(5,length(mags)-i))
  if ( max_j >= 1 ) {
    for ( j in 1:max_j ) {
      points(mags[i],times[i+j]-times[i],pch=20,col=alpha(cols[j],0.5))
    }    
  }
}

# Update legend to make it easier to understand (by Qi Zhang)
legend(6.5,max(diff(times,lag=5)),c("Interval of the 1st Aftershock","Interval of the 2nd Aftershock",
                                    "Interval of the 3rd Aftershock","Interval of the 4th Aftershock",
                                    "Interval of the 5th Aftershock"),pch=20,col=cols)
dev.off()

## Plot the mean of intervals between shocks vs magnitude of the first shock
maglist=sort(unique(mags))

jpeg(file="successorsMean.jpeg",1400,800)
cols=c("pink","deeppink",'red',"deeppink4","blue")

# Modified by Qi Zhang to add title to the plot and tune the y-lab to make it straight forward.
plot(maglist,rep(0,length.out=length(maglist)),pch=20,ylim=c(0,10),xlim=c(0,8),
     ylab="Interval",xlab="Magnitude",
     main="Mean of Aftershock Intervals of Mainshocks of Same Magnitude")

# Plot interval for aftershocks
for (i in 1:length(maglist)){
  # Re-write the internal loop to fix incorrect indexing (by Qi Zhang)
  matched=which(mags==maglist[i])
  for(j in 1:5) {
    adjusted_matched=matched[matched<(length(mags)-j)]
    points(maglist[i],mean(times[adjusted_matched+j]-times[adjusted_matched]),pch=19,col=cols[j])    
  }
}
# Update legend to make it easier to understand (by Qi Zhang)
legend(6.5,10,c("Interval of the 1st Aftershock","Interval of the 2nd Aftershock",
                "Interval of the 3rd Aftershock","Interval of the 4th Aftershock",
                "Interval of the 5th Aftershock"),pch=20,col=cols)
dev.off()
###bin0.1
maglist=seq(min(mags),max(mags)+0.1,0.1)

jpeg(file="successorsBinp1.jpeg",1400,800)
cols=c("pink","deeppink",'red',"deeppink4","blue")

# Modified by Qi Zhang to add title to the plot and tune the y-lab to make it straight forward.
plot(maglist,rep(0,length.out=length(maglist)),pch=20,ylim=c(0,3),xlim=c(0,8),
     ylab="Time",xlab="Magnitude",
     main="Mean of Aftershock Intervals of Mainshocks of Similar Magnitude")

# Plot interval for aftershocks
for (i in 1:length(maglist)){
  # Re-write the internal loop to fix incorrect indexing (by Qi Zhang)
  matched=which(mags>=maglist[i] & mags<=maglist[i+1])
  for(j in 1:5) {
    adjusted_matched=matched[matched<(length(mags)-j)]
    points(maglist[i],mean(times[adjusted_matched+j]-times[adjusted_matched]),pch=19,col=cols[j])    
  }
}

# Update legend to make it easier to understand (by Qi Zhang)
legend(6.5,3,c("Interval of the 1st Aftershock","Interval of the 2nd Aftershock",
               "Interval of the 3rd Aftershock","Interval of the 4th Aftershock",
               "Interval of the 5th Aftershock"),pch=20,col=cols)
dev.off()