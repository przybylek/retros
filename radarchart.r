https://www.r-bloggers.com/2014/11/an-xpd-tion-into-r-plot-margins/

https://rdrr.io/cran/fmsb/man/radarchart.html
https://www.r-graph-gallery.com/142-basic-radar-chart.html
https://www.r-graph-gallery.com/143-spider-chart-with-saveral-individuals.html

library(fmsb)

par(mar=c(1, 0, 1, 0), xpd=TRUE) #bottom, left, top and right margins 
layout(matrix(1:6, ncol=3)) #draw 6 plots to device

# dev.off() i zamknaæ Rstudio!!!

gameAvg_DF <- as.data.frame( t(gamesAVG) )
for(i in 1:6) {
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    # data <- rbind(rep(5,7), rep(1,7), gameAvg_DF[i,]) 
    data <- rbind(rep(4,7), rep(2.5,7), gameAvg_DF[i,]) 
    #data <- data[,1:6] #without Q7
    data <- data[,c("Q1", "Q3", "Q4", "Q5", "Q6")] #without Q2 and Q7
    radarchart(data , title=row.names[[i]], seg=3, caxislabels=seq(2.5,4,0.5), axistype=1, calcex=0.9, axislabcol="grey30", plwd=3, vlcex=1.3 )
}

gamePositive_DF <- as.data.frame( t(gamesPositiveRatio) )
for(i in 1:6) {
    data <- rbind(rep(0.7,7), rep(0,7), gamePositive_DF[i,]) 
    #data <- data[,1:6] #without Q7
    data <- data[,c("Q1", "Q3", "Q4", "Q5", "Q6")] #without Q2 and Q7
    radarchart(data , title=row.names[[i]], seg=3, caxislabels=seq(0.1,0.7,0.2), axistype=1, calcex=0.9, axislabcol="grey30", plwd=3, vlcex=1.3 )
}

gameNegative_DF <- as.data.frame( t(gamesNegativeRatio) )
for(i in 1:6) {
    data <- rbind(rep(0.7,7), rep(0,7), gameNegative_DF[i,]) 
    #data <- data[,1:6] #without Q7
    data <- data[,c("Q1", "Q3", "Q4", "Q5", "Q6")] #without Q2 and Q7
    radarchart(data , title=row.names[[i]], seg=3, caxislabels=seq(0.1,0.7,0.2), axistype=1, calcex=0.9, axislabcol="grey30", plwd=3, vlcex=1.3 )
}

# rowMeans(gameNegative_DF[,c("Q1", "Q3", "Q4", "Q5", "Q6", "Q7")])

gameNeutral_DF <- 1 - gamePositive_DF - gameNegative_DF

colors_border=c( rgb(0.0,0.0,1.0,0.9), rgb(1.0,0.0,0.0,0.9) )
colors_in=c( rgb(0.0,0.0,1.0,0.0), rgb(1.0,0.0,0.0,0.0) )

colors_border=c( "blue", "red")

for(i in 1:6) {
    data <- rbind(rep(0.7,7), rep(0,7), rbind(gamePositive_DF[i,], gameNegative_DF[i,]) ) 
    #data <- data[,1:6] #without Q7
    data <- data[,c("Q1", "Q3", "Q4", "Q5", "Q6")] #without Q2 and Q7
    radarchart(data , title=row.names[[i]], seg=3, caxislabels=seq(0.1,0.7,0.2), axistype=1, calcex=0.9, axislabcol="grey30", pty=c(16,1), plwd=2, plty=1, vlcex=1.3, pcol=colors_border , pfcol=colors_in  )
    if(i == 5) {
        legend("topright",inset=-0.1, legend=c("agree", "disagree"), col=c( "blue", "red"), pch=c(16,1), cex=1.05, box.lty=0)
    }
}


legend("topright",inset=-0.05, legend=c("agree", "disagree"), col=c( "blue", "red"), lty=c(1,1), cex=0.9, box.lty=0)



#Multi-group radar
layout(matrix(1:1, ncol=1))
data <- rbind(rep(3.5,7), rep(2.5,7), gameAvg_DF)
data <- data[1:7,1:6] #without Q7 and without 360 and MountainClimbing
radarchart(data , seg=2, caxislabels=seq(2.5,3.5,0.5), axistype=1, calcex=0.8, axislabcol="grey30" )




#######
colors_border=c( rgb(0.0,0.0,1.0,0.9), rgb(1.0,0.0,0.0,0.9), rgb(0.7,0.7,0.7,0.9) ) 

colors_in=c( rgb(0.0,0.0,1.0,0.0), rgb(1.0,0.0,0.0,0.0), rgb(0.7,0.7,0.7,0.0) )

colors_border=c( "blue", "red", "darkgrey")

for(i in 1:6) {
    data <- rbind(rep(0.7,7), rep(0,7), rbind(gamePositive_DF[i,], gameNegative_DF[i,], gameNeutral_DF[i,]) ) 
    #data <- data[,1:6] #without Q7
    data <- data[,c("Q1", "Q3", "Q4", "Q5", "Q6")] #without Q2 and Q7
    radarchart(data , title=row.names[[i]], seg=3, caxislabels=seq(0.1,0.7,0.2), axistype=1, calcex=0.9, axislabcol="grey30", pty=c(16,1,8), plwd=2, plty=c(1,1,0), vlcex=1.3, pcol=colors_border , pfcol=colors_in  )
    if(i == 5) {
        legend("topright",inset=c(-0.1,-0.17), legend=c("agree", "disagree", "neutral"), col=c( "blue", "red", "darkgrey"), pch=c(16,1,8), cex=1.05, box.lty=0, bty="n")
    }
}

# Use option bty = "n" in legend to remove the box around the legend.
