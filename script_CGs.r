# Zenodo
require(HH)

baseDir = file.path("d:", "Workspace", "retros")
setwd(baseDir)

datasetsDir = file.path(baseDir, "datasets")
outDir = file.path(baseDir, "out")


myDir1 = file.path(datasetsDir, "OKE_A")
myDir2 = file.path(datasetsDir, "OKE_B")

myDir3 = file.path(datasetsDir, "Dyna_A")
myDir4 = file.path(datasetsDir, "Dyna_B")

myDir5 = file.path(datasetsDir, "Senti_A")
myDir6 = file.path(datasetsDir, "Senti_B")

q <- c("The game produces better results than the standard approach",
       "The game should be permanently adopted by your team",
       "The game fosters participants' creativity",
       "The game fosters participants' motivation and involvement",
       "The game improves communication among the team members",
       "The game makes participants more willing to attend the meeting",
       "The game is easy to understand and play"
)
q_short <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7") 

matrix.names <- c("OKE_A", "OKE_B", "Dyna_A", "Dyna_B", "Senti_A", "Senti_B")
row.names <- c("Starfish", "Sailboat", "Mad/Sad/Glad", "Mood++", "5L's", "360 Degrees", "Mountain climbing") #after reordering

orderOKE <- c(1, 2, 6, 3, 4, 5, 7) 
orderDynatraceA <- c(1, 2, 3, 5, 4, 6, 7)
orderDynatraceB <- c(1, 2, 3, 4, 5, 6, 7)
orderSentiOne <- c(1, 2, 3, 4, 5, 6, 7)

emptyRow <- c(0, 0, 0, 0, 0)

column.names <- c("Strongly Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree","Strongly Agree")

numberOfTeams <- length(matrix.names)
numberOfGames <- length(row.names)
likertLevels <- length(column.names)
evaluationDim <- c(numberOfGames, likertLevels)
plotDim <- c(numberOfGames, likertLevels, numberOfTeams)

xAxisDim <- c(-100, 100)
xAxisDim <- c(-9, 9)
title <- ""
withoutLegend <- -9.5
withLegend <- -0.5

#gamesAVG and gamesPositiveRatio agregate data for RadarChart
gamesAVG <- matrix(nrow = length(q_short), ncol = length(row.names))
gamesPositiveRatio <- matrix(nrow = length(q_short), ncol = length(row.names))
gamesNegativeRatio <- matrix(nrow = length(q_short), ncol = length(row.names))

rownames(gamesAVG) <- q_short
colnames(gamesAVG) <- row.names

rownames(gamesPositiveRatio) <- q_short
colnames(gamesPositiveRatio) <- row.names
rownames(gamesNegativeRatio) <- q_short
colnames(gamesNegativeRatio) <- row.names



for (csvFile in list.files(path=myDir1, pattern = "\\.csv$")) {
  myPath1 = paste(myDir1, csvFile, sep="/") 
  myPath2 = paste(myDir2, csvFile, sep="/") 
  myPath3 = paste(myDir3, csvFile, sep="/") 
  myPath4 = paste(myDir4, csvFile, sep="/") 
  myPath5 = paste(myDir5, csvFile, sep="/") 
  myPath6 = paste(myDir6, csvFile, sep="/")

  outPath = paste(outDir, substr(csvFile, 1, nchar(csvFile)-4), sep="/") 
  pngFile = paste(outPath, "png", sep=".") 
  csvOutFile = paste(outPath, "csv", sep=".") 
  png(pngFile, width = 1200, height = 250) #800x300

  games1 <- read.csv(myPath1, sep=";", row.names = 1)
  games1[nrow(games1) + 1,] <- emptyRow #append an empty row
  games1[nrow(games1) + 1,] <- emptyRow #append an empty row
  games1 <- games1[orderOKE,] #reorder
  games2 <- read.csv(myPath2, sep=";", row.names = 1)
  games2[nrow(games2) + 1,] <- emptyRow #append an empty row
  games2[nrow(games2) + 1,] <- emptyRow #append an empty row
  games2 <- games2[orderOKE,] #reorder

  games3 <- read.csv(myPath3, sep=";", row.names = 1)
  games3[nrow(games3) + 1,] <- emptyRow #append an empty row
  games3 <- games3[orderDynatraceA,] #reorder
  games4 <- read.csv(myPath4, sep=";", row.names = 1)
  games4[nrow(games4) + 1,] <- emptyRow #append an empty row
  games4 <- games4[orderDynatraceB,] #reorder

  games5 <- read.csv(myPath5, sep=";", row.names = 1)
  games5 <- games5[orderSentiOne,] #reorder
  games6 <- read.csv(myPath6, sep=";", row.names = 1)
  games6 <- games6[orderSentiOne,] #reorder

  m = data.matrix(games1, rownames.force = NA)
  t1_array <- array(m, dim=evaluationDim, dimnames = list(row.names,column.names) )
  t1 <- apply( t1_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  m = data.matrix(games2, rownames.force = NA)
  t2_array <- array(m, dim=evaluationDim, dimnames = list(row.names,column.names) )
  t2 <- apply( t2_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  m = data.matrix(games3, rownames.force = NA)
  t3_array <- array(m, dim=evaluationDim, dimnames = list(row.names,column.names) )
  t3 <- apply( t3_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  m = data.matrix(games4, rownames.force = NA)
  t4_array <- array(m, dim=evaluationDim, dimnames = list(row.names,column.names) )
  t4 <- apply( t4_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  m = data.matrix(games5, rownames.force = NA)
  t5_array <- array(m, dim=evaluationDim, dimnames = list(row.names,column.names) )
  t5 <- apply( t5_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  m = data.matrix(games6, rownames.force = NA)
  t6_array <- array(m, dim=evaluationDim, dimnames = list(row.names,column.names) )
  t6 <- apply( t6_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

########## temporary
########## t1_array[4,] <- emptyRow
########## temporary

  # counts for each game and each question 
  allGames_array <- t1_array + t2_array + t3_array + t4_array + t5_array + t6_array

  #Delete the 7th row ("Mountain climbing") from the array
  # allGames_array <- allGames_array[-7,]

  i <- substr(csvFile, 2, nchar(csvFile)-4)
  title = paste(q_short[strtoi(i)], ". ", q[strtoi(i)], sep="")

  # averages for each game
  t_All <- apply( allGames_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )
  gamesAVG[strtoi(i),] <- t_All

  t_positive <- apply( allGames_array, 1, function(x) sum( x*c(0, 0, 0, 1, 1)/sum(x) ) ) # positive opinions / all opinions
  gamesPositiveRatio[strtoi(i),] <- t_positive

  t_negative <- apply( allGames_array, 1, function(x) sum( x*c(1, 1, 0, 0, 0)/sum(x) ) ) # negative opinions / all opinions
  gamesNegativeRatio[strtoi(i),] <- t_negative




  ########## Generating csv files with averages ###########
  avgMatrix <- cbind(t1, t2, t3, t4, t5, t6, t_All)
  colnames(avgMatrix) <- c(matrix.names, "ALL")

  #Delete the 7th row ("Mountain climbing") from the array
  avgMatrix <- avgMatrix[-7,]
  write.csv(avgMatrix, file = csvOutFile, na="")


  likertPlot <- array(c(t1_array,t2_array,t3_array,t4_array,t5_array,t6_array), dim = plotDim, dimnames = list(row.names,column.names,matrix.names))

  #Delete the 7th row ("Mountain climbing") from the array
  likertPlot <- likertPlot[-7,,]

  img <- likert(likertPlot, layout=c(6,1), as.percent=FALSE, xlim=xAxisDim, xlab=NULL, main=title, scales = list(y = list(cex = 1.0), x = list(cex = 1.0)), auto.key=list(cex = 1.2),
    par.settings=list(
      layout.heights=list(key.axis.padding=0, top.padding=0, bottom.padding=withoutLegend),
      layout.widths=list(key.right=1.5, right.padding=1.5)
    )
  )

  img2 <- likert(allGames_array, as.percent=TRUE, xlab=NULL, main=title, scales = list(y = list(cex = 1.0), x = list(cex = 1.0)), auto.key=list(cex = 1.2),
    par.settings=list(
      layout.heights=list(key.axis.padding=0, top.padding=0, bottom.padding=withoutLegend),
      layout.widths=list(key.right=1.5, right.padding=1.5)
    )
  )

  print(img)
  dev.off()
}

xAxisDim <- c(-5, 8) #q1, q4
xAxisDim <- c(-7, 7) #q2, q3, q6
xAxisDim <- c(-8, 8) #q5
xAxisDim <- c(-3, 9) #q7


