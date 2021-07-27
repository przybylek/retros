# Zenodo
require(HH)

loadAndReorderSingleQuestionData <- function(path, newOrder, numberOfAllGames) {
  emptyRow <- c(0, 0, 0, 0, 0)
  games <- read.csv(path, sep=";", row.names = 1)
  while (nrow(games) < numberOfAllGames) {#append empty rows if needed
    games[nrow(games) + 1,] <- emptyRow 
  } 
  games <- games[newOrder,] #reorder
  return(games)
}

convertIntoArray <- function(gamesDF, rowNames, columnNames) {
  gamesMatrix <- data.matrix(gamesDF, rownames.force = NA)
  arrayDim <- c(length(rowNames), length(columnNames))
  gamesArray <- array(gamesMatrix, dim=arrayDim, dimnames = list(rowNames, columnNames) )
  return(gamesArray)
}

generateChart <- function(likertArray, xAxisDim, title, legend) {
  hasSubplots <- if( length(dim(likertArray)) == 3 ) TRUE else FALSE  
  bottomPadding <- if(legend) -0.5 else -9.5  
  auto.key <- list(cex = 1.2)
  scales <- list(
    y = list(cex = 1.0), 
    x = list(cex = 1.0)
  )
  par.settings <- list(
    layout.heights = list(key.axis.padding=0, top.padding=0, bottom.padding=bottomPadding),
    layout.widths = list(key.right=1.5, right.padding=1.5)
  )
  if(hasSubplots) {
    numberOfSubplots <- dim(likertArray)[3]
    img <- likert(likertArray, layout=c(numberOfSubplots,1), as.percent=FALSE, xlim=xAxisDim, xlab=NULL, main=title, auto.key = auto.key, scales = scales, par.settings = par.settings)
  } else {
    img <- likert(likertArray, as.percent=TRUE, xlab=NULL, main=title, scales = scales, auto.key = auto.key, par.settings = par.settings)
  }  
  return(img)
}


baseDir = file.path("d:", "Workspace", "retros")
setwd(baseDir)

datasetsDir = file.path(baseDir, "datasets")
outDir = file.path(baseDir, "out")

teamNames <- c("OKE_A", "OKE_B", "Dyna_A", "Dyna_B", "Senti_A", "Senti_B")
numberOfTeams <- length(teamNames)

orderOKE <- c(1, 2, 6, 3, 4, 5, 7) 
orderDynatraceA <- c(1, 2, 3, 5, 4, 6, 7)
orderDynatraceB <- c(1, 2, 3, 4, 5, 6, 7)
orderSentiOne <- c(1, 2, 3, 4, 5, 6, 7)

teams <- list(
  list(teamName="OKE_A", gameOrder=orderOKE),
  list(teamName="OKE_B", gameOrder=orderOKE),
  list(teamName="Dyna_A", gameOrder=orderDynatraceA),
  list(teamName="Dyna_B", gameOrder=orderDynatraceB),
  list(teamName="Senti_A", gameOrder=orderSentiOne),
  list(teamName="Senti_B", gameOrder=orderSentiOne)
)

# gameOrder <- list(orderOKE, orderOKE, orderDynatraceA, orderDynatraceB, orderSentiOne, orderSentiOne)
# teams <- dict(items = gameOrder, keys = teamNames)

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

column.names <- c("Strongly Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree","Strongly Agree")
likertLevels <- length(column.names)

row.names <- c("Starfish", "Sailboat", "Mad/Sad/Glad", "Mood++", "5L's", "360 Degrees", "Mountain climbing") #after reordering
numberOfGames <- length(row.names)


plotDim <- c(numberOfGames, likertLevels, numberOfTeams)

xAxisDim <- c(-100, 100)
xAxisDim <- c(-9, 9)
title <- ""


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
  likertPlotArrays <- c()
  teamResultsCombined <- NULL # counts for each game and each question
  for(team in teams) {  
    csvFilePath <- file.path(baseDir, "datasets", team$teamName, csvFile)
    games <-loadAndReorderSingleQuestionData(csvFilePath, team$gameOrder, numberOfGames)
    t_array <- convertIntoArray(games, row.names, column.names)
    t <- apply( t_array, MARGIN=1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) ) # calculate the average grade for each game (row)
    # Setting 1 as parameter of the MARGIN argument means that we apply a function to every row of an array
    
    likertPlotArrays <- c(likertPlotArrays, t_array)
    teamResultsCombined <- if( is.null(teamResultsCombined) ) t_array else teamResultsCombined+t_array
  }
   
  # teamResultsCombined <- t1_array + t2_array + t3_array + t4_array + t5_array + t6_array

  likertPlot <- array(likertPlotArrays, dim = plotDim, dimnames = list(row.names,column.names,teamNames))
  
  #Delete the 7th row ("Mountain climbing") from the array
  likertPlot <- likertPlot[-7,,]

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

  games1 <-loadAndReorderSingleQuestionData(myPath1, orderOKE, numberOfGames)
  games2 <-loadAndReorderSingleQuestionData(myPath2, orderOKE, numberOfGames)
  games3 <-loadAndReorderSingleQuestionData(myPath3, orderDynatraceA, numberOfGames)
  games4 <-loadAndReorderSingleQuestionData(myPath4, orderDynatraceB, numberOfGames)
  games5 <-loadAndReorderSingleQuestionData(myPath5, orderSentiOne, numberOfGames)
  games6 <-loadAndReorderSingleQuestionData(myPath6, orderSentiOne, numberOfGames)

  t1_array <- convertIntoArray(games1, row.names, column.names)
  t1 <- apply( t1_array, MARGIN=1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) ) # calculate the average grade for each game (row)
  # Setting 1 as parameter of the MARGIN argument means that we apply a function to every row of an array
  
  t2_array <- convertIntoArray(games2, row.names, column.names)
  t2 <- apply( t2_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  t3_array <- convertIntoArray(games3, row.names, column.names)
  t3 <- apply( t3_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  t4_array <- convertIntoArray(games4, row.names, column.names)
  t4 <- apply( t4_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  t5_array <- convertIntoArray(games5, row.names, column.names)
  t5 <- apply( t5_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )

  t6_array <- convertIntoArray(games6, row.names, column.names)
  t6 <- apply( t6_array, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )


  #Delete the 7th row ("Mountain climbing") from the array
  # teamResultsCombined <- teamResultsCombined[-7,]

  i <- substr(csvFile, 2, nchar(csvFile)-4)
  title = paste(q_short[strtoi(i)], ". ", q[strtoi(i)], sep="")

  # averages for each game
  t_All <- apply( teamResultsCombined, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )
  gamesAVG[strtoi(i),] <- t_All

  t_positive <- apply( teamResultsCombined, 1, function(x) sum( x*c(0, 0, 0, 1, 1)/sum(x) ) ) # positive opinions / all opinions
  gamesPositiveRatio[strtoi(i),] <- t_positive

  t_negative <- apply( teamResultsCombined, 1, function(x) sum( x*c(1, 1, 0, 0, 0)/sum(x) ) ) # negative opinions / all opinions
  gamesNegativeRatio[strtoi(i),] <- t_negative




  ########## Generating csv files with averages ###########
  avgMatrix <- cbind(t1, t2, t3, t4, t5, t6, t_All)
  colnames(avgMatrix) <- c(teamNames, "ALL")

  #Delete the 7th row ("Mountain climbing") from the array
  avgMatrix <- avgMatrix[-7,]
  write.csv(avgMatrix, file = csvOutFile, na="")

  
  img <- generateChart(likertPlot, xAxisDim, title, legend=FALSE)
  imgGamesMerged <- generateChart(teamResultsCombined, xAxisDim, title, legend=FALSE)
  
  print(img)
  dev.off()
}

xAxisDim <- c(-5, 8) #q1, q4
xAxisDim <- c(-7, 7) #q2, q3, q6
xAxisDim <- c(-8, 8) #q5
xAxisDim <- c(-3, 9) #q7


