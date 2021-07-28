# Zenodo
# https://adv-r.hadley.nz/r6.html
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

datasetsDir = file.path(baseDir, "datasets") #subdirectories in "datasets" must be the same as teamNames
outDir = file.path(baseDir, "out")

teamNames <- c("OKE_A", "OKE_B", "Dyna_A", "Dyna_B", "Senti_A", "Senti_B")

gameNames <- c("Starfish", "Sailboat", "Mad/Sad/Glad", "Mood++", "5L's", "360 Degrees", "Mountain climbing") #after reordering
numberOfGames <- length(gameNames)

#order of the games in the csv files according to gameNames
gameOrderOKE <- c(1, 2, 6, 3, 4, 5, 7) 
gameOrderDynatraceA <- c(1, 2, 3, 5, 4, 6, 7)
gameOrderDynatraceB <- c(1, 2, 3, 4, 5, 6, 7)
gameOrderSentiOne <- c(1, 2, 3, 4, 5, 6, 7)

teams <- list(
  list(teamName="OKE_A", gameOrder=gameOrderOKE),
  list(teamName="OKE_B", gameOrder=gameOrderOKE),
  list(teamName="Dyna_A", gameOrder=gameOrderDynatraceA),
  list(teamName="Dyna_B", gameOrder=gameOrderDynatraceB),
  list(teamName="Senti_A", gameOrder=gameOrderSentiOne),
  list(teamName="Senti_B", gameOrder=gameOrderSentiOne)
)

questions <- list(
  Q1="The game produces better results than the standard approach",
  Q2="The game should be permanently adopted by your team",
  Q3="The game fosters participants' creativity",
  Q4="The game fosters participants' motivation and involvement",
  Q5="The game improves communication among the team members",
  Q6="The game makes participants more willing to attend the meeting",
  Q7="The game is easy to understand and play"
)

q_short <- names(questions)

# myDir1 = file.path(datasetsDir, "OKE_A")


column.names <- c("Strongly Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree","Strongly Agree")
likertLevels <- length(column.names)



plotDim <- c(numberOfGames, likertLevels, length(teams))

xAxisDim <- c(-100, 100)
xAxisDim <- c(-9, 9)
title <- ""


#gamesAVG and gamesPositiveRatio agregate data for RadarChart
gamesAVG <- matrix(nrow = length(questions), ncol = length(gameNames))
gamesPositiveRatio <- matrix(nrow = length(questions), ncol = length(gameNames))
gamesNegativeRatio <- matrix(nrow = length(questions), ncol = length(gameNames))

rownames(gamesAVG) <- names(questions)
colnames(gamesAVG) <- gameNames

rownames(gamesPositiveRatio) <- names(questions)
colnames(gamesPositiveRatio) <- gameNames
rownames(gamesNegativeRatio) <- names(questions)
colnames(gamesNegativeRatio) <- gameNames

for (i in seq(1, length(questions))) {
  question <- questions[i]
  qId <- names(question)
  csvFileName = paste(tolower(qId), "csv", sep=".") 

  gamesArrayAllTeams <- c()
  teamResultsCombined <- NULL
  gamesAvgArrayAllTeams <- NULL
  for(team in teams) {  
    csvFilePath <- file.path(baseDir, "datasets", team$teamName, csvFileName)
    gamesDF <-loadAndReorderSingleQuestionData(csvFilePath, team$gameOrder, numberOfGames)
    gamesArray <- convertIntoArray(gamesDF, gameNames, column.names)       
    gamesArrayAllTeams <- c(gamesArrayAllTeams, gamesArray)
    teamResultsCombined <- if( is.null(teamResultsCombined) ) gamesArray else teamResultsCombined+gamesArray
    gamesAvgArray <- apply( gamesArray, MARGIN=1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) ) # calculate the average grade for each game (row)
    # Setting 1 as parameter of the MARGIN argument means that we apply a function to every row of an array
    gamesAvgArrayAllTeams <- if( is.null(gamesAvgArrayAllTeams) ) gamesAvgArray else cbind(gamesAvgArrayAllTeams, gamesAvgArray)
  }
  
  likertPlot <- array(gamesArrayAllTeams, dim = plotDim, dimnames = list(gameNames,column.names,teamNames))
  
  #Delete the 7th row ("Mountain climbing") from the array
  likertPlot <- likertPlot[-7,,]
  # teamResultsCombined <- teamResultsCombined[-7,]

  title = paste(qId, question, sep=". ")

  imgLikert <- generateChart(likertPlot, xAxisDim, title, legend=FALSE)
  # imgTeamResultsCombined <- generateChart(teamResultsCombined, xAxisDim, title, legend=FALSE)

  ########## Generating png file with Likert charts ###########
  pngFileName = paste(tolower(qId), "png", sep=".")
  pngFilePath <- file.path(outDir, pngFileName)    
  png(pngFilePath, width = 1200, height = 250) #800x300
  print(imgLikert)
  dev.off()


  # averages for each game
  t_All <- apply( teamResultsCombined, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )
  gamesAVG[strtoi(i),] <- t_All

  t_positive <- apply( teamResultsCombined, 1, function(x) sum( x*c(0, 0, 0, 1, 1)/sum(x) ) ) # positive opinions / all opinions
  gamesPositiveRatio[strtoi(i),] <- t_positive

  t_negative <- apply( teamResultsCombined, 1, function(x) sum( x*c(1, 1, 0, 0, 0)/sum(x) ) ) # negative opinions / all opinions
  gamesNegativeRatio[strtoi(i),] <- t_negative


  ########## Generating csv file with averages ###########
  csvOutFilePath = file.path(outDir, csvFileName)

  gamesAvgArrayAllTeams <- cbind(gamesAvgArrayAllTeams, t_All)
  colnames(gamesAvgArrayAllTeams) <- c(teamNames, "ALL")

  #Delete the 7th row ("Mountain climbing") from the array
  gamesAvgArrayAllTeams <- gamesAvgArrayAllTeams[-7,]
  write.csv(gamesAvgArrayAllTeams, file = csvOutFilePath, na="")

}

xAxisDim <- c(-5, 8) #q1, q4
xAxisDim <- c(-7, 7) #q2, q3, q6
xAxisDim <- c(-8, 8) #q5
xAxisDim <- c(-3, 9) #q7


