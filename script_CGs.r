if(!require(HH)){
    install.packages("HH")
    library(HH)
}

loadAndReorderSingleQuestionData <- function(path, newOrder, numberOfAllGames) {
  # emptyRow <- c(0, 0, 0, 0, 0)
  games <- read.csv(path, sep=";", row.names = 1)
  emptyRow <- rep(0, ncol(games))
  while (nrow(games) < numberOfAllGames) {#append empty rows if needed
    games[nrow(games) + 1,] <- emptyRow 
  } 
  games <- games[newOrder,] #reorder
  return(games)
}

convertIntoArray <- function(gamesDF, gameNames, likertLevels) {
  gamesMatrix <- data.matrix(gamesDF, rownames.force = NA)
  dimNames <- list(gameNames, likertLevels)
  arrayDim <-  as.numeric( lapply(dimNames, length) )
  gamesArray <- array(gamesMatrix, dim=arrayDim, dimnames=dimNames)
  return(gamesArray)
}

removeLegend <- function(img) {  
  img$legend$bottom$args$text[] <- " "
  img$legend$bottom$args$rect[] <- 0
  if (!is.null(img$legend$bottom$args$title)) img$legend$bottom$args$title <- " "
  return(img)
}

generateChart <- function(likertArray, xAxisDim, title, legend=TRUE, bottom.padding=0, top.padding=0) {
  hasSubplots <- if( length(dim(likertArray)) == 3 ) TRUE else FALSE  
  auto.key <- list(cex = 0.9) # 1.2 for png
  scales <- list(
    y = list(cex = 1.0), 
    x = list(cex = 1.0)
  )
  par.settings <- list(
    layout.heights = list(key.axis.padding=-2, top.padding=top.padding, bottom.padding=bottom.padding),
    layout.widths = list(key.right=1.5, right.padding=1.5)
  )
  if(hasSubplots) {
    numberOfSubplots <- dim(likertArray)[3]
    img <- likert(likertArray, layout=c(numberOfSubplots,1), as.percent=FALSE, xlim=xAxisDim, xlab=NULL, main=title, auto.key = auto.key, scales = scales, par.settings = par.settings)
  } else {
    img <- likert(likertArray, as.percent=TRUE, xlab=NULL, main=title, scales = scales, auto.key = auto.key, par.settings = par.settings)
  }  
  if(!legend) img <- removeLegend(img)
  return(img)
}


baseDir = file.path("d:", "Workspace", "retros")
setwd(baseDir)

datasetsDir = file.path(baseDir, "datasets") #subdirectories in "datasets" must be the same as teamNames
outDir = file.path(baseDir, "out")

gameNames <- c("Starfish", "Sailboat", "Mad/Sad/Glad", "Mood++", "5L's", "360 Degrees", "Mountain climbing") #after reordering

#order of the games in the input csv files according to gameNames
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
teamNames <- unlist( lapply(teams,  function(t) {t[[1]]}) )
 
likertLevels <- c("Strongly Disagree", "Somewhat Disagree", "Neither Agree nor Disagree", "Somewhat Agree","Strongly Agree")

questions <- list(
  Q1="The game produces better results than the standard approach",
  Q2="The game should be permanently adopted by your team",
  Q3="The game fosters participants' creativity",
  Q4="The game fosters participants' motivation and involvement",
  Q5="The game improves communication among the team members",
  Q6="The game makes participants more willing to attend the meeting",
  Q7="The game is easy to understand and play"
)

xAxisDimensions <- list(
  Q1=list(-5, 8),
  Q2=list(-7, 7),
  Q3=list(-7, 7),
  Q4=list(-5, 8),
  Q5=list(-7, 8),
  Q6=list(-7, 7),
  Q7=list(-3, 9)
)

#gamesAVG, gamesPositiveRatio and gamesNegativeRatio agregate data for RadarChart
gamesAVG <- matrix(nrow = length(questions), ncol = length(gameNames))
rownames(gamesAVG) <- names(questions)
colnames(gamesAVG) <- gameNames

gamesPositiveRatio <- gamesAVG #a copy of gamesAVG
gamesNegativeRatio <- gamesAVG

oneQuestionPerImage <- TRUE
likertPlots <- list()

for (i in seq(1, length(questions))) {
  question <- questions[i]
  qId <- names(question)
  csvFileName = paste(tolower(qId), "csv", sep=".")   

  gamesArrayAllTeams <- c()
  teamResultsCombined <- NULL
  gamesAvgArrayAllTeams <- NULL
  for(team in teams) {  
    csvFilePath <- file.path(baseDir, "datasets", team$teamName, csvFileName)
    gamesDF <-loadAndReorderSingleQuestionData(csvFilePath, team$gameOrder, length(gameNames))
    gamesArray <- convertIntoArray(gamesDF, gameNames, likertLevels)
    gamesArrayAllTeams <- c(gamesArrayAllTeams, gamesArray)
    teamResultsCombined <- if( is.null(teamResultsCombined) ) gamesArray else teamResultsCombined+gamesArray
    gamesAvgArray <- apply( gamesArray, MARGIN=1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) ) # calculate the average grade for each game (row)
    # Setting 1 as parameter of the MARGIN argument means that we apply a function to every row of an array
    gamesAvgArrayAllTeams <- if( is.null(gamesAvgArrayAllTeams) ) gamesAvgArray else cbind(gamesAvgArrayAllTeams, gamesAvgArray)
  }
    
  dimNames <- list(gameNames, likertLevels, teamNames)
  plotDim <-  as.numeric( lapply(dimNames, length) )
  likertArray <- array(gamesArrayAllTeams, dim = plotDim, dimnames = dimNames)
  
  #Delete the 7th row ("Mountain climbing") from the array
  likertArray <- likertArray[-7,,]
  # teamResultsCombined <- teamResultsCombined[-7,]

  title = paste(qId, question, sep=". ")
  xAxisDim <- c(unlist(xAxisDimensions[[i]]))


  ########## Generating png files with Likert charts ###########
  if(oneQuestionPerImage) {
    legend <- FALSE 
    bottom.padding <- if(legend) -0.5 else -7.5
    imgLikert <- generateChart(likertArray, xAxisDim, title, legend=legend, bottom.padding=bottom.padding)
    pngFileName = paste(tolower(qId), "png", sep=".")
    pngFilePath <- file.path(outDir, pngFileName)    
    png(pngFilePath, width = 1200, height = 250) #800x300
    print(imgLikert)
    dev.off()
  }
  # imgTeamResultsCombined <- generateChart(teamResultsCombined, xAxisDim, title, legend=FALSE)

  if(i==1) imgLikert <- generateChart(likertArray, xAxisDim, title, legend=FALSE, top.padding=0.5, bottom.padding=-5)
  else if(i==7) imgLikert <- generateChart(likertArray, xAxisDim, title, legend=TRUE, bottom.padding=0) #bottom.padding=4
  else imgLikert <- generateChart(likertArray, xAxisDim, title, legend=FALSE, bottom.padding=-5)

  likertPlots <- append(likertPlots, list(imgLikert))

  # averages for each game
  tmp_All <- apply( teamResultsCombined, 1, function(x) sum( x*c(1, 2, 3, 4, 5)/sum(x) ) )
  gamesAVG[strtoi(i),] <- tmp_All

  tmp_positive <- apply( teamResultsCombined, 1, function(x) sum( x*c(0, 0, 0, 1, 1)/sum(x) ) ) # positive opinions / all opinions
  gamesPositiveRatio[strtoi(i),] <- tmp_positive

  tmp_negative <- apply( teamResultsCombined, 1, function(x) sum( x*c(1, 1, 0, 0, 0)/sum(x) ) ) # negative opinions / all opinions
  gamesNegativeRatio[strtoi(i),] <- tmp_negative


  ########## Generating csv file with averages ###########
  csvOutFilePath = file.path(outDir, csvFileName)

  gamesAvgArrayAllTeams <- cbind(gamesAvgArrayAllTeams, tmp_All)
  colnames(gamesAvgArrayAllTeams) <- c(teamNames, "ALL")

  #Delete the 7th row ("Mountain climbing") from the array
  gamesAvgArrayAllTeams <- gamesAvgArrayAllTeams[-7,]
  write.csv(gamesAvgArrayAllTeams, file = csvOutFilePath, na="")
}

# heights <- c(1.02,1,1,1,1,1,1.192) # save as EPS 1000x1500
heights <- rep(1, length(likertPlots))
heights[1] <- 1.02
heights[length(heights)] <- 1.192
args <- append(likertPlots, list(ncol=1, heights=heights))
do.call('grid.arrange', args)



########## Draw radar charts ##########
if(!require(fmsb)){
    install.packages("fmsb")
    library(fmsb)
}

par(mar=c(0, 0, 1.5, 0), xpd=TRUE) #bottom, left, top and right margins 
layout(matrix(1:6, ncol=3)) #draw 6 plots to device

gamePositive_DF <- as.data.frame( t(gamesPositiveRatio) )
gameNegative_DF <- as.data.frame( t(gamesNegativeRatio) )
gameNeutral_DF <- 1 - gamePositive_DF - gameNegative_DF

colors_border=c( rgb(0.0,0.0,1.0,0.9), rgb(1.0,0.0,0.0,0.9), rgb(0.7,0.7,0.7,0.9) ) 
colors_in=c( rgb(0.0,0.0,1.0,0.0), rgb(1.0,0.0,0.0,0.0), rgb(0.7,0.7,0.7,0.0) )
colors_border=c( "blue", "red", "darkgrey")

lineBelowTitle = 0.5

for(i in 1:6) {
    data <- rbind(rep(0.7,7), rep(0,7), rbind(gamePositive_DF[i,], gameNegative_DF[i,], gameNeutral_DF[i,]) ) 
    data <- data[,c("Q1", "Q3", "Q4", "Q5", "Q6")] #without Q2 and Q7
    radarchart(data , title=gameNames[[i]], line = lineBelowTitle, cex.main = 1.1, seg=3, axistype=1, caxislabels=seq(0.1,0.7,0.2), calcex=0.9, axislabcol="grey30", pty=c(16,1,8), plwd=2, plty=c(1,1,0), vlcex=1.1, pcol=colors_border , pfcol=colors_in  )
    if(i == 5) { # Use option bty = "n" in legend to remove the box around the legend.
        legend("topright", inset=c(-0.02,-0.16), legend=c("agree", "disagree", "neutral"), col=c( "blue", "red", "darkgrey"), pch=c(16,1,8), cex=1.05, box.lty=0, bty="n")
    }
}