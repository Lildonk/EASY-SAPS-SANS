# load the package
library(stats4)
library(lattice)
library(mirt)
library(shiny)
library(readxl)
library(catR)
library(writexl)
library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)

# load the data
questions <- read.csv("Questions.csv")
load("sapsCoef.Rdata")
load("sapsResultGRM.Rdata")
questions2<- read.csv("Questions2.csv")
load("sansCoef.Rdata")
load("sansResultGRM.Rdata")


# set the start 
start <- startItems(sapsCoef, model="GRM")$items
start2 <- startItems(sansCoef, model="GRM")$items
#print(start2)

# ui
ui <- dashboardPage(
  dashboardHeader(title = "CAT SAPS/SANS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("SAPS", tabName = "SAPS"),
      menuItem("SANS", tabName = "SANS"),
      menuItem("ABOUT", tabName = "ABOUT")
    )
  ),
  dashboardBody(
    tags$style(
      HTML("
        .custom-box {
          background-color: #FFFF00;
          padding: 10px;
          border-radius: 5px;
          box-shadow: 0 2px 5px rgba(0, 0, 0, 0.2);
          color: #333;
        }
      ")
    ),
    tabItems(
      
      tabItem(tabName = "SAPS",
              fluidRow(
                column(
                  width = 12,
                  uiOutput("questionUI")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  actionButton("nextquestion", "Next", class = "btn-primary")
                )
              ),
              fluidRow(
                column(
                width = 12,
                plotOutput("plot")
                )
              )
      ),
    
      tabItem(tabName = "SANS",
              fluidRow(
                column(
                  width = 12,
                  uiOutput("questionUI2")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  actionButton("nextquestion2", "Next", class = "btn-primary")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  plotOutput("plot2")
                )
              )
      ),
      tabItem(tabName = "ABOUT",
              fluidRow(
                column(
                  width = 12,
                  ""
                )
              )
      )
    )
  )
)

# server
server <- function(input, output, session) {
  currentQuestionIndex <- reactiveVal(start)
  itemlist <- reactiveVal(c())
  thetalist <- reactiveVal(c())
  errorlist <- reactiveVal(c())
  n <- reactiveVal(1)
  totalScore <- reactiveVal(c())
  x <- reactiveVal(replace(0, 1:30 , NA))
  theta <- reactiveVal(c())
  totalScoreList <- reactiveVal(c())
  upperlist <- reactiveVal(c())
  lowerlist <- reactiveVal(c())
  
  currentQuestionIndex2 <- reactiveVal(start2)
  itemlist2 <- reactiveVal(c())
  thetalist2 <- reactiveVal(c())
  errorlist2 <- reactiveVal(c())
  n2 <- reactiveVal(1)
  totalScore2 <- reactiveVal(c())
  x2 <- reactiveVal(replace(0, 1:20 , NA))
  theta2 <- reactiveVal(c())
  totalScoreList2 <- reactiveVal(c())
  upperlist2 <- reactiveVal(c())
  lowerlist2 <- reactiveVal(c())
  
  output$questionUI <- renderUI({
    if(currentQuestionIndex() != -1) {
      currentQuestion <- questions[currentQuestionIndex(),]
      
      box(
        title = as.character(currentQuestion[2]),
        width = 6,
        radioButtons(
          paste0("answer_", currentQuestionIndex()),
          label = as.character(currentQuestion[3]),
          choices = as.numeric(currentQuestion[4:9])
        )
      )
    } else {
      box(
        "Final Score Estimated!", status = "primary", solidHeader = TRUE, width = NULL,
        div(class = "custom-box",
            paste("The number of qusetions is", length(thetalist()), 
                  ",and the final total score is", totalScore())
        )
      )
    }
  })
  
  observeEvent(input$nextquestion, {
    
    if(currentQuestionIndex() == -1) {
      currentQuestionIndex(start)
      itemlist(c())
      thetalist(c())
      errorlist(c())
      totalScoreList(c())
      upperlist(c())
      lowerlist(c())
      
      n(1)
      totalScore(c())
      x(replace(0, 1:30 , NA))
      theta(c())
    } else {
      index <- currentQuestionIndex()
      #print(paste("question", index, sep = ""))
      currentAnswer <- input[[paste0("answer_", index)]]
      if(is.null(currentAnswer)) {return()}
      #print(currentAnswer)
      x <- as.numeric(replace(x(), index, currentAnswer))
      x(x)
      theta <- thetaEst(sapsCoef, x, model = "GRM", range = c(-6, 6), method = "BM")
      theta(theta)
      thetalist <- c(thetalist(), theta)
      #print(thetalist)
      thetalist(thetalist)
      
      error <- semTheta(theta, sapsCoef, x, range = c(-6, 6), model = "GRM", method = "BM")
      errorlist <- c(errorlist(), error)
      #print(errorlist)
      errorlist(errorlist)
      stop <- list(rule = "precision", thr = 0.39)
      checkstop <- checkStopRule(theta, error, n, sapsCoef, model = "GRM", stop=stop)
      
      Theta <- matrix(seq(-6,6,.001))
      lower_ci = theta - error
      #print(lower_ci)
      upper_ci = theta + error
      #print(upper_ci)
      theta <- round(theta, digits = 2)
      tscore = cbind(Theta, expected.test(
        sapsResultGRM,
        Theta,
      ))
      
      
      itemlist <- c(itemlist(), index)
      print(itemlist)
      itemlist(itemlist)
      
      item <- nextItem(sapsCoef, model = "GRM", theta = theta, out = itemlist, range = c(-6, 6))
      currentQuestionIndex(item$item)
      n <- n() + 1
      n(n)
      

      totalScore <- tscore[findInterval(theta,tscore[,1]),2]
      lower <- tscore[findInterval(round(lower_ci, digits = 2),tscore[,1]),2]
      upper <- tscore[findInterval(round(upper_ci, digits = 2),tscore[,1]),2]
      if (round(totalScore, digits = 2) == 3.48){
        totalScore = 0
        lower = 0
        upper = 0
      }
      totalScore(totalScore)
      totalScoreList <- c(totalScoreList(), totalScore)
      totalScoreList(totalScoreList)
      lowerlist <- c(lowerlist(), lower)
      lowerlist(lowerlist)
      upperlist <- c(upperlist(), upper)
      upperlist(upperlist)
      

      if(n > 11 | theta<=-1.2 | checkstop$decision != FALSE){
        currentQuestionIndex(-1)
        totalScore <- tscore[findInterval(theta,tscore[,1]),2]
        if (round(totalScore, digits = 2) == 3.48){
          totalScore = 0
        }
        totalScore(totalScore)
      }
    }
    
  })
  
  output$plot <- renderPlot({

    
    if(length(thetalist()) > 0){
      df <- data.frame(
        x = c(1:length(totalScoreList())),
        y = totalScoreList(),
        upper_ci = upperlist(),
        lower_ci = lowerlist()
      )
      ggplot(df, aes(x = x, y = y)) +
        geom_point() +
        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.3) +
        labs(title = "", x = "n", y = "score") +
        ylim(c(0,150)) + 
        theme_bw()
    }
  })
  
  
  
  #SANS
  output$questionUI2 <- renderUI({
    if(currentQuestionIndex2() != -1) {
      currentQuestion <- questions2[currentQuestionIndex2(),]
      
      box(
        title = as.character(currentQuestion[2]),
        width = 6,
        radioButtons(
          paste0("answer_", currentQuestionIndex2()),
          label = as.character(currentQuestion[3]),
          choices = as.numeric(currentQuestion[4:9])
        )
      )
    } else {
      box(
        "Final Score Estimated!", status = "primary", solidHeader = TRUE, width = NULL,
        div(class = "custom-box",
            paste("The number of qusetions is", length(thetalist2()), ",and the final total score is", totalScore2())
        )
      )
    }
  })
  
  observeEvent(input$nextquestion2, {
    if(currentQuestionIndex2() == -1) {
      currentQuestionIndex2(start2)
      itemlist2(c())
      thetalist2(c())
      errorlist2(c())
      totalScoreList2(c())
      upperlist2(c())
      lowerlist2(c())
      
      n2(1)
      totalScore2(c())
      x2(replace(0, 1:20 , NA))
      theta2(c())
    } else {
      index <- currentQuestionIndex2()
      #print(paste("question", index, sep = ""))
      currentAnswer <- input[[paste0("answer_", index)]]
      if(is.null(currentAnswer)) {return()}
      #print(currentAnswer)
      x2 <- as.numeric(replace(x2(), index, currentAnswer))
      x2(x2)
      theta2 <- thetaEst(sansCoef, x2, model = "GRM", range = c(-6, 6), method = "BM")
      theta2(theta2)
      thetalist2 <- c(thetalist2(), theta2)
      print(thetalist2)
      thetalist2(thetalist2)
      
      error <- semTheta(theta2, sansCoef, x2, range = c(-6, 6), model = "GRM", method = "BM")
      errorlist2 <- c(errorlist2(), error)
      #print(errorlist2)
      errorlist2(errorlist2)
      stop <- list(rule = "precision", thr = 0.17)
      checkstop <- checkStopRule(theta2, error, n2, sansCoef, model = "GRM", stop=stop)

      
      Theta <- matrix(seq(-6,6,.001))
      lower_ci = theta2 - error
      #print(lower_ci)
      upper_ci = theta2 + error
      #print(upper_ci)
      theta2 <- round(theta2, digits = 2)
      tscore = cbind(Theta, expected.test(
        sansResultGRM,
        Theta,
      ))
      
      itemlist2 <- c(itemlist2(), index)
      #print(itemlist2)
      itemlist2(itemlist2)
      item <- nextItem(sansCoef, model = "GRM", theta = theta2, out = itemlist2, range = c(-6, 6))
      currentQuestionIndex2(item$item)
      n2 <- n2() + 1
      n2(n2)
      
      totalScore2 <- tscore[findInterval(theta2,tscore[,1]),2]
      lower <- tscore[findInterval(round(lower_ci, digits = 2),tscore[,1]),2]
      upper <- tscore[findInterval(round(upper_ci, digits = 2),tscore[,1]),2]
      if (round(totalScore2, digits = 3) == 2.085){
        totalScore2 = 0
        lower = 0
        upper = 0
      }
      
      
      totalScore2(totalScore2)
      totalScoreList2 <- c(totalScoreList2(), totalScore2)
      totalScoreList2(totalScoreList2)
      lowerlist2 <- c(lowerlist2(), lower)
      lowerlist2(lowerlist2)
      upperlist2 <- c(upperlist2(), upper)
      upperlist2(upperlist2)
      
      if(n2 > 15 | theta2<=-1.3 | checkstop$decision != FALSE){
        currentQuestionIndex2(-1)
        totalScore2 <- tscore[findInterval(theta2,tscore[,1]),2]
        if (round(totalScore2, digits = 3) == 2.085){
          totalScore2 = 0
        }
        totalScore2(totalScore2)
      }
    }
    
  })
  
  output$plot2 <- renderPlot({
    if(length(totalScoreList2()) > 0){
      df <- data.frame(
        x = c(1:length(totalScoreList2())),
        y = totalScoreList2(),
        upper_ci = upperlist2(),
        lower_ci = lowerlist2()
      )
      ggplot(df, aes(x = x, y = y)) +
        geom_point() +
        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.3) +
        labs(title = "", x = "n", y = "score") +
        ylim(c(0,100)) + 
        theme_bw()
    }
  })
}

shinyApp(ui, server)

