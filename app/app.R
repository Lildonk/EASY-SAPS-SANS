# load the package
library(stats4)
library(lattice)
library(mirt)
library(shiny)
library(readxl)
library(catR)
library(writexl)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(DT)
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

# ui
ui <- dashboardPage(
  dashboardHeader(title = "EASY-SAPS/SANS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PREDICT", tabName = "PREDICT", icon = icon("chart-line", lib = "font-awesome", style = "margin-right: 3px;margin-left: 1px;")),
      menuItem("CLASSIFY", tabName = "CLASSIFY", icon = icon("scale-balanced", lib = "font-awesome")),
      menuItem("ABOUT", tabName = "ABOUT", icon = icon("info-circle", lib = "font-awesome", style = "margin-right: 3px;margin-left: 1px;"))
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
      tabItem(tabName = "PREDICT",
              fluidRow(
                column(
                  width = 6,
                  uiOutput("questionUI")
                ),
                column(
                  width = 6,
                  uiOutput("questionUI2")
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  actionButton("nextquestion", "Next", class = "btn-primary", style = "margin-left: 13px;"),
                  style = "margin-bottom: 20px;",
                ),
                column(
                  width = 4,
                  actionButton("startover", "Start Over", class = "btn-primary", style = "margin-left: 13px;"),
                  style = "margin-bottom: 20px;",
                ),
                column(
                  width = 2,
                  actionButton("nextquestion2", "Next", class = "btn-primary", style = "margin-left: 13px;"),
                  style = "margin-bottom: 20px;",
                ),
                column(
                  width = 4,
                  actionButton("startover2", "Start Over", class = "btn-primary", style = "margin-left: 13px;"),
                  style = "margin-bottom: 20px;",
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("result3")
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  plotOutput("plot")
                ),
                column(
                  width = 6,
                  plotOutput("plot2")
                )
              ),
              
              fluidRow(
                column(
                  width = 6,
                  DTOutput("result1")
                ),
                column(
                  width = 6,
                  DTOutput("result2")
                )
              )


      ),
      
      tabItem(tabName = "CLASSIFY",
              fluidRow(
                column(
                  width = 6,
                  uiOutput("questionUI4"),
                  ),
                column(
                  width = 6,
                  column(
                    width = 12,
                    numericInput("cutoff", label = "Threshold", min = 1, max = 249, value = 37, step = 1)
                  ),
                  column(
                    width = 12,
                    useShinyjs(),
                    actionButton("nextquestion4", "Next", class = "btn-primary", style = "margin-bottom: 15px")
                  ),
                  column(
                    width = 12,
                    actionButton("startover4", "Start Over", class = "btn-primary", style = "margin-bottom: 15px")
                  ),

                ),
                
              ),

              fluidRow(
                column(
                  width = 6,
                  plotOutput("plot4_1")
                ),
                column(
                  width = 6,
                  plotOutput("plot4_2")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  DTOutput("result41")
                )
              )
      ),    
      
      tabItem(tabName = "ABOUT",
              fluidRow(
                column(
                  width = 12,
                  a("User Menu",href="https://docs.google.com/document/d/1jX8BbQtb1Xhjo8XZyspQoRCidSEpQ0iGPCk2nxdvUQI")
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
  answerlist11 <- reactiveVal(c())
  titlelist11 <-  reactiveVal(c())
  
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
  answerlist12 <- reactiveVal(c())
  titlelist12 <- reactiveVal(c())

  currentQuestionIndex4_1 <- reactiveVal(start)
  itemlist4_1 <- reactiveVal(c())
  thetalist4_1 <- reactiveVal(c())
  errorlist4_1 <- reactiveVal(c())
  n4_1 <- reactiveVal(1)
  totalScore4_1 <- reactiveVal(c())
  x4_1 <- reactiveVal(replace(0, 1:30 , NA))
  theta4_1 <- reactiveVal(c())
  totalScoreList4_1 <- reactiveVal(c())
  upperlist4_1 <- reactiveVal(c())
  lowerlist4_1 <- reactiveVal(c())
  flag4_1 <- reactiveVal(1)
  answerlist4_1 <- reactiveVal(c())
  titlelist4 <-  reactiveVal(c())
  catelist4 <- reactiveVal(c())
  answerlist4 <- reactiveVal(c())
  
  currentQuestionIndex4_2 <- reactiveVal(start2)
  itemlist4_2 <- reactiveVal(c())
  thetalist4_2 <- reactiveVal(c())
  errorlist4_2 <- reactiveVal(c())
  n4_2 <- reactiveVal(1)
  totalScore4_2 <- reactiveVal(c())
  x4_2 <- reactiveVal(replace(0, 1:20 , NA))
  theta4_2 <- reactiveVal(c())
  totalScoreList4_2 <- reactiveVal(c())
  upperlist4_2 <- reactiveVal(c())
  lowerlist4_2 <- reactiveVal(c())
  flag4_2 <- reactiveVal(0)
  answerlist4_2 <- reactiveVal(c())
  # titlelist42 <-  reactiveVal(c())
  # catelist42 <- reactiveVal(c())
  
  
  output$questionUI <- renderUI({
    if(currentQuestionIndex() != -1) {
      shinyjs::show("nextquestion")
      currentQuestion <- questions[currentQuestionIndex(),]
      
      box(
        title = as.character(currentQuestion[2]),
        width = 12,
        radioButtons(
          paste0("answer_", currentQuestionIndex()),
          label = as.character(currentQuestion[3]),
          choices = c("0 Absent", "1 Mild","2 Minor", "3 Moderate", "4 Moderately severe", "5 Severe"),
          selected = "0 Absent"
        )
      )
    } else {
      shinyjs::hide("nextquestion")
      box(
        "Total SAPS Score Estimated!", status = "primary", solidHeader = TRUE, width = NULL, 
        div(class = "custom-box",
            paste("The number of qusetions is", length(thetalist()), 
                  ",and the predicted total SAPS score is",round(totalScore(),digits = 2))),
        div(class = "custom-box",
            paste("Please finish both SAPS and SANS test to obtain the total score."))
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
      answerlist11(c())
      titlelist11(c())
      
      n(1)
      totalScore(c())
      x(replace(0, 1:30 , NA))
      theta(c())
    } else {
      index <- currentQuestionIndex()
      if(is.null(input[[paste0("answer_", index)]])) {return()}
      currentAnswer <- as.numeric(substr(input[[paste0("answer_", index)]],1,1))
      x <- as.numeric(replace(x(), index, currentAnswer))
      x(x)
      theta <- thetaEst(sapsCoef, x, model = "GRM", range = c(-6, 6), method = "BM")
      theta(theta)
      thetalist <- c(thetalist(), theta)
      thetalist(thetalist)
      
      error <- semTheta(theta, sapsCoef, x, range = c(-6, 6), model = "GRM", method = "BM")
      errorlist <- c(errorlist(), error)
      errorlist(errorlist)
      stop <- list(rule = "precision", thr = 0.39)
      checkstop <- checkStopRule(theta, error, n, sapsCoef, model = "GRM", stop=stop)
      
      Theta <- matrix(seq(-6,6,.001))
      lower_ci = theta - error
      upper_ci = theta + error
      theta <- round(theta, digits = 2)
      tscore = cbind(Theta, expected.test(
        sapsResultGRM,
        Theta,
      ))
      
      itemlist <- c(itemlist(), index)
      itemlist(itemlist)
      
      answerlist11 <- c(answerlist11(), as.numeric(currentAnswer))
      answerlist11(answerlist11)
      
      currentQuestion <- questions[currentQuestionIndex(),]
      title = as.character(currentQuestion[3])
      titlelist11 <- c(titlelist11(),title)
      titlelist11(titlelist11)
      
      item <- nextItem(sapsCoef, model = "GRM", theta = theta, out = itemlist, range = c(-6, 6))
      answer <- currentQuestionIndex(item$item)
      n <- n() + 1
      n(n)
      
      totalScore <- tscore[findInterval(theta,tscore[,1]),2]
      lower <- tscore[findInterval(round(lower_ci, digits = 2),tscore[,1]),2]
      upper <- tscore[findInterval(round(upper_ci, digits = 2),tscore[,1]),2]
      if (round(totalScore, digits = 2) == 3.04){
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
        if (round(totalScore, digits = 2) == 3.04){
          totalScore = 0
        }
        totalScore(totalScore)
      }
    }
    
  })
  
  observeEvent(input$startover,{
    currentQuestionIndex(start)
    itemlist(c())
    thetalist(c())
    errorlist(c())
    totalScoreList(c())
    upperlist(c())
    lowerlist(c())
    answerlist11(c())
    titlelist11(c())
    n(1)
    totalScore(c())
    x(replace(0, 1:30 , NA))
    theta(c())
  })
  
  output$plot <- renderPlot({
    if(length(totalScoreList()) > 0){
      df <- data.frame(
        x = c(1:length(totalScoreList())),
        y = totalScoreList(),
        upper_ci = upperlist(),
        lower_ci = lowerlist()
      )
      ggplot(df, aes(x = x, y = y)) +
        geom_point() +
        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.3) +
        labs(title = "SAPS", x = "Total Number of Questions Answered", y = "Prediected SAPS Score") +
        ylim(c(0,150)) + 
        theme_bw() +
        scale_x_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
        theme(plot.title = element_text(hjust = 0.5, size = 18))
    }
    else {
      ggplot() +
        geom_point() +
        labs(title = "SAPS", x = "Total Number of Questions Answered", y = "Prediected SAPS Score") +
        ylim(c(0,150)) + 
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 18))
    }
  })
  
  output$result1 <- renderDT({
    if(length(answerlist11()) > 0){
      data <- data.frame(Question = unlist(titlelist11()), Answer = unlist(answerlist11()))
      datatable(data,options = list(dom = 't'))
                                                 
    }
  })
  
  #SANS
  output$questionUI2 <- renderUI({
    if(currentQuestionIndex2() != -1) {
      shinyjs::show("nextquestion2")
      currentQuestion <- questions2[currentQuestionIndex2(),]
      
      box(
        title = as.character(currentQuestion[2]),
        width = 12,
        radioButtons(
          paste0("answer_", currentQuestionIndex2()),
          label = as.character(currentQuestion[3]),
          #choices = as.numeric(currentQuestion[4:9]),
          choices = c("0 Absent", "1 Mild","2 Minor", "3 Moderate", "4 Moderately severe", "5 Severe"),
          selected = "0 Absent"
        )
      )
    } else {
      shinyjs::hide("nextquestion2")
      box(
        "Total SANS Score Estimated!", status = "primary", solidHeader = TRUE, width = NULL,
        div(class = "custom-box",
            paste("The number of qusetions is", length(thetalist2()), 
                  ",and the predicted total SANS score",round(totalScore2(),digits = 2))),
        div(class = "custom-box",
            paste("Please finish both SAPS and SANS test to obtain the total score."))
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
      answerlist12(c())
      titlelist12(c())
      
      n2(1)
      totalScore2(c())
      x2(replace(0, 1:20 , NA))
      theta2(c())
    } else {
      index <- currentQuestionIndex2()
      if(is.null(input[[paste0("answer_", index)]])) {return()}
      currentAnswer <- as.numeric(substr(input[[paste0("answer_", index)]],1,1))

      x2 <- as.numeric(replace(x2(), index, currentAnswer))
      x2(x2)
      theta2 <- thetaEst(sansCoef, x2, model = "GRM", range = c(-6, 6), method = "BM")
      theta2(theta2)
      thetalist2 <- c(thetalist2(), theta2)
      thetalist2(thetalist2)
      
      error <- semTheta(theta2, sansCoef, x2, range = c(-6, 6), model = "GRM", method = "BM")
      errorlist2 <- c(errorlist2(), error)
      errorlist2(errorlist2)
      stop <- list(rule = "precision", thr = 0.17)
      checkstop <- checkStopRule(theta2, error, n2, sansCoef, model = "GRM", stop=stop)

      
      Theta <- matrix(seq(-6,6,.001))
      lower_ci = theta2 - error
      upper_ci = theta2 + error
      theta2 <- round(theta2, digits = 2)
      tscore = cbind(Theta, expected.test(
        sansResultGRM,
        Theta,
      ))
      
      itemlist2 <- c(itemlist2(), index)
      # print(itemlist2)
      itemlist2(itemlist2)
      
      answerlist12 <- c(answerlist12(), as.numeric(currentAnswer))
      answerlist12(answerlist12)
      
      currentQuestion <- questions2[currentQuestionIndex2(),]
      title = as.character(currentQuestion[3])
      titlelist12 <- c(titlelist12(),title)
      titlelist12(titlelist12)
      
      item <- nextItem(sansCoef, model = "GRM", theta = theta2, out = itemlist2, range = c(-6, 6))
      currentQuestionIndex2(item$item)
      n2 <- n2() + 1
      n2(n2)
      
      totalScore2 <- tscore[findInterval(theta2,tscore[,1]),2]
      lower <- tscore[findInterval(round(lower_ci, digits = 2),tscore[,1]),2]
      upper <- tscore[findInterval(round(upper_ci, digits = 2),tscore[,1]),2]
      if (round(totalScore2, digits = 3) == 1.537){
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
        if (round(totalScore2, digits = 3) == 1.537){
          totalScore2 = 0
        }
        totalScore2(totalScore2)
      }
    }
    
  })
  
  observeEvent(input$startover2, {
    currentQuestionIndex2(start2)
    itemlist2(c())
    thetalist2(c())
    errorlist2(c())
    totalScoreList2(c())
    upperlist2(c())
    lowerlist2(c())
    answerlist12(c())
    titlelist12(c())
    n2(1)
    totalScore2(c())
    x2(replace(0, 1:20 , NA))
    theta2(c())
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
        labs(title = "SANS", x = "Total Number of Questions Answered", y = "Prediected SANS Score") +
        ylim(c(0,100)) + 
        theme_bw() +
        scale_x_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
        theme(plot.title = element_text(hjust = 0.5, size = 18))
    }
    else {
      ggplot() +
        geom_point() +
        labs(title = "SANS", x = "Total Number of Questions Answered", y = "Prediected SANS Score") +
        ylim(c(0,100)) + 
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 18))
    }
  })
  
  output$result2 <- renderDT({
    if(length(answerlist12()) > 0){
      data <- data.frame(Question = unlist(titlelist12()), Answer = unlist(answerlist12()))
      datatable(data,options = list(dom = 't'))
    }
  })
  
  output$result3 <- renderUI({
    if(currentQuestionIndex() ==-1  & currentQuestionIndex2() == -1){
      res1 <- round(unlist(totalScoreList())[length(totalScoreList())],digits = 2)
      res2 <- round(unlist(totalScoreList2())[length(totalScoreList2())],digits = 2)

        div(class = "custom-box",
            paste("The Predicted Total Score of SANS + SAPS is：",res1 + res2))


    }
  })
  
  #Classify 
  output$questionUI4 <- renderUI({
    if(currentQuestionIndex4_1() != -1 || currentQuestionIndex4_2() != -1) {
      shinyjs::show("nextquestion4")
      if(currentQuestionIndex4_1() == -1) {
        flag4_1(0)
        flag4_2(1)
      }
      if(currentQuestionIndex4_2() == -1) {
        flag4_1(1)
        flag4_2(0)
      }
      if(currentQuestionIndex4_1() != -1 && flag4_1() == 1) {
        currentQuestion <- questions[currentQuestionIndex4_1(),]
        answer <- paste0("answer_", currentQuestionIndex4_1())
      }
      
      if(currentQuestionIndex4_2() != -1 && flag4_2() == 1) {
        currentQuestion <- questions2[currentQuestionIndex4_2(),]
        answer <- paste0("answer_", currentQuestionIndex4_2())
      }
      
      box(
        title = as.character(currentQuestion[2]),
        width = 12,
        radioButtons(
          answer,
          label = as.character(currentQuestion[3]),
          choices = c("0 Absent", "1 Mild","2 Minor", "3 Moderate", "4 Moderately severe", "5 Severe"),
          selected = "0 Absent"
        )
      )
    } else {
      shinyjs::hide("nextquestion4")
      box(
        "Classification Finished", status = "primary", solidHeader = TRUE, width = NULL,
        div(class = "custom-box",
            paste("The patient's total score is classified as", 
                  ifelse(totalScore4_1()+totalScore4_2() > input$cutoff, "above", "below")),
                  input$cutoff
        )
      )
    }
  })
  
  observeEvent(input$nextquestion4, {
    if(currentQuestionIndex4_1() == -1 && currentQuestionIndex4_2() == -1) {
      currentQuestionIndex4_1(start)
      itemlist4_1(c())
      thetalist4_1(c())
      errorlist4_1(c())
      totalScoreList4_1(c())
      upperlist4_1(c())
      lowerlist4_1(c())
      answerlist4_1(c())
      titlelist4(c())
      catelist4(c())
      answerlist4(c())
      
      n4_1(1)
      totalScore4_1(c())
      x4_1(replace(0, 1:30 , NA))
      theta4_1(c())
      
      currentQuestionIndex4_2(start)
      itemlist4_2(c())
      thetalist4_2(c())
      errorlist4_2(c())
      totalScoreList4_2(c())
      upperlist4_2(c())
      lowerlist4_2(c())
      answerlist4_2(c())
      # titlelist42(c())
      
      n4_2(1)
      totalScore4_2(c())
      x4_2(replace(0, 1:30 , NA))
      theta4_2(c())
    } else {
      if(flag4_1() == 1){
        index <- currentQuestionIndex4_1()
        if(is.null(input[[paste0("answer_", index)]])) {return()}
        currentAnswer <- as.numeric(substr(input[[paste0("answer_", index)]],1,1))
        x4_1 <- as.numeric(replace(x4_1(), index, currentAnswer))
        x4_1(x4_1)
        theta4_1 <- thetaEst(sapsCoef, x4_1, model = "GRM", range = c(-6, 6), method = "BM")
        theta4_1(theta4_1)
        thetalist4_1 <- c(thetalist4_1(), theta4_1)
        thetalist4_1(thetalist4_1)
        
        error <- semTheta(theta4_1, sapsCoef, x4_1, range = c(-6, 6), model = "GRM", method = "BM")
        errorlist4_1 <- c(errorlist4_1(), error)
        # print(errorlist4_1)
        errorlist4_1(errorlist4_1)
        stop <- list(rule = "precision", thr = 0.39)
        checkstop <- checkStopRule(theta4_1, error, n4_1, sapsCoef, model = "GRM", stop=stop)
        
        Theta <- matrix(seq(-6,6,.001))
        lower_ci = theta4_1 - error
        upper_ci = theta4_1 + error
        theta4_1 <- round(theta4_1, digits = 2)
        tscore = cbind(Theta, expected.test(
          sapsResultGRM,
          Theta,
        ))
        
        answerlist4_1 <- c(answerlist4_1(), as.numeric(currentAnswer))
        answerlist4_1(answerlist4_1)
        answerlist4 <- c(answerlist4(), as.numeric(currentAnswer))
        answerlist4(answerlist4)
        
        currentQuestion <- questions[currentQuestionIndex4_1(),]
        print(currentQuestion)
        
        title = as.character(currentQuestion[3])
        titlelist4 <- c(titlelist4(),title)
        titlelist4(titlelist4)
        
        
        cate = as.character((currentQuestion[2]))
        print(cate)
        catelist4 <- c(catelist4(),cate)
        catelist4(catelist4)
        
        
        itemlist4_1 <- c(itemlist4_1(), index)
        itemlist4_1(itemlist4_1)
        item <- nextItem(sapsCoef, model = "GRM", theta = theta4_1, out = itemlist4_1, range = c(-6, 6))
        currentQuestionIndex4_1(item$item)
        n4_1 <- n4_1() + 1
        n4_1(n4_1)
        
        totalScore4_1 <- tscore[findInterval(theta4_1,tscore[,1]),2]
        lower <- tscore[findInterval(round(lower_ci, digits = 2),tscore[,1]),2]
        upper <- tscore[findInterval(round(upper_ci, digits = 2),tscore[,1]),2]
        if (round(totalScore4_1, digits = 2) <= 3.88){
          currentQuestionIndex4_1(-1)
          totalScore4_1 = 0
          lower = 0
          upper = 0
        }
        
        totalScore4_1(totalScore4_1)
        totalScoreList4_1 <- c(totalScoreList4_1(), totalScore4_1)
        totalScoreList4_1(totalScoreList4_1)
        lowerlist4_1 <- c(lowerlist4_1(), lower)
        lowerlist4_1(lowerlist4_1)
        upperlist4_1 <- c(upperlist4_1(), upper)
        upperlist4_1(upperlist4_1)
        
        if(n4_1 > 11 | theta4_1<=-1.238 | checkstop$decision != FALSE){
          # print(n4_1)
          currentQuestionIndex4_1(-1)
          totalScore4_1 <- tscore[findInterval(theta4_1,tscore[,1]),2]
          if (round(totalScore4_1, digits = 2) <= 3.88){
            totalScore4_1 = 0
          }
          totalScore4_1(totalScore4_1)
        }
        flag4_1(0)
        flag4_2(1)
      }else{
        index <- currentQuestionIndex4_2()
        if(is.null(input[[paste0("answer_", index)]])) {return()}
        currentAnswer <- as.numeric(substr(input[[paste0("answer_", index)]],1,1))
        x4_2 <- as.numeric(replace(x4_2(), index, currentAnswer))
        x4_2(x4_2)
        theta4_2 <- thetaEst(sansCoef, x4_2, model = "GRM", range = c(-6, 6), method = "BM")
        theta4_2(theta4_2)
        thetalist4_2 <- c(thetalist4_2(), theta4_2)
        thetalist4_2(thetalist4_2)
        
        error <- semTheta(theta4_2, sansCoef, x4_2, range = c(-6, 6), model = "GRM", method = "BM")
        errorlist4_2 <- c(errorlist4_2(), error)
        errorlist4_2(errorlist4_2)
        stop <- list(rule = "precision", thr = 0.17)
        checkstop <- checkStopRule(theta4_2, error, n4_2, sansCoef, model = "GRM", stop=stop)
        
        Theta <- matrix(seq(-6,6,.001))
        lower_ci = theta4_2 - error
        upper_ci = theta4_2 + error
        theta4_2 <- round(theta4_2, digits = 2)
        tscore = cbind(Theta, expected.test(
          sansResultGRM,
          Theta,
        ))
        
        answerlist4_2 <- c(answerlist4_2(), as.numeric(currentAnswer))
        answerlist4_2(answerlist4_2)
        
        answerlist4 <- c(answerlist4(), as.numeric(currentAnswer))
        answerlist4(answerlist4)
        
        currentQuestion <- questions2[currentQuestionIndex4_2(),]
        print(currentQuestion)
        
        
        title = as.character(currentQuestion[3])
        titlelist4 <- c(titlelist4(),title)
        titlelist4(titlelist4)
        
        cate = as.character(currentQuestion[2])
        print(cate)
        catelist4 <- c(catelist4(),cate)
        catelist4(catelist4)
        
        itemlist4_2 <- c(itemlist4_2(), index)
        itemlist4_2(itemlist4_2)
        item <- nextItem(sansCoef, model = "GRM", theta = theta4_2, out = itemlist4_2, range = c(-6, 6))
        currentQuestionIndex4_2(item$item)
        n4_2 <- n4_2() + 1
        n4_2(n4_2)
        

        totalScore4_2 <- tscore[findInterval(theta4_2,tscore[,1]),2]
        lower <- tscore[findInterval(round(lower_ci, digits = 2),tscore[,1]),2]
        upper <- tscore[findInterval(round(upper_ci, digits = 2),tscore[,1]),2]
        if (round(totalScore4_2, digits = 3) <= 1.663){
          currentQuestionIndex4_2(-1)
          totalScore4_2 = 0
          lower = 0
          upper = 0
        }
        totalScore4_2(totalScore4_2)
        totalScoreList4_2 <- c(totalScoreList4_2(), totalScore4_2)
        totalScoreList4_2(totalScoreList4_2)
        lowerlist4_2 <- c(lowerlist4_2(), lower)
        lowerlist4_2(lowerlist4_2)
        upperlist4_2 <- c(upperlist4_2(), upper)
        upperlist4_2(upperlist4_2)
        
        
        if(n4_2 > 15 | theta4_2<=-1.34 | checkstop$decision != FALSE){
          currentQuestionIndex4_2(-1)
          totalScore4_2 <- tscore[findInterval(theta4_2,tscore[,1]),2]
          if (round(totalScore4_2, digits = 3) <= 1.663){
            totalScore4_2 = 0
          }
          totalScore4_2(totalScore4_2)
        }
        flag4_1(1)
        flag4_2(0)
      }



      if (n4_1() > 2 && n4_2() > 2){

        if (Reduce('+', as.numeric(answerlist4_1())) == 0) {
          lower4_1 = 0
        }
        else {
          lower4_1 = lowerlist4_1()[length(lowerlist4_1())]
        }

        if (Reduce('+',  as.numeric(answerlist4_2())) == 0) {
          lower4_2 = 0
        }
        else {
          lower4_2 = lowerlist4_2()[length(lowerlist4_2())]
        }

        scoreTotal = totalScore4_1() + totalScore4_2()
        upperTotal = upperlist4_1()[length(upperlist4_1())] + upperlist4_2()[length(upperlist4_2())]
        lowerTotal = lower4_1 + lower4_2


        if (input$cutoff>=upperTotal || input$cutoff<=lowerTotal) {
          currentQuestionIndex4_1(-1)
          currentQuestionIndex4_2(-1)
        }
      }
    }
  })
  
  observeEvent(input$startover4, {
    currentQuestionIndex4_1(start)
    itemlist4_1(c())
    thetalist4_1(c())
    errorlist4_1(c())
    totalScoreList4_1(c())
    upperlist4_1(c())
    lowerlist4_1(c())
    answerlist4_1(c())
    titlelist4(c())
    catelist4(c())
    answerlist4(c())
    
    n4_1(1)
    totalScore4_1(c())
    x4_1(replace(0, 1:30 , NA))
    theta4_1(c())
    
    currentQuestionIndex4_2(start)
    itemlist4_2(c())
    thetalist4_2(c())
    errorlist4_2(c())
    totalScoreList4_2(c())
    upperlist4_2(c())
    lowerlist4_2(c())
    answerlist4_2(c())
    # titlelist42(c())
    
    n4_2(1)
    totalScore4_2(c())
    x4_2(replace(0, 1:30 , NA))
    theta4_2(c())
    
  })
  
  output$plot4_1 <- renderPlot({
    if(length(totalScoreList4_1())){
      df <- data.frame(
        x = c(1:length(totalScoreList4_1())),
        y = totalScoreList4_1(),
        upper_ci = upperlist4_1(),
        lower_ci = lowerlist4_1()
      )
      ggplot(df, aes(x = x, y = y)) +
        geom_point() +
        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.3) +
        labs(title = "SAPS", x = "Total Number of Questions Answered", y = "Prediected SAPS Score") +
        ylim(c(0,150)) + 
        theme_bw() +
        scale_x_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    }
    else {
      ggplot() +
        geom_point() +
        labs(title = "SAPS", x = "Total Number of Questions Answered", y = "Prediected SAPS Score") +
        ylim(c(0,150)) + 
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 18))
    }
  })
  
  output$plot4_2 <- renderPlot({
    if(length(totalScoreList4_2()) > 0){
      df <- data.frame(
        x = c(1:length(totalScoreList4_2())),
        y = totalScoreList4_2(),
        upper_ci = upperlist4_2(),
        lower_ci = lowerlist4_2()
      )
      ggplot(df, aes(x = x, y = y)) +
        geom_point() +
        geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.3) +
        labs(title = "SANS", x = "Total Number of Questions Answered", y = "Prediected SANS Score") +
        ylim(c(0,100)) + 
        theme_bw() +
        scale_x_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    }
    else {
      ggplot() +
        geom_point() +
        labs(title = "SANS", x = "Total Number of Questions Answered", y = "Prediected SANS Score") +
        ylim(c(0,100)) + 
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 18))
    }
  })
  
  output$result41 <- renderDT({
    if(length(answerlist4()) > 0){
      data <- data.frame(Category = unlist(catelist4()), Question = unlist(titlelist4()), Answer = unlist(answerlist4()))
      datatable(data,options = list(dom = 't'))
    }
  })
  
  # output$result42 <- renderDT({
  #   if(length(answerlist4_2()) > 0){
  #     data <- data.frame(Question = unlist(titlelist42()), Answer = unlist(answerlist4_2()))
  #     datatable(data,options = list(dom = 't'))
  #   }
  # })
}

shinyApp(ui, server)

