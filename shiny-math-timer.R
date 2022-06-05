library(shiny)
library(tidyverse)
library(tictoc)
library(glue)


ui <- fluidPage(
  h2(strong("Solve as many sums as you can in 10 seconds")),
  actionButton("start", "Play"),
  verbatimTextOutput("question_print", placeholder = TRUE),
  numericInput("answer", "Enter Your Answer", min = 0, value = NA),
  uiOutput("last_time")
)

server <- function(input, output, session){
  
  end_game <- reactiveVal(TRUE)
  
  # the two numbers to sum
  num1 <- reactiveVal(NULL)
  num2 <- reactiveVal(NULL)
  
  # the timer for the correct answer
  timer <- reactiveVal(NULL)
  
  # results from the game
  last_time <- reactiveVal(NULL)
  time_remaining <- reactiveVal(10)
  correct <- reactiveVal(0)
  
  
  # initialize the game
  observe({
    updateNumericInput(session, "answer",value = NA)
    last_time(NULL)
    time_remaining(10)
    correct(0)
    end_game(FALSE)
    num1(sample.int(10, size = 1))
    num2(sample.int(10, size = 1))
    while (num1()==num2()){
      num2(sample.int(10, size = 1))
    }
    tic()
  }) %>% 
    bindEvent(input$start)
  
  question <- reactive({
    if(end_game()){
      "Click play to start the game"
    }
    else {
      glue("what is the sum of {req(num1())} and {req(num2())}?")
    }
  })
  
  # check for correct answer and update score
  observe({
    if(!end_game() && isolate(num1())+req(num2()) == req(input$answer)){
      timer(toc())
      last_time(round(timer()$toc - timer()$tic, 2))
    }
  })
  
  # update time and score
  observe({
    time_remaining(max(0, time_remaining()-last_time()))
    end_game(time_remaining() == 0)
    updateNumericInput(session, "answer",value = NA)
    correct(correct()+1)
  }) %>% 
    bindEvent(req(last_time()))
  
  # re draw
  observe({
    if(!end_game()){
      num1(sample.int(10, size = 1))
      num2_temp <- sample.int(10, size = 1)
      while (num1()==num2_temp){
        num2_temp <- sample.int(10, size = 1)
      }
      num2(num2_temp)
      tic()
    }
  }) %>% 
    bindEvent(req(correct())) 
  
  output$question_print <- renderText({
    question()
  })
  
  output$last_time <- renderUI({
    if(end_game() && correct()>0){
      HTML(glue("<b style='color:red'>Game Over!<br>Your Final Score is {correct()} !</b>"))
    }
    else {
      HTML(glue("
              <b>
              Your answer took {last_time()} seconds.<br>
              You have {round(time_remaining(), 2)} seconds left.<br>
              Your score is {correct()}
              </b>
              "))
    
    }
  })
  
}

shinyApp(ui, server)