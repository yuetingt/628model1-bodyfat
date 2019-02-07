library(shiny)
ui <- fluidPage(
  titlePanel("Body Fat Calculator for male"),
  sidebarPanel(
    img(src = "bodyfat.png", width = "100%")
  ),
  mainPanel(
    h4("Please input corresponding measurements"),
    numericInput('ABDOMEN', 'ABDOMEN(cm)', 92,min = 0),
    numericInput('WEIGHT', 'WEIGHT(kg)', 80,min = 0),
    numericInput('WRIST', 'WRIST(cm)', 18,min = 0),
    submitButton("Calculate", icon("refresh")),
    tags$head(tags$style("#text1{display:inline
                           }")),
    h4("Your bodyfat percentage is around",style="display:inline"),textOutput('text1'),
    tags$head(tags$style("#text1{color: blue;
                         font-size: 21px;
                         font-style: italic;
                         }"
                         )
    )
  )
)

server <- function(input, output) {
  model=function(ABDOMEN,WEIGHT,WRIST){
    bodyFat=-45.32+0.91*ABDOMEN-0.013*WEIGHT*WRIST
    if (bodyFat>0 & bodyFat<100){
      return(round(bodyFat,2))
    }
    else{
      print("Your input is not valid!!!")
    }
  }
  textnote<-reactive({
    
  
    if (input$ABDOMEN>250 | input$ABDOMEN<=0 | input$WEIGHT>250 | input$WEIGHT<=0 | input$WRIST<=0 | input$WRIST>50){
      paste("Your input is not valid!!!")
    }else{
      paste(model(input$ABDOMEN,input$WEIGHT,input$WRIST),"%")
    }
  })
    output$text1=renderText({textnote()})
}

shinyApp(ui = ui, server = server)
