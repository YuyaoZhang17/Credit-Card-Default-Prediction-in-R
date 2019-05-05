
# shiny 
# interface code
library(pROC)
library(shiny)
library(h2o)

h2o.init(nthreads=-1)
data <- read.csv("D:/NYU/Curriculum/CapstoneProject/UCI_Credit_Card.csv",header=TRUE)
data_ann <- data
colnames(data_ann)[25] <- "default_flag"
data_train_ann <- data_ann[,-1][t_rain,]
data_test_ann <- data_ann[,-1][-t_rain,]


# End setup code
##############################

##############################
## Define the user interface

inter_face <- shiny::fluidPage(
  titlePanel("ANN Performance"),
  fluidRow(
    column(width=4, sliderInput("hidden_layer1", label="first hidden layer:",
                                min=1, max=20, value=16, step=1)),
    column(width=4, sliderInput("hidden_layer2", label="second hidden layer:",
                                min=0, max=20, value=16, step=1)),
    column(width=4, sliderInput("hidden_layer3", label="third hidden layer:",
                                min=0, max=20, value=0, step=1)),
    column(width=4, selectInput("activation", label="activation function:",
                                list("Rectifier"="Rectifier",
                                     "Tanh"="Tanh",
                                     "Maxout"="Maxout")))),
  column(5,"ROC",plotOutput("auc_ann", height = 800,width = 800)),
  column(4,textOutput("text")),
  mainPanel(plotOutput("ANN Performance", width="150%", height=500))
)  # end fluidPage interface



##############################
## Define the server code
# The function ser_ver() accepts the arguments "input" and "output".
# server code
ser_ver <- function(input, output) {
  da_ta <- reactive({
    hidden_layer1 <- input$hidden_layer1
    hidden_layer2 <- input$hidden_layer2
    activation <- input$activation
    classifier<- h2o.deeplearning(y='default_flag',
                                  training_frame=as.h2o(data_train_ann,),
                                  activation=activation,
                                  hidden=c(hidden_layer1,hidden_layer2),
                                  epochs=100,
                                  train_samples_per_iteration=-2)
    prob_pred=h2o.predict(classifier, newdata=as.h2o(data_test_ann[-24]))
    y_prob_pred=as.vector(prob_pred)
    auc_ann <-  roc(data_test_ann$default_flag,  y_prob_pred, plot = TRUE)
    round(auc_ann$auc,digits = 3)
  })
  output$auc_ann <- renderPlot({
    hidden_layer1 <- input$hidden_layer1
    hidden_layer2 <- input$hidden_layer2
    activation <- input$activation
    classifier<- h2o.deeplearning(y='default_flag',
                                  training_frame=as.h2o(data_train_ann,),
                                  activation=activation,
                                  hidden=c(hidden_layer1,hidden_layer2),
                                  epochs=100,
                                  train_samples_per_iteration=-2)
    prob_pred=h2o.predict(classifier, newdata=as.h2o(data_test_ann[-24]))
    y_prob_pred=as.vector(prob_pred)
    
    roc<-roc(data_test_ann$default_flag,y_prob_pred, plot = TRUE)
    plot(roc)
    
    text(0,0.3,
         lab=paste0(
           "first hidden layer = ", format(hidden_layer1, digits=3), "\n",
           "second hidden layer = ", format(hidden_layer2, digits=3), "\n",
           "activation function = ", format(activation), "\n",
           "AUC = ", da_ta()),
         adj=c(1, 1), cex=1.2, lwd=2)
  })
}

##############################
## Return a Shiny app object

shiny::shinyApp(ui=inter_face, server=ser_ver)


h2o.shutdown()
