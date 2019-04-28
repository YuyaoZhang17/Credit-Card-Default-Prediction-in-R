


##############################
# Below is the setup code that runs only once at startup 
# when the shiny app is started.
# In the setup code you can load packages, define functions 
# and variables, source files, and load data.

# Load packages here (if needed)

library(shiny)
library(gbm)
library(pROC)

# Load data

data <- read.csv("./UCI_Credit_Card.csv",header=TRUE)
data_default <- data
#do not show expenentials
options(scipen=999)

# Converting target column to factor
data_default$default.payment.next.month <- 
  as.factor(data_default$default.payment.next.month)


# For further analysis, we need features names to demonstrate themselves
# So we need to convert some feature from numeric to factor to 
# make them more descriptive

# Convert Sex data 1,2 to Male and Female
data_default$SEX <- as.factor(data_default$SEX)
levels(data_default$SEX) <- c("Male","Female")

# Convert Education level data
data_default$EDUCATION <- as.factor(data_default$EDUCATION)
levels(data_default$EDUCATION) <- c("Others",
                                    "Graduate School",
                                    "Unversity",
                                    "High School",
                                    "Others",
                                    "Others",
                                    "Others")
#ai:can we make others and unknown into one level? 

# Convert Marriage level data
data_default$MARRIAGE <- as.factor(data_default$MARRIAGE)
levels(data_default$MARRIAGE) <- c("Others","Married","Single","Others")

# Convert Repayment Status columns to Factors
data_default$PAY_0 <-as.factor(data_default$PAY_0)
data_default$PAY_2 <- as.factor(data_default$PAY_2)
data_default$PAY_3 <- as.factor(data_default$PAY_3)
data_default$PAY_4 <- as.factor(data_default$PAY_4)
data_default$PAY_5 <- as.factor(data_default$PAY_5)
data_default$PAY_6 <- as.factor(data_default$PAY_6)

# Convert default.payment.next.month to Factors 
data_default$default.payment.next.month <- as.factor(data_default$default.payment.next.month)
levels(data_default$default.payment.next.month) <- c("No" , "Yes")

#default.payment.next.month is too long 
colnames(data_default)[25] <- "default_flag"

data_default$AGE.group<-cut(data_default$AGE,c(20,40,60,80))

n_train <- 0.8*nrow(data_default)
set.seed(1121)
t_rain <- sample(1:nrow(data_default),n_train)


data_gbm <- data_default[,-26]
data_gbm$default_flag<-as.numeric(data_gbm$default_flag)
data_gbm$default_flag<-data_gbm$default_flag-1
data_gbm <- data_gbm[,colnames(data_gbm)!="ID"]

data_train_gbm <- data_gbm[t_rain,]
data_test_gbm <- data_gbm[-t_rain,]


# define model perforamnce function
gbm_model <- gbm(default_flag ~ .,
                 data = data_train_gbm,
                 n.trees = 500,
                 distribution = "bernoulli",
                 interaction.depth = 2,
                 shrinkage = 0.1,
                 bag.fraction = 0.5,
                 train.fraction = 0.8)


gbm_test <-  predict(gbm_model, newdata = data_test_gbm, n.trees = 500)

auc_gbm <-  roc(data_test_gbm$default_flag, gbm_test, plot = TRUE)

print(auc_gbm)


# End setup code
##############################

##############################
## Define the user interface

inter_face <- shiny::fluidPage(
  titlePanel("GBM Performance"),
  # Create slider inputs with parameters to auc
  fluidRow(
    column(width=4, sliderInput("n.trees", label="ntree:",
                                min=0, max=3000, value=500, step=100)),
    column(width=4, sliderInput("interaction.depth", label="depth:",
                                min=0, max=10, value=2, step=1)),
    column(width=4, sliderInput("shrinkage", label="shrinkage:",
                                min=0, max=0.3, value=0.1, step=0.01)),
    column(width=4, sliderInput("bag.fraction", label="bag fraction:",
                                min=0, max=1, value=0.5, step=0.1)),
    column(width=4, sliderInput("train.fraction", label="train fraction:",
                                min=0, max=1, value=0.8, step=0.1))),
  column(5,
         "ROC",
         
         # With the conditionalPanel, the condition is a JavaScript
         # expression. In these expressions, input values like
         # input$n are accessed with dots, as in input.n
         plotOutput("auc_gbm", height = 800,width = 800)
  ),
  column(4,textOutput("text")),
  
  
  # end fluidRow
  
  # Render plot in panel
  mainPanel(plotOutput("GBM Performance", width="150%", height=500))
  
)  # end fluidPage interface



##############################
## Define the server code
# The function ser_ver() accepts the arguments "input" and "output".

ser_ver <- function(input, output) {
  
  ## Re-calculate the model with new parameters
  # The function reactive() accepts a block of expressions
  # which calculate the model, and returns the model output.
  da_ta <- reactive({
    
    # Extract model parameters from the argument "input"
    n.trees <- input$n.trees
    interaction.depth <- input$interaction.depth
    shrinkage <- input$shrinkage
    bag.fraction <- input$bag.fraction
    train.fraction <- input$train.fraction
    
    
    # train the model
    gbm_model <- gbm(default_flag ~ .,
                     data = data_train_gbm,
                     n.trees = n.trees,
                     distribution = "bernoulli",
                     interaction.depth = interaction.depth,
                     shrinkage = shrinkage,
                     bag.fraction = bag.fraction,
                     train.fraction = train.fraction)
    
    gbm_test <-  predict(gbm_model, newdata = data_test_gbm, n.trees = n.trees)
    
    
    auc_gbm <-  roc(data_test_gbm$default_flag, gbm_test, plot = FALSE)
    
    round(auc_gbm$auc,digits = 3)
    
  })
  
  output$auc_gbm <- renderPlot({
    
    # extract model parameters
    n.trees <- input$n.trees
    interaction.depth <- input$interaction.depth
    shrinkage <- input$shrinkage
    bag.fraction <- input$bag.fraction
    train.fraction <- input$train.fraction
    
    
    gbm_model <- gbm(default_flag ~ .,
                     data = data_train_gbm,
                     n.trees = n.trees,
                     distribution = "bernoulli",
                     interaction.depth = interaction.depth,
                     shrinkage = shrinkage,
                     bag.fraction = bag.fraction,
                     train.fraction = train.fraction)
    
    gbm_test <-  predict(gbm_model, newdata = data_test_gbm, n.trees = n.trees)
    
    
    #plot ROC
    roc<-roc(data_test_gbm$default_flag, gbm_test, plot = TRUE)
    plot(roc)
    
    
  
    #text of parameters
    text(0,0.3,
       lab=paste0(
        "ntree = ", format(n.trees, digits=3), "\n",
        "depth = ", format(interaction.depth, digits=3), "\n",
        "shrinkage = ", format(shrinkage, digits=3), "\n",
        "bag fraction = ", format(bag.fraction, digits=3), "\n",
        "train fraction = ", format(train.fraction, digits=3), "\n",
        "AUC = ", da_ta()),
    adj=c(1, 1), cex=1.2, lwd=2)
  })
}

##############################
## Return a Shiny app object

shiny::shinyApp(ui=inter_face, server=ser_ver)

