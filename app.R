#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Dataset: 
#  https://www.kaggle.com/marlonferrari/elearning-student-reactions
library(ggplot2)
library(dplyr)
library(corrplot)
library(pROC)
library(ROCR)
library(Metrics)
library(popbio)

df = read.csv("online_classroom_data.csv")

# Clean
df$sk1_classroom =
    as.numeric(sub(",", ".", df$sk1_classroom, fixed = TRUE))
df$sk2_classroom =
    as.numeric(sub(",", ".", df$sk2_classroom, fixed = TRUE))
df$sk3_classroom =
    as.numeric(sub(",", ".", df$sk3_classroom, fixed = TRUE))
df$sk4_classroom =
    as.numeric(sub(",", ".", df$sk4_classroom, fixed = TRUE))
df$sk5_classroom =
    as.numeric(sub(",", ".", df$sk5_classroom, fixed = TRUE))
df$Approved = factor(df$Approved, levels = c(0,1))
dfNums = df[,2:15]
ui <- fluidPage(

    # Application title
    titlePanel("E-learning Data Exploration and Logistic Regression Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout( 
        sidebarPanel( 
            sliderInput("bins",
                        "Number of bins (histogram):",
                        min = 15,
                        max = 50,
                        value = 30),
            selectInput(
                "boxPlotVar", 
                "Box Plot Variable",
                choices=colnames(dfNums)),
            selectInput(
                "densVar",
                "Density Plot Variable",
                choices=colnames(dfNums)),
            selectInput(
                "densAlpha",
                "Density Plot Alpha",
                choices=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
            ),
            selectInput(
                "histVar",
                "Histogram Variable",
                choices=colnames(dfNums)),
            sliderInput(
                "trainPct",
                "Portion of data as training",
                min = 0.75,
                max = 0.9,
                value = 0.8
            ),
            sliderInput(
                "logThresh",
                "Min P-value for test output of 1",
                min = 0.5,
                max = 0.9,
                value = 0.5
            )
        ), # close sideBarPanel

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(tabPanel("Bar Graph", plotOutput("approvalGraph")),
                        tabPanel("Density Plot", plotOutput("dens")),
                        tabPanel("Box Plot", plotOutput("boxPlot")),
                        tabPanel("Histogram", plotOutput("hist")),
                        tabPanel("Logistic Regression", 
                                 plotOutput("corPlot"),
                                 verbatimTextOutput("trainComp"),
                                 plotOutput("ROC"))
            ) # close tabset panel
        ) # close mainPanel
    ) # close sideBarLayout
) # close fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$approvalGraph = renderPlot({
        ggplot(df, aes(x = Approved, fill = Approved)) +
            geom_bar() +
            ggtitle("Count of students by approval type")
    })
    output$boxPlot <- renderPlot({
        # boxplot broken down by approval
        ggplot(df, aes(x = Approved, y = get(input$boxPlotVar))) +
            geom_boxplot() +
            ylab(input$boxPlotVar) +
            scale_y_continuous(trans="log10") +
            ggtitle("Boxplot broken down by approval")
    })
    output$dens = renderPlot({
        ggplot(df, aes(x = get(input$densVar), fill = Approved)) +
            geom_density(alpha = input$densAlpha) +
            scale_x_continuous(trans="log10") +
            ggtitle("Density broken down by approval") +
            xlab(input$densVar)
    })
    output$hist = renderPlot({
        ggplot(df, aes(x = get(input$histVar), fill = Approved)) +
            geom_histogram(bins = input$bins) +
            facet_wrap(~ Approved) +
            scale_x_continuous(trans="log10") +
            ggtitle("Count broken down by approval") +
            xlab(input$histVar)
    })
    output$corPlot = renderPlot({
        correlations = cor(df[,2:15])
        corrplot(correlations, method = "circle")
    })
    
    trainPct = reactive({
        input$trainPct
    })
    # train and test set
    output$trainComp = renderPrint({
        set.seed(124)
        sample_size = floor(trainPct() * nrow(df))
        train_ind = sample(seq_len(nrow(df)), size = sample_size)
        
        train = df[train_ind, ]
        test = df[-train_ind, ]
        paste("Number of training data points is:",sample_size)
        glmModel = glm(Approved ~ ., 
                       data = train, 
                       family = "binomial",
                       control = list(maxit=200))
        print("--------Model Summary--------")
        print(summary(glmModel))
        print("--------Deviance Table Analysis--------")
        print(anova(glmModel, test = 'Chisq'))
        print("--------Top of Predicted Test Data--------")
        test$modPredict = predict(glmModel, 
                                  newdata = test, 
                                  type = "response")
        
        print(head(test))
        
        test$origModPredict = test$modPredict
        test$modPredict = ifelse(test$modPredict > input$logThresh, 1, 0)
        
        # Correctness of model
        print("--------Overall Model Performance--------")
        mean(test$modPredict == test$Approved)
    })
    output$ROC = renderPlot({
        set.seed(124)
        sample_size = floor(trainPct() * nrow(df))
        train_ind = sample(seq_len(nrow(df)), size = sample_size)
        
        train = df[train_ind, ]
        test = df[-train_ind, ]
        paste("Number of training data points is:",sample_size)
        glmModel = glm(Approved ~ ., 
                       data = train, 
                       family = "binomial",
                       control = list(maxit=200))
        
        # ROC plot
        pred = predict(glmModel, test, type = "response")
        pObject = ROCR::prediction(pred, test$Approved )
        
        rocObj = ROCR::performance(pObject, measure="tpr", x.measure="fpr")
        aucObj = ROCR::performance(pObject, measure="auc")  
        plot(rocObj, main = paste("ROC Plot: Area under the curve =", round(aucObj@y.values[[1]] ,4))) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
