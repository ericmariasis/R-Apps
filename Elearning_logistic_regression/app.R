#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
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
            tabsetPanel(tabPanel("Cover Page/Overview",
                                 h3("Dataset Overview"),
                                 p("The dataset and additional info can be found at:"),
                                 p(HTML('<body><a href="https://www.kaggle.com/marlonferrari/elearning-student-reactions">https://www.kaggle.com/marlonferrari/elearning-student-reactions</a></body>')),
                                 p("Each datapoint is a student who took an online class and a number of metrics were recorded 
                                   for each student such as whether they approved of the class, number of total posts, 
                                   and other similar metrics."),
                                 h3("Motivation"),
                                 p("With the rapidly rising cost of living on a campus and taking classes in person, 
                                   online learning is starting to become a more convenient and price effective way to 
                                   receive an education in various subject areas. Since e-learning is still relatively new, 
                                   it is useful to gather and analyze data about which aspect of an online learning environment 
                                   help to retain students and keep them interested. 
                                   This is especially important since on many MOOC sites, they cite that around 90% 
                                   of their students do not finish courses they start. If this data can help educational institutions make 
                                   informed choices about how best to disseminate subject content through online means, more people 
                                   can benefit from the experience, and a large amount of expense can be saved 
                                   by not having to worry about the costs associated with having to repeatedly conduct a 
                                   course live."),
                                 h3("Conclusions and Findings"),
                                 p("In general, the data indicates that 
                                   students showing a higher degree of engagement in 
                                   the online classroom are far more likely to say they 
                                   approved of the class than students who scored low on 
                                   metrics indicating class engagement. The various measures of engagement 
                                   such as the number of helpful posts and number of reactions to 
                                   posts correlated so strongly with whether the students 
                                   ultimately approved of the class, that the logistic regression model 
                                   used for the analysis performed nearly perfectly when the data was split 
                                   into a training set and a test set and a prediction glm 
                                   model was run."),
                                 h3("Algorithm Overview"),
                                 p("The algorithm used for the analysis was
                                   logistic regression. In the E-learning data, 
                                   there was a clear dependent/target variable of interest 
                                   which was whether or not each student 
                                   ultimately approved of the class at the end. 
                                   The data value of the approval is 0 if the student 
                                   did not approve of the class, and the value is 1 
                                   if the student did approve of the class."),
                                 p("If the target value of interest was continuous rather than 
                                   discrete, perhaps general linear regression would have been 
                                   a suitable algorithm to use. The way general linear regression works 
                                   is that the model predicts the value of the target variable based on 
                                   the linear combination of the input, or feature, variables. 
                                   The equation would look something like y = (Bo) + (B1)x + ... 
                                   where Bo is the y intercept of the model and B1 is the coefficient of 
                                   one of the features. Regression models in general are suitable for this data 
                                   because the input variables are numerical and continuous. A different method would 
                                   have to be used if the variables were categorical in nature."),
                                 p("The reason logistic regression must be used for this dataset 
                                   rather than general linear regression is that the output value of the 
                                   approval designation must be binary, and bounded between 0 and 1.
                                   With regular linear regression, there is no guarantee that the output values 
                                   would be bounded between 0 and 1. Logistic regression makes its predictions 
                                   of the output value based on a function called the sigmoid function. The sigmoid 
                                   function is defined such that low input values approach 0 for the output, and high values of input approach 1 for the output.  
                                   Every output value is between 0 and 1 which is what is needed for binary data. 
                                   One of the two main reasons logistic regression works well with this dataset is that 
                                   the input/independent variables are numerical and continous values, such as the 
                                   number of helpful posts. The other main reason is that the input variables 
                                   tend to strongly correlate with the approval designation. Since logistic regression 
                                   can output values between 0 and 1, a threshold is usually designated to say that if 
                                   the output of the model is greater than a certain value, designate it as 1 - else 
                                   designate it as 0. For example, a common threshold is that if the output is 
                                   greater than 0.5, assign the value to 1 otherwise assign it to 0. 
                                   In this web application, it allows you to adjust this threshold to see how 
                                   the performance of the test model is affected. Use the 'Min P-value for test output of 1' 
                                   slider to adjust this value.")),
                        tabPanel("Bar Graph", plotOutput("approvalGraph")),
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
