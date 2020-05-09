## app.R ##
library(shinydashboard)
library(caret)
library(randomForest)
gse <- read.csv("GSE120396_expression_matrix.txt", row.names = 1)
load("rejection_status.rdata")

largevar = apply(gse, 1, var)
ind = which(largevar > quantile(largevar, 0.9))
X = as.matrix(t(gse[ind,]))
pca <- prcomp(X, scale = TRUE)
y <- rejection_status
X2 <- pca$x[,1:20]

ui <- dashboardPage(
  dashboardHeader(title = "Kidney transplant"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("ambulance")),
      menuItem("Dimension Reduction", tabName = "d1", icon = icon("bullhorn")),
      menuItem("Random Forest Model", tabName = "d2", icon = icon("bar-chart-o"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "Introduction",
              h3("It is known that Kidney transplant is often the treatment of choice to people with end-stage kidney disease. And it is important to understand the the characteristic of stable patients and patients who experienced rejection. This study is aimed to find out how well could us achieve in predicting  patient outcome in Kidney transplant that is whether stable or rejection as well as which genes are most important in predicting the patient outcome.
                  To addressing the question interested, random forest model was selected to make predictions of patient outcomes.The data used is GSE120396 RNA-seq which includes 88 samples(patients) with over 10000 gene expressions and the response outcome is whether these  patients stable or rejection in  Kidney transplant.
                  Also, to reduce the dimensions of gene expressions, dimension reduction techniques such as PCA could be chosen before applying Random forest model. If variances
                  method is chosen, then features with variances larger than 90% variances are remained, if PCA is chosen then the first 20 PCs are used.
                 ")
      ),
      
      tabItem(tabName = "d1",
              fluidRow(
                box(plotOutput("plot1", height = 600)),
                box(
                  title = "Select a dimension reduction method:",
                  radioButtons("radio1", "Methods", c("Variances" = "Variances", "PCA" = "PCA"))
                  
                ),
                                box(tableOutput("table1")),
                box(verbatimTextOutput("text1"))
              )),
      
      tabItem(tabName = "d2",
              fluidRow(
                box(plotOutput("plot2", height = 600)),
                box(
                  title = "",
                  radioButtons("radio2", "Methods", c("Variances" = "Variances", "PCA" = "PCA"))
                  
                ),
                box(verbatimTextOutput("text2"))
              )
      )
      
    )
  )
)


server <- function(input, output) {
 
  
  output$plot1 <- renderPlot({
  
     if(input$radio1 == "PCA") {
       
       screeplot(pca, main = "scree plot of PCs")
       
     } else {
      
       heatmap(as.matrix(t(gse[ind,])))
       
     }

  })
  
  output$table1 <-  renderTable(
    { 
      if(input$radio1 == "PCA") {
        
        a <- data.frame(gene =row.names(pca$rotation), pca$rotation)
        a[1:20,1:10]
        
      } else {
        NULL
      }
    }
    
  )
  
  output$text1 <-  renderText(
    { 
      if(input$radio1 == "PCA") {
        
        NULL
        
      } else {
       
        colnames(X)  
        
      }
    }
    
  )
  
  output$plot2 <- renderPlot({
    
    if(input$radio2 == "PCA") {
      
      set.seed(1)
      rf <- randomForest(x = X2, y = as.factor(y))
      varImpPlot(rf, main = "PCA importance plot")
      
    } else {
     
      set.seed(1)
      rf <- randomForest(x = X, y = as.factor(y))
      varImpPlot(rf, main = "gene expressions importance plot")
      
    }
    
  })
  
  output$text2 <-  renderPrint(
    { 
      if(input$radio1 == "PCA") {
        set.seed(1)
        rf <- randomForest(x = X2, y = as.factor(y))
        a <-  confusionMatrix(predict(rf), as.factor(y), positive = "Yes")
        a
        
      } else {
        set.seed(1)
        rf <- randomForest(x = X, y = as.factor(y))
        a <-  confusionMatrix(predict(rf), as.factor(y), positive = "Yes")
        a
      }
    }
    
  )
  

  
  
}

shinyApp(ui, server)