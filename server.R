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
shinyServer(function(input, output) {
  
  
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
)
