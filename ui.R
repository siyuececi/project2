## app.R ##

library(shiny)
library(shinydashboard)


shinyUI(dashboardPage(
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
                 Also, to reduce the dimensions of gene expressions, dimension reduction techniques such as PCA could be choosen before applying Random forest model. If variances
                 method is choosen, then features with variances larger than 90% variances are remained, if PCA is choosen then the first 20 PCs are used.
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


)