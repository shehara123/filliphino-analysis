# Load R packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(readr)
library(recipes)  # for feature engineering
# Modeling packages
library(glmnet)   # for implementing regularized regression
library(caret)    # for automating the tuning process


#import dataset
Family_Income_and_Expenditure <- read_csv("F:/R/Family Income and Expenditure.csv")
attach(Family_Income_and_Expenditure)


df <-data.frame(food_exp=`Total Rice Expenditure`,income=`Total Household Income`,
                region=`Region`,agri_ind=as.factor(`Agricultural Household indicator`),
                med= `Medical Care Expenditure`,adults=`Total Number of Family members`,
                babies=`Members with age less than 5 year old`,
                kids=`Members with age 5 - 17 years old`)

detach(Family_Income_and_Expenditure)
view(df)

#reducing observations
set.seed(111)
sample_size = round(nrow(df)*.80) # setting what is 80%
index = sample(seq_len(nrow(df)), size = sample_size)

df = df[-index, ]
attach(df)

#Importing model
cv_glmnet <- readRDS(file = "./cv_glmnet.rda")

# Define UI

ui <- fluidPage(
  
 style="background-image: url(https://wallpaperaccess.com/full/4393443.jpg); background-size: cover;
  position: absolute;
  top: 0; left: 0; right: 0; bottom: 0;
  z-index: -1;"
  ,theme = shinytheme("united"),
  
  tags$head(tags$style("#text{color: red;font-size: 60px;}")),
  
  fluidRow(style = "border: 8px  outset #add9ff;",
    div(style = "height:18vh;",align = "center",tags$p(tags$b("PREDICTOR",
         style="font-size:80px;color:#b9fc00;
         font-family: 'Broadway';"),tags$b(".com",style="font-size:40px;color:#f1fc1c;
         font-family: 'Broadway';")) 
               )),
  fluidRow(
    column(8,offset = 0,style='padding:0px;',div(
      style =  "height:82vh;",
      
      ##INput table
      tags$table(style="border-spacing: 15px;",
        tags$tr(
          tags$td(style="font-size:40px;color:#f2fa02;font-family: 'Times New Roman'",tags$b("Income")),
          tags$td(style="padding-left: 30px;",numericInput("income",label='',value = 1)),
          tags$td()
        ),
        
        tags$tr(
          tags$td(style="font-size:40px;color:#f2fa02;font-family: 'Times New Roman'",tags$b("Region")),
          tags$td(style="padding-left: 30px;",selectInput(
            "region",'',c("Caraga","III - Central Luzon","ARMM","II - Cagayan Valley",
            "NCR","IVA - CALABARZON","VIII - Eastern Visayas","IVB - MIMAROPA","V - Bicol Region","
VII - Central Visayas","X - Northern Mindanao","IX - Zasmboanga Peninsula","CAR","
I - Ilocos Region","XII - SOCCSKSARGEN","XI - Davao Region","VI - Western Visayas")
          )),
          tags$td()
        ),
        
        tags$tr(
          tags$td(style="font-size:40px;color:#f2fa02;font-family: 'Times New Roman'",tags$b("Agri_Ind")),
          tags$td(style="padding-left: 30px;",selectInput(
            "agri",'',c("0","1","2")
          )),
          tags$td(rowspan="3",style="padding-left: 50px;",align="center",
                  actionButton(inputId = "find",label = "FIND",
                  style="background-color: #e7e7e7; color: black;font-size:50px;
                   border-radius: 12px; "))
        ),
        
        tags$tr(
          tags$td(style="font-size:40px;color:#f2fa02;font-family: 'Times New Roman'",tags$b("Med_expnd")),
          tags$td(style="padding-left: 30px;",numericInput("med",label='',value = 1))
        ),
        
        tags$tr(
          tags$td(style="font-size:40px;color:#f2fa02;font-family: 'Times New Roman'",tags$b("Adults")),
          tags$td(style="padding-left: 30px;",numericInput("adults",label='',value = 1))
        ),
        
        tags$tr(
          tags$td(style="font-size:40px;color:#f2fa02;font-family: 'Times New Roman'",tags$b("Kids")),
          tags$td(style="padding-left: 30px;",numericInput("kids",label='',value = 0)),
          tags$td()
        ),
        
        tags$tr(
          tags$td(style="font-size:40px;color:#f2fa02;font-family: 'Times New Roman'",tags$b("Babies")),
          tags$td(style="padding-left: 30px;",numericInput("babies",label='',value = 1)),
          tags$td()
        )
        
        
      )                          )),
    
    
    
    
    
    
    column(4,offset = 0,style='padding:0px;', div(style = "height:82vh; background-color:#ededeb;opacity:0.78",
                  tags$p(tags$b("Expected monthly expenditure "),style="font-size:40px;padding:20px;font-family: 'Times New Roman"
                         ,textOutput("text"))))
  )
  
  ) # fluidPage

# Define server function  
server <- function(input, output) {
  
  data <- eventReactive(input$find, {
    dt=df
    
    dt[1,1]=4;dt[1,2]=input$income;dt[1,3]=input$region;dt[1,4]=input$agri
    dt[1,5]=input$med;dt[1,6]=input$adults;dt[1,7]=input$babies;dt[1,8]=input$kids
   
    dt1=model.matrix(food_exp ~ ., dt)[, -1]
    
   round(as.numeric(exp(predict(cv_glmnet,newdata = t(as.matrix(dt1[1,]))))),2)
    
   })
  
  output$text <-renderText(paste(data(),"pessos"))
}

# Create Shiny object
shinyApp(ui = ui, server = server)

