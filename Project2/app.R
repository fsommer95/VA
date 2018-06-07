#library(devtools)
library(shiny)
library(shinydashboard)
library(DT)
library(stringr)
library(dplyr)
library(readr)
library(base)
library(ggplot2)
library(stats)
library(rsconnect)
library(tidyr)
library(stringi)
library(cluster)
library("factoextra")
library(ggfortify)
library(cluster)
library(ggfortify)



#Loading Data Set from file and saving it ugetwnder european_soccer_dataset
babynames <- read_rds("babynames.rds")


ui <- dashboardPage(
  dashboardHeader(title = "Clustering Babynames", titleWidth = 450),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    titlePanel("Task 1"),
    
    #InputID bestimmt die ID der Inputs, wird später in Server befehlen aufgerufen
    
    
    fluidRow(
      box( 
        
        title = "League History", 
        solidHeader = TRUE, 
        status = "primary",
        width = 4,
        dataTableOutput("Prepared_Data")
      )),
    
    fluidRow(
      box( 
        
        title = "League History", 
        solidHeader = TRUE, 
        status = "primary",
        width = 4,
        dataTableOutput("Prepared_Data1")
      ),
      
      box(
        
        sliderInput("centroids", h3("Number of centroids"),
                    min = 2, max = 20, value = 2),
        # k is the number of clusters
        plotOutput("k")
      ),
      
      box(
        plotOutput("plot1"),
        plotOutput("plot2")

      )
      
      
      )
    

    
  ))



#########################################################################################################################################################
server<-function(input,output){
  
  
  #Calculate frequency of names 
  name_frequency <- reactive({ 
    babynames$forename %>% table() %>% data.frame()%>% setNames(c("forename", "Frequency")) %>% filter(Frequency >=5)
  })
  
  #Filter dataset regarding names 
  prep_data <- reactive({ 
    babynames %>%  left_join(name_frequency(), by= c('forename'))  %>% na.omit() %>% setNames(c("year","sex","rank","forename","frequency")) 
  })
  
  prep_data_frame <- reactive({ 
    data.frame(prep_data(), maxRank(), minRank() ) 
  })
##########################################################################################################################################################  
  
  
  #Minimaler/Maximaler Rank 
  maxRank <- reactive({ 
    max(babynames$rank) 
  })

  
  minRank <- reactive({ 
    min(babynames$rank) 
  })
  
  #Funktion fuer Popularity rank 
  pop_func <- function(a,b,c) (1-((a-c)/(b-c)))*100
  
  #Berechnung Popularity rank
  popularity_score <- reactive({  
    apply(prep_data_frame()[,c(3,6,7)],1, function(x) pop_func(x[1],x[2],x[3])) %>% round(digits=2)
    
  })
  
  popularity_rank_frame <- reactive({  
    data.frame(prep_data_frame(), popularity_score()) %>% select(-c(5,6,7)) %>% setNames(c("year","sex","rank","forename","popularity_score")) 
    
  })
  
  
    popularity_rank_frame_filled <- reactive({  
      popularity_rank_frame() %>% complete(forename, year = 1890:2017,
        fill = list(popularity_score= 0))
    })
    
      
    #reshape the popularity rank matrix
   nice_data <- reactive({  
      x <- popularity_rank_frame_filled()
      #Remove the sex and rank collumn
      x$sex  <- NULL
      x$rank <- NULL
      #reshape dataframe 
     ## x %>% gather (forename, year)
      x <- spread(x, key = year, value = popularity_score)
      return(x)
      })
    
   
   #reduce 128 Dimensions to 2 in a meaningful way --> use just 2 years --> prcomp()
   # Default S3 method:
   #prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE,
   #       tol = NULL, rank. = NULL, ...)
      #just use the numbers and reduce the names
      #nice_data.active <- nice_data[1:379, 1:129]
    #first create a numeric matrix!!!
   #test2 <- data.matrix(nice_data[1:379, 2:129])
   #autoplot(prcomp(test2), data = nice_data)
   
   nice_data2 <- reactive({  
     data.matrix(nice_data()[1:379, 2:129])
   })
   
   #plot the data --> scatterplot
   

   
   
    metroids1 <- reactive({
      autoplot(prcomp(nice_data2()), data = nice_data())
          })
    
    metroids2 <- reactive({
      clusplot(pam(nice_data2(), input$centroids), nice_data()[1] , labels = 4, lines = 0, color = TRUE, stand = TRUE)
      #autoplot(prcomp(nice_data2()), data = nice_data(), label.size = input$k)
      #autoplot(pam(nice_data2()[-5], 3), frame = TRUE, frame.type = 'norm')
     
      })
    

    test <- reactive({ 
     data.frame(nice_data())
    }) 

  
  ##################################################################################################################
  
  output$Prepared_Data <- renderDT(
    test(),
    options = list(lengthChange = FALSE),
    rownames = FALSE)
    
    
    output$plot2  <- renderPlot({
      metroids2()
    })   
  output$plot1  <- renderPlot({
    metroids1()
  }) 

  

}

#output$clustering <- fviz_cluster(clustering, popularity_rank_frame_filled_reshape())

shinyApp(ui=ui, server=server)

