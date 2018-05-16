library(devtools)
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
        
        plotOutput("clustering")
        
      ))
    

    
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
      
    popularity_rank_frame_filled_reshape <- reactive({  
      

      x <- popularity_rank_frame_filled()
      #Remove the sex and rank collumn
      x$sex  <- NULL
      x$rank <- NULL
      
      #reshape dataframe 
     ## x %>% gather (forename, year)
      x <- spread(x, key = year, value = popularity_score)

      return(x)
      })
    
#k-means clustering
#http://www.sthda.com/english/wiki/print.php?id=236
#####################################################################################################################################################
    #cluster <- reactive({ 
    #  cluster::pam( popularity_rank_frame_filled_reshape(), input$centroids) 
    #  print(cluster::pam( popularity_rank_frame_filled_reshape(), input$centroids) )
    #})
    
    cluster <- reactive({ 
      print("clutering!!!")
      #Data preparation and descriptive statistic
      df <- popularity_rank_frame_filled_reshape()
      print(df)
      print("scale")
      #df <- scale(df)
      print(df)

      #Determine the number of optimal clusters in the data
      #Compute k-means clustering
      set.seed(123)
      print("km.res")
      km.res <- pam(df, input$centroids)  
      print(km.res)
      
      print("aggregate")
      aggregate(popularity_rank_frame_filled_reshape(), by=list(cluster=km.res$cluster), mean)
      print(aggregate(popularity_rank_frame_filled_reshape(), by=list(cluster=km.res$cluster), mean))  
      
      #print data
      print(fviz_cluster(km.res, df))
      return(km.re)
      
      
    }) 
 
    

    
        
######################################################################################################################################################    
  
  
  test <- reactive({ 
    data.frame( popularity_rank_frame_filled_reshape())
    
  })
  
  
 ## test2 <- reactive({ 
##    data.frame( cluster()  )
  ##    })
  
  output$Prepared_Data <- renderDT(
    test(),
    options = list(lengthChange = FALSE),
    rownames = FALSE)
  
  output$Prepared_Data1 <- renderDT(
    cluster(),
    options = list(lengthChange = FALSE),
    rownames = FALSE)
}

output$clustering <- fviz_cluster(clustering, popularity_rank_frame_filled_reshape())


shinyApp(ui=ui, server=server)

