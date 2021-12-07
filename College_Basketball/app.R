library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(mapproj)
library(shinythemes)
library(RColorBrewer)


sports <- read_csv("data/college_revenue.csv")
sports <- sports[order(sports$`Institution Name`),]
sports$`Sanction Name` = toupper(sports$`Sanction Name`)
colnames(sports) [48] <- 'Grand Total Expenses'
colnames(sports) [39] <- 'Grand Total Revenue'
schools <- unique(sports$`Institution Name`)
sanctions <- sort(unique(sports$`Sanction Name`))
states <- sort(unique(sports$`State CD`))
min_year <- min(sports$`Survey Year`)
max_year <- max(sports$`Survey Year`)
sports$'Total Profit' <- sports$`Grand Total Revenue` - sports$`Grand Total Expenses`

# Define UI for application that draws a histogram
ui <- navbarPage(# Application title
    title="College Basketball Revenue and Expenses: 2003-2016",
    footer = 'Shiny App Project - DSBA 5122: Visual Analytics',
    theme = shinytheme('cosmo'),
    
    # Sidebar with a slider input for number of bins
    tabsetPanel(
        tabPanel("Heat Map",
            sidebarLayout(
                sidebarPanel(
                    
                    radioButtons(
                      "value",
                      "Value to Display:",
                      list("Total Revenue","Average Revenue","Total Expense","Average Expense")
                    ),
                    sliderInput(
                      "year",
                      "Select Year:",
                      min = min_year,
                      max = max_year,
                      value = 2003,
                      step = 1,
                      sep = ""
                    ),
                    radioButtons(
                        "sanction",
                        "Select Sanction:",
                        choices = sanctions,
                        selected = 'NCAA'
                    )
                    ),
                mainPanel(plotOutput("mapPlot")))
    ),
    tabPanel("Total Revenue and Expenses",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("sanctions", "Select Sanction:", choices = sanctions, 
                              selected = 'NCAA'),
                 uiOutput("cat_choice"),
                 
                 p(strong("Use the checkbox below to compare with average revenue and expenses 
                          for all schools in same sanction.")),
                 checkboxInput("averages", "Show Averages", value=FALSE),
                
               ),
               mainPanel(
                 fluidRow(plotOutput("linechart", height = "600px", width = "600px")),
                 fluidRow(
                   column(10,
                          br(),
                          p('\n When revenue and expenses are close in value, you may not be able to see expenses line.'))
                 )
               )
               
             )
    ),
    tabPanel('Top 10 Schools',
    sidebarLayout(
      sidebarPanel(
        sliderInput("year3",
                    "Select Year:",
                    min = min_year,
                    max = max_year,
                    value = 2003,
                    step = 1,
                    sep = ""),
        radioButtons("sanc", "Select Sanction:", choices = sanctions, selected = 'NCAA'),
        uiOutput("state_choice"),
        radioButtons('value2',
                     'Value to Display:',
                     list("Total Revenue","Total Profit", "Total Expenses"),
                     selected = 'Total Revenue')
      ),
      
      
      mainPanel(
        plotOutput("top10", height = "700px", width = "900px")
      )
      )
    ),
    
    
    tabPanel("Men's vs Women's",
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year2",
                             "Select Year:",
                             min = min_year,
                             max = max_year,
                             value = 2003,
                             step = 1,
                             sep = ""),
                 selectInput("college", "Select School:", schools,
                             selected = schools[1])
               ),
               
               mainPanel(
                 plotOutput("schoolPlot")
               )
             )
            
             ),
    
    
    tabPanel('About',
             br(),
             column(1),
             column(8, 
                    h5('This app was developed by Adonis Abdullah, Taylor Cox, and Connor Derrick.'),
                    p("It was the result of Chase Romano's Visual Analytics course at the University of 
                       North Carolina at Charlotte through the Data Science and 
                       Business Analytics MS program."),
                    br(),
                    HTML("<p> View application on <a href = 'https://github.com/tcox17/Sports_Analytics'> 
                          Taylor's Github </a>"),
                    br(),
                    HTML("<p> View application on <a href = 'https://github.com/cvderrick/College_Basketball_Revenues'> 
                         Connor's Github </a>"),
                    br(),
                    HTML("<p> View application on <a href = 'https://github.com/Adonis35/College-Basketball-Revenue-Expenses'> 
                         Adonis's Github </a>"),
                    hr(),
                    HTML('<a href = "https://www.linkedin.com/in/adonis-abdullah/" 
                         style = "color: #FFC300"> Adonis Abdullah Linkedin</a>'),
                    br(),
                    HTML('<a href = "https://www.linkedin.com/in/taylor-cox-uncc/"
                         style = "color: #FFC300"> Taylor Cox Linkedin</a>'),
                    br(),
                    HTML('<a href = "https://www.linkedin.com/in/connorvderrick/"
                         style = "color: #FFC300"> Connor Derrick Linkedin</a>')
             ),
             column(3)
    
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mapPlot <- renderPlot({
        state_grouping <- sports %>% 
            filter(`Survey Year`==input$year) %>%
            filter(`Sanction Name`==input$sanction) %>%
            group_by(region,`Survey Year`) %>%
            summarise(`Total Revenue` =sum(`Grand Total Revenue`), 
                      `Total Expense`=sum(`Grand Total Expenses`), 
                      `Average Revenue` =mean(`Grand Total Revenue`), 
                      `Average Expense`=mean(`Grand Total Expenses`))
        state_grouping
        
        states <- map_data("state")
        map.df <- merge(states, state_grouping, by="region", all.state_grouping=T)
        map.df <- map.df[order(map.df$order),]
        
        # draw the histogram with the specified number of bins
        ggplot(map.df, aes(x=long,y=lat,group=group))+
            geom_polygon(aes(fill=pull(map.df,input$value)))+
            geom_path()+ 
            scale_fill_gradientn(input$value,colours=rev(heat.colors(3)),na.value="grey90", 
                                 labels = scales::label_number_si())+
            coord_map() +
            theme_bw() +
            labs(title = 'Basketball Revenue and Expenses by State', x = '', y = '') +
            theme(axis.ticks = element_blank(),
                  axis.text.x = element_blank(), 
                  axis.text.y = element_blank(),
                  panel.grid = element_blank())
            
            
                
    })
    df_school <- reactive({
      sports %>% filter(`Institution Name` == input$school)
    })
    
    df_sanction <- reactive({
      sports %>% filter(`Sanction Name` == input$sanctions)
    })
    
    output$cat_choice <- renderUI({
      selectInput(inputId = 'school',
                  label = 'Select School:',
                  choices=(sports 
                           [sports$`Sanction Name`==input$sanctions,"Institution Name"]),
                  selected = (sports 
                              [sports$`Sanction Name`==input$sanctions,"Institution Name"])[1])
    })
    
    output$linechart <- renderPlot({
      df <- df_school()
      
      
      p <- ggplot(data = df,
                  aes(x = `Survey Year`)) +
        geom_line(aes(y= `Grand Total Expenses`, color = 'firebrick3'), size = 1.5) +
        geom_line(aes(y= `Grand Total Revenue`, color = 'seagreen4'), size = 1.5) +
        scale_color_identity(name = 'Measure',
                             breaks = c('firebrick3', 'seagreen4'),
                             labels = c('Expenses', 'Revenue'),
                             guide = 'legend')+
        labs(title = input$school,
             x='Year', y ='US Dollars ($)') +
        scale_y_continuous(labels = scales::label_number_si()) +
        theme_bw(base_size =  16) 
        #theme(title = element_text(size = 17),
              #axis.text.x = element_text(size = 15),
              #axis.text.y = element_text(size = 15, face = "bold"))
      
      if (input$averages) {
       sanc_df <- df_sanction()
       
        sanc_df_Exp <- sanc_df %>%
          group_by(`Survey Year`) %>%
          summarise_at(vars(`Grand Total Expenses`), list(name = mean))
         
        sanc_df_Rev <- sanc_df %>%
          group_by(`Survey Year`) %>%
          summarise_at(vars(`Grand Total Revenue`), list(name = mean))
        
        p <- p +
          geom_line(data = sanc_df_Exp,
                    aes(y = name,
                        color="grey"), size = 1.25,
                        linetype = 'dashed')+
          geom_line(data = sanc_df_Rev,
                    aes(y = name,
                        color="black"), size = 1.25,
                        linetype = 'dashed')+
          scale_color_identity(name = 'Averages',
                               breaks = c('grey', 'black', 'firebrick3', 'seagreen4'),
                               labels = c('Average Expenses', 'Average Revenue', 'Expenses', 'Revenue'),
                               guide = 'legend')
      }
      
      
      p
      
      
    })
    output$state_choice <- renderUI({
      selectInput(inputId = 'state',
                  label = 'Select State:',
                  choices=(sports 
                           [sports$`Sanction Name`==input$sanc,"State CD"]),
                  selected = (sports 
                              [sports$`Sanction Name`==input$sanc,"State CD"])[1])
    })
    
    output$top10 <- renderPlot({
      state_grouping <- sports %>% 
        filter(`Survey Year`==input$year3) %>% 
        filter(`Sanction Name`==input$sanc) %>%
        filter(`State CD` == input$state) %>%
        arrange(`Grand Total Revenue`) %>%
        top_n(`Grand Total Revenue`,n=10)
      
      profit_grouping <- sports %>% 
        filter(`Survey Year`==input$year3) %>% 
        filter(`Sanction Name`==input$sanc) %>%
        filter(`State CD` == input$state) %>%
        arrange(`Total Profit`) %>%
        top_n(`Total Profit`,n=10)
      
      exp_grouping <- sports %>% 
        filter(`Survey Year`==input$year3) %>% 
        filter(`Sanction Name`==input$sanc) %>%
        filter(`State CD` == input$state) %>%
        arrange(-`Grand Total Expenses`) %>%
        top_n(`Grand Total Expenses`,n=-10)
      
      
      
      gg <- ggplot(state_grouping, aes(x =reorder(`Institution Name`, `Grand Total Revenue`))) +
        geom_bar(aes(y = `Grand Total Revenue`, color = `Institution Name`, 
                     fill = `Institution Name`), stat = 'identity')+ 
        coord_flip() +
        theme_bw(base_size =  16) +
        scale_color_brewer(palette="Spectral") +
        scale_fill_brewer(palette="Spectral") +
        scale_y_continuous(labels = scales::label_number_si())+
        labs(title = 'Top Schools with Highest Revenue',
             x='', y ='Revenue ($)')
      
      gg2 <- ggplot(profit_grouping, aes(x =reorder(`Institution Name`, `Total Profit`))) +
        geom_bar(aes(y = `Total Profit`, color = `Institution Name`, 
                     fill = `Institution Name`), stat = 'identity')+ 
        coord_flip() +
        theme_bw(base_size =  16) +
        scale_color_brewer(palette="Spectral") +
        scale_fill_brewer(palette="Spectral") +
        scale_y_continuous(labels = scales::label_number_si())+
        labs(title = 'Top Schools with Highest Profit',
             x='', y ='Profit ($)')
      
      gg3 <- ggplot(exp_grouping, aes(x =reorder(`Institution Name`, - `Grand Total Expenses`))) +
        geom_bar(aes(y = `Grand Total Expenses`, color = `Institution Name`, 
                     fill = `Institution Name`), stat = 'identity')+ 
        coord_flip() +
        theme_bw(base_size =  16) +
        scale_color_brewer(palette="Spectral") +
        scale_fill_brewer(palette="Spectral") +
        scale_y_continuous(labels = scales::label_number_si())+
        labs(title = 'Top Schools with Lowest Expenses',
             x='', y ='Expenses ($)')
      
      
      
      if(input$value2 == 'Total Profit'){
        gg2
      }
      
      else if(input$value2 == 'Total Expenses'){
        gg3
      }
      else{
        gg
      }
      
    })
    
    
    df_college <- reactive({
      sports %>%
        filter(`Survey Year` == input$year2) %>%
        filter(`Institution Name` == input$college) %>%
        select(`Institution Name`, `Revenues Men's Team`, `Expenses Men's Team`,
               `Revenues Women's Team`, `Expenses Women's Team`) %>%
        pivot_longer(cols = !`Institution Name`,
                     names_to = "Variables", values_to = "Dollars")
    })
    
    output$schoolPlot <- renderPlot({
      df2 <- df_college()
      
      ggplot(df2, aes(x = Variables, y = Dollars, fill = Variables)) + 
        geom_col() +
        theme_bw(base_size =  16)+
        scale_y_continuous(labels = scales::label_number_si())+
        labs(title = "Men's vs Women's Revenue and Expenses",
              x= '',y ='US Dollars($)') +
        scale_fill_manual(values = c('firebrick3', 'darksalmon', 'seagreen4', 'palegreen3'))+
        scale_color_manual(name = 'Averages',
                           breaks = c('firebrick3', 'darksalmon', 'seagreen4', 'palegreen3'),
                           labels = c("Expenses Men's Team", "Expenses Women's Team", 
                                      "Revenues Men's Team", "Revenues Women's Team"),
                           guide = 'legend')
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
