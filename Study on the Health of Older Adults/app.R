# Author: Jennifer Le
# Date: 2/3/25
# Modified from 
  # https://github.com/rstudio/shiny-examples/tree/main/030-basic-datatable 

# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)

questions_sorted <- sort(c(unique(as.character(df$Question))))

get_qID <- function(input){
  question_id <- questions_fct_tbl %>% 
    filter(Question.y == input) %>% pull(QuestionID)
  return(question_id)
}

dashboard1 <-
  tabPanel(
    "Explore Dataset",
    sidebarLayout(
      sidebarPanel(
        selectInput("Topic", "Topic:", c("All",unique(as.character(df$Class)))),
        selectInput("Question", "Question:", c("All",unique(as.character(df$Question)))),
        selectInput("Location", "Location:", c("All",unique(as.character(df$Location)))),
        selectInput("Age Group", "Age Group:", c("All",unique(as.character(df$Age_Group)))),
        selectInput("Gender or Race", "Gender or Race:", c("All",unique(as.character(df$Gender_or_Race))))
      ),
      mainPanel(
        DT::dataTableOutput("table")
      )
    )
  )

dashboard2 <-
  tabPanel(
    "Ranking by Question",
    sidebarLayout(
      sidebarPanel(
        selectInput('Question2','Question:', questions_sorted,
                    selected='Self-reported health of "fair" or "poor"'),
        selectInput('GroupBy','Group By:', c('Gender_or_Race','Location','Age_Group'))
      ),
      mainPanel(
        plotOutput('barplot')
      )
    )
  )

dashboard3 <-
  tabPanel(
    "Correlation Exploration",
    sidebarLayout(
      sidebarPanel(
        selectInput("x_axis", "Question 1 (x-axis):", questions_sorted,
                    selected="Experiencing frequent mental distress"),
        selectInput("y_axis", "Question 2 (y-axis):", questions_sorted,
                    selected="Smoking every day or some days"),
        verbatimTextOutput("cor"),
        "Interestingly, having a physical or mental disability has a much weaker
        correlation with frequent mental distress than smoking."
      ),
      mainPanel(
        plotlyOutput('scatterplot')
      )
    )
  )

dashboard4 <-
  tabPanel(
    "Map",
    fluidRow(
      column(4, selectInput("QuestionMap", "Question", questions_sorted, 
                            selected="Have a lifetime diagnosis of depression")),
      column(4, selectInput("Color", "Color Theme", c("Blue", "Purple", "Green", "Orange"))),
      column(1, checkboxInput("allYears", "All Years", value = TRUE)),
      column(3, sliderInput("Year", "Year", 2015, 2022, 2015, step=1, sep=""))
    ),
    plotlyOutput('map')
  )

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "Analysis of Older Adult's Health",
    dashboard1,
    dashboard2,
    dashboard3,
    dashboard4
  ) 
) 

# Define server function  
server <- function(input, output, session) {
  output$table <- DT::renderDataTable(DT::datatable({
    data <- select(df, Class, Question, Value,
                   Location, Age_Group, Gender_or_Race)
    
    if (input$Topic != "All") {
      data <- data[data$Class == input$Topic,]
    } 
    if (input$Question != "All") {
      data <- data[data$Question == input$Question,]
    }
    if (input$Location != "All") {
      data <- data[data$Location == input$Location,]
    }
    if (input$'Age Group' != "All") {
      data <- data[data$Age_Group == input$'Age Group',]
    }
    if (input$'Gender or Race' != "All") {
       data <- data[data$Gender_or_Race == input$'Gender or Race',]
    }
    data
  }))
  
  output$barplot <- renderPlot({
    # get question_id from question user chooses
    qID <- get_qID(input$Question2)
    
    # create a data frame summarizing results, use get() to transform string id to variable
    df_res <- df_pivot %>% 
      filter(!is.na(get(qID))) %>% 
      group_by(get(input$GroupBy)) %>% 
      summarize(avg_percent = mean(get(qID))) %>% 
      arrange(avg_percent) 
    
    # Get the first column name safely
    # Uses !!sym() to safely reference the first column dynamically.
    col_name <- sym(names(df_res)[1]) 
    
    # create plot 
    plot <- 
      ggplot(df_res, mapping = 
          aes(
            y=reorder(!!col_name, avg_percent), 
            x=avg_percent,
            fill = avg_percent
          ), color=avg_percent) + 
      geom_col() +
      scale_fill_gradient(high = '#b20087', low = '#ffcd61') +
      labs(
        y=input$GroupBy, 
        x='Average Percentage'
      ) +
      geom_text(
        aes(label = sprintf("%.1f",avg_percent))
      ) +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)
      )
    
    plot
    
    # ggplotly(plot, tooltip = 'x')
    
  }, height = 600)
  
  output$scatterplot <- renderPlotly({
    # get question id from question input
    qID1 <- get_qID(input$x_axis)
    qID2 <- get_qID(input$y_axis)
    
    # create result set that filters out null values
    df_res <- df_pivot %>% 
      filter(!is.na(get(qID1)) & !is.na(get(qID2)))
    
    # create scatter plot
    plot_ly(
      data = df_res,
      x = ~get(qID1),
      y = ~get(qID2),
      type = "scatter",
      mode = "markers",
      marker = list(size = 7, color = 'black'),
      text = ~paste(
        "x: ", get(qID1), "<br>",
        "y: ", get(qID2), "<br>",
        Location, "<br>",
        Gender_or_Race, "<br>",
        Age_Group, "<br>", sep=""
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = input$x_axis),
        yaxis = list(title = input$y_axis),
        height = 600
      )
    
    # ggplot graph code below
    # plot <- ggplot(df_res, mapping=aes(
    #     x=get(qID1), 
    #     y=get(qID2),
    #     text(paste(
    #       "x: ", x, "<br>",
    #       "y: ", y, "<br>",
    #       Location, "<br>",
    #       Gender_or_Race, "<br>",
    #       Age_Group, "<br>", sep=""
    #     ))
    #   )) +
    #   geom_point() + geom_smooth() +
    #   labs(
    #     x=input$x_axis,
    #     y=input$y_axis
    #   ) +
    #   theme(
    #     axis.title = element_text(size = 16),
    #     axis.text = element_text(size = 13)
    #   )
    # 
    # ggplotly(plot, tooltip = "text")
    
  })
  
  output$cor <- renderText({
    # get question id from question input
    qID1 <- get_qID(input$x_axis)
    qID2 <- get_qID(input$y_axis)
    
    # use = "complete.obs" removes rows with null values before computing the value
    ce <- cor(df_pivot[[qID1]], df_pivot[[qID2]], use = "complete.obs")
    ce <- round(ce, digits = 3)
    
    strength <- ""
    
    # compute correlation strength
    if(0 <= abs(ce) & abs(ce) <= 0.1) {
      strength <- "No or very weak correlation"
    } else if(0.1 < abs(ce) & abs(ce) <= 0.3) {
      strength <- "Weak correlation"
    } else if(0.3 < abs(ce) & abs(ce) <= 0.5) {
      strength <- "Moderate correlation"
    } else if(0.5 < abs(ce) & abs(ce) <= 0.7) {
      strength <- "Strong correlation"
    } else if(0.7 < abs(ce) & abs(ce) <= 1) {
      strength <- "Very Strong correlation"
    }
    
    paste("Correlation Coefficient: ", ce, " \n\n-> ", strength, "\n\n", sep="")
  })
  
  output$map <- renderPlotly({
    # get question ID from choosen question
    qID <- get_qID(input$QuestionMap)
    
    df_res <- NULL
    
    # if user doesn't want to filter by year
    if(input$allYears){
      df_res <- df_pivot %>% 
        filter(!is.na(get(qID))) %>% 
        group_by(Location) %>% 
        summarize(avg_percent = mean(get(qID))) %>% 
        arrange(avg_percent) 
    } 
    # if user wants to filter by year
    else {
      df_res <- df_pivot %>% 
        filter(!is.na(get(qID)) & YearStart == input$Year & YearEnd == input$Year) %>% 
        group_by(Location) %>% 
        summarize(avg_percent = mean(get(qID))) %>% 
        arrange(avg_percent) 
    }
    
    # create themes that will change depending on user input
    theme <- c()
    if(input$Color == "Blue") {
      theme <- c("#e8f0ff", "#a5c2ff","#1862fb", "#001a50")
    }else if (input$Color == "Purple") {
      theme <- c("#f8efff", "#dfb8ff","#9411ff", "#1f0039")
    } else if (input$Color == "Green") {
      theme <- c("#e4f5e9", "#91daa6","#008324", "#00230a")
    } else if (input$Color == "Orange") {
      theme <- c("#ffebd1", "#ffb95a","#d37b00", "#461e00")
    }
    
    # merge the result set with location dataframe so that we can use a map
    df_res <- merge(df_res, location, by="Location")
    
    # create plotly interactive map
    fig <- plot_ly(
      data = df_res,
      locations = ~LocationAbbr,  
      locationmode = "USA-states",
      z = ~avg_percent,  
      type = "choropleth",
      colorscale = list(
        c(0, 0.333, 0.666, 1), theme
      )
    ) %>%
      layout(geo = list(scope = "usa"))  
    
    fig  
  })
} # server


# Create Shiny object
shinyApp(ui,server)
