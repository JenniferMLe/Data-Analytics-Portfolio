# Author: Jennifer Le
# Date: 2/3/25
# Modified from 
  # https://github.com/rstudio/shiny-examples/tree/main/030-basic-datatable 

# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)

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
        selectInput('Question2','Question:', c(unique(as.character(df$Question)))),
        selectInput('GroupBy','Group By:', c('Location', 'Age_Group', 'Gender_or_Race'))
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
        selectInput("x_axis", "Question 1 (x-axis):", c(unique(as.character(df$Question))),
                    selected="Experiencing frequent mental distress"),
        selectInput("y_axis", "Question 2 (y-axis):", c(unique(as.character(df$Question))),
                    selected="Smoking every day or some days"),
        verbatimTextOutput("cor"),
        "Interestingly, having a physical or mental disability has a much weaker
        correlation with frequent mental distress than smoking."
      ),
      mainPanel(
        plotOutput('scatterplot')
      )
    )
  )

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "Analysis of Older Adult's Health",
    dashboard1,
    dashboard2,
    dashboard3
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
    question_id <- questions_fct_tbl %>% 
      filter(Question.y == input$Question2) %>% pull(QuestionID)
    
    # create a data frame summarizing results, use get() to transform string id to variable
    df_res <- df_pivot %>% 
      filter(!is.na(get(question_id))) %>% 
      group_by(get(input$GroupBy)) %>% 
      summarize(avg_percent = mean(get(question_id))) %>% 
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
  
  output$scatterplot <- renderPlot({
    
    # get question_id from question1 
    question_id1 <- questions_fct_tbl %>% 
      filter(Question.y == input$x_axis) %>% pull(QuestionID)
    
    # get question_id from question2
    question_id2 <- questions_fct_tbl %>% 
      filter(Question.y == input$y_axis) %>% pull(QuestionID)
    
    df_res <- df_pivot %>% 
      filter(!is.na(get(question_id1)) & !is.na(get(question_id2)))
    
    ggplot(df_res, mapping=aes(x=get(question_id1), y=get(question_id2))) + 
      geom_point() + geom_smooth() +
      labs(
        x=input$x_axis,
        y=input$y_axis
      ) +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)
      ) 
  }, height=600)
  
  output$cor <- renderText({
    # get question_id from question1 
    question_id1 <- questions_fct_tbl %>% 
      filter(Question.y == input$x_axis) %>% pull(QuestionID)
    
    # get question_id from question2
    question_id2 <- questions_fct_tbl %>% 
      filter(Question.y == input$y_axis) %>% pull(QuestionID)
    
    # use = "complete.obs" removes rows with null values before computing the value
    ce <- cor(df_pivot[[question_id1]], df_pivot[[question_id2]], use = "complete.obs")
    ce <- round(ce, digits = 3)
    
    strength <- ""
    
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
    
    paste("Correlation Coefficient: ", ce, " \n\n-> ", strength, 
          "\n\n",
          sep="")
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
