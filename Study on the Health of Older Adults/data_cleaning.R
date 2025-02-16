# Load R packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(maps)

df<- read.csv(unz("Healthy_Aging_Data.csv.zip","Healthy_Aging_Data.csv"))

# remove rows with no data
df <- filter(df, !is.na(Data_Value) | !is.na(Data_Value_Alt))

# create table mapping symbol to footnotes
footnote <- distinct(df, Data_Value_Footnote_Symbol, Data_Value_Footnote)

# create mapping from location to longitude and latitude
location <- distinct(df, LocationAbbr, LocationDesc, Geolocation)
location <- rename(location, Location = LocationDesc)

# create table mapping question ID to question
questions_fct_tbl <- df %>% 
  distinct(QuestionID, Question) %>% 
  arrange(QuestionID)

# download questions table so we can edit the questions in excel
# we can also do this in R (see example replacing NA values below) but it requires more work
write.csv(questions_fct_tbl, "questions.csv", row.names = FALSE)

# read into global environment
questions_shortened <- read_csv("questions_shortened.csv")

# join df with questions_shortened on question ID
df <- merge(x=df, y=questions_shortened, by="QuestionID")
questions_fct_tbl <- merge(x=questions_fct_tbl, y= questions_shortened, by='QuestionID')
rm(questions_shortened)

# remove unnecessary columns and rename columns
df <- df %>% 
  mutate(
    Data_Value_Unit = NULL, 
    DataValueTypeID = NULL, 
    Datasource = NULL,
    Data_Value_Footnote = NULL,
    StratificationCategory1 = NULL,
    StratificationCategory2 = NULL,
    StratificationCategoryID1 = NULL,
    StratificationCategoryID2 = NULL,
    StratificationID1 = NULL,
    StratificationID2 = NULL,
    Question.x = NULL,
    Data_Value_Alt = NULL
  ) %>% 
  rename(
    Percentage = Data_Value,
    Age_Group = Stratification1,
    Gender_or_Race = Stratification2,
    Question = Question.y,
    Location = LocationDesc
  ) 

# replace values
df$Age_Group[df$Age_Group == 'Overall'] <- '50+'
df$Gender_or_Race[df$Gender_or_Race == ''] <- 'Overall'
df$Gender_or_Race[df$Gender_or_Race == 'Black, non-Hispanic'] <-'Black'
df$Gender_or_Race[df$Gender_or_Race == 'White, non-Hispanic'] <-'White'

# pivot table so each question gets its own column
df_pivot = select(df, 'YearStart','YearEnd','Location',
                  'Age_Group', 'Gender_or_Race','QuestionID','Percentage')

df_pivot = pivot_wider(df_pivot, names_from = QuestionID, values_from = Percentage)

# create plots to test here
# result <- distinct(df, Gender_or_Race)
# result <- arrange(result, YearStart)

