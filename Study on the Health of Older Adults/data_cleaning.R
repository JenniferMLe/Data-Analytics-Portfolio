df<- read.csv("Healthy_Aging_Data.csv")

# remove rows with no data
df <- filter(df, !is.na(Data_Value) | !is.na(Data_Value_Alt))

# create table mapping symbol to footnotes
footnote <- distinct(df, Data_Value_Footnote_Symbol, Data_Value_Footnote)

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

# replace values
df$Stratification1[df$Stratification1 == 'Overall'] <- '50+'
df$Stratification2[df$Stratification2 == ''] <- 'Overall'

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
    Value = Data_Value,
    Age_Group = Stratification1,
    Gender_or_Race = Stratification2,
    Question = Question.y,
    Location = LocationDesc
  ) 

# pivot table so each question gets its own column
df_pivot = select(df, 'YearStart','YearEnd','Location',
                  'Age_Group', 'Gender_or_Race','QuestionID','Value')
df_pivot = pivot_wider(df_pivot, names_from = QuestionID, values_from = Value)

df_res <- df_pivot %>% 
  filter(!is.na(Q01)) %>% 
  group_by(Gender_or_Race) %>% 
  summarize(avg_percent = mean(Q01)) %>% 
  arrange(avg_percent) 

plot <- 
  ggplot(data = df_res, 
    mapping = aes(y=reorder(Gender_or_Race, avg_percent), x=avg_percent)) + 
  geom_col(fill='skyblue') +
  labs(y="Gender or Race") +
  geom_text(aes(label = sprintf("%.1f",avg_percent))) +
  theme(
    axis.text.x = element_text(size = 10),  # X-axis tick label size
    axis.text.y = element_text(size = 10)   # Y-axis tick label size
  )

plot

ggplot(df_pivot, mapping=aes(x=Q01, y=Q02)) + geom_point()

# ggplotly(plot, tooltip = 'x')

result <- distinct(df, Topic)
result <- arrange(result, YearStart)




