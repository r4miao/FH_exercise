library(tidyverse)
library(gt)

# Load the txt file into R
df <- read.table("exampledata_FH.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# Split termyear into two columns: season and year
df <- df %>%
  mutate(semester_year = termyear) %>%   
  separate(semester_year, into = c("semester", "year"), sep = " ")

df$semester <- factor(df$semester, 
                      levels = c("Fall", "Spring"))
df <- df %>%
  arrange(schoolid, sid, year, semester)

df <- df %>%
  group_by(schoolid, sid) %>%
  mutate(
    beginning_termyear = first(termyear),  # First termyear for each student
    terminal_termyear = last(termyear)     # Last termyear for each student
  ) %>%
  ungroup()  %>% 
  arrange(schoolid, sid, year, semester)


# Generate the retention variable
df <- df %>%
  mutate(retention = ifelse(terminal_termyear == "Spring 2022", 1, 0))

retention_summary <- df %>%
  distinct(schoolid, sid, .keep_all = TRUE) %>%   
  summarize(
    total_individuals = n(),
    retention_count = sum(retention == 1),
    retention_percentage = (retention_count / total_individuals) * 100
  )
View(df)
View(retention_summary)

# Recode parent1 and parent2 into factor variables with specified levels and labels
df$parent1 <- factor(df$parent1, 
                       levels = c(1, 2, 3, 4), 
                       labels = c("High school graduation or below", 
                                  "Some college level courses (no degree)", 
                                  "College completion/graduate", 
                                  "Post-graduate degree"))

df$parent2 <- factor(df$parent2, 
                       levels = c(1, 2, 3, 4), 
                       labels = c("High school graduation or below", 
                                  "Some college level courses (no degree)", 
                                  "College completion/graduate", 
                                  "Post-graduate degree"))

# Generate a new variable 'first_gen' based on the conditions for parent1 and parent2
df <- df %>%
  mutate(first_gen2 = ifelse(parent1 == "High school graduation or below" & 
                              parent2 == "High school graduation or below", 
                            "Yes", "No"),
         first_gen1 = ifelse(parent1 %in% c("High school graduation or below","Some college level courses (no degree)")  & 
                             parent2 %in% c("High school graduation or below","Some college level courses (no degree)"), 
                            "Yes", "No"))
race_table <- df %>%
  distinct(sid, .keep_all = TRUE) %>%
  with(table(raceethnic, schoolid)) %>% 
  prop.table(margin = 2) * 100

library(kableExtra)
kable(race_table, digits = 1, caption = "Cross-Tabulation of Race/Ethnicity by School (Percentages Only)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# Calculate the percentages for the dummy variable 'deved'
summary_table <- df %>%
  count(deved) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  mutate(deved = ifelse(deved == 1, "Yes", "No"))   

# Create a publication-ready table with gt
summary_table %>%
  gt() %>%
  tab_header(
    title = "Percentage of Developmental Education Participation"
  ) %>%
  cols_label(
    deved = "Developmental Education",
    n = "Count",
    percentage = "Percentage"
  ) %>%
  fmt_number(
    columns = vars(percentage),
    decimals = 2
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  )

 

