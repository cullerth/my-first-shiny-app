library(tidyverse)

data_to_viz <- read_csv("data/data-to-explore.csv")

final_grades <- data_to_viz %>%
  group_by(subject, gender) %>%
  mutate(subject = recode(subject, 
                          "AnPhA" = "Anatomy",
                          "BioA" = "Biology", 
                          "FrScA" = "Forensics", 
                          "OcnA" =  "Oceanography", 
                          "PhysA" = "Physics")) %>%
  summarise(final_grade = mean(proportion_earned * 100, na.rm = TRUE)) 
  # filter(gender %in% c(input$gender)) %>% 
  # filter(subject %in% c(input$subject))

ggplot(data = final_grades) +
  geom_col(mapping = aes(x = subject, y = final_grade, fill = gender), position = "dodge") +
  labs(title = "Average Final Grades by Gender & Subject",
       x = 'Subject',
       y = "Final Grade",
       color = "Gender",
       caption = "Is there a gender difference in final grades across subjects?") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = 0.5))

# ggplot(final_grades, aes(x = subject)) +
#   geom_point(aes(y = final_grade, color = gender)) +
  # labs(title = "Average Final Grades by Gender & Subject",
  #      x = 'Final Grades',
  #      y = "Subject",
  #      color = "Gender",
  #      caption = "Is there a gender difference in final grades across subjects?") +
  # theme(plot.title = element_text(hjust = 0.5)) +
  # theme(plot.caption = element_text(hjust = 0.5))
