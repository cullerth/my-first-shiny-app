library(shiny)
library(tidyverse)

data_to_viz <- read_csv("data/data-to-explore.csv")

ui <- fluidPage(
  titlePanel("Gender & Final Grades by Course"),
  sidebarLayout(position = "left",
                sidebarPanel(
                  #Checkbox for gender
                  checkboxGroupInput("subject", 
                                     "Which course(s) would you like to see data for?", 
                                     choices = list("Anatomy",
                                                    "Biology", 
                                                    "Forensics", 
                                                    "Oceanography", 
                                                    "Physics"),
                                     selected = c("Anatomy", "Biology", "Forensics", "Oceanography", "Physics")), 
                  checkboxGroupInput("gender",
                                     "Which gender(s) would you like to see data for?",
                                     choices = list("F",
                                                    "M"),
                                     selected = c("F", "M"))
                  
                ),
                # Displays the scatterplot and help text
                mainPanel(
                  
                  plotOutput("scatterplot"),
                  
                  p("The data avaialble here generates bar graphs representing the final grade by subject and gender for a set of online STEM courses. It appears that for 3 out of the 5 subjects represented (Anatomy Biology, and Forensics, female students scored higher on average than male students. Male students had a slight edge on female students in two courses (Oceonography and Physics). It is worth noting that gender data was not available for a significant portion of students -- 227 of 943 (approximately 24%). Additionally, this only represents a binary conception of gender and doesn't account for other gender identities that students may hold. In any case, given that women, trans, and nonbinary people are still underrepresented in STEM fields, this information could be useful in designing online STEM courses to achieve greater gender parity to ensure long term student success and retention. ")
                  
                )
  )
)



server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    
    #Creates a data frame for scatter plot using reactive inputs from ui
    final_grades <- data_to_viz %>%
      group_by(subject, gender) %>%
      mutate(subject = recode(subject, 
                              "AnPhA" = "Anatomy",
                              "BioA" = "Biology", 
                              "FrScA" = "Forensics", 
                              "OcnA" =  "Oceanography", 
                              "PhysA" = "Physics")) %>%
      summarise(final_grade = mean(proportion_earned * 100, na.rm = TRUE)) %>%
      filter(gender %in% c(input$gender)) %>% 
      filter(subject %in% c(input$subject))
    
    #Creates scatterplot for main panel in the ui    
    ggplot(data = final_grades) +
      geom_col(mapping = aes(x = subject, y = final_grade, fill = gender), position = "dodge") +
      labs(title = "Average Final Grades by Gender & Subject",
           x = 'Subject',
           y = "Final Grade",
           color = "Gender",
           caption = "Is there a gender difference in final grades across subjects?") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.caption = element_text(hjust = 0.5))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
