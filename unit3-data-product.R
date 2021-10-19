library(shiny)
library(tidyverse)

data_to_viz <- read_csv("data/data-to-explore.csv")

ui <- fluidPage(
  titlePanel("Gender & Final Grades"),
  sidebarLayout(position = "left",
                sidebarPanel(
                  #Checkbox for gender
                  checkboxGroupInput("Gender",
                                     "Which gender would you like to see data for?",
                                     choices = list("female",
                                                    "male"),
                                     selected = NULL)

                ),
                # Displays the scatterplot and help text
                mainPanel(
                  
                  plotOutput("scatterplot"),
                  
                  p("In the scatter plot above, each point indicates the average final grade by subject and gender for a set of online STEM courses. It appears that for 3 out of the 5 subjects represented (AnPhA, BioA, and FrScA), female students scored higher on average than male students. Male students had a slight edge on female students in two courses (OcnA and PhysA). It is worth noting that gender data was not available for a significant portion of students -- 227 of 943 (approximately 24%). Additionally, this only represents a binary conception of gender and doesn't account for other gender identities that students may hold. In any case, given that women, trans, and nonbinary people are still underrepresented in STEM fields, this information could be useful in designing online STEM courses to achieve greater gender parity to ensure long term student success and retention. ")
                  
                )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    
    #Creates a data frame for scatter plot using reactive inputs from ui
    female <- data_to_viz %>%
      group_by(subject) %>%
      filter(gender == "F") %>%
      summarise(final_grade = mean(proportion_earned * 100, na.rm = TRUE))
    
    male <- data_to_viz %>%
      group_by(subject) %>%
      filter(gender == "M") %>%
      summarise(final_grade = mean(proportion_earned * 100, na.rm = TRUE)) 
    
    combined_final_grades <- merge(female, male, by = 'subject') %>%
      rename(female = 'final_grade.x', male = 'final_grade.y')
    
    #Creates scatterplot for main panel in the ui    
    ggplot(combined_final_grades, aes(x = subject)) +
      geom_point(aes(y = input$female, color = 'Female')) +
      geom_point(aes(y = input$male, color = 'Male')) +
      labs(title = "Average Final Grades by Gender & Subject",
           x = 'Final Grades',
           y = "Subject",
           color = "Gender",
           caption = "Is there a gender difference in final grades across subjects?") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.caption = element_text(hjust = 0.5)) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

