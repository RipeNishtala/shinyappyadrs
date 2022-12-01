setwd("C:/Users/pn403/OneDrive - University of Bath/Desktop/Mendley")
library(shiny)
library(tidyverse)
library(rio)
library(modeldata)


faersdata <- import("DRUG_ADVERSE_REACTIONS_COUNT.txt",sep="$")

faers_adrs <- faers_adrs <- faersdata %>% 
  arrange(DRUG) %>%
  filter(count_of_reaction > 500) %>% 
  drop_na()

faers_adrs$count_of_reaction <- as.numeric(faers_adrs$count_of_reaction)
faers_adrs$Adverse_Event <- as.factor(faers_adrs$Adverse_Event)
faers_adrs$DRUG <- as.factor(faers_adrs$DRUG)

ui <- fluidPage(selectInput(inputId="DRUG",
                            label="Drug:",
                            choices =faers_adrs$DRUG),
                sliderInput(inputId = "count",
                            label="Count Range:",
                            min=501,
                            max=7000,
                            step = 1000,
                            value = 575,
                            sep=""),
                submitButton(text="Create my plot!"),
                plotOutput(outputId = "adrplot")
)

server <- function(input, output) {
  
  output$adrplot <- renderPlot(
    
    faers_adrs %>% 
      filter(DRUG %in% input$DRUG) %>%
      ggplot(aes(x=Adverse_Event,y=count_of_reaction,width=.5))+
      geom_bar(stat='identity')+
      #coord_flip()+
      scale_x_discrete(expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
            axis.ticks.length = unit(0, "mm"),
            axis.title = element_blank(),
            axis.line.y.left = element_line(color = "black"),
            axis.text.y = element_text(family = "sans", size = 8),
            axis.text.x = element_text(family = "sans", size =10, angle = 90)))
}


shinyApp(ui = ui, server = server)
