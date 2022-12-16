# Shiny app: Exploratory analysis of single target detection data
# Jeremy Holden & Alex Ross
# Dec 5, 2022.
# Revised Dec 12

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(ggiraph)
library(gridExtra)
load("processed_data.Rdata")

trackdat <- processed_data %>% 
  mutate(Quadrat = case_when(
    Angle_major_axis >= 0 & Angle_minor_axis >=0 ~ "NE",
    Angle_major_axis >= 0 & Angle_minor_axis < 0 ~ "NW",
    Angle_major_axis < 0 & Angle_minor_axis >=0 ~ "SE",
    Angle_major_axis < 0 & Angle_minor_axis < 0 ~ "SW"
    ))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Fish Track Ping Exploration"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "fishID",
                  label = strong("Select Fish ID"),
                  choices = unique(trackdat$fishNum),
                  selected = "LT016"),
      tabPanel("Quadrat Summary", tableOutput("quadTable"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Axis Distances",girafeOutput("distPlot")),
        tabPanel("Axis Angles", girafeOutput("anglePlot")),
        tabPanel("TS distribution", plotOutput("tsHisto")),
        tabPanel("Aspect Angle", plotOutput("orientDist"))
      )
    )
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderGirafe({
    # generate bins based on input$bins from ui.R
    plotdat <- filter(trackdat, fishNum == input$fishID)
    gg_distance <- ggplot(plotdat) + 
      geom_point_interactive(
        aes(Distance_minor_axis, Distance_major_axis, color = TS_mean,
            tooltip = round(TS_mean, 1))) +
      geom_text(x = 1, y = -1, label = "SE") +
      geom_text(x = -1, y = 1, label = "NW") +
      geom_text(x = -1, y = -1, label = "SW") +
      geom_text(x = 1, y = 1, label = "NE") +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      scale_color_viridis_c()
    
    girafe(ggobj = gg_distance)
  })
  
  output$anglePlot <- renderGirafe({
    plotdat <- filter(trackdat, fishNum == input$fishID)
    gg_angles <- ggplot(plotdat) + 
      geom_point_interactive(
        aes(Angle_minor_axis, Angle_major_axis, color = TS_mean,
            tooltip = round(TS_mean, 1))) +
      geom_text(x = 4, y = -4, label = "SE") +
      geom_text(x = -4, y = 4, label = "NW") +
      geom_text(x = -4, y = -4, label = "SW") +
      geom_text(x = 4, y = 4, label = "NE") +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      scale_color_viridis_c()
    girafe(ggobj = gg_angles)
  })
  
  output$tsHisto <- renderPlot({
    plotdat <- filter(trackdat, fishNum == input$fishID)
    
    ne <- ggplot(filter(plotdat, Quadrat == "NE"), aes(TS_mean)) + 
      geom_density(fill = "grey", alpha = 0.5) +
      ggtitle("NE") +
      theme_bw()
    nw <- ggplot(filter(plotdat, Quadrat == "NW"), aes(TS_mean)) + 
      geom_density(fill = "grey", alpha = 0.5) +
      ggtitle("NW") +
      theme_bw()
    se <- ggplot(filter(plotdat, Quadrat == "SE"), aes(TS_mean)) + 
      geom_density(fill = "grey", alpha = 0.5) +
      ggtitle("SE") +
      theme_bw()
    sw <- ggplot(filter(plotdat, Quadrat == "SW"), aes(TS_mean)) + 
      geom_density(fill = "grey", alpha = 0.5) +
      ggtitle("SW") +
      theme_bw()
    
    grid.arrange(nw, ne, sw, se)
  })
  
  output$orientDist <- renderPlot({
    plotdat <- filter(trackdat, fishNum == input$fishID)
    
    ne <- ggplot(filter(plotdat, Quadrat == "NE"), aes(aspectAngle)) + 
      geom_density(fill = "grey", alpha = 0.5) +
      ggtitle("NE") +
      theme_bw()
    nw <- ggplot(filter(plotdat, Quadrat == "NW"), aes(aspectAngle)) + 
      geom_density(fill = "grey", alpha = 0.5) +
      ggtitle("NW") +
      theme_bw()
    se <- ggplot(filter(plotdat, Quadrat == "SE"), aes(aspectAngle)) + 
      geom_density(fill = "grey", alpha = 0.5) +
      ggtitle("SE") +
      theme_bw()
    sw <- ggplot(filter(plotdat, Quadrat == "SW"), aes(aspectAngle)) + 
      geom_density(fill = "grey", alpha = 0.5) +
      ggtitle("SW") +
      theme_bw()
    
    grid.arrange(nw, ne, sw, se)
  })
    
  output$quadTable <- renderTable({
    plotdat <- filter(trackdat, fishNum == input$fishID)
    plotdat %>% group_by(Quadrat) %>% 
      summarize(Ntargets = n())
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# end
