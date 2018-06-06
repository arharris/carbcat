# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2018-06-05: Initialized Test Script
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#
# DESCRIPTION:
# I'm playing around with and testing Shiny to see what we can do.
#
# =============================================================================

source("../misc-functions.R")
load.libraries(c("shiny","leaflet"))

# Define a UI for the shiny app
ui <- fluidPage(
  # Add a title
  titlePanel("Welcome to CARBCAT"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      helpText("All existence is suffering, only sheep attempt to anesthetize themselves"),
      radioButtons(inputId = "ag.or.forest",
                   label = "Residue Sector",
                   choices = list("Forestry","Agriculture")),
      selectInput(inputId = "treatment.type",
                  label = "Treatment Type",
                  choices = list("Thin from below: 20% basal area",
                                 "Thin from below: 40% basal area",
                                 "Thin from above: 20% basal area",
                                 "Thin from above: 40% basal area",
                                 "You shouldn't actually see this option")),
      checkboxGroupInput(inputId = "ghg.species",
                         label = "Emissions to Track",
                         choices = list("CO2","CH4","N2O"))
    ),
    mainPanel(
      leafletOutput("mymap"),
      p(),
      actionButton("recalc", "New points")
    )
  )
)

server <- function(input, output, session) {
  msg1 <- paste(sep = "<br/>",
                "<b>Forestry Residue Volume:</b> 250,000 tons",
                "<b>Chief Agricultural Product(s):</b> Soylent Green (Silage)",
                "<b>Average Temperature:</b> 6000 K",
                "<b>Average Annual Rainfall:</b> 300 in",
                "<b>Existing Pulp Market:</b> Sort of, but that's Hank's deal and he's kind of weird."
  )
  
  circles <- data.frame(Lat=40,
                        Long=-124,
                        Radius = 1000,
                        Message=msg1)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircles(data = circles,lat = ~ Lat,lng = ~ Long, radius = ~ Radius, popup = ~ Message)
  })
}

shinyApp(ui, server)