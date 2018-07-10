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
load.libraries(c("shiny","leaflet","raster"))

# Define a UI for the shiny app
ui <- fluidPage(
  # Add a title
  titlePanel("Welcome to CARBCAT"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      helpText("Select your input for CARBCAT below:"),
      radioButtons(inputId = "ag.or.forest", # Used for all modules
                   label = "Residue Sector",
                   choices = list("Forestry","Agriculture")),
      selectInput(inputId = "treatment.type", # Used for harvest/processingfor forestry and woody agriculture
                  label = "Treatment Type",
                  choices = list("Thin from below: 20% basal area",
                                 "Thin from below: 40% basal area",
                                 "Thin from above: 20% basal area",
                                 "Thin from above: 40% basal area",
                                 "You shouldn't actually see this option")),
      selectInput(inputId = "post.harvest.processing", # Used in harvest/processing - processing of materials TO BE HARVESTED
                  label = "Post Harvest Processing",
                  choices = list("Briquette",
                                 "Torrefaction",
                                 "Pelletization",
                                 "None")),
      selectInput(inputId = "in.field.processing", # Used in in-field ag, maybe forestry?
                  label = "Post Harvest Processing",
                  choices = list("Grinding/Dicing",
                                 "None")),
      selectInput(inputId = "burn.or.decay", # Used in in-field ag, in-field forestry.
                  label = "In-Field Residue Fate",
                  choices = list("Decay Only",
                                 "Pile Burn",
                                 "In-Field Burn")),
      selectInput(inputId = "possible.plants",
                  label = "Biomass Plant",
                  choices = list("DG Fairhaven Power, LLC",
                                 "Eel River Power",
                                 "Sierra Pacific Industries Anderson")),
      sliderInput(inputId = "piled.biomass",
                  label = "Percent of Biomass Piled",
                  min = 0,
                  max = 100,
                  value = 50),
      sliderInput(inputId = "burned.biomass",
                  label = "Percent of Biomass Burned",
                  min = 0,
                  max = 100,
                  value = 10),
      sliderInput(inputId = "burn.year",
                  label = "Year of Biomass Burning (If applicable)",
                  min = 0,
                  max = 100,
                  value = 10),
      checkboxGroupInput(inputId = "ghg.species",
                         label = "Emissions to Track",
                         choices = list("CO2","CH4","N2O"))
    ),
    mainPanel(
      leafletOutput("mymap"),
      plotOutput("sampleEmissions"),
      p()
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
  
  squares <- data.frame(Lat1=40,
                        Lat2=40.015,
                        Long1=-124,
                        Long2=-123.985,
                        Fill= "blue",
                        Message=msg1)
  plot.data <- data.frame(x=1:100)
  plot.data$y <- 3*exp(1/(plot.data$x))
  r <- raster("/Users/harrisa/Desktop/CBI project Research/forShiny2.tif")
#  crs(r) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  output$mymap <- renderLeaflet({
    leaflet(options = clickOpts(id='test')) %>%
      addTiles() %>%
      addRasterImage(r) %>%
      addRectangles(data = squares,lng1 = ~ Long1, lat1 = ~ Lat1, lng2 = ~ Long2, lat2 = ~ Lat2, fill = ~ Fill, popup = ~ Message)
  })
  output$sampleEmissions <- renderPlot({
    ggplot(plot.data) +
      geom_point(aes(x=x,y=y)) +
      ylab("Annual Emissions (Tons CO2)") +
      xlab("Year")
  })
}

shinyApp(ui, server)