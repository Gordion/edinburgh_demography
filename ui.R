require(shiny)

shinyUI(pageWithSidebar(
  
  ###  Application title
  headerPanel("Demographics of Edinburgh 1851 - 2011"),
  
  ### Sidebar with sliders and HTML
  sidebarPanel(
    # Slider: choose year
    sliderInput("anno",
                "Year:",
                min=1851,
                max=2011,
                step= 10,
                sep="",
                value=1851,
                animate = TRUE)
  ),
  
  ### Main Panel
  mainPanel(
    # Show the plot
    plotOutput("pyramid", height="600px")
  )
))
