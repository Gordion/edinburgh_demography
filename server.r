# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
# Set working directory (to run code within RStudio)
#setwd("ShinyApps/pyramid")

# Load data
#load("data.Rdata")
CeliaEdinburghData <- read.csv("CeliaEdinburghData.csv")
CeliaEdinburghData<-arrange(CeliaEdinburghData,Year)

# Define useful functions
#roundAny = function(x, accuracy, f = round) {f(x/accuracy) * accuracy}

number_of_censuses <- length(unique(edinburgh_population$time))

male <- CeliaEdinburghData %>%
  filter(SEX=='Male') %>%
  mutate(POP = POP * -1,
         NEW_AGE = rep(1:15,
                       number_of_censuses))

female <- CeliaEdinburghData %>%
  filter(SEX == 'Female') %>%
  mutate(NEW_AGE = rep(1:15,
                       number_of_censuses))

edinburgh_population <- rbind(male, female) %>%
  mutate(abs_pop = abs(POP)) %>%
  rename(time=Year)

### Main shinyServer function
shinyServer(function(input, output) {
  
  ### Plot output: pyramid
  output$pyramid <- renderPlot({
    # Define data column from slider input
    
    
    
    year_data <- filter(edinburgh_population, time == input$anno)
    
    ggplot(year_data, aes(x = NEW_AGE, y = POP, fill = SEX, width = 1)) +
      coord_fixed() + 
      coord_flip() + 
      geom_bar(data = subset(year_data, SEX == "Female"), stat = "identity") +
      geom_bar(data = subset(year_data, SEX == "Male"), stat = "identity") +
      scale_y_continuous(breaks = seq(-30000, 30000, 10000),
                         labels = paste0(as.character(c(seq(30000, 0, -10000), c(10000, 20000,30000))), ""), 
                         limits = c(min(edinburgh_population$POP),
                                    max(edinburgh_population$POP))) +
      theme_economist(base_size = 14) + 
      scale_fill_manual(values = c('#ff9896', '#d62728')) + 
      ggtitle(paste0(input$anno, '\nTotal Population: ', 
                     prettyNum(sum(year_data$abs_pop),
                               big.mark=',')
        )) + 
      ylab('Population') + 
      xlab('Age') + 
      scale_x_continuous(breaks = year_data$NEW_AGE,
                         labels = year_data$AGE) +
      theme(legend.position = "bottom", legend.title = element_blank()) + 
      guides(fill = guide_legend(reverse = TRUE))
    
  })
})
