##loading libraries
library(shiny)
library(tidyverse)
library(bslib)
data(iris)



##learnt to build this app during my online class or tutorial, though made some minor changes.
ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),

    titlePanel("Old Faithful Geyser Data"),

    sidebarLayout(
        sidebarPanel(
          selectInput("Species","Pick Iris Specie",choices = as.character(unique(iris$Species))),
          selectInput("X", "Pick X mapping", choices = colnames(select(iris,-Species))[1:2]),
          selectInput("Y", "Pick Y mapping", choices = colnames(select(iris,-Species))[3:4])
        ),

        mainPanel(
          plotOutput("iris_plot"),
          tableOutput("iris_data")
        )
    )
)

server <- function(input, output) {
  bs_themer()
  data<- reactive({
    iris%>%
      filter(Species==input$Species)%>%
      select(input$X, input$Y)
  })
  
  output$iris_plot<- renderPlot({
    data()%>%
      setNames(c("X", "Y")) %>% 
      
      ggplot(aes(x=X, y= Y))+
      geom_point()
    
  })
  output$iris_data<-renderTable({
    data()
    })

}


shinyApp(ui = ui, server = server)
