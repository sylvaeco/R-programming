#the libraries used 
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggtext)
library(ggcorrplot)
library(maps)
library(shinycssloaders)
library(markdown)
library(tidyverse)

my_data<- USArrests 

states<- rownames(my_data)
states
my_data<-my_data %>% 
  mutate(State=states)

#Choices for Selectinput- NB: with States column
c1 <- my_data %>% 
  select(-State) %>% names()

#Choices for Selectinput- NB: without States  and Urban Population column
c2 <- my_data %>% 
  select(-"State",-"UrbanPop") %>% 
  names()


 
ui <-dashboardPage(
  #defines the top of the dashboard
  dashboardHeader(title = "Exploring the 1973 US Arrest data with R & shiny Dashboard", 
                  titleWidth = 500,
                  tags$li(class="dropdown", tags$a(href="hub.com/sylvaeco/sylvaeco",icon("github"), "My Github Channel", target= "_blank")),
                  tags$li(class="dropdown", tags$a(href="web.facebook.com/sylvanus.iyoha.9",icon("facebook"), "My Facebook Channel", target= "_blank")),
                  tags$li(class="dropdown", tags$a(href="linkedin.com/in/sylvanus-iyoha-a9b468186",icon("Linkedin"), "My Linkedin Channel", target= "_blank"
                                                   ))),
  dashboardSidebar(
    #defines the side menu
    sidebarMenu(
      id = "sidebar",
      
      #defines first menu
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem("Visualization", tabName = "viz", icon = icon("chart-line")),
      conditionalPanel("input.sidebar=='viz' && input.t2=='distro'", selectInput(inputId = "var1", label = "select the variable", choices = c1, selected = "Rape")),
      conditionalPanel("input.sidebar=='viz' && input.t2=='trends'", selectInput(inputId = "var2", label = "select the arrest type", choices = c2)),
      conditionalPanel("input.sidebar=='viz' && input.t2=='relation'",selectInput(inputId = "var3", label = "select the X variable", choices = c1, selected = "Rape")),
      conditionalPanel("input.sidebar=='viz' && input.t2=='relation'",selectInput(inputId = "var4", label = "select the Y variable", choices = c1, selected = "Assault")),
      menuItem("Choropleth Map", tabName = "map", icon = icon("map"))
    )
),
  dashboardBody(
    fluidPage(
    tabItems(
      #defines first items
      tabItem(tabName = "data",
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"), fluidRow(
                     column(width = 8, tags$img(src="pexels-rosemary-ketchum-1464215.jpg", width= 600, height=300),
                           tags$br(),
                           tags$a("Photo by Photopixel"), align= "center"), 
                     column(width = 4, tags$br(), 
                             tags$p("This dataset comes along with base R and conatins statistics, in arrest per 100,000 residents for Assault,
                                    Murder and Rape in each of the 50 US states in 1973. 
                                    Also given is the percent of the population living in urban areas")))),
                     tabPanel("Data", icon=icon("address-card"), dataTableOutput("dataT")),
                     tabPanel("Structure", icon=icon("address-card"), verbatimTextOutput("structure")),
                     tabPanel("Summary Statistics", icon=icon("address-card"), verbatimTextOutput("summary")),
                     tabPanel("Documentation", icon=icon("address-card"), includeMarkdown("compoundmarkdown1.Rmd")))),

      #defines second tabitem
      tabItem(tabName = "viz",
      tabBox(id="t2", width = 12, 
      tabPanel(title = "Crime Trends by State",
               fluidRow(tags$div(align="center", box(tableOutput("high5"), title = textOutput("head1") , collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE)),
                        tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE))
               ),
               value = "trends", withSpinner(plotlyOutput("bar"))),
      tabPanel(title ="Distribution", value = "distro",withSpinner(plotlyOutput("histplot"))),
      tabPanel(title ="Correlation Matrix", id = "corr", plotlyOutput("cor")),
      tabPanel(title ="Relationship Among Types & Urban Population",value = "relation",
               radioButtons(inputId = "fit", label ="Select Smooth Method", choices = c("loess", "lm"), selected = "lm", inline = T),
               withSpinner(plotlyOutput("scatter")))
              )
       ), 
      
      #defines third tabitem
      tabItem(
        tabName = "map",
        box(      selectInput("crimetype","Select Arrest Type", choices = c2, selected="Rape", width = 250),
                  withSpinner(plotOutput("map_plot")), width = 12))
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  my_data<- USArrests
  states<- rownames(my_data)
  my_data<-my_data %>% 
    mutate(State=states)
  #Structure
  output$structure<- renderPrint(
    my_data %>% 
      str()
  )
  
  #Summary
  output$summary<- renderPrint(
    my_data %>% summary()
    
  )
  
  #DataTable
  output$dataT<- renderDataTable(
    my_data  )
  
  #stacked histogram and boxplot
  output$histplot<-renderPlotly({
    #Histogram
    p1<-my_data %>% 
      plot_ly() %>% 
      add_histogram(~get(input$var1)) %>% 
      layout(xaxis=list(title=(input$var1)))
    
    #Boxplot
    p2<-my_data %>% 
      plot_ly() %>% 
      add_boxplot(~get(input$var1)) %>% 
      layout(yaxis=list(showticklabels=F))
    
    #stacking the above plots
    subplot(p2, p1,nrows =  2) %>%
      hide_legend() %>% 
      layout(title="Distribution Chart - Histogram and Boxplot",
             yaxis=list(title="Frequency"))
  })
    
    #scatter plot
    output$scatter<- renderPlotly({
      
      
      #creating scatterplot for relationships using ggplot
      p<-my_data %>% 
        ggplot(aes(x= get(input$var3) , y= get(input$var4)))+
        geom_point()+
        geom_smooth(method =get(input$fit))+
        labs(title = paste("Relationship between", input$var3, "and", input$var4," Arrests"),
             x= input$var3,
             y=input$var4)+
        theme(plot.title = element_textbox_simple
              (size=10, halign=0.5))
      
        ggplotly(p)
  }  )
    output$cor<- renderPlotly({
        my_df <- my_data %>%
          select(-State)
        # Compute a correlation matrix
        corr <- round(cor(my_df), 1)
        
    #compute a matrix of correlation p-values
     p.mat<- cor_pmat(my_df)
     
     corr.plot<- ggcorrplot(
       corr,
       hc.order = T, 
       lab = T,
       outline.color = "white",
       p.mat = p.mat
     )
    ggplotly(corr.plot)
    })
    
    ###Barchart- State wise trend
    output$bar<- renderPlotly({
      my_data %>% 
        plot_ly() %>% 
        add_bars(x=~State, y=~get(input$var2)) %>% 
        layout(title= paste("Statewise Arrests for ",input$var2),
               xaxis= list(title="State"),
               yaxis= list(title=paste(input$var2," Arrests per 100,000 Residents") ))
    })
    
    
    ###rendering the table with 5 states with high arrests for specific crimes types
    output$high5<- renderTable(
      ###top 5 states with high rape rates
      my_data %>% 
        select(State, input$var2) %>% 
        arrange(desc( get(input$var2)))%>% 
        head(5)
    ) 

    ###rendering the table with 5 states with low arrests for specific crimes types
    output$low5<- renderTable(
    ###top 5 states with low rape rates
    my_data %>% 
      select(State, input$var2) %>% 
      arrange(get(input$var2))%>% 
      head(5)
    )
    ###rendering the box header
    output$head1<- renderText(
      paste("5 States with high rate of ", input$var2, "Arrests")
    )
    
    ###rendering the box header
    output$head2<- renderText(
      paste("5 States with low rate of ", input$var2, "Arrests")
    )
    
    
    ###Map creation
    state_map <- map_data("state")
    my_data1 = my_data %>% 
      mutate(State = tolower(State)) 
    
    merged =right_join(my_data1, state_map,  by=c("State" = "region"))
    
    ###adding state abbreviations and center locations for each states. create a dataframe
    st = data.frame(abb= state.abb, stname= tolower(state.name), x=state.center$x, y= state.center$y)
    new_join = left_join(merged, st, by=c("State" = "stname"))
    
    ###Choropleth Map 
    output$map_plot <- renderPlot({
      new_join %>% 
        ggplot(aes(x=long, y=lat,fill=get(input$crimetype) , group = group)) +
        geom_polygon(color="black", size=0.4) +
        scale_fill_gradient(low="#73A5C6", high="#001B3A", name = paste(input$crimetype, "Arrest rate")) +
        theme_void() +
        labs(title =paste(" Choropleth map of ", input$crimetype , " Arrests per 100,000 residents by state in 1973")) +
        theme(
          plot.title = element_textbox_simple(face="bold", 
                                              size=18,
                                              halign=0.5),
          
          legend.position = c(0.2, 0.1),
          legend.direction = "horizontal"
          
        ) +
        geom_text(aes(x=x, y=y, label=abb), size = 4, color="white")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
