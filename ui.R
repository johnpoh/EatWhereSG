require(shiny)
require(rCharts)
require(googleVis)


shinyUI(navbarPage("eAT wHERE? SG",
                   tabPanel("Main",sidebarLayout(
                     #headerPanel(h4("Dining Map Singapore - restaurant status in a glance..."), windowTitle = "RMap"),
                     mainPanel(tabsetPanel(
                     #tabPanel("Hello",htmlOutput("hello")),   
                     tabPanel("Map", mapOutput("map_container")),
                     tabPanel("Data", 
                              #checkboxInput(inputId = "pageable", label = "Paginate?"),
                              #conditionalPanel("input.pageable==true",
                                               #numericInput(inputId = "pagesize",
                                                            #label = "Restaurants per page",5)),
                              htmlOutput("myTable")),
                     tabPanel("Feeds",htmlOutput("feeds")),
                     tabPanel("Motion Chart",htmlOutput("motionchart"))),width = 8),
                     #tabPanel("Relationships", selectInput('variablex','Variable X:', c("Likes" = 'likes',"Talking" = 'talking', "Here" = 'here')),
                              #selectInput('variabley','Variable Y:', c("Likes" = 'likes',"Talking" = 'talking', "Here" = 'here')),
                              #htmlOutput("myChart"))), width = 8),
                     
                     sidebarPanel(
                       h4("Time - SG/KL"),
                       h5(strong(textOutput("datetimenow"))),
                       h4("Restaurant Summary"),
                       checkboxInput(inputId = "centeronrestaurant", label = "Center Map on Restaurant?", value = FALSE),
                       selectInput('restaurantsummary','Search Restaurant',sort(restaurantdf()$name)),
                       tabsetPanel(
                         tabPanel("Stats",htmlOutput("mySummary")),
                         tabPanel("Hours",htmlOutput("myHours")),
                         tabPanel("More",htmlOutput("myInfo"))
                       ),
                       h2(""),
                       #tableOutput('hello'),
                       width = 4)
                     
                     )),
                   tabPanel("About",
                            h3("Dining Map SG is..."),
                            HTML("<p>a one stop portal for finding out the open/close status of restaurants in Singapore.
                                 This site also provides a some quick facebook statistics as can be seen in
                                 the sidebar of the main page. All data are pulled at regular intervals from Facebook
                                 via the graph api in json format. On the map, restaurants which are currently open are highlighted in
                                 green while restaurants which are closed are in red. Unknown statuses are depicted in blue. 
                                 This site is still work in progress and we are currently populating our database with more
                                 restaurants.</p>"),
                            HTML("<p>This site is developed in R Studio with Shiny, Leaflet, RCharts, Googlevis Packages. The source codes 
                                 may be found on GitHub at the following <a href= 'https://github.com/PCOmmodore/RestaurantsMap'>link.</a>")
                            )
)
)

  
    
    
  
  
