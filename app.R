#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
    # CSS style sheets for navbar
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/skeleton.css")
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/normalize.css")
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
    ),
    
    # Custom build navbar; I liked it's look
    tags$nav(class = "navbar", id = NULL, NULL,
             HTML('<div class="container">
                    <div class="row">
                    <div class="four columns"><h5 class="navbar-heading">Monty Hall Problem</h5></div>
                    <div  class="eight columns">
                    </div>
                    </div>
                    </div>'
    )),
    
    # Main UI is in  the tabs where there will be a about, game, and simulation
    tabsetPanel(type = 'tabs',
                tabPanel("About", column(width = 8, includeHTML("www/about.html"))),
                tabPanel("Game", fluidRow(
                    # A column of 3 and column of 9
                    # 3 is for buttons and 9 for rendering images
                    column(width = 3,
                        # conditional panels change the UI for various points in the game
                        conditionalPanel( "input.select % 3 == 0",
                            selectInput("selectDoor", label = "Select Door", 
                            choices = list("Door 1" = "1", "Door 2" = "2", "Door 3" = "3"))
                        ), conditionalPanel( "input.select % 3 == 1", 
                            htmlOutput("current"),
                            selectInput("stay", label = "Select Strategy",
                            choices = list("Stay" = TRUE, "Switch" = FALSE))
                        ), conditionalPanel("input.select % 3 == 2", 
                            htmlOutput("winner")
                        ), actionButton("select", "Continue")
                    ),
                    column(width = 9, htmlOutput("doors"))
                )),
                tabPanel("Door Selection",
                         column(width = 3),
                         column(width = 9,
                        plotOutput("door_selection")
                ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    generate_game <- function() {
        # creates the initual random game setup
        prizes <- sample(c("car", "goat", "goat"), size = 3)
        door_num <- as.character(c(1:3))
        doors <- cbind(door_num, prizes)
        return(doors)
    }
    values <- reactiveValues()
    values$door_choice <- c(0, 0, 0)
    
    # the names of the image files for html rendering
    images <- list("door" = "images/transparent_door.png",
                   "car" = "images/car.jpg",
                   "goat" = "images/goat.jpg")

    game_update <- function() {
        # the main reactive function
        # updates the game matrix based on user selection and uses .rda's to
        # maintain state; majority of game logic is here
        if (input$select %% 3 == 0) {
            isolate({
                values$game <- generate_game()
                values$game <- cbind(values$game, rep(images[["door"]], 3))
            })
        } else if (input$select %% 3 == 1) {
            show_door <- as.character(min(which(c(values$game[,1] != input$selectDoor & values$game[,2] != "car"))))
            new_im <- images[[values$game[as.numeric(show_door),2]]]
            isolate({
                d <- as.numeric(input$selectDoor)
                values$door_choice[d] <- values$door_choice[[d]] + 1/2
                values$show_door <- show_door
                values$game[as.numeric(show_door),3] <- new_im
            })
        } else if (input$select %% 3 == 2) {
            if (!(as.logical(input$stay))) {
                picked_door <- values$game[,1] != values$show_door & values$game[,1] != input$selectDoor
            } else {
                picked_door <- values$game[,1] == input$selectDoor
            }
            isolate({
                values$picked_door <- picked_door
                values$game[,3] <- sapply(values$game[,2], function(x){images[[x]]})
            })
        }
        values$game
    }

    output$doors <- renderText({
        # renders the html for the doors and prizes; simpler than independent renders
        values$game <- game_update()
        HTML(paste0('<div><img src="', values$game[1,3],'" height="200" width="200">',
               '<img src="', values$game[2,3],'" height="200" width="200">',
               '<img src="', values$game[3,3],'" height="200" width="200">'))
    })
    
    output$current <- renderText({
        HTML(paste('<h6><b>Your current door --', input$selectDoor, '</b></h6>'))
    })
    
    output$winner <- renderText({
        door <- which(values$picked_door)[[1]]
        prize <- values$game[values$picked_door, 2]
        HTML(paste('<h5>You\'ve picked', door, 'and won a', prize, '!</h5>'))
    })
    
    output$door_selection <- renderPlot({
        barplot(values$door_choice, main = "Door Selected", ylim = c(0, max(values$door_choice)),
                names.arg = c("Door 1", "Door 2", "Door 3"), col = "sky blue")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
