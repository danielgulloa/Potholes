setTimeSliceMax <- function(n){
     return(168/as.numeric(n))
}


shinyUI(fluidPage(
  
  titlePanel("Real-World Data of Boston Potholes"),     
   
  fluidRow(column(width=2,
      selectInput(inputId = "potdata",
              label = "Select Dataset",
              choices = c("All Potholes", "Potholes in intersection with Waze"),
              selected = "All Potholes")
          ),
  
  column(width=4,
      selectInput(inputId = "n_breaks",
      label = "Number of bins in histogram (approximate):",
      choices = c(10, 20, 35, 50,75,100,150,200,250,300,350,400,450,500,550,600,650,700),
      selected = 450)
       ),
   column(width=2,
          radioButtons(inputId="Granularity", label="Time", choices=c("days","hours","minutes"), selected = "hours")
   ),
     column(width=12,
          sliderInput(inputId="minmax", label="Range", min=-1, max=5000, value=c(1,500), width="1500px", step = 1, round = FALSE, ticks = TRUE, animate = FALSE)
          )
  ),
     
  
  
  
mainPanel(
      textOutput("text0")  ,    
      textOutput("text1")
    ),


  
  
  plotOutput(outputId = "main_plot", height = "300px"),
hr(),
hr(),


fluidRow(column(width=4,
               selectInput("gridsize", label = h3("Select Grid Size"), 
                    choices = list("1x1 (no grid)" = 1, "10x10" = 2, "15x15" = 3,"20x20"=4,"25x25"=5,"30x30"=6,"35x35"=7,"40x40"=8,"80x80"=9,"120x120"=10,"160x160"=11,"200x200"=12,"240x240"=13,"400x400"=14), 
                    selected = 2),
               textOutput("gridWarning")
               ),
         column(width=4,
               selectInput("timeSliceSize", label = h3("Select Time Slice Size"), 
               choices = list("1 hour (168 time slices)" = 1, "4 hours (42 time slices)" = 4, "8 hours (21 time slices)" = 8,"12 hours (14 time slices)"=12,"16 hours (10 time slices)"=16,"1 day (7 time slices)"=24,"ALL (1 time slice)"=(24*7)), 
               selected = 4)
               ),
          
          column(width=4,
                 
                          sliderInput("CurrentTime", label = h3("Current Time Slice"), min = 1, 
                         max = 168, value = 2,animate=animationOptions(interval = 2000, loop = TRUE, playButton = h3("Play time slices"), pauseButton = h3("Pause"))),
               
                
                
                
               checkboxInput("showmap", label = "Show Approximate Map", value = FALSE),                 
               checkboxInput("fixReports",label="Fix by removing reports",value=FALSE),
               checkboxInput("fixPotholes",label="Fix by adding potholes",value=FALSE),
               sliderInput("crazyThreshold",label="Threshold of reports to add pothole",min=1,max=5,value=2)
          )
),

fluidRow(
     column(width=12,offset=3,
            plotOutput(outputId ="scatterPlot",height="700px", width = "700px"),
            h2(textOutput("gridSummary")),
            textOutput("openPotholeText"),
           textOutput("newPotholeText"),
            textOutput("closedPotholeText"),
               textOutput("N_reports")
     )),
fluidRow(

     column(width=6,offset=3,
           plotOutput(outputId="miniHist", height="300px",width="600px"),
          sliderInput(inputId="setMax", label="Maximum hours display in histogram", min=-1, max=500, value=30, step = NULL, round = FALSE, ticks = TRUE, animate = FALSE),
             textOutput("text2"),
              textOutput("text3")
     )
),

fluidRow(
     column(width=10,offset=-1,
          plotOutput(outputId="barPlot",height="500px",width="1200px"),
          textOutput("explainBar")
     )),
     
fluidRow(column(width=4,
  sliderInput("LocChooser", label = "Locations with x values", min = 0, 
        max = 100, value = c(1, 10))
  )),
fluidRow(   
     column(width=2,offset=4,
          checkboxGroupInput(inputId="plotvar", label="\n\nTime Series to plot:", choices=c("Open potholes","New potholes","Closed potholes","Reports"), selected = "Reports")
          ),
     column(width=10,
          plotOutput(outputId = "timeLine", height = "500px",width="800px")
      )
 ),
fluidRow(
     column(width=2,
          textOutput("bottomText")
     ),
     column(width=2,
          selectInput(inputId="LocX", 
               label = "Location X", 
               choices=1:400, 
               selected = 2)
     ),
     column(width=2,
          selectInput(inputId="LocY", 
               label = "Location Y", 
               choices=1:400, 
               selected = 2)
     ),        
     column(width=1,
                checkboxInput("showsquare", label = "Show square in map", value = FALSE)
     ),
     column(width=1,
                checkboxInput("showgrid",label = "Show surrounding areas",value=FALSE)
     ),  
     column(width=4,
              checkboxInput("actionTable", label = "Show Table (can take a while if grid is too fine)",value=FALSE)        
     ),
     plotOutput(outputId = "LocationTimeLine"),
     hr(),
     hr(),
     hr(),
     column(width=4,   
          uiOutput("locationSummary")
     )     
     ),
fluidRow(
     column(width=12,
            div(style = "height:100px;"),
            plotOutput("LocationTimeLineGrid")
            )
     ),


hr(),
hr(),
fluidRow(
  column(width=4,
         textOutput("matrixSummary")
         ),
  column(width=6,
                tableOutput("locationsTable")
                
          ),
  column(width=3,
          checkboxGroupInput(inputId="compareGrids", label="\n\nGrids sizes to compare", choices=c("10","15","20","25","30","35","40","400"), selected = c("10","20"))
         ),
  column(width = 3,
         checkboxGroupInput(inputId="compareCells", label="\n\nCell Values to compare", choices=c("Infs","0s","<1",">=1","Empty"), selected = c("<1",">=1"))
         ),
  column(width=2,
        checkboxInput("singleBar", label = "Show percentages in multiple bars",value=FALSE)         
          )
),

fluidRow(width=10,
     plotOutput("gridAnalysis",height = "700px")
     )








))