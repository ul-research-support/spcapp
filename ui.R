library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(3,wellPanel(
        textInput("token",label="API token",value=NA)
        ,p("Copy and paste your API token above!")
        ,actionButton("button","Get REDCap Data")
        ,p("Click the button to pull data from REDCap! This may take some time, please be patient.")
      ,conditionalPanel(
        condition="input.button > 0"
        ,uiOutput("hospital")
        ,uiOutput("item")
        ,actionButton("selection","Confirm your selection")
        ,p("You must re-confirm if you change your hospital or item!")
        )
      ,conditionalPanel(
        condition="input.selection > 0"
        ,uiOutput("dates")
        ,selectInput("standard",label="Standardization Factor",
                                      choices=c(100,1000,10000),selected=1000)
            ,selectInput("plottype",label="Plot Type",
                     choices=c("U-chart"="u","P-chart"="p"))
            ,numericInput("benchmark",label="Benchmark",value=0)
        ,actionButton("goButton","Create/Update Chart")
        ,p("Changes to hospital, item, dates, standardization, plot type, and benchmark will not be 
           reflected on the chart unless this button is clicked again!"))))
  ,column(9,
    fluidRow(plotOutput("plot1",height=550))
    ,br()
    ,fluidRow(tableOutput("table1"),align="center")
    ,br()
    ,fluidRow(tableOutput("table2"),align="center")
   ))))

