# server.R
library(qcc)
library(lubridate)
library(zoo)
library(xtable)
library(redcapAPI)
library(epitools)

source("helpers.R")

shinyServer(function(input, output){
  redcap.data<-eventReactive(input$button, {
    api.func(input$token)
  })
  
  hosp.choices<-reactive({
    req(redcap.data())
    hosp.func(redcap.data())
  })

  output$hospital<-renderUI({
    req(hosp.choices())
    selectInput("hospital",label="Hospitals Available:",
                choices=hosp.choices())
  })
    
  item.choices<-reactive({
    req(redcap.data())
    item.func(input$hospital,redcap.data())
  })

  output$item<-renderUI({
    req(redcap.data())
    selectInput("item",label="Item of Interest:",
                choices=item.choices())
  })
  
  trimmed.data<-eventReactive(input$selection, {
    req(redcap.data)
    trim.func(redcap.data(),input$hospital,input$item)
  })
  
  times<-reactive({
    req(trimmed.data())
    date.func(trimmed.data())
  })
  
  startdate<-reactive({
    req(times())
    times()[[2]]
  })
  
  enddate<-reactive({
    req(times())
    times()[[1]]
  })
  
  mindate<-reactive({
    req(times())
    times()[[3]]
  })
  
  output$dates<-renderUI({
      dateRangeInput("dates", label = "Date Range",
                  end=enddate(),
                  start=startdate(),
                  max=enddate(),
                  min=mindate())
  })
  
  chart.data<-reactive({
    req(times)
    date.trim.func(input$dates[1],input$dates[2],trimmed.data())
  })

  yaxis<-reactive({
    yaxe.func(redcap.data(),input$item,input$standard)
  })
  
  spcchart <- eventReactive(input$goButton, {
    spc.chart(chart.data(),as.numeric(input$standard),input$plottype,yaxis()[[1]],as.numeric(input$benchmark))
  })

  output$plot1<-renderPlot({
    spcchart()
  })
  
  table1 <- eventReactive(input$goButton, {
    req(chart.data())
    rolling.table(chart.data(),input$standard,yaxis()[[2]],yaxis()[[3]])
  })

  output$table1<-renderTable({
    table1()
  },include.colnames=F)
  
  table2 <- eventReactive(input$goButton, {
    req(chart.data())
    ytd.table(chart.data(),input$standard)
  })
  
  output$table2<-renderTable({
    table2()
  })
  
})