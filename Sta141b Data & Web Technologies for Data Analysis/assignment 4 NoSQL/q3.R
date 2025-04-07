library(shiny)
library(nycflights13)
library(tidyverse)

# Write a shiny app which reproduces https://ucdsta141.uc.r.appspot.com/
# (the web server may need time to warm up sometimes, just be patient).
# You'll need two datasets from nycflights13, `flights` and `airports`

# The histogram shows the distribution of arrival delay time from "origin"
# to "dest" in different months. If no month is select, it shows the distribution
# of the whole year.

# The "Summary Statistics" table was computed by using the `summary()` function
# of the corresponding data.

# We also tally the number of flights from "origin" to "dest" in different months.

# You don't have to 100% reproduce the same app, it's good enough if your app
# provides the same functionality. However, in order to get full credits,
# your app should also handle the following edge cases,

# 1. Some combinations of routes do not exist.
# 2. Some routes may have only flights in certain months.
#    For example, JFK to Albuquerque only had flights from April to December.
# 3. Your app should in general free of bugs and errors.
origins <- c("La Guardia", "John F Kennedy Intl", "Newark Liberty Intl")
originFaa <- airports$faa[airports$name %in% origins]

dat <- flights %>% filter(origin%in% originFaa)
destFaa <- dat$dest
destNames <- airports$name[airports$faa %in% destFaa]
ui <- fluidPage(
  titlePanel("Arrival Delay"),
  sidebarLayout(
    sidebarPanel(
      selectInput("origin", label="Origin", choices=origins,selected = "John F Kennedy Intl"),
      uiOutput("ui1"),
      uiOutput("ui2")
    ),
    mainPanel(
      plotOutput(outputId = "plot"),
      verbatimTextOutput(outputId = "text"),
      tableOutput(outputId = "table")
    )
  ),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)

server <- function(input, output, session) {
  
  output$ui1 <- renderUI({
    origin <- airports$faa[airports$name==input$origin]
    dat <- dat[dat$origin==origin, ]
    x <- airports$name[airports$faa %in% unique(dat$dest)]
    selectInput("destination", label="Destination", choices=c("-", sort(x)))
  })
  
  output$ui2 <- renderUI({
    origin <- airports$faa[airports$name==input$origin]
    dat <- dat[dat$origin==origin, ]
    if(input$destination!="-"){
      dest <- airports$faa[airports$name==input$destination]
      dat <- dat[dat$dest==dest, ]
    }
    selectInput("month", label="Month", choices=c("-", month.name[sort(unique(dat$month))]))
  })
  
  
  output$plot <- renderPlot({
    origin <- airports$faa[airports$name==input$origin]
    dat2 <- dat[dat$origin==origin, ]
    if(length(input$destination)==0){
      return
    }
    if(length(input$month)==0){
      return
    }
    if(input$destination!="-"){
      dest <- airports$faa[airports$name==input$destination]
      dat2 <- dat2[dat2$dest==dest,]
    }
    if(input$month!="-"){
      dat2 <- dat2[dat2$month==which(month.name==input$month),]
    }
    if(nrow(dat2)!=0){
      ggplot(data=dat2, aes(x=arr_delay)) + geom_histogram(bins=30) + labs(x="delay")
    }
  })
  
  output$text <- renderPrint({
    origin <- airports$faa[airports$name==input$origin]
    dat2 <- dat[dat$origin==origin, ]
    if(length(input$destination)==0){
      return
    }
    if(length(input$month)==0){
      return
    }
    if(input$destination!="-"){
      dest <- airports$faa[airports$name==input$destination]
      dat2 <- dat2[dat2$dest==dest,]
    }
    if(input$month!="-"){
      dat2 <- dat2[dat2$month==which(month.name==input$month),]
    }
    summary(dat2$arr_delay)
  })
  
  output$table <- renderTable({
    origin <- airports$faa[airports$name==input$origin]
    dat2 <- dat[dat$origin==origin, ]
    if(length(input$destination)==0){
      return
    }
    if(length(input$month)==0){
      return
    }
    if(input$destination!="-"){
      dest <- airports$faa[airports$name==input$destination]
      dat2 <- dat2[dat2$dest==dest,]
    }
    if(input$month!="-"){
      dat2 <- dat2[dat2$month==which(month.name==input$month),]
    }
    if(nrow(dat2)!=0){
      df <- as.data.frame(table(dat2$month))
      month.index <- as.numeric(as.character(df$Var1))
      df <- as.data.frame(t(df))
      colnames(df) <- month.abb[month.index]
      df <- df[-1,]
      df
    }
    
    
  })
}

shinyApp(ui, server)
