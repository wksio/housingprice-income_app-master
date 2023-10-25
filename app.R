library(leaflet)
library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(shinythemes)
# setwd("/Volumes/KINGSTON/projects/houseprice_app")
mean_income = read.csv("CSV_Files/mean_monthly_household_income_1970-2020.csv")
mean_houseprice = read.csv("CSV_Files/quarter_house_price_2009-2021.csv")
cordinate = read.csv("CSV_Files/cordinate.csv")
affordability = read.csv("CSV_Files/affordability.csv")

#make the data year to be consecutive
Year = data.frame(Year = seq(1970, 2020, 0.25))
##Outer merge the Year created with the mean_income
mean_income = merge(Year, mean_income, by = "Year", all = T)

##replace n.a. or "" with null
library(naniar)
mean_income = replace_with_na_all(data = mean_income, condition = ~ . %in% c("n.a", ""))
mean_income[, 2:24] <- sapply(mean_income[, 2:24], as.numeric)

##use linear interpolation to fill up the null value in each column
library(imputeTS)
for (i in c(2:24)) {
  mean_income[, i] = na_interpolation(mean_income[, i], option = "linear")
}

mean_income2 = data.frame(t(mean_income))
mean_income2 = cbind(rownames(mean_income2),
                     data.frame(mean_income2, row.names = NULL))[c(1, 2, 9:24), ]
colnames(mean_income2) = mean_income2[1, ]
mean_income2 = mean_income2[-1, ]

mean_houseprice2 = data.frame(t(mean_houseprice))
mean_houseprice2 = cbind(rownames(mean_houseprice2),
                         data.frame(mean_houseprice2, row.names = NULL))
colnames(mean_houseprice2) = mean_houseprice2[1, ]
mean_houseprice2 = mean_houseprice2[-1, ]

affordability2 = data.frame(t(affordability))
affordability2 = cbind(rownames(affordability2),
                       data.frame(affordability2, row.names = NULL))
colnames(affordability2) = affordability2[1, ]
affordability2 = affordability2[-1, ]
statess1 = mean_income2[, 1]
statess2 = mean_houseprice2[, 1]
statess3 = affordability2[, 1]
mean_income3 = subset (mean_income, select = -c(3:8))
ui <-
  navbarPage(
    theme = shinytheme("sandstone"),
    "Malaysia Mean House House Price and Mean Household Income",
    id = "nav",
    tabPanel("Interactive map", (
      div(
        class = "outer",
        tags$head(# Include our custom CSS
          includeCSS("style.css")),
        
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 60,
          left = "auto",
          right = 20,
          bottom = "auto",
          width = 330,
          height = "auto",
          uiOutput("slider"),
          textOutput("Malaysia"),
          plotOutput("timeplot", height = 200),
          plotOutput("timeplotstate", height = 250)
        ),
        absolutePanel(
          id = "map_input",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE,
          top = 60,
          left = 45,
          right = "auto",
          bottom = "auto",
          width = 330,
          selectInput(
            "select_map_types",
            h3("Select map types"),
            choices = list(
              "House Price" = 1,
              "Household Income" = 2,
              "Affordability" = 3
            ),
            selected = 1
          )
        ),
        
        
      )
    )),
    tabPanel("Comparison",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "comparison_type",
                   "Type of Comparison",
                   c(
                     "Mean Income" = 1,
                     "Mean Housprice" = 2,
                     "Affordability" = 3
                   ),
                   selected = 1
                 ),
                 conditionalPanel(
                   condition = "input.comparison_type == 1",
                   pickerInput(
                     "state_select1",
                     "States:",
                     choices = as.character(statess1),
                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                     selected = statess1[1],
                     multiple = TRUE
                   ),
                 ),
                 
                 conditionalPanel(
                   condition = "input.comparison_type == 2",
                   pickerInput(
                     "state_select2",
                     "States:",
                     choices = as.character(statess2),
                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                     selected = statess1[1],
                     multiple = TRUE
                   ),
                 ),
                 conditionalPanel(
                   condition = "input.comparison_type == 3",
                   pickerInput(
                     "state_select3",
                     "States:",
                     choices = as.character(statess3),
                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                     selected = statess1[1],
                     multiple = TRUE
                   ),
                 )
               ),
               
               
               mainPanel(plotlyOutput("comparison_plot"), width = 6)
               
             )),
    tabPanel(
      "Ranking",
      titlePanel("Ranking of Each State"),
      
      # Create a new Row in the UI for selectInputs
      fluidRow(column(
        4,
        selectInput(
          "comparison_type2",
          "Type of Comparison",
          c(
            "Mean Income" = 1,
            "Mean Housprice" = 2,
            "Affordability" = 3
          ),
          selected = 1
        )
        
      ),
      column(4,
             uiOutput("slider2")),),
      DTOutput('tbl')
    )
    
    
  )

server <- function(input, output, session) {
  observe({
    if (input$select_map_types == 2) {
      output$slider <- renderUI({
        sliderInput(
          "year",
          "Year:",
          min = minimum,
          max = maximum,
          value = 2019,
          step = gap[1]
        )
      })
      map_year = data.frame(colnames(mean_income2)[-1])
      map_year[, 1] = as.numeric(map_year[, 1])
      minimum = min(map_year[, 1])
      maximum = max(map_year[, 1])
      gap = diff(map_year[, 1])
    }
    else if (input$select_map_types == 1) {
      output$slider <- renderUI({
        sliderInput(
          "year",
          "Year:",
          min = minimum,
          max = maximum,
          value = 2019,
          step = gap[1]
        )
      })
      map_year = data.frame(colnames(mean_houseprice2)[-1])
      map_year[, 1] = as.numeric(map_year[, 1])
      minimum = min(map_year[, 1])
      maximum = max(map_year[, 1])
      gap = diff(map_year[, 1])
    }
    else if (input$select_map_types == 3) {
      output$slider <- renderUI({
        sliderInput(
          "year",
          "Year:",
          min = minimum,
          max = maximum,
          value = 2019,
          step = gap[1]
        )
      })
      map_year = data.frame(colnames(affordability2)[-1])
      map_year[, 1] = as.numeric(map_year[, 1])
      minimum = min(map_year[, 1])
      maximum = max(map_year[, 1])
      gap = diff(map_year[, 1])
    }
    
  })
  observe({
    if (input$select_map_types == 2) {
      take.v = input$year
      if (is.null(take.v)) {
        take.v = 2019
      }
      else{
        take.v = input$year
      }
      column.no = match(take.v, colnames(mean_income2))
      mean_income2 = data.frame(mean_income2[, c(1, column.no)])
      colnames(mean_income2) = c("State", "value")
      isolate({
        map_data = merge(
          x = cordinate ,
          y = mean_income2,
          by = "State",
          all.x = TRUE
        )
        map_data[, 4] = as.numeric(map_data[, 4])
        library(rgdal)
        states1 <- rgdal::readOGR("CSV_Files/states.geojson")
        states1 = merge(states1, map_data, by.x = "State")
        colorData2 = states1@data[["value"]]
        pal2 = colorBin("YlOrRd", colorData2, 7, pretty = FALSE)
        plotdata = mean_income
      })
      poptitle = "Mean Monthly Household Income: RM"
      selectMalaysia <-
        mean_income2[mean_income2$State == "Malaysia", ]
      output$Malaysia = renderText({
        print(paste("Malaysia Overall Mean: RM" , selectMalaysia$value))
      })
      output$timeplot = renderPlot({
        plot(
          plotdata$Year,
          plotdata$Malaysia,
          xlab = "Year",
          ylab = "RM"
          ,
          main = "Mean Monthly Household Income Malaysia",
          type = "l"
        )
      })
      
      
    }
    
    else if (input$select_map_types == 1) {
      take.v = input$year
      if (is.null(take.v)) {
        take.v = 2019
      }
      else{
        take.v = input$year
      }
      column.no = match(take.v, colnames(mean_houseprice2))
      mean_houseprice2 = data.frame(mean_houseprice2[, c(1, column.no)])
      colnames(mean_houseprice2) = c("State", "value")
      isolate({
        map_data = merge(
          x = cordinate ,
          y = mean_houseprice2,
          by = "State",
          all.y = TRUE
        )
        map_data[, 4] = as.numeric(map_data[, 4])
        library(rgdal)
        states1 <- rgdal::readOGR("CSV_Files/states.geojson")
        states1 = merge(states1, map_data, by.x = "State")
        colorData2 = states1@data[["value"]]
        pal2 = colorBin("YlOrRd", colorData2, 7, pretty = FALSE)
        plotdata = mean_houseprice
      })
      poptitle = "Mean House Price: RM"
      selectMalaysia <-
        mean_houseprice2[mean_houseprice2$State == "Malaysia", ]
      output$Malaysia = renderText({
        print(paste("Malaysia Overall Mean: RM" , selectMalaysia$value))
      })
      output$timeplot = renderPlot({
        plot(
          plotdata$Year,
          plotdata$Malaysia,
          xlab = "Year",
          ylab = "RM"
          ,
          main = "Mean House Price Malaysia",
          type = "l"
        )
      })
      
    }
    else if (input$select_map_types == 3) {
      take.v = input$year
      if (is.null(take.v)) {
        take.v = 2019
      }
      else{
        take.v = input$year
      }
      column.no = match(take.v, colnames(affordability2))
      affordability2 = data.frame(affordability2[, c(1, column.no)])
      colnames(affordability2) = c("State", "value")
      isolate({
        map_data = merge(
          x = cordinate ,
          y = affordability2,
          by = "State",
          all.y = TRUE
        )
        map_data[, 4] = as.numeric(map_data[, 4])
        library(rgdal)
        states1 <- rgdal::readOGR("CSV_Files/states.geojson")
        states1 = merge(states1, map_data, by.x = "State")
        colorData2 = states1@data[["value"]]
        risk.bins <- c(0, 3, 4, 5, 10)
        pal2 <- colorBin("YlOrRd", bins = risk.bins, pretty = FALSE)
        plotdata = affordability
        label1 = c(
          "Affordable",
          "Moderately Unaffordable",
          "Seriously Unaffordable",
          "Severly Unaffordable"
        )
      })
      poptitle = "Affordability: "
      selectMalaysia <-
        affordability2[affordability2$State == "Malaysia", ]
      output$Malaysia = renderText({
        print(paste("Malaysia Overall Mean:" , selectMalaysia$value))
      })
      output$timeplot = renderPlot({
        plot(
          plotdata$Year,
          plotdata$Malaysia,
          xlab = "Year",
          ylab = "Affordability"
          ,
          main = "Affordability Malaysia",
          type = "l"
        )
      })
      
    }
    
    leafletProxy("map", data = map_data) %>%
      clearShapes() %>%
      addLegend(
        "bottomleft",
        pal = pal2,
        values = colorData2,
        title = poptitle,
        labels = label1,
        layerId = "colorLegend"
        
      ) %>%
      addPolygons(
        data = states1,
        layerId =  ~ State,
        fillOpacity = 0.6,
        fillColor = pal2(colorData2)
        
      )
    
    # Show a popup at the given location
    showPopup <- function(State, Latitude, Longitude, data) {
      selectedState <- map_data[map_data$State == State, ]
      content <- as.character(tagList(
        tags$h4("State:", selectedState$State),
        tags$br(),
        tags$h6(poptitle, selectedState$value)
      ))
      leafletProxy("map") %>% addPopups(Longitude, Latitude, content, layerId = "popup")
      output$timeplotstate = renderPlot({
        plot(
          data$Year,
          data[[State]],
          xlab = "Year",
          ylab = ""
          ,
          main = State,
          type = "l"
        )
      })
    }
    
    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        showPopup(event$id, event$lat, event$lng, plotdata)
      })
    })
    
  })
  
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 108.9758,
              lat = 4.2105,
              zoom = 6) %>%
      addProviderTiles(providers$Stamen.Toner)
    
  })
  
  observe({
    if (input$comparison_type == 1) {
      if (is.null(input$state_select1)) {
        selectedstate = c("Malaysia")
      }
      else{
        selectedstate = input$state_select1
      }
      isolate({
        dataplot = mean_income3
        column.state = c()
        for (i in selectedstate) {
          column.state = c(column.state, match(i, colnames(dataplot)))
        }
        titleplot = "Mean Household Income"
      })
      
    }
    else if (input$comparison_type == 2) {
      if (is.null(input$state_select2)) {
        selectedstate = c("Malaysia")
      }
      else{
        selectedstate = input$state_select2
      }
      isolate({
        dataplot = mean_houseprice
        column.state = c()
        for (i in selectedstate) {
          column.state = c(column.state, match(i, colnames(dataplot)))
        }
        titleplot = "Mean House Price"
      })
      
    }
    else if (input$comparison_type == 3) {
      if (is.null(input$state_select3)) {
        selectedstate = c("Malaysia")
      }
      else{
        selectedstate = input$state_select3
      }
      isolate({
        dataplot = affordability
        column.state = c()
        for (i in selectedstate) {
          column.state = c(column.state, match(i, colnames(dataplot)))
        }
        titleplot = "Affordability"
      })
      
    }
    
    library(randomcoloR)
    library(ggplot2)
    n <- length(column.state)
    palette <- distinctColorPalette(n)
    my.cols <- palette
    my.names <- names(dataplot[, column.state])
    names(my.cols) <- my.names
    plotsss <-  ggplot(dataplot, aes(x = Year))
    plotsss
    
    for (i in selectedstate) {
      plotsss <- plotsss + geom_line(aes_(y = as.name(i), colour =
                                            i))
    }
    plotsss + scale_colour_manual("",
                                  breaks = as.character(my.names),
                                  values = my.cols) +
      labs(title = titleplot) + xlab("Year") +
      ylab("y axis")
    output$comparison_plot = renderPlotly({
      ggplotly(plotsss)
    })
  })
  
  observe({
    if (input$comparison_type2 == 1) {
      output$slider2 <- renderUI({
        sliderInput(
          "year2",
          "Year:",
          min = minimum2,
          max = maximum2,
          value = 2019,
          step = gap2[1]
        )
      })
      map_year2 = data.frame(colnames(mean_income2)[-1])
      map_year2[, 1] = as.numeric(map_year2[, 1])
      minimum2 = min(map_year2[, 1])
      maximum2 = max(map_year2[, 1])
      gap2 = diff(map_year2[, 1])
    }
    else if (input$comparison_type2 == 2) {
      output$slider2 <- renderUI({
        sliderInput(
          "year2",
          "Year:",
          min = minimum2,
          max = maximum2,
          value = 2019,
          step = gap2[1]
        )
      })
      map_year2 = data.frame(colnames(mean_houseprice2)[-1])
      map_year2[, 1] = as.numeric(map_year2[, 1])
      minimum2 = min(map_year2[, 1])
      maximum2 = max(map_year2[, 1])
      gap2 = diff(map_year2[, 1])
    }
    else if (input$comparison_type2 == 3) {
      output$slider2 <- renderUI({
        sliderInput(
          "year2",
          "Year:",
          min = minimum2,
          max = maximum2,
          value = 2019,
          step = gap2[1]
        )
      })
      map_year2 = data.frame(colnames(affordability2)[-1])
      map_year2[, 1] = as.numeric(map_year2[, 1])
      minimum2 = min(map_year2[, 1])
      maximum2 = max(map_year2[, 1])
      gap2 = diff(map_year2[, 1])
    }
    
  })
  observe({
    if (input$comparison_type2 == 1) {
      take.v = input$year2
      if (is.null(take.v)) {
        take.v = 2019
      }
      else{
        take.v = input$year2
      }
      column.no = match(take.v, colnames(mean_income2))
      mean_income2 = data.frame(mean_income2[, c(1, column.no)])
      colnames(mean_income2) = c("State", "value")
      
      n1 = length(mean_income2[, 1])
      datatable = mean_income2[order(-mean_income2$value), ]
      datatable[, "Rank"] = c(1:n1)
      datatable <- datatable %>%
        mutate(
          state = paste0(
            '<img src="https://raw.githubusercontent.com/kang20006/picture/main/',
            State,
            '.png" height="30"</img>'
          )
        ) %>% select(state, State, value, Rank)
    }
    else if (input$comparison_type2 == 2) {
      take.v = input$year2
      if (is.null(take.v)) {
        take.v = 2019
      }
      else{
        take.v = input$year2
      }
      column.no = match(take.v, colnames(mean_houseprice2))
      mean_housepriceA = data.frame(mean_houseprice2[, c(1, column.no)])
      colnames(mean_housepriceA) = c("State", "value")
      
      n1 = length(mean_housepriceA[, 1])
      datatable = mean_housepriceA[order(-mean_housepriceA$value), ]
      datatable[, "Rank"] = c(1:n1)
      datatable <- datatable %>%
        mutate(
          state = paste0(
            '<img src="https://raw.githubusercontent.com/kang20006/picture/main/',
            State,
            '.png" height="30"</img>'
          )
        ) %>% select(state, State, value, Rank)
    }
    else if (input$comparison_type2 == 3) {
      take.v = input$year2
      if (is.null(take.v)) {
        take.v = 2019
      }
      else{
        take.v = input$year2
      }
      column.no = match(take.v, colnames(affordability2))
      affordabilityA = data.frame(affordability2[, c(1, column.no)])
      colnames(affordabilityA) = c("State", "value")
      
      n1 = length(affordabilityA[, 1])
      datatable = affordabilityA[order(affordabilityA$value), ]
      datatable[, "Rank"] = c(1:n1)
      datatable <- datatable %>%
        mutate(
          state = paste0(
            '<img src="https://raw.githubusercontent.com/kang20006/housingprice-income_app/main/',
            State,
            '.png" height="30"</img>'
          )
        ) %>% select(state, State, value, Rank)
    }
    
    output$tbl <- DT::renderDataTable({
      DT::datatable(
        datatable,
        options = list(
          paging = FALSE,
          autoWidth = TRUE,
          ## use smart column width handling
          server = FALSE,
          ## use client-side processing
          dom = 'Bfrtip',
          buttons = c('csv', 'excel'),
          columnDefs = list(list(
            targets = '_all', className = 'dt-center'
          ))
          
        ),
        extensions = 'Buttons',
        selection = 'single',
        ## enable selection of a single row
        filter = 'top',
        ## include column filters at the bottom
        rownames = FALSE,
        ## don't show row numbers/names
        escape = FALSE
      ) %>% formatStyle(column = 4, backgroundColor = styleEqual(c(1, 2, 3), c("gold", "silver", "#CD7F32")))
      
      
    })
    
    
  })
  
  
  
  
  
}


shinyApp(ui = ui, server = server)
