library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(leaflet)
library(plotly)
library(tmap)
library(tidyverse)
library(DT)
library(ggplot2)
library(gganimate)
library(gridExtra)
# options("rgdal_show_exportToProj4_warnings"="none") 
# library(rgdal)

# Import data
load("WQS_indicator_APP.Rdata")
# D <- read.csv("WQS_indicator_APP_data.csv", header = TRUE)
# d <- read.csv("WQS_indicator_segment_APP_data.csv", header = TRUE)
D$Attainment <- round(D$Attainment, 2)
d <- as.data.frame(d)
d$DU <- factor(d$DU, levels = c("MSN", "OW", "DW", "DC", "CHLA", "SWBG"))

# UI
ui <- fluidPage(
  
  # Add a theme
  theme = shinytheme("flatly"),
  
  # Add a title panel
  titlePanel("Chesapeake Bay Water Quality Standards Attainment Indicator"),
  
  # Add a disclaimer
  print("This R Shiny APP is designed for visualizing the water quality standards attainment for the tidal segments of Chesapeake Bay."),
  br(),
  print("Notes:"),
  br(),
  print("1. Attainment indicator: 1 = full attainment; 0 = not in full attainment."),
  br(),
  print("2. Designated use: OW = open water dissolved oxygen (DO); DW = deep water DO; DC = deep channel DO; MSN = migratory spawning and nursery DO; CHLA = chlorophyll-a; SWBG = shallow water bay grasses / water clarity."),
  br(),
  print("3. Assessment period: Year represents the first year of a 3-year assessment period, e.g., year 1985 represents the 1985-1987 period; year 2018 represents the 2018-2020 period."),
  # print("Water-quality criteria: DO = dissolved oxygen, CHLA = chlorophyll-a, SWBG = shallow water bay grasses / water clarity."),
  # print("Designated uses: OW = open water, DW = deep water, DC = deep channel, MSN = migratory spawning and nursery, SW = shallow water."),
  br(),
  print("4. Official Website: https://www.chesapeakeprogress.com/clean-water/water-quality."),
  br(),
  print("5. References: https://doi.org/10.1016/j.scitotenv.2018.05.025; https://doi.org/10.3389/fmars.2018.00422."),
  br(),
  print("6. For questions or feedback, please contact Qian Zhang (qzhang@chesapeakebay.net)."),
  hr(),
  
  navbarPage(
    "",
    
    # Section 1
    tabPanel(
      "Indicator",
      mainPanel(
        tabsetPanel(
          tabPanel("Overall Indicator", plotlyOutput("myfigure1", "800px", "400px")),
          tabPanel("Designated Use", plotlyOutput("myfigure2", "800px", "800px")),
          tabPanel("Data Table", dataTableOutput("mytable1"))
        )
      )
    ),
    
    # Section 2
    tabPanel(
      "Segment Table",
      sidebarLayout(
        sidebarPanel(
          # radioButtons(
          #   inputId = "Cutoff",
          #  label = "Specify the minimum attainment to allow for minor non-attainment",
          #  choices = c("Default: Only 100% = attainment" = 1),
          # choices = c("Default: Only 100% = attainment" = 1,
          #             "Values above 99% = attainment" = 0.99,
          #             "Values above 95% = attainment" = 0.95,
          #             "Values above 90% = attainment" = 0.90
          # ),
          #  selected = "1"
          # ),
          # br(),
          # helpText("Default = 0"),
          
          # Add a download button
          downloadButton(outputId = "download_data", label = "Download Data"),
          helpText("Note: Click this button to download all data. If user inputs are provided below, only the selected data will be downloaded."),
          br(),
          selectInput(
            inputId = "Segment",
            label = "Specify the segments to show data",
            choices = c("All", as.character(unique(d$Segment))),
            multiple = TRUE,
            selected = c("All"),
            width = NULL
          ),
          helpText("Note: Multiple or all segments can be selected."),
          br(),
          checkboxGroupInput(
            inputId = "DU",
            label = "Specify the designated use (DU) to show data",
            # choices = c("All", "MSN_DO", "OW_DO", "DW_DO", "DC_DO", "Chla", "SAV_Clartiy"),
            choices = c("All DUs", "MSN", "OW", "DW", "DC", "CHLA", "SWBG"),
            selected = c("All DUs")
          ),
          helpText("Note: Multiple or all DUs can be selected."),
          br(),
          sliderInput(
            inputId = "Year",
            label = "Specify the years to show data",
            min = 1985,
            max = 2018,
            value = c(1985, 2018),
            width = NULL
          ),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Data Table", dataTableOutput("mytable2"))
          )
        )
      )
    ),
    
    # Section 3
    tabPanel(
      "Segment Figure",
      sidebarLayout(
        sidebarPanel(
          
          selectInput(
            inputId = "Segment_for_figure",
            label = "Specify the segment to show time series",
            choices = c(as.character(unique(d$Segment))),
            multiple = FALSE,
            selected = c("CB4MH"),
            width = NULL
          ),
          br(),
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Data Figure", plotlyOutput("myfigure3", "900px", "400px")),
          )
        )
      )
    ),
    
    # Section 4
    tabPanel(
      "Segment Map",
      sidebarLayout(
        sidebarPanel(
          # radioButtons(
          #   inputId = "Cutoff",
          #  label = "Specify the minimum attainment to allow for minor non-attainment",
          #  choices = c("Default: Only 100% = attainment" = 1),
          # choices = c("Default: Only 100% = attainment" = 1,
          #             "Values above 99% = attainment" = 0.99,
          #             "Values above 95% = attainment" = 0.95,
          #             "Values above 90% = attainment" = 0.90
          # ),
          #  selected = "1"
          # ),
          # br(),
          # helpText("Default = 0"),
          
          sliderInput(
            inputId = "Year1",
            label = "Specify the year to show maps",
            min = 1985,
            max = 2018,
            value = 2018,
            width = NULL
          ),
          helpText("Note: Default = 2018, i.e., the 2018-2020 assessment period."),
          helpText("Note: It takes ~15 seconds to generate the map.")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("MSN", tmapOutput("mymap1", "800px", "800px")),
            tabPanel("OW", tmapOutput("mymap2", "800px", "800px")),
            tabPanel("DW", tmapOutput("mymap3", "800px", "800px")),
            tabPanel("DC", tmapOutput("mymap4", "800px", "800px")),
            tabPanel("CHLA", tmapOutput("mymap5", "800px", "800px")),
            tabPanel("SWBG", tmapOutput("mymap6", "800px", "800px"))
          )
        )
      )
    ),
    
    # Section 5
    tabPanel(
      "Segment Comparison",
      sidebarLayout(
        sidebarPanel(
          # radioButtons(
          #   inputId = "Cutoff",
          #  label = "Specify the minimum attainment to allow for minor non-attainment",
          #  choices = c("Default: Only 100% = attainment" = 1),
          # choices = c("Default: Only 100% = attainment" = 1,
          #             "Values above 99% = attainment" = 0.99,
          #             "Values above 95% = attainment" = 0.95,
          #             "Values above 90% = attainment" = 0.90
          # ),
          #  selected = "1"
          # ),
          # br(),
          # helpText("Default = 0"),
          
          sliderInput(
            inputId = "Y1",
            label = "Specify the 1st year for comparison",
            min = 1985,
            max = 2018,
            value = 2017,
            width = NULL
          ),
          helpText("Note: Default = 2017, i.e., the 2017-2019 assessment period."),
          sliderInput(
            inputId = "Y2",
            label = "Specify the 2nd year for comparison",
            min = 1985,
            max = 2018,
            value = 2018,
            width = NULL
          ),
          helpText("Note: Default = 2018, i.e., the 2018-2020 assessment period."),
          helpText("Note: It takes ~30 seconds to generate the map.")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Table", dataTableOutput("mytable3")),
            tabPanel("MSN", fluidRow(
              column(6, tmapOutput("mymap1A", width = "100%", height = 800)),
              column(6, tmapOutput("mymap1B", width = "100%", height = 800))
            )),
            tabPanel("OW", fluidRow(
              column(6, tmapOutput("mymap2A", width = "100%", height = 800)),
              column(6, tmapOutput("mymap2B", width = "100%", height = 800))
            )),
            tabPanel("DW", fluidRow(
              column(6, tmapOutput("mymap3A", width = "100%", height = 800)),
              column(6, tmapOutput("mymap3B", width = "100%", height = 800))
            )),
            tabPanel("DC", fluidRow(
              column(6, tmapOutput("mymap4A", width = "100%", height = 800)),
              column(6, tmapOutput("mymap4B", width = "100%", height = 800))
            )),
            tabPanel("CHLA", fluidRow(
              column(6, tmapOutput("mymap5A", width = "100%", height = 800)),
              column(6, tmapOutput("mymap5B", width = "100%", height = 800))
            )),
            tabPanel("SWBG", fluidRow(
              column(6, tmapOutput("mymap6A", width = "100%", height = 800)),
              column(6, tmapOutput("mymap6B", width = "100%", height = 800))
            ))
          )
        )
      )
    ),
    
    # Section 6
    tabPanel(
      "Segment Animation",
      mainPanel(
        tabsetPanel(
          tabPanel("MSN", imageOutput("mygif1", "800px", "800px")),
          tabPanel("OW", imageOutput("mygif2", "800px", "800px")),
          tabPanel("DW", imageOutput("mygif3", "800px", "800px")),
          tabPanel("DC", imageOutput("mygif4", "800px", "800px")),
          tabPanel("CHLA", imageOutput("mygif5", "800px", "800px")),
          tabPanel("SWBG", imageOutput("mygif6", "800px", "800px"))
        )
      )
    )
  )
)

server <- function(input, output) {
  # Making a reactive object called points where we filter the data
  # filtered_data <- reactive({
  #  data <- subset(d, Segment %in% input$Segment)
  #  data
  # })
  
  # Table output 1
  output$mytable1 <- renderDataTable(
    {
      data <- D %>%
        select(DU, Year, Attainment) %>%
        spread(key = "DU", value = "Attainment") %>%
        select(Year, Total, MSN, OW, DW, DC, CHLA, SWBG)
      data
    },
    options = list(iDisplayLength = 50)
  )
  
  # Table output 2
  output$mytable2 <- renderDataTable(
    {
      if (mean(input$Segment == "All") == 1) {
        data1 <- d
      }
      
      if (mean(input$Segment == "All") < 1) {
        selection <- as.vector(input$Segment[input$Segment != "All"])
        data1 <- d %>%
          filter(Segment %in% selection)
      }
      
      data2 <- data1 %>%
        select(Segment, DU, Year, Attainment) %>%
        mutate(Attainment = ifelse(Attainment >= 1, 1, 0)) %>%
        # mutate(Attainment = ifelse(Attainment >= input$cutoff, 1, 0)) %>%
        spread(key = "DU", value = "Attainment") %>%
        filter(Year >= input$Year[1]) %>%
        filter(Year <= input$Year[2]) %>%
        select(Segment, Year, MSN, OW, DW, DC, CHLA, SWBG)
      
      if (mean(input$DU == "All DUs") == 1) {
        data3 <- data2
      }
      
      if (mean(input$DU == "All DUs") < 1) {
        selection <- as.vector(input$DU[input$DU != "All DUs"])
        data3 <- data2[, c("Segment", "Year", selection)]
      }
      
      # Only show rows with at least one non-NA value beyond Segment and Year
      data3 <- data3[rowSums(!is.na(data3)) > 2, ]
      data3
    },
    options = list(aLengthMenu = c(10, 25, 50, 100), iDisplayLength = 50)
  )
  
  # Table output 3
  output$mytable3 <- renderDataTable(
    {
      data1 <- d %>%
        as.data.frame() %>%
        select(Segment, DU, Year, Attainment) %>%
        mutate(Attainment = ifelse(Attainment >= 1, 1, 0)) %>%
        filter(Year == input$Y1) %>%
        spread(key = "DU", value = "Attainment")
      
      data2 <- d %>%
        as.data.frame() %>%
        select(Segment, DU, Year, Attainment) %>%
        mutate(Attainment = ifelse(Attainment >= 1, 1, 0)) %>%
        filter(Year == input$Y2) %>%
        spread(key = "DU", value = "Attainment")
      
      data <- data1
      data[, -1] <- data2[, -1] - data1[, -1]
      data <- data %>% select(-Year)
      data$index <- NA
      for (i in 1:dim(data)[1])
      {
        data$index[i] <- sum(data[i, -1] == 1, na.rm = TRUE) + sum(data[i, -1] == -1, na.rm = TRUE)
      }
      data <- data %>%
        filter(index > 0) %>%
        select(-index)
      
      data[data == 0] <- NA
      data[data == 1] <- paste("Fail (", input$Y1, ") -> Pass (", input$Y2, ")", sep = "")
      data[data == -1] <- paste("Pass (", input$Y1, ") -> Fail (", input$Y2, ")", sep = "")
      
      data
    },
    options = list(aLengthMenu = c(10, 25, 50, 100), iDisplayLength = 50)
  )
  
  # Create a download handler
  output$download_data <- downloadHandler(
    filename = "CB_WQS_attainment_data.csv",
    content = function(file) {
      if (mean(input$Segment == "All") == 1) {
        data1 <- d
      }
      
      if (mean(input$Segment == "All") < 1) {
        selection <- as.vector(input$Segment[input$Segment != "All"])
        data1 <- d %>%
          filter(Segment %in% selection)
      }
      
      data2 <- data1 %>%
        select(Segment, DU, Year, Attainment) %>%
        mutate(Attainment = ifelse(Attainment >= 1, 1, 0)) %>%
        spread(key = "DU", value = "Attainment") %>%
        filter(Year >= input$Year[1]) %>%
        filter(Year <= input$Year[2]) %>%
        select(Segment, Year, MSN, OW, DW, DC, CHLA, SWBG)
      
      if (mean(input$DU == "All DUs") == 1) {
        data3 <- data2
      }
      
      if (mean(input$DU == "All DUs") < 1) {
        selection <- as.vector(input$DU[input$DU != "All DUs"])
        data3 <- data2[, c("Segment", "Year", selection)]
      }
      
      # Only show rows with at least one non-NA value beyond Segment and Year
      data3 <- data3[rowSums(!is.na(data3)) > 2, ]
      data3
      
      # Write the filtered data into a CSV file
      write.csv(data3, file, row.names = FALSE)
    }
  )
  
  # Figure Output 1
  output$myfigure1 <- renderPlotly({
    ggplotly({
      ggplot(
        D %>%
          filter(DU == "Total") %>%
          select(DU, Year, Attainment),
        aes(x = Year, y = Attainment)
      ) +
        geom_point(pch = 16, col = "deepskyblue", cex = 2) +
        geom_line(lwd = 1) +
        theme_bw(base_size = 15) +
        ylim(0, 60) +
        labs(
          x = "Assessment period", y = "Attainment, percent",
          title = "The overall WQS attainment indicator"
        ) +
        geom_smooth(method = "loess")
      # stat_cor(method = "spearman", cor.coef.name = "rho", label.x.npc = "left", label.y.npc = "top") +
      # stat_regline_equation(label.x.npc = "center", label.y.npc = "bottom")
    })
  })
  
  # Figure Output 2
  output$myfigure2 <- renderPlotly({
    ggplotly({
      ggplot(
        D %>%
          filter(DU != "Total") %>%
          select(DU, Year, Attainment) %>%
          mutate(DU = factor(DU, levels = c("MSN", "OW", "DW", "DC", "CHLA", "SWBG"))),
        aes(x = Year, y = Attainment)
      ) +
        geom_point(pch = 16, col = "deepskyblue", cex = 2) +
        geom_line(lwd = 1) +
        theme_bw(base_size = 15) +
        ylim(0, 100) +
        labs(
          x = "Assessment period", y = "DU-specific attainment, percent",
          title = "",
          subtitle = ""
        ) +
        geom_smooth(method = "loess") +
        # stat_cor(method = "spearman", cor.coef.name = "rho", label.x.npc = "left", label.y.npc = "top") +
        # stat_regline_equation(label.x.npc = "center", label.y.npc = "bottom") +
        facet_wrap(~DU, ncol = 2)
    })
  })
  
  # Figure Output 3
  output$myfigure3 <- renderPlotly({
    data1 <- d %>% 
      mutate(DU = factor(DU, levels = c("MSN", "OW", "DW", "DC", "CHLA", "SWBG"))) %>% 
      filter(Segment %in% input$Segment_for_figure) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail")) %>% 
      complete(Year, DU)
    
    ggplotly({
      ggplot(data1, aes(x = Year, y = DU, fill = Attainment)) +
        geom_tile() +
        scale_fill_manual(values = c("brown", "darkgreen", "coral2")) +
        scale_x_continuous(breaks = seq(1985, 2020, 5)) +
        labs(
          x = "Assessment period", y = "Designated use",
          fill = "Status",
          title = input$Segment_for_figure,
          subtitle = ""
        ) 
    })
  })
  
  # Map 1
  output$mymap1 <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Year1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "MSN", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("MSN ", input$Year1, "-", input$Year1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map 2
  output$mymap2 <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Year1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "OW", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("OW ", input$Year1, "-", input$Year1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map 3
  output$mymap3 <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Year1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "DW", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("DW ", input$Year1, "-", input$Year1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map 4
  output$mymap4 <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Year1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "DC", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("DC ", input$Year1, "-", input$Year1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map 5
  output$mymap5 <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Year1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "CHLA", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("CHLA ", input$Year1, "-", input$Year1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map 6
  output$mymap6 <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Year1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail")) %>%
      filter(DU == "SWBG") %>%
      filter(!is.na(Attainment))
    
    cb1 <- CB_104_segments
    cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_104" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("SWBG ", input$Year1, "-", input$Year1 + 2, sep = ""), id = "CBSEG_104",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_104_segments) +
      tm_borders(col = "black")
  })
  
  # Map Comparison 1
  output$mymap1A <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "MSN", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("MSN ", input$Y1, "-", input$Y1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  output$mymap1B <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y2) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "MSN", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("MSN ", input$Y2, "-", input$Y2 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map Comparison 2
  output$mymap2A <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "OW", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("OW ", input$Y1, "-", input$Y1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  output$mymap2B <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y2) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "OW", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("OW ", input$Y2, "-", input$Y2 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map Comparison 3
  output$mymap3A <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "DW", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("DW ", input$Y1, "-", input$Y1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  output$mymap3B <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y2) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "DW", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("DW ", input$Y2, "-", input$Y2 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map Comparison 4
  output$mymap4A <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "DC", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("DC ", input$Y1, "-", input$Y1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  output$mymap4B <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y2) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "DC", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("DC ", input$Y2, "-", input$Y2 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map Comparison 5
  output$mymap5A <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "CHLA", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("CHLA ", input$Y1, "-", input$Y1 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  output$mymap5B <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y2) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_92_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "CHLA", ], by = c("CBSEG_92" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("CHLA ", input$Y2, "-", input$Y2 + 2, sep = ""), id = "CBSEG_92",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_92_segments) +
      tm_borders(col = "black")
  })
  
  # Map Comparison 6
  output$mymap6A <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y1) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_104_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "SWBG", ], by = c("CBSEG_104" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("SWBG ", input$Y1, "-", input$Y1 + 2, sep = ""), id = "CBSEG_104",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_104_segments) +
      tm_borders(col = "black")
  })
  
  output$mymap6B <- renderTmap({
    d1 <- d %>%
      filter(Year == input$Y2) %>%
      mutate(Attainment = ifelse(Attainment >= 1, "Pass", "Fail"))
    cb1 <- CB_104_segments
    cb1@data <- left_join(cb1@data, d1[d1$DU == "SWBG", ], by = c("CBSEG_104" = "Segment"))
    DU_attainment <- cb1
    
    tm_shape(DU_attainment) +
      tm_fill("Attainment",
              title = paste("SWBG ", input$Y2, "-", input$Y2 + 2, sep = ""), id = "CBSEG_104",
              showNA = TRUE, colorNA = "grey",
              palette = c("Pass" = "darkgreen", "Fail" = "coral2")
      ) +
      tm_layout(scale = 0.9) +
      tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
      tm_shape(CB_104_segments) +
      tm_borders(col = "black")
  })
  
  # GIF 1
  output$mygif1 <- renderImage(
    {
      list(
        src = "gif/MSN_Attainment_Indicator_Animation.gif",
        contentType = "image/gif",
        width = 600, height = 800
      )
    },
    deleteFile = FALSE
  )
  
  # GIF 2
  output$mygif2 <- renderImage(
    {
      list(
        src = "gif/OW_Attainment_Indicator_Animation.gif",
        contentType = "image/gif",
        width = 600, height = 800
      )
    },
    deleteFile = FALSE
  )
  
  # GIF 3
  output$mygif3 <- renderImage(
    {
      list(
        src = "gif/DW_Attainment_Indicator_Animation.gif",
        contentType = "image/gif",
        width = 600, height = 800
      )
    },
    deleteFile = FALSE
  )
  
  # GIF 4
  output$mygif4 <- renderImage(
    {
      list(
        src = "gif/DC_Attainment_Indicator_Animation.gif",
        contentType = "image/gif",
        width = 600, height = 800
      )
    },
    deleteFile = FALSE
  )
  
  # GIF 5
  output$mygif5 <- renderImage(
    {
      list(
        src = "gif/CHLA_Attainment_Indicator_Animation.gif",
        contentType = "image/gif",
        width = 600, height = 800
      )
    },
    deleteFile = FALSE
  )
  
  # GIF 6
  output$mygif6 <- renderImage(
    {
      list(
        src = "gif/SWBG_Attainment_Indicator_Animation.gif",
        contentType = "image/gif",
        width = 600, height = 800
      )
    },
    deleteFile = FALSE
  )
}

# Run the application
shinyApp(ui = ui, server = server)

# End
