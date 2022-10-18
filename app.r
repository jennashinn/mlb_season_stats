library(bslib)
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)
library(thematic)
library(tidyr)
library(tidyverse)
library(rsconnect)

# First Tab
batting <- read_csv('hit_df.csv')
batting %>% count(batter_name) -> b
batter_list <- filter(b, n >= 40) %>%
  select(batter_name) %>%
  pluck()

# Second Tab
hr_df <- read_csv('hit_df.csv') 
hr_df %>% group_by(batter_name) %>% 
  summarize(HR = sum(HR)) -> s
hr_list <- filter(s, HR >= 30) %>% 
  select(batter_name) %>% 
  pluck()

# Third Tab
pitch_count <- read_csv('pitch_df.csv') %>%
  rename(pitch = pitch_type)
pitch_count %>% count(pitcher_name) -> pc
pitcher_list <- filter(pc, n >= 790) %>%
  select(pitcher_name) %>%
  pluck()


thematic_shiny()

ui <- fluidPage(

    theme = shinythemes :: shinytheme("slate"),
    tabsetPanel(
      type = "pills",
 ###### INDIVIDUAL BATTING STATS TAB #######
      tabPanel(
        "Individual Stats",
        titlePanel("Individual Batting Statistics from the 2021 Season"),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId= "battername", label = "Batter Name:", 
                        selected = "Freddie Freeman", 
                        choices = batter_list),
            radioButtons("measure", "Measure:", c("Launch Speed", "Hits", 
                                                  "Home Runs", "Expected BA", 
                                                  "Expected wOBA"), inline = FALSE),
            fluidRow(h5("Highlight a portion of the plot to see the statistics for that area")),
            width = 3),
          mainPanel(
            plotOutput("batting_plot", brush = brushOpts("plot_brush", fill = "#0000ff"),
                       click = "plot_click"),
            tableOutput("batting_table"), 
            width = 9)
        )
      ), 
###### HOME RUN COMPARISON TAB #####
      tabPanel(
        "HR Comparison",
        titlePanel("Strike Zone Locations of 2021 Home Runs"),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId="player1", label="Choose First Player:", 
                        selected = "Salvador Perez",
                        choices = hr_list),
            selectInput(inputId="player2", label="Choose Second Player:", 
                        selected = "Vladimir Guerrero Jr.",
                        choices = hr_list),
            radioButtons(inputId="ptype", label="Choose \n Pitch Type",
                         choices = c("All" = "all", "Fastballs" = "fb", "Off-Speed" = "os")),
            width = 3),
          mainPanel(
            plotOutput("hrPlot", width = "100%"), 
            width = 9)
        )
      ),
####### PITCH COUNT TAB ############
      tabPanel("Pitch Count",
          titlePanel("Pitch Type by Count and Outs from 2021"), 
          sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "pitchername", label = 'Select a Pitcher:', 
                        selected = 'Corbin Burnes',  
                        choices = pitcher_list),
            tableOutput("pitch_count_table"),
            checkboxGroupInput("pitch", "Pitch Type:",
                               c( "FC", "FS", "FF", "SI",
                                  "CH", "CU", "SL", "KC"),
                               selected = c("FC", "CU"),
                               inline = TRUE),
            checkboxGroupInput("count", "Count:",
                               c("0-0", "0-1", "0-2", 
                                 "1-0", "1-1", "1-2", 
                                 "2-0", "2-1", "2-2", 
                                 "3-0", "3-1", "3-2"),
                               selected = c("0-0", "0-1"),
                               inline = TRUE), 
            radioButtons(inputId="outs_considered", label="Consider Number of Outs?",
                         choices = c("No" = "no", "Yes" = "yes")),
            sliderInput("out", "Number of Outs:", 
                        min = 0,
                        max = 2,
                        value = 0), 
            width = 3),
          mainPanel(
            plotOutput("pitch_count_plot"),
            width = 9)
        )
      )
    )
)

server <- function(input, output, session) {

##### INDIVIDUAL BATTING STATS TAB #########

  output$batting_plot <- renderPlot({
    add_zone <- function(){
      topKzone <- 3.5
      botKzone <- 1.6
      inKzone <- -0.85
      outKzone <- 0.85
      kZone <- data.frame(
        x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
        y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
      )
      geom_path(aes(.data$x, .data$y),
                data=kZone, lwd = 0.75)
    }
    centertitle <- function(){
      theme(plot.title = element_text(size = 16, hjust = 0.5, vjust = 0.8, angle = 0))
    }
    if(input$measure == "Launch Speed"){
      ggplot() +
        geom_point(
          data = filter(batting, batter_name %in% input$battername),
          aes(plate_x, plate_z, color = launch_speed), 
          size = 0.75) +
        add_zone() +
        xlab("Plate X") + ylab("Plate Y") + 
        xlim(-2.5, 2.5) + ylim(0, 4.5) + 
        centertitle() + 
        coord_fixed() +
        scale_color_distiller(palette = "RdPu", name = "Launch Speed")
    }
    else if(input$measure == "Hits"){
      batting$hit <- as.character(batting$H)
      ggplot() +
        geom_point(
          data = filter(batting, batter_name %in% input$battername),
          aes(plate_x, plate_z, color = hit), 
          size =  0.75, alpha = 0.75) +
        add_zone() +
        xlab("Plate X") + ylab("Plate Y") + 
        xlim(-2.5, 2.5) + ylim(0, 4.5) + 
        scale_color_hue(name = "Hits") +
        centertitle() + 
        coord_equal()
      
    } else if(input$measure == "Home Runs"){
      batting$HR <- as.character(batting$HR)
      ggplot() +
        geom_point(data = filter(batting, batter_name %in% input$battername),
                   aes(plate_x, plate_z, color = HR), 
                   size = 0.75, alpha = 0.75) +
        add_zone() +
        xlab("Plate X") + ylab("Plate Y") + 
        xlim(-2.5, 2.5) + ylim(0, 4.5) + 
        scale_color_hue(name = "Home Runs") +
        centertitle() + 
        coord_equal()
      
    } else if(input$measure == "Expected BA"){
      ggplot() +
        geom_point(data = filter(batting, batter_name %in% input$battername),
                   aes(plate_x, plate_z,
                       color = xBA),
                   size = 0.75) +
        add_zone() +
        xlab("Plate X") + ylab("Plate Y") + 
        xlim(-2.5, 2.5) + ylim(0, 4.5) + 
        centertitle() +
        coord_equal() +
        scale_color_distiller(palette = "RdPu", name = "xBA")
      
    } else if(input$measure == "Expected wOBA"){
      ggplot() +
        geom_point(data = filter(batting, batter_name %in% input$battername),
                   aes(plate_x, plate_z,
                       color = xwOBA), 
                   size = 0.75) +
        add_zone() +
        xlab("Plate X") + ylab("Plate Y") +       
        xlim(-2.5, 2.5) + ylim(0, 4.5) + 
        centertitle() +
        coord_equal() +
        scale_color_distiller(palette = "RdPu", name = "xwOBA")
    }
  }, res = 96)
  

  output$batting_table <- renderTable({
    req(input$plot_brush)
    sc1 <- brushedPoints(filter(batting,
                                batter_name %in% input$battername),
                         input$plot_brush)
    data.frame(BIP = nrow(sc1),
               H = sum(sc1$H),
               HR = sum(sc1$HR),
               LS = mean(sc1$launch_speed),
               H_Rate = sum(sc1$H) / nrow(sc1),
               HR_Rate = sum(sc1$HR) / nrow(sc1),
               xBA = mean(sc1$xBA), 
               xwOBA = mean(sc1$xwOBA))
    
  }, digits = 3, width = '75%', align = 'c',
  bordered = TRUE,
  caption = "Brushed Region Stats",
  caption.placement = "top")


##### HOMERUN COMPARISON TAB ######
  output$hrPlot <- renderPlot({
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.95
    outKzone <- 0.95
    kZone <- data.frame(
      x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
      y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
    hr_new <- filter(hr_df, 
                     batter_name %in% c(input$player1, input$player2), HR == 1)
    if(input$ptype == "fb") {hr_new <- filter(hr_new, ptype == "fb")} 
    if(input$ptype == "os") {hr_new <- filter(hr_new, ptype == "os")}
    ggplot(kZone, aes(x, y)) +
      geom_point(data=hr_new,
                 aes(x=plate_x, y=plate_z, color = pitch_type),
                 size = 1.5, alpha = 0.75) +
      geom_path(lwd=1, alpha = 0.75) +
      coord_fixed() +
      scale_color_hue(name = "Pitch Type") +
      xlim(-2.5, 2.5) + ylim(0, 4.5) + 
      xlab("Plate X") + ylab("Plate Y") + 
      facet_wrap(~ batter_name, ncol = 2) + 
      theme(strip.text = element_text(size = 20))
  })


  ##### PITCH COUNT TAB #####
  output$pitch_count_table <- renderTable({
    nice_table <- function(d){
      d %>%
        group_by(pitch) %>%
        summarize(N = n()) %>%
        filter(is.na(pitch) == FALSE) %>%
        pivot_wider(
          names_from = pitch,
          values_from = N
        )
    }
    nice_table(filter(pitch_count,
                      pitcher_name %in% input$pitchername))
  })

  output$pitch_count_plot <- renderPlot({
    add_zone <- function(){
      topKzone <- 3.5
      botKzone <- 1.6
      inKzone <- -0.85
      outKzone <- 0.85
      kZone <- data.frame(
        x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
        y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
      )
      geom_path(aes(.data$x, .data$y),
                data=kZone, lwd = 0.5)
    }
    centertitle <- function(){
      theme(plot.title = element_text(size = 16, hjust = 0.5,
                                       vjust = 0.8, angle = 0))
    }

    
    pitch_count_new <- filter(pitch_count,
                              count %in% input$count,
                              pitch %in% input$pitch,
                              pitcher_name %in% input$pitchername)
    if(input$outs_considered == "yes") {pitch_count_new <- filter(pitch_count_new, 
                                                                  outs_when_up %in% input$out)}

    ggplot() +
      geom_point(data = pitch_count_new,
                 aes(plate_x, plate_z),
                 size = 0.75, alpha = 0.75,
                 color = "skyblue") +
      add_zone() +
      # centertitle() +
      coord_equal() +
      xlab("Plate X") + ylab("Plate Y") +
      xlim(-2.5, 2.5) + ylim(0, 4.5) + 
      facet_grid(count ~ pitch) +
      theme(strip.text = element_text(size = 10))
  }, res = 96)
}

thematic_shiny()
shinyApp(ui, server)
