library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(magrittr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(viridis)  
library(RColorBrewer)
library(data.table)
library(summarytools)
library(parameters)
library(DT)
library(descriptr)
library(dashboardthemes)
library(rgdal)
library(ggplot2)
library(dplyr)
library(sf)
library(raster)
library(ggpubr)
library(maps)
library(rnaturalearth)
library(rgeos)
library(ggmap)

#importing data

pop <- read.csv("final.csv")
abortion <- read.csv("per_year_birth_abortion.csv")
reasons <- read.csv("reasons_2020.csv")
gender_abortion <- read.csv("before_week_12_2020.csv")
my_df <- read.csv('Abort_reasons_2020_Copy.csv')
arm <- st_read('Armenia_Marzes', quiet = T)


age_group = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+") 
years = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
marz = c("Armavir", "Ararat", "Aragatsotn", "Kotayk", "Gegharkunik", "Tavush", "Lori", "Shirak", "Vayots Dzor", "Syunik", "Yerevan" )
reason = c("by_doctor", "choice", "other_reason", "total")
colnames_ = c('region',"by_doctor",'choice', 'total','before_week_12', "week_12_24")
map_reason = c("By doctor's appointment", "By choice", "Another reason", "Total abortions")


year_range <- range(pop$Year)

birth_abortion = na.omit(abortion)


new_df = merge(reasons, gender_abortion, by=1)%>%
  subset(select = -c(year, total, other_reason))%>%
  slice(1:(n()-1))
colnames(new_df) <- c('region',"by_doctor",'choice', 'before_week_12', "week_12_24")

cols_scatter_x <- c("by_doctor", 'before_week_12')
cols_scatter_y <- c('choice', "week_12_24")

names_scatter_x <- c("By doctor's appointment", 'Before week 12')
names_scatter_y <- c("By choice", "Week 12-24")


arm_new <- arm %>%
  left_join(my_df[, c('region', 'by_doctor', 'choice', 'other_reason', 'total', "Group")], 
            by = c('Name_Eng' = 'region'))


no_yerevan <- arm_new[-6, ]



# Define UI for application
header = dashboardHeader(
  title = "Abortion"
)


sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("About",
             tabName = 'about'),
    menuItem("Visualization",
             tabName = 'vis')
  )
)

body = dashboardBody(
  shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  tabItems(
    tabItem(tabName = 'about',
            imageOutput('image'),
            textOutput("About0"),
            textOutput("About1"),
            textOutput("About2"),
            textOutput("About3"),
            textOutput("About4"),
            textOutput("About5"),
            textOutput("About6"),
            textOutput("About7"),
            textOutput("About8"),
            textOutput("About9"),
            
            textOutput("Authors"),
            tags$head(tags$style("#About0{color: black;
                                 font-size: 45px;
                                 font-style: bold;
                                 margin-top: -380px;
                                 margin-left: 23px;
                                 }")),
            
            tags$head(tags$style("#About1{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#About2{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#About3{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#About4{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#About5{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 height: 50px;
                                 }")),
            tags$head(tags$style("#About6{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#About7{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#About8{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),
            tags$head(tags$style("#About9{color: black;
                                 font-size: 18px;
                                 font-style: oblique;
                                 margin-left: 20px;
                                 }")),

            tags$head(tags$style("#Authors{color: darkblue;
                                 font-size: 30px;
                                 font-style: italic;
                                 margin-top: 200px;
                                 margin-left: 20px;
                                 }"))
    ),
    tabItem(tabName = 'vis',
            tabsetPanel(
              tabPanel("Boxplot",
                       sidebarLayout(
                         sidebarPanel(
                           radioGroupButtons(
                             inputId = "age",
                             label = "Age group",
                             choiceNames = age_group,
                             choiceValues = unique(pop$Age)
                           ),
                           sliderInput(
                              "year",
                             label = "Year", 
                             step = 1,
                             min = year_range[1], 
                             max = year_range[2], 
                             value = year_range
                          
                           ), 
                           prettyRadioButtons(
                             inputId = "yerevan",
                             label = "Add Yerevan?", 
                             choices = c("Yes", "No"),
                             icon = icon("check"),
                             animation = "jelly"
                           )
                           ),
                         

                         
                         mainPanel(plotOutput('boxplot'))
                       )
                  ),
              tabPanel("Barplot",
                       sidebarLayout(
                         sidebarPanel(
                           radioGroupButtons(
                             inputId = "marz",
                             label = "Marz",
                             choiceNames = marz,
                             choiceValues = unique(pop$region)
                           ),
                           
                           prettyRadioButtons(
                             inputId = "barplot_position_type",
                             label = "Position", 
                             choices = c("Dodge", "Stack", "Fill"),
                             icon = icon("check"),
                             animation = "jelly"
                           )
                         ),
                         mainPanel(plotOutput('barplot'))
                       )
              ),
              
              
                    tabPanel("Maps",
                             sidebarLayout(
                               sidebarPanel(
                                 radioGroupButtons(
                                   inputId = "map_reason",
                                   label = "Reason",
                                   choiceNames = map_reason,
                                   choiceValues = map_reason
                                 ),
                                 
                                 ),
                                   
                                   mainPanel(plotOutput('map'))
                                 )
                        ),
                      
              tabPanel("Abortion Barplot",
                       sidebarLayout(
                         sidebarPanel(
                           
                           prettyRadioButtons(
                             inputId = "barplot_type",
                             label = " ", 
                             choices = c("Birth", "Abortion"),
                             icon = icon("check"),
                             animation = "jelly"
                           )
                         ),
                         mainPanel(plotOutput('Abortion'))
                       )
              ),
              tabPanel("Scatterplot",
                       sidebarLayout(
                         sidebarPanel(
                           radioGroupButtons(
                             inputId = "x",
                             label = "X axis",
                             choiceNames = names_scatter_x,
                             choiceValues = cols_scatter_x
                           ),
                           
                           radioGroupButtons(
                             inputId = "y",
                             label = "Y axis",
                             choiceNames = names_scatter_y,
                             choiceValues = cols_scatter_y
                           ),
                         ),
                         mainPanel(plotOutput('scatter'))
                       )
              ),
              ))))



ui <- dashboardPage(header, sidebar, body)



server <- function(input, output){
  output$About0 <- renderText({
    
    " Sex-selective Abortion"})
  
  output$About1 <- renderText({
    
    "A procedure that ends a pregnancy is a common health intervention. According to the World Health Organization (WHO), " 
  })
  
  output$About2 = renderText({
    "every year, the number of abortions is estimated to be 40-50 million, which means about 125,000 abortions "
  })
  
  output$About3 = renderText({
    " happen in one day. The topic of abortion is controversial worldwide, emerging a conflict between people" 
  })
  
  output$About4 = renderText({
    "about whether it should be illegal or not. Armenia is not an exception. The abortion rate in 2014 was estimated"
  })
  
  output$About5 = renderText({
    " at 21.77% of all pregnancies. The Armenian law on abortion allows one to terminate a pregnancy in the first trimester."
  })
  
  output$About6 = renderText({
    " Termination can also be done before twelve weeks based on a physician's determination and the woman's consent. "
  })
  
  output$About7 = renderText({
    " In all other cases, abortions are not allowed, including gender-based termination. However, it is said that"
  })
  
  output$About8 = renderText({
    " sex-selective abortions remain a problem in Armenia, giving a preference to a boy. By observing the data, we can understand"
  })
  
  output$About9 = renderText({
    "  and gain insight into some worrying questions and see where we stand."
  })
  


  output$Authors = renderText({
    "By Astghik Kostanyan,  Anna Misakyan,  Anahit Navoyan"
  })
  output$image = renderImage(
    list(
      src = "abortion.jpg",
      contentType = "image/jpg",
      width = "100%", height = "630px"
    ))
  
  
  output$boxplot <- renderPlot({
    
    if (input$yerevan == 'Yes') {
      
      pop%>%
        filter(pop$Age == input$age)%>%
        filter(Year > input$year[1], Year < input$year[2])%>%
        ggplot(aes(x = region, y = population, fill = gender)) +
        geom_boxplot() + labs(x = "", y = "population", 
                              title = paste0("Distribution of ", input$age,
                                             " age group in ", input$year[1], 
                                             " - ", input$year[2]), 
                              fill = "Gender") +
        scale_fill_manual(values = c("#FC766AFF", "#5B84B1FF")) + theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16)
        )
      
    } else if (input$yerevan == 'No') {
      pop %>% 
        filter(region != "Yerevan")%>%
        filter(Age == input$age)%>%
        filter(Year > input$year[1], Year < input$year[2])%>%
        ggplot(aes(x = region, y = population, fill = gender)) +
        geom_boxplot() + labs(x = "", y = "population",
                              title = paste0("Distribution of ", input$age,
                                             " age group in ",  input$year[1], 
                                             " - ", input$year[2],
                                             " Yerevan excluded"), 
                              fill = "Gender") +
        scale_fill_manual(values = c("#FC766AFF", "#5B84B1FF")) + theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16)
        )
    }
    
  })

  output$barplot <- renderPlot({
    df_cutted_barplot = pop %>%
      filter(pop$region == input$marz)
    
    if (input$barplot_position_type == 'Dodge') {
      
      ggplot(df_cutted_barplot, aes(x = df_cutted_barplot$Year, y = df_cutted_barplot$population, 
                                    fill = gender)) + 
        geom_col(position = 'dodge') +
        theme_bw() + labs(x = '', y = 'population', 
                          title = paste0("Male and Female Population of ", input$marz , " from 2012-2020"), fill = "Gender") +
        scale_fill_manual(values = c("#FC766AFF", "#5B84B1FF")) + scale_x_continuous(breaks = seq(2012, 2022, 1)) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16)
        )
      
    } else if (input$barplot_position_type == "Stack") {
      ggplot(df_cutted_barplot, aes(x = df_cutted_barplot$Year, y = df_cutted_barplot$population, 
                                    fill = gender)) + 
        geom_col( position = 'stack') +
        labs(x = '', y = 'population', title = paste0("Stacked Male and Female Population of ", input$marz, " from 2012-2020"), fill = "Gender") +
        scale_fill_manual(values = c("#FC766AFF", "#5B84B1FF")) +
        scale_x_continuous(breaks = seq(2012, 2022, 1)) +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5, size = 16))
      
      
    }
    else if (input$barplot_position_type == "Fill"){
      ggplot(df_cutted_barplot, aes(x = df_cutted_barplot$Year, y = df_cutted_barplot$population, 
                                    fill = gender)) + 
        geom_col(position = 'fill') +
        labs(x = '', y = 'population', title = paste0("Percentage Population of ", input$marz, " from 2012-2020"), fill = "Gender") +
        scale_fill_manual(values = c("#FC766AFF", "#5B84B1FF")) +
        scale_x_continuous(breaks = seq(2012, 2022, 1)) +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5, size = 16))
      }
    
  })
  
  
  output$Abortion <- renderPlot({
    
    if (input$barplot_type == 'Birth') {
      
      ggplot(birth_abortion, aes(x = birth_abortion$year, y = birth_abortion$birth )) + 
        geom_col(position = 'dodge') +
        theme_bw() +
        scale_x_continuous(breaks = seq(2000, 2020, 1)) +
        ylim(0, 50000) + labs(x = "", y = "births", title = "Births per year") +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16)
        )
      
    } else if (input$barplot_type == "Abortion") {
      ggplot(birth_abortion,  aes(x = birth_abortion$year, y = birth_abortion$abortion 
      )) + 
        geom_col( position = 'dodge') +
        ylim(0, 50000) + 
        scale_x_continuous(breaks = seq(2000, 2020, 1)) +
        labs(x = '', y = "abortions", title = "Abortions per year") +
        theme_bw() +
        theme(plot.title = element_text(hjust =0.5, size = 16))
      
    }
    
  })
  
  
  output$scatter <- renderPlot({
    if (input$x == cols_scatter_x[1]){
    x_value = names_scatter_x[1]
    }
    else if (input$x == cols_scatter_x[2]){
      x_value = names_scatter_x[2]
    }
    if (input$y == cols_scatter_y[1]){
      y_value = names_scatter_y[1]
    }
    else if (input$y == cols_scatter_y[2]){
      y_value = names_scatter_y[2]
    }
        
        
    ggplot(new_df, aes(x = new_df[, input$x], y = new_df[, input$y])) + 
       geom_text(aes(label = new_df$region), check_overlap = T)+
      labs(title = paste0("Abortion in all regions depending on time and reason"), 
           x= x_value, y = y_value) + theme_light() + theme(
             plot.title = element_text(hjust = 0.5, size = 16)
           )
  
  })
  
  
  output$map <- renderPlot({
    
    
    if (input$map_reason == "By doctor's appointment") {
      ggplot(data = no_yerevan, aes(fill = by_doctor)) + geom_sf() + 
        labs(title = "Abortion by doctor's appointment", fill = '') + theme_light() +
        theme(
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
        scale_fill_viridis_c()
    } else if (input$map_reason == "By choice"){
      ggplot(data = no_yerevan, aes(fill = choice)) + geom_sf() + 
        labs(title = 'Abortion by Choice', fill = '') + theme_light() +
        theme(
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
        scale_fill_viridis_c()
      
    }
    else if (input$map_reason == "Another reason"){
      ggplot(data = no_yerevan, aes(fill = other_reason)) + geom_sf() + 
        labs(title = 'Abortion for Another reason', fill = '') + theme_light() +
        theme(
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
        scale_fill_viridis_c()
      
    }
    else if (input$map_reason == "Total abortions"){
      ggplot(data = no_yerevan, aes(fill = total)) + geom_sf() + 
        labs(title = "Total abortions", fill = '') + theme_light() +
        theme(
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
        scale_fill_viridis_c()
         }
  
    
  })

  
  
  
}

shinyApp(ui = ui, server = server)

 
