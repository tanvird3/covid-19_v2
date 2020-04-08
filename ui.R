library(shiny)
shinyUI(fluidPage (
  theme = shinytheme("superhero"),
  headerPanel("COVID-19 Data Visualizer"),
  sidebarPanel(
    width = 2,
    selectInput(
      "countries",
      label = "Select Countries",
      choices =
        country_list,
      selected = c(country_list[which(country_list == "India")], country_list[which(country_list ==
                                                                                      "Pakistan")], country_list[which(country_list == "Malaysia")]),
      multiple = T
    ),
    submitButton(text = "View")
  ),
  mainPanel (h1(""),
             tabsetPanel(
               tabPanel(
                 "Global Status",
                 plotlyOutput("figG"),
                 br(),
                 plotlyOutput("global_time"),
                 br(),
                 plotlyOutput("global_cfr"),
                 br(),
                 plotlyOutput("global_p"),
                 br(),
                 plotlyOutput("global_recov_dead")
               ),
               tabPanel(
                 "Comparative Charts",
                 plotlyOutput("fig_confirm"),
                 br(),
                 plotlyOutput("fig_dead"),
                 br(),
                 plotlyOutput("fig_recov")
               ),
               tabPanel(
                 "Ratio Analysis",
                 plotlyOutput("fig_confirm_S"),
                 br(),
                 plotlyOutput("fig_confirm_D"),
                 br(),
                 plotlyOutput("fig_Ratio"),
                 br(),
                 plotlyOutput("fig_cfr_print")
               )
             ))
))
