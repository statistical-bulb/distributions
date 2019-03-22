###########################################################################
# R shiny app distributions
# Sven Knüppel
# 2019-03-22
# under construction
#

library(shiny)

ui = fluidPage(
  tabsetPanel(
    tabPanel("Binomial", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "binom_n",
                             label = "Size",
                             min = 1,
                             max = 300,
                             value = 30),
                 sliderInput(inputId = "binom_p",
                             label = "Probability",
                             min = 0,
                             max = 1,
                             value = 0.5),
                 checkboxInput(inputId = "binom_checkbox",
                               label = strong("Show normal approximation (red)"),
                               value = FALSE),
                 br(),
                 br(),
                 actionButton("reset_binom", "Reset")
               ),
               mainPanel(
                 plotOutput(outputId = "dist_binomial")
               )
             )
    ),
    tabPanel("Normal", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "normal_mu",
                             label = "Mean",
                             min = -30,
                             max = 30,
                             value = 0),
                 sliderInput(inputId = "normal_sigma",
                             label = "Standard deviation",
                             min = 0.1,
                             max = 10,
                             value = 1,
                             step = 0.01),
                 br(),
                 br(),
                 actionButton("reset_normal", "Reset")
               ),
               mainPanel(
                 plotOutput(outputId = "dist_normal")
               )
             )
    ),
    tabPanel("Normal (area)", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 numericInput("n_mean", "Mean", "0",
                              step = 0.1),
                 numericInput("n_sd", "Standard deviation", "1", min = 0.01,
                              step = 0.01),
                 
                 selectInput("n_area", "Area", 
                             choices = list("middle" = 1, "lower tail" = 2,
                                            "upper tail" = 3), selected = 1),
                 br(), br(), br(),
                 numericInput("lbound", "Lower bound", "-1",
                              step = 0.1),
                 numericInput("ubound", "Upper bound", "1",
                              step = 0.1),
                 br(),
                 br(),
                 actionButton("reset_normal_area", "Reset")
               ),
               mainPanel(
                 plotOutput(outputId = "area_normal"),
                 div(textOutput("txt_out"), 
                     align = "center", 
                     style = "font-size:200%;")
               )
             )
    ),
    tabPanel("t", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "t_df",
                             label = "Degree of freedom (df)",
                             min = 1,
                             max = 100,
                             value = 1),
                 br(),
                 br(),
                 actionButton("reset_t", "Reset")
               ),
               mainPanel(
                 plotOutput(outputId = "dist_t")
               )
             )
    )
  )
)

