###########################################################################
# R shiny app distributions
# Sven Knüppel
# 2019-03-25
# under construction
#

library(shiny)

ui = fluidPage(
  tabsetPanel(
    tabPanel("Binomial", fluid = TRUE,
             titlePanel("Binomial distribution B(n, p)"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "binom_n",
                             label = "Size n",
                             min = 1,
                             max = 100,
                             value = 30),
                 sliderInput(inputId = "binom_p",
                             label = "Probability p",
                             min = 0,
                             max = 1,
                             value = 0.5),
                 checkboxInput(inputId = "binom_checkbox_normal",
                               label = strong("Show normal approximation (green)"),
                               value = FALSE),
                 checkboxInput(inputId = "binom_checkbox_table",
                               label = strong("Table of Binomial Distribution"),
                               value = FALSE),
                 br(),
                 br(),
                 actionButton("reset_binom", "Reset"),
                 br(),
                 br(),
                 helpText(a(href = "https://github.com/statistical-bulb/distributions", 
                            target = "_blank",
                            "Find code"))
               ),
               mainPanel(
                 fluidRow(
                   verticalLayout( 
                     plotOutput(outputId = "dist_binomial"),
                     conditionalPanel(
                       condition = ("input.binom_checkbox_table == 1"),
                       div(htmlOutput("binomial_table_txt"), 
                           style = "font-size:125%;"),
                       p("The numbers in the table represent the number of 
                         successes x out of n, probability p of success, pmf 
                         for probability mass function P(X=x) (binomial 
                         probabilities) and cdf for cumulative density 
                         function P(X <= x)."),
                       DT::dataTableOutput("binomial_table")
                     )
                   )
                 )
               )
             )
    ),
    # Binomial (area) -----------------------------------------------------------
    tabPanel("Binomial (area)", fluid = TRUE,
             titlePanel("Calculation of the area using the Binomial 
                        model"),
             sidebarLayout(
               sidebarPanel(
                 numericInput("b_size_n", "Size n", "15",
                              step = 1),
                 numericInput("b_p", "Probability p", "0.5", min = 0.01,
                              step = 0.1),
                 selectInput("b_area", "Area", 
                             choices = list("middle" = 1, 
                                            "lower tail" = 2,
                                            "upper tail" = 3,
                                            "both tails" = 4), selected = 1),
                 br(), br(), br(),
                 conditionalPanel(
                   condition = ("input.b_area == 1 | 
                                input.b_area == 3 |
                                input.b_area == 4"),
                   numericInput("lbound_b", "Lower bound", "5",
                                step = 1)),
                 
                 conditionalPanel(
                   condition = ("input.b_area == 1 | 
                                input.b_area == 2 |
                                input.b_area == 4"),
                   numericInput("ubound_b", "Upper bound", "10",
                                step = 1)),
                 br(),
                 br(),
                 actionButton("reset_b_area", "Reset"),
                 br(),
                 br(),
                 helpText(a(href = "https://github.com/statistical-bulb/distributions", 
                            target = "_blank",
                            "Find code"))
                 ),
               mainPanel(
                 div(htmlOutput("txt_out_b"), 
                     align = "center", 
                     style = "font-size:125%;"),
                 plotOutput(outputId = "area_b")
               )
               )
             ),
    # Normal distribution -------------------------------------------------
    tabPanel("Normal", fluid = TRUE,
             titlePanel(
               div(HTML("Normal distribution N(mu, sigma<sup>2</sup>)"))),
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
                             value = 2,
                             step = 0.01),
                 br(),
                 br(),
                 actionButton("reset_normal", "Reset"),
                 br(),
                 br(),
                 helpText(a(href = "https://github.com/statistical-bulb/distributions", 
                            target = "_blank",
                            "Find code"))
               ),
               mainPanel(fluidRow(
                 verticalLayout( 
                   div(htmlOutput("dist_normal_txt"), 
                       align = "center", 
                       style = "font-size:115%;"),
                   plotOutput("dist_normal1"), 
                   plotOutput("dist_normal2")
                 )
               )
               )
             )
    ),
    # Normal (area) -----------------------------------------------------------
    tabPanel("Normal (area)", fluid = TRUE,
             titlePanel("Calculation of the area under the normal 
                        distribution curve"),
             sidebarLayout(
               sidebarPanel(
                 numericInput("n_mean", "Mean", "0",
                              step = 0.1),
                 numericInput("n_sd", "Standard deviation", "1", min = 0.01,
                              step = 0.01),
                 selectInput("n_area", "Area", 
                             choices = list("middle" = 1, 
                                            "lower tail" = 2,
                                            "upper tail" = 3,
                                            "both tails" = 4), selected = 1),
                 br(), br(), br(),
                 conditionalPanel(
                   condition = ("input.n_area == 1 | 
                                input.n_area == 3 |
                                input.n_area == 4"),
                   numericInput("lbound", "Lower bound", "-1",
                                step = 0.1)),
                 
                 conditionalPanel(
                   condition = ("input.n_area == 1 | 
                                input.n_area == 2 |
                                input.n_area == 4"),
                   numericInput("ubound", "Upper bound", "1",
                                step = 0.1)),
                 br(),
                 br(),
                 actionButton("reset_normal_area", "Reset"),
                 br(),
                 br(),
                 helpText(a(href = "https://github.com/statistical-bulb/distributions", 
                            target = "_blank",
                            "Find code"))
               ),
               mainPanel(
                 div(htmlOutput("txt_out"), 
                     align = "center", 
                     style = "font-size:125%;"),
                 plotOutput(outputId = "area_normal")
               )
             )
    ),
    # t distribution ------------------------------------------------------
    tabPanel("t", fluid = TRUE,
             titlePanel(
               div(HTML("t distribution t<sub>df</sub>"))),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "t_df",
                             label = "Degree of freedom (df)",
                             min = 1,
                             max = 150,
                             value = 1),
                 br(),
                 checkboxInput(inputId = "t_checkbox_table",
                               label = strong("Table of t Distribution"),
                               value = FALSE),
                 br(),
                 br(),
                 br(),
                 actionButton("reset_t", "Reset"),
                 br(),
                 br(),
                 helpText(a(href = "https://github.com/statistical-bulb/distributions", 
                            target = "_blank",
                            "Find code"))
               ),
               mainPanel(fluidRow(
                 verticalLayout( 
                   div(htmlOutput("dist_t_txt"), 
                       align = "center", 
                       style = "font-size:115%;"),
                   plotOutput("dist_t1"), 
                   plotOutput("dist_t2"),
                   conditionalPanel(
                     condition = ("input.t_checkbox_table == 1"),
                     div(htmlOutput("t_table_txt"), 
                         style = "font-size:125%;"),
                     p("The numbers in the table represent value x, 
                         degree of freedom df,
                         pdf for t distribution,
                         cdf for t distribution, and
                         pdf_snormal and cdf_snormal for standard normal."),
                     DT::dataTableOutput("t_table")
                   )
                 )
               )
               )
             )
    ),
    # t (area) -----------------------------------------------------------
    tabPanel("t (area)", fluid = TRUE,
             titlePanel("Calculation of the area under the t 
                        distribution curve"),
             sidebarLayout(
               sidebarPanel(
                 numericInput("t_df2", "Degree of freedom (df)", "1",
                              min = 1,
                              step = 1),
                 selectInput("t_area", "Area", 
                             choices = list("middle" = 1, 
                                            "lower tail" = 2,
                                            "upper tail" = 3,
                                            "both tails" = 4), selected = 1),
                 br(), br(), br(),
                 conditionalPanel(
                   condition = ("input.t_area == 1 | 
                                input.t_area == 3 |
                                input.t_area == 4"),
                   numericInput("lbound_t", "Lower bound", "-1",
                                step = 0.1)),
                 
                 conditionalPanel(
                   condition = ("input.t_area == 1 | 
                                input.t_area == 2 |
                                input.t_area == 4"),
                   numericInput("ubound_t", "Upper bound", "1",
                                step = 0.1)),
                 br(),
                 br(),
                 actionButton("reset_t_area", "Reset"),
                 br(),
                 br(),
                 helpText(a(href = "https://github.com/statistical-bulb/distributions", 
                            target = "_blank",
                            "Find code"))
               ),
               mainPanel(
                 div(htmlOutput("txt_out_t"), 
                     align = "center", 
                     style = "font-size:125%;"),
                 plotOutput(outputId = "area_t")
               )
             )
    )
  )
)
