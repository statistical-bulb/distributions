###########################################################################
# R shiny app distributions
# Sven Knüppel
# 2019-03-22
# under construction
#

library(shiny)
library(data.table)
library(DT)

col1 <- "#FFBF00" # dark color amber #FFBF00
col2 <- "#FFE5B4" # light color peach #FFE5B4


server <- function(input, output, session) {
  
  # Binomial distribution ---------------------------------------------------
  output$dist_binomial <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2)
    n <- input$binom_n
    p <- input$binom_p
    x <- 0:n
    y <- dbinom(0:n, size = n, prob = p)
    par(mar = c(5, 4, 1, 2) + 0.1)
    plot(x, y,
         type = "h",
         lwd = 4,
         col = col1,
         xlab = "k",
         ylab = "Probability"
    )
    if (input$binom_checkbox) {
      curve(dnorm(x,
                  mean = n * p,
                  sd = sqrt(n * p * (1 - p))
      ),
      add = TRUE,
      col = "forestgreen",
      lwd = 2
      )
    }
  })
  
  output$binomial_table <- DT::renderDataTable({
    n <- input$binom_n
    p <- input$binom_p
    x <- 0:n    
    binomial_data <- datatable(
      data.frame(
        k = x, 
        n = n,
        p = p,
        pmf = dbinom(0:n, size = n, prob = p),
        cdf = pbinom(0:n, size = n, prob = p)
        
      ), 
      options = list(searching = FALSE,
                     rownames = FALSE,
                     paging = FALSE
                     )
    )%>%
      formatRound(c(4:7), 6) 
  }
  )
  # Reset button
  observeEvent(input$reset_binom, {
    updateSliderInput(session, "binom_n", value = 30)
    updateSliderInput(session, "binom_p", value = 0.5)
    updateCheckboxInput(session, "binom_checkbox", value = FALSE)
  })
  
  # Normal distribution -----------------------------------------------------
  
  output$dist_normal1 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2)
    # N(mu, sigma)
    mu <- input$normal_mu
    sigma <- input$normal_sigma
    xs <- c(-4, 4, mu - 4 * sigma, mu + 4 * sigma)
    x <- seq(min(xs), max(xs), length = 200)
    y <- dnorm(x, mean = mu, sd = sigma)
    # N(0, 1)
    yn <- dnorm(x, mean = 0, sd = 1)
    # Density function
    plot(x, yn,
         type = "l",
         xlim = range(x),
         ylim = range(c(y, yn)),
         lwd = 2,
         col = col2,
         main = "Probability-density function (pdf)",
         xlab = "",
         ylab = "Density",
         font.lab = 2 
    )
    # add standard normal distribution
    lines(
      x = c(0, 0),
      y = c(0, 0.3989423),
      lwd = 2,
      col = col2,
      lty = 3
    )
    # add N(0, 1)
    lines(x, y,
          lwd = 3,
          col = col1
    )
    lines(
      x = c(mu, mu),
      y = c(0, dnorm(mu, mean = mu, sd = sigma)),
      lwd = 2,
      col = col1,
      lty = 3
    )
  })   
  
  output$dist_normal2 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2)
    # N(mu, sigma)
    mu <- input$normal_mu
    sigma <- input$normal_sigma
    xs <- c(-4, 4, mu - 4 * sigma, mu + 4 * sigma)
    x <- seq(min(xs), max(xs), length = 200)
    y <- dnorm(x, mean = mu, sd = sigma)
    # N(0, 1)
    yn <- dnorm(x, mean = 0, sd = 1)
    plot(x, pnorm(x, mean = 0, sd = 1),
         type = "l",
         lwd = 2,
         col = col2,
         main = "Cumulative density function (cdf)",
         xlab = "",
         ylab = expression(bold(paste(phi, "(x)"))),
         font.lab = 2,
         cex = 3
    )
    lines(x, pnorm(x, mean = mu, sd = sigma),
          lwd = 3,
          col = col1
    )
    lines(
      x = c(0, 0),
      y = c(0, 0.5),
      lwd = 2,
      col = col2,
      lty = 3
    )
    lines(
      x = c(mu, mu),
      y = c(0, pnorm(mu, mean = mu, sd = sigma)),
      lwd = 2,
      col = col1,
      lty = 3
    )
  })
  
  output$dist_normal_txt <- renderText({
    text <- "grey standard normal distribution"
    text
  })
  
  # Reset button normal distribution
  observeEvent(input$reset_normal, {
    updateSliderInput(session, "normal_mu", value = 0)
    updateSliderInput(session, "normal_sigma", value = 1)
  })
  
  # Normal distribution area ------------------------------------------------
  output$area_normal <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2)
    # N(mu, sigma)
    validate(
      need(!is.na(input$n_mean), 
           message = "input <Mean> should be numeric."),
      need(!is.na(input$n_sd), 
           message = "input <Standard deviation> should be numeric."),
      need(!is.na(input$lbound), 
           message = "input <Lower bound> should be numeric."),
      need(!is.na(input$ubound), 
           message = "input <Lower bound> should be numeric."),
      need(input$lbound <= input$ubound, 
           message = "input <Lower bound> should be smaller <Upper bound>.")
    )
    mu <- input$n_mean
    sigma <- input$n_sd
    lb <- input$lbound
    ub <- input$ubound
    xs <- c(lb, ub, mu - 4 * sigma, mu + 4 * sigma)
    x <- seq(min(xs), max(xs), length = 200)
    y <- dnorm(x, mean = mu, sd = sigma)
    #op <- par(mar = c(5, 4, 0.2, 2) + 0.1)
    # Density function
    plot(x, y,
         type = "n", 
         xlab = "x",
         xlim = range(x),
         main = "",
         font.lab = 2,
         cex = 3
    )
    abline(h = 0)
    # add standard normal distribution
    xs_a <- seq(lb, ub, length.out = 200)
    ys_a <- dnorm(xs_a, mean = mu, sd = sigma)
    polygon(
      x = c(lb, xs_a, ub),
      y = c(0, ys_a, 0),
      col = col2,
      border = NA
    )
    # to make the curve more visible
    lines(x, y,
          lwd = 4,
          col = col1
    )
    # par(op)
  })
  output$txt_out <- renderText({
    text <- "P(X)"
    text
  })
  # Reset button normal distribution
  observeEvent(input$reset_normal_area, {
    updateNumericInput(session, "n_mean", value = 0)
    updateNumericInput(session, "n_sd", value = 1)
    updateNumericInput(session, "lbound", value = -1)
    updateNumericInput(session, "ubound", value = 1)
  })
  
  # t distribution -----------------------------------------------------
  output$dist_t <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2)
    # t(df)
    t_df <- input$t_df
    x <- seq(-4, 4, length = 100)
    y <- dt(x, df = t_df)
    yn <- dnorm(x, mean = 0, sd = 1)
    op <- par(
      mar = c(5, 4, 0.2, 2) + 0.1,
      mfrow = c(2, 1)
    )
    # Density
    plot(x, yn,
         type = "l", xlab = "x",
         xlim = range(x),
         ylim = range(c(y, yn)),
         lwd = 1,
         col = col2,
         ylab = "Density",
         main = "",
         font.lab = 2 
    )
    # add t(df)
    lines(x, y,
          lwd = 1,
          col = col1
    )
    # Distribution function
    plot(x, pnorm(x, mean = 0, sd = 1),
         type = "l",
         lwd = 1,
         col = col2,
         font.lab = 2 
    )
    lines(x, pt(x, df = t_df),
          lwd = 1,
          col = col1
    )
    par(op)
  })
  # Reset button normal distribution
  observeEvent(input$reset_t, {
    updateSliderInput(session, "t_df", value = 1)
  })
}
