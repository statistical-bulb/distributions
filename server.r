###########################################################################
# R shiny app distributions
# Sven Knüppel
# 2019-03-25
# under construction
#

library(shiny)
library(DT)

col1 <- "#FFBF00" # dark color amber #FFBF00
col2 <- "#FFE5B4" # light color peach #FFE5B4

server <- function(input, output, session) {
  # Binomial distribution ---------------------------------------------------
  output$dist_binomial <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    n <- input$binom_n
    p <- input$binom_p
    x <- 0:n
    y <- dbinom(0:n, size = n, prob = p)
    par(mar = c(5, 4, 1, 2) + 0.1)
    plot(x, y,
         type = "h",
         lwd = 4,
         col = col1,
         xlab = "",
         ylab = "Binomial probability"
    )
    if (input$binom_checkbox_normal) {
      # De Moivre-Laplace theorem
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
  output$binomial_table_txt <- renderText({
    n <- input$binom_n
    p <- input$binom_p
    paste0(
      "<b>Binomial distribution B(",n, "," , p, ")</b>")
  })
  output$binomial_table <- DT::renderDataTable({
    n <- input$binom_n
    p <- input$binom_p
    x <- 0:n    
    binomial_data <- datatable(
      data.frame(
        x = x, 
        n = n,
        p = p,
        pmf = dbinom(0:n, size = n, prob = p),
        cdf = pbinom(0:n, size = n, prob = p)
        
      ), 
      options = list(searching = FALSE,
                     paging = FALSE
      ), 
      rownames = FALSE
    )%>%
      formatRound(c(4:7), 6) 
  })
  # Reset button
  observeEvent(input$reset_binom, {
    updateSliderInput(session, "binom_n", value = 30)
    updateSliderInput(session, "binom_p", value = 0.5)
    updateCheckboxInput(session, "binom_checkbox_normal", value = FALSE)
    updateCheckboxInput(session, "binom_checkbox_table", value = FALSE)
  })
  # Binomial distribution area ------------------------------------------------
  output$txt_out_b <- renderText({
    validate(
      need(!is.na(input$b_size_n), 
           message = ""),
      need(0 < input$b_size_n,
           message = "<Size n> must be greater than zero"),
      need(!is.na(input$b_p), 
           message = ""),
      need((0 <= input$b_p) | (input$b_p >= 1),
           message = "The probability p must be between 0 and 1"),
      need(!is.na(input$lbound_b), 
           message = ""),
      need(!is.na(input$ubound_b), 
           message = ""),
      need(0 <= input$lbound_b,
           message = "<Lower bound> must be greater or equal than zero"),
      need(0 <= input$ubound_b,
           message = "<Upper bound> must be greater or equal than zero"),
      need(input$lbound_b <=  input$b_size_n,
           message = "<Lower bound> must be smaller or equal to <Size n>"),
      need(input$ubound_b <= input$b_size_n,
           message = "<Upper bound> must be smaller or equal to <Size n>"),
      if (input$b_area == 1 | input$b_area == 4) {
        need(input$lbound_b < input$ubound_b, 
             message = "<Lower bound> must be smaller than <upper bound>")
      }
    )
    n <- as.integer(input$b_size_n)
    p <- input$b_p
    lb <- as.integer(input$lbound_b)
    ub <- as.integer(input$ubound_b)
    if (input$n_area == 1) {
      # middle
      area <- 
        paste0("X ~ Binom(", n, ", ", p, ")<br>P(", lb, " <= X <= ", ub , ") = ",
               round(sum(dbinom(lb:ub, n, p)), 4))
    }
    if (input$b_area == 2) {
      # lower tail
      area <- 
        paste0("X ~ Binom(", n, ", ", p, ")<br>P(X <= ", ub , ") = ",
               round(sum(dbinom(0:ub, n, p)), 4))
    }
    if (input$b_area == 3) {
      # upper tail
      area <- 
        paste0("X ~ Binom(", n, ", ", p, ")<br>P(", lb , " <= X) = ",
               round(sum(dbinom(lb:n, n, p)), 4))
    }
    if (input$b_area == 4) {
      # both tails
      area <- 
        paste0("X ~ Binom(", n, ", ", p, ")<br>P(X <= ", 
               lb , " or ", ub, " <= X) = ", 
               round(
                 sum(dbinom(c(0:lb, ub:n), n, p)),
                 4))
    }
    area <- paste0("<font color='#ff0000'><b>",
                   area,
                   "</b></font>")
  })
  
  output$area_b <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    validate(
      need(!is.na(input$b_size_n), 
           message = ""),
      need(0 < input$b_size_n,
           message = "<Size n> must be greater than zero"),
      need(!is.na(input$b_p), 
           message = ""),
      need((0 <= input$b_p) | (input$b_p >= 1),
           message = "The probability p must be between 0 and 1"),
      need(!is.na(input$lbound_b), 
           message = ""),
      need(!is.na(input$ubound_b), 
           message = ""),
      need(0 <= input$lbound_b,
           message = "<Lower bound> must be greater or equal than zero"),
      need(0 <= input$ubound_b,
           message = "<Upper bound> must be greater or equal than zero"),
      need(input$lbound_b <=  input$b_size_n,
           message = "<Lower bound> must be smaller or equal to <Size n>"),
      need(input$ubound_b <= input$b_size_n,
           message = "<Upper bound> must be smaller or equal to <Size n>"),
      if (input$b_area == 1 | input$b_area == 4) {
        need(input$lbound_b < input$ubound_b, 
             message = "<Lower bound> must be smaller than <upper bound>")
      }
    )
    n <- as.integer(input$b_size_n)
    p <- input$b_p
    lb <- as.integer(input$lbound_b)
    ub <- as.integer(input$ubound_b)
    x <- 0:n
    y <- dbinom(0:n, size = n, prob = p)
    par(mar = c(5, 4, 1, 2) + 0.1)
    plot(x, y,
         type = "h",
         lwd = 4,
         col = col1,
         xlab = "",
         ylab = "Binomial probability"
    )
    
    # plot area 
    if (input$b_area == 1) {
      # middle
      xs_a <- seq(lb, ub, by = 1)
      ys_a <- dbinom(xs_a, size = n, prob = p)
      for (i in seq_along(xs_a)) {
        lines(c(xs_a[i], xs_a[i]), 
              c(0, ys_a[i]),
              col = "red",
              lwd = 5)  
      }
    }
    if (input$b_area == 2) {
      # lower tail
      xs_a <- seq(0, ub, by = 1)
      ys_a <- dbinom(xs_a, size = n, prob = p)
      for (i in seq_along(xs_a)) {
        lines(c(xs_a[i], xs_a[i]), 
              c(0, ys_a[i]),
              col = "red",
              lwd = 5)  
      }
    }
    if (input$b_area == 3) {
      # upper tail
      xs_a <- seq(lb, n, by = 1)
      ys_a <- dbinom(xs_a, size = n, prob = p)
      for (i in seq_along(xs_a)) {
        lines(c(xs_a[i], xs_a[i]), 
              c(0, ys_a[i]),
              col = "red",
              lwd = 5)  
      }
    }
    if (input$b_area == 4) {
      # both tails
      xs_a1 <- seq(0, lb, by = 1)
      ys_a1 <- dbinom(xs_a1, size = n, prob = p)
      xs_a2 <- seq(ub, n, by = 1)
      ys_a2 <- dbinom(xs_a2, size = n, prob = p)
      
      for (i in seq_along(xs_a1)) {
        lines(c(xs_a1[i], xs_a1[i]), 
              c(0, ys_a1[i]),
              col = "red",
              lwd = 5)  
      }
      for (i in seq_along(xs_a2)) {
        lines(c(xs_a2[i], xs_a2[i]), 
              c(0, ys_a2[i]),
              col = "red",
              lwd = 5)  
      }
    }
  })
  # Reset button normal area
  observeEvent(input$reset_b_area, {
    updateNumericInput(session, "b_size_n", value = 15)
    updateNumericInput(session, "b_p", value = 0.5)
    updateNumericInput(session, "b_area", value = 1)
    updateNumericInput(session, "lbound_b", value = 5)
    updateNumericInput(session, "ubound_b", value = 10)
  })
  
  # Normal distribution -----------------------------------------------------
  output$dist_normal1 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    # N(mu, sigma)
    mu <- input$normal_mu
    sigma <- input$normal_sigma
    xs <- c(-4, 4, mu - 4 * sigma, mu + 4 * sigma)
    x <- seq(min(xs), max(xs), length = 400)
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
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    # N(mu, sigma)
    mu <- input$normal_mu
    sigma <- input$normal_sigma
    xs <- c(-4, 4, mu - 4 * sigma, mu + 4 * sigma)
    x <- seq(min(xs), max(xs), length = 400)
    y <- dnorm(x, mean = mu, sd = sigma)
    # N(0, 1)
    yn <- dnorm(x, mean = 0, sd = 1)
    plot(x, pnorm(x, mean = 0, sd = 1),
         type = "l",
         lwd = 2,
         col = col2,
         main = "Cumulative density function (cdf)",
         xlab = "",
         ylab = "Probability",
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
    mu <- input$normal_mu;
    sigma2 <- round(input$normal_sigma^2, 2);
    paste0(
      "<font color='#FFE5B4'><b>Standard normal distribution N(0,1)</b></font><br>
    <font color='#FFBF00'><b>Normal distribution N(",
      mu, ",", sigma2, ")</b></font>")
  })
  
  # Reset button normal distribution
  observeEvent(input$reset_normal, {
    updateSliderInput(session, "normal_mu", value = 0)
    updateSliderInput(session, "normal_sigma", value = 2)
  })
  
  # Normal distribution area ------------------------------------------------
  # calculate and print area under the curve
  output$txt_out <- renderText({
    validate(
      need(!is.na(input$n_mean), 
           message = "input <Mean> should be numeric."),
      need(!is.na(input$n_sd), 
           message = "input <Standard deviation> should be numeric."),
      need(!is.na(input$lbound), 
           message = "input <Lower bound> should be numeric."),
      need(!is.na(input$ubound), 
           message = "input <Upper bound> should be numeric."),
      if (input$n_area == 1 | input$n_area == 4) {
        need(input$lbound <= input$ubound, 
             message = "input <Lower bound> should be smaller <Upper bound>.")
      }
    )
    mu <- input$n_mean
    sigma <- input$n_sd
    lb <- input$lbound
    ub <- input$ubound
    if (input$n_area == 1) {
      # middle
      area <- 
        paste0("P(", lb, " < X < ", ub , ") = ",
               round(pnorm(ub, mean = mu, sd = sigma) -  
                       pnorm(lb, mean = mu, sd = sigma), 4))
    }
    if (input$n_area == 2) {
      # lower tail
      area <- 
        paste0("P(X < ", ub , ") = ",
               round(pnorm(ub, mean = mu, sd = sigma), 4))
    }
    if (input$n_area == 3) {
      # upper tail
      area <- 
        paste0("P(", lb , " < X) = ",
               round(pnorm(lb, mean = mu, sd = sigma,
                           lower.tail = FALSE), 4))
    }
    if (input$n_area == 4) {
      # both tails
      area <- 
        paste0("P(X < ", lb , " or ", ub, " < X) = ", 
               round(
                 pnorm(lb, mean = mu, sd = sigma) +
                   pnorm(ub, mean = mu, sd = sigma, lower.tail = FALSE),
                 4))
    }
    area <- paste0("<font color='#FFBF00'><b>",
                   area,
                   "</b></font>")
  })
  
  output$area_normal <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    # N(mu, sigma)
    validate(
      need(!is.na(input$n_mean), 
           message = ""),
      need(!is.na(input$n_sd), 
           message = ""),
      need(!is.na(input$lbound), 
           message = ""),
      need(!is.na(input$ubound), 
           message = ""),
      if (input$n_area == 1 | input$n_area == 4) {
        need(input$lbound <= input$ubound, 
             message = "")
      }
    )
    mu <- input$n_mean
    sigma <- input$n_sd
    lb <- input$lbound
    ub <- input$ubound
    xs <- c(lb - 2, ub + 2, mu - 4 * sigma, mu + 4 * sigma)
    x <- seq(min(xs), max(xs), length = 400)
    y <- dnorm(x, mean = mu, sd = sigma)
    
    # density function
    op <- par(mar = c(5, 4, 0.1, 2) + 0.1)
    plot(x, y,
         type = "n", 
         xlab = "",
         ylab = "density",
         xlim = range(x),
         ylim = c(0, max(y)),
         main = "",
         font.lab = 2,
         cex = 3
    )
    abline(h = 0)
    
    # plot area under the curve
    if (input$n_area == 1) {
      # middle
      xs_a <- seq(lb, ub, length.out = 400)
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
            col = col1)
    }
    if (input$n_area == 2) {
      # lower tail
      xs_a <- seq(min(xs), ub, length.out = 400)
      ys_a <- dnorm(xs_a, mean = mu, sd = sigma)
      polygon(
        x = c(min(xs), xs_a, ub),
        y = c(0, ys_a, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    if (input$n_area == 3) {
      # upper tail
      xs_a <- seq(lb, max(xs), length.out = 400)
      ys_a <- dnorm(xs_a, mean = mu, sd = sigma)
      polygon(
        x = c(lb, xs_a, max(xs)),
        y = c(0, ys_a, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    if (input$n_area == 4) {
      # both tails
      xs_a1 <- seq(min(xs), lb, length.out = 400)
      ys_a1 <- dnorm(xs_a1, mean = mu, sd = sigma)
      xs_a2 <- seq(ub, max(xs), length.out = 400)
      ys_a2 <- dnorm(xs_a2, mean = mu, sd = sigma)
      polygon(
        x = c(min(xs), xs_a1, lb),
        y = c(0, ys_a1, 0),
        col = col2,
        border = NA
      )
      polygon(
        x = c(ub, xs_a2, max(xs)),
        y = c(0, ys_a2, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    
    par(op)
  })
  # Reset button normal area
  observeEvent(input$reset_normal_area, {
    updateNumericInput(session, "n_mean", value = 0)
    updateNumericInput(session, "n_sd", value = 1)
    updateNumericInput(session, "n_area", value = 1)
    updateNumericInput(session, "lbound", value = -1)
    updateNumericInput(session, "ubound", value = 1)
  })
  
  # t distribution -----------------------------------------------------
  output$dist_t1 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    # t(df)
    t_df <- input$t_df
    x <- seq(-10, 10, length = 400)
    y <- dt(x, df = t_df)
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
  })   
  
  output$dist_t2 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    # t(df)
    t_df <- input$t_df2
    x <- seq(-10, 10, length = 400)
    y <- dt(x, df = t_df)
    yn <- dnorm(x, mean = 0, sd = 1)
    plot(x, pnorm(x, mean = 0, sd = 1),
         type = "l",
         lwd = 2,
         col = col2,
         main = "Cumulative density function (cdf)",
         xlab = "",
         ylab = "Probability",
         font.lab = 2,
         cex = 3
    )
    lines(x, 
          pt(x, df = t_df),
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
  })
  output$dist_t_txt <- renderText({
    t_df <- input$t_df;
    paste0(
      "<font color='#FFE5B4'><b>Standard normal distribution N(0,1)</b></font><br>
    <font color='#FFBF00'><b>t distribution t<sub>",
      t_df, "</sub></b></font>")
  })
  
  output$t_table <- DT::renderDataTable({
    t_df <- input$t_df
    x <- seq(-4, 4, by = 0.1)    
    t_data <- datatable(
      data.frame(
        x = x, 
        df = t_df,
        pdf = dt(x, df = t_df),
        cdf = pt(x, df = t_df),
        pdf_snormal = dnorm(x),
        cdf_snormal = dnorm(x)
      ), 
      options = list(searching = FALSE,
                     paging = FALSE
      ), 
      rownames = FALSE
    ) %>%
      formatRound(1, 1) %>%
      formatRound(3:6, 6) 
  })
  # Reset button t distribution
  observeEvent(input$reset_t, {
    updateSliderInput(session, "t_df", value = 1)
    updateCheckboxInput(session, "t_checkbox_table", value = FALSE)
  })
  
  # t distribution area ------------------------------------------------
  output$txt_out_t <- renderText({
    validate(
      need(!is.na(input$t_df2), 
           message = "input <df> should be integer."),
      need(!(as.integer(input$t_df2) <= 0), 
           message = "input <df> should be greater zero."),
      need(!is.na(input$lbound_t), 
           message = "input <Lower bound> should be numeric."),
      need(!is.na(input$ubound_t), 
           message = "input <Upper bound> should be numeric."),
      if (input$t_area == 1 | input$t_area == 4) {
        need(input$lbound_t <= input$ubound_t, 
             message = "input <Lower bound> should be smaller <Upper bound>.")
      }
    )
    
    t_df <- as.integer(input$t_df2)
    lb <- input$lbound_t
    ub <- input$ubound_t
    if (input$t_area == 1) {
      # middle
      area <- 
        paste0("P(", lb, " < T(df = ", t_df, ") < ", ub , ") = ",
               round(pt(ub, df = t_df) -  
                       pt(lb, df = t_df), 4))
    }
    if (input$t_area == 2) {
      # lower tail
      area <- 
        paste0("P(T(df = ", t_df, ") < ", ub , ") = ",
               round(pt(ub, df = t_df), 4))
    }
    if (input$t_area == 3) {
      # upper tail
      area <- 
        paste0("P(", lb , " < T(df = ", t_df, ")) = ",
               round(pt(lb, df = t_df,
                        lower.tail = FALSE), 4))
    }
    if (input$t_area == 4) {
      # both tails
      area <- 
        paste0("P(T(df = ", t_df, ") < ", lb , " or ", ub, " < T(df = ", t_df, ") = ", 
               round(
                 pt(lb, df = t_df) +
                   pt(ub, df = t_df, lower.tail = FALSE),
                 4))
    }
    area <- paste0("<font color='#FFBF00'><b>",
                   area,
                   "</b></font>")
  })
  
  output$area_t <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    validate(
      need(!is.na(input$t_df2), 
           message = "input <df> should be integer."),
      need(!(as.integer(input$t_df2) <= 0), 
           message = "input <df> should be greater zero."),
      need(!is.na(input$lbound_t), 
           message = "input <Lower bound> should be numeric."),
      need(!is.na(input$ubound_t), 
           message = "input <Upper bound> should be numeric."),
      if (input$t_area == 1 | input$t_area == 4) {
        need(input$lbound_t <= input$ubound_t, 
             message = "input <Lower bound> should be smaller <Upper bound>.")
      }
    )
    t_df <- as.integer(input$t_df2)
    lb <- input$lbound_t
    ub <- input$ubound_t
    xs <- c(lb - 2, ub + 2, - 4, 4)
    x <- seq(min(xs), max(xs), length = 400)
    y <- dt(x, df = t_df)
    
    # density function
    op <- par(mar = c(5, 4, 0.1, 2) + 0.1)
    plot(x, y,
         type = "n", 
         xlab = "",
         ylab = "density",
         xlim = range(x),
         ylim = c(0, max(y)),
         main = "",
         font.lab = 2,
         cex = 3
    )
    abline(h = 0)
    
    # plot area under the curve
    if (input$t_area == 1) {
      # middle
      xs_a <- seq(lb, ub, length.out = 400)
      ys_a <- dt(xs_a, df = t_df)
      polygon(
        x = c(lb, xs_a, ub),
        y = c(0, ys_a, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    if (input$t_area == 2) {
      # lower tail
      xs_a <- seq(min(xs), ub, length.out = 400)
      ys_a <- dt(xs_a, t_df)
      polygon(
        x = c(min(xs), xs_a, ub),
        y = c(0, ys_a, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    if (input$t_area == 3) {
      # upper tail
      xs_a <- seq(lb, max(xs), length.out = 400)
      ys_a <- dt(xs_a, t_df)
      polygon(
        x = c(lb, xs_a, max(xs)),
        y = c(0, ys_a, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    if (input$t_area == 4) {
      # both tails
      xs_a1 <- seq(min(xs), lb, length.out = 400)
      ys_a1 <- dt(xs_a1, df = t_df)
      xs_a2 <- seq(ub, max(xs), length.out = 400)
      ys_a2 <- dt(xs_a2, t_df)
      polygon(
        x = c(min(xs), xs_a1, lb),
        y = c(0, ys_a1, 0),
        col = col2,
        border = NA
      )
      polygon(
        x = c(ub, xs_a2, max(xs)),
        y = c(0, ys_a2, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    
    par(op)
  })
  # Reset button t area
  observeEvent(input$reset_t_area, {
    updateNumericInput(session, "t_df2", value = 1)
    updateNumericInput(session, "t_area", value = 1)
    updateNumericInput(session, "lbound_t", value = -1)
    updateNumericInput(session, "ubound_t", value = 1)
  })
  # Chi2 distribution -----------------------------------------------------
  output$dist_chi21 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    par(mar = c(5, 4, 1, 2) + 0.2)
    # chi2(df)
    chi2_df <- input$chi2_df
    xmax <- qchisq(0.975, df = chi2_df)
    if (chi2_df >= 2) {
      xmax <- qchisq(0.999, df = chi2_df)
    }
    x <- seq(0.001, xmax, length = 400)
    y <- dchisq(x, df = chi2_df)
    # Density function
    plot(x, y,
         type = "l",
         xlim = c(0.001, xmax),
         ylim = range(y),
         lwd = 3,
         col = col1,
         main = "Probability-density function (pdf)",
         xlab = "",
         ylab = "Density",
         font.lab = 2
    )
  }) 
  output$dist_chi22 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    par(mar = c(5, 4, 1, 2) + 0.2)
    # chi2(df)
    chi2_df <- input$chi2_df
    xmax <- qchisq(0.975, df = chi2_df)
    if (chi2_df >= 2) {
      xmax <- qchisq(0.999, df = chi2_df)
    }
    x <- seq(0.001, xmax, length = 400)
    y <- pchisq(x, df = chi2_df)
    plot(x, y,
         type = "l",
         lwd = 2,
         col = col1,
         main = "Cumulative density function (cdf)",
         xlab = "",
         ylab = "Probability",
         font.lab = 2,
         cex = 3
    )
    
  })
  # Reset button t distribution
  observeEvent(input$reset_chi2, {
    updateSliderInput(session, "chi2_df", value = 1)
  })
  
  # Chi2 distribution area ------------------------------------------------
  output$txt_out_chi2 <- renderText({
    validate(
      need(!is.na(input$chi2_df22), 
           message = "input <df> should be integer."),
      need(!(as.integer(input$chi2_df22) <= 0), 
           message = "input <df> should be greater zero."),
      need(!is.na(input$lbound_chi2), 
           message = "input <Lower bound> should be numeric."),
      need(!is.na(input$ubound_chi2), 
           message = "input <Upper bound> should be numeric."),
      need(!(input$lbound_chi2 <= 0), 
           message = "input <Lower bound> should be greater zero."),
      need(!(input$ubound_chi2 <= 0), 
           message = "input <Upper bound> should be greater zero."),
      if (input$chi2_area == 1 | input$chi2_area == 4) {
        need(input$lbound_chi2 <= input$ubound_chi2, 
             message = "input <Lower bound> should be smaller <Upper bound>.")
      }
    )
    chi2_df <- as.integer(input$chi2_df22)
    lb <- input$lbound_chi2
    ub <- input$ubound_chi2
    if (input$t_area == 1) {
      # middle
      area <- 
        paste0("P(", lb, " < X2(df = ", chi2_df, ") < ", ub , ") = ",
               round(pchisq(ub, df = chi2_df) -  
                       pchisq(lb, df = chi2_df), 4))
    }
    if (input$chi2_area == 2) {
      # lower tail
      area <- 
        paste0("P(X2(df = ", chi2_df, ") < ", ub , ") = ",
               round(pchisq(ub, df = chi2_df), 4))
    }
    if (input$chi2_area == 3) {
      # upper tail
      area <- 
        paste0("P(", lb , " < X2(df = ", chi2_df, ")) = ",
               round(pchisq(lb, df = chi2_df,
                            lower.tail = FALSE), 4))
    }
    if (input$chi2_area == 4) {
      # both tails
      area <- 
        paste0("P(X2(df = ", chi2_df, ") < ", lb , " or ", ub, " < X2(df = ", chi2_df, ") = ", 
               round(
                 pchisq(lb, df = chi2_df) +
                   pchisq(ub, df = chi2_df, lower.tail = FALSE),
                 4))
    }
    area <- paste0("<font color='#FFBF00'><b>",
                   area,
                   "</b></font>")
  })
  
  output$area_chi2 <- renderPlot({
    par(cex.lab = 1.5, cex.axis = 1.2, font.lab = 2, font.axis = 2)
    validate(
      need(!is.na(input$chi2_df22), 
           message = "input <df> should be integer."),
      need(!(as.integer(input$chi2_df22) <= 0), 
           message = "input <df> should be greater zero."),
      need(!is.na(input$lbound_chi2), 
           message = "input <Lower bound> should be numeric."),
      need(!is.na(input$ubound_chi2), 
           message = "input <Upper bound> should be numeric."),
      need(!(input$lbound_chi2 <= 0), 
           message = "input <Lower bound> should be greater zero."),
      need(!(input$ubound_chi2 <= 0), 
           message = "input <Upper bound> should be greater zero."),
      if (input$chi2_area == 1 | input$chi2_area == 4) {
        need(input$lbound_chi2 <= input$ubound_chi2, 
             message = "input <Lower bound> should be smaller <Upper bound>.")
      }
    )
    chi2_df <- as.integer(input$chi2_df22)
    lb <- input$lbound_chi2
    ub <- input$ubound_chi2
    
    xmax <- qchisq(0.975, df = chi2_df)
    if (chi2_df >= 2) {
      xmax <- qchisq(0.999, df = chi2_df)
    }
    xmax <- max(c(xmax, lb, ub))  # ?????????????????????????????
    xmin <- min(c(0.001, lb, ub)) # ?????????????????????????????
    x <- seq(xmin, xmax, length = 400)
    y <- dchisq(x, df = chi2_df)
    
    # density function
    op <- par(mar = c(5, 4, 0.1, 2) + 0.1)
    plot(x, y,
         type = "n", 
         xlab = "",
         ylab = "density",
         xlim = c(xmin, xmax),
         ylim = c(0, max(y)),
         main = "",
         font.lab = 2,
         cex = 3
    )
    abline(h = 0)
    
    # plot area under the curve
    if (input$chi2_area == 1) {
      # middle
      xs_a <- seq(lb, ub, length.out = 400)
      ys_a <- dchisq(xs_a, df = chi2_df)
      polygon(
        x = c(lb, xs_a, ub),
        y = c(0, ys_a, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    if (input$chi2_area == 2) {
      # lower tail
      xs_a <- seq(min(x), ub, length.out = 400)
      ys_a <- dchisq(xs_a, chi2_df)
      polygon(
        x = c(min(x), xs_a, ub),
        y = c(0, ys_a, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    if (input$chi2_area == 3) {
      # upper tail
      xs_a <- seq(lb, max(x), length.out = 400)
      ys_a <- dchisq(xs_a, chi2_df)
      polygon(
        x = c(lb, xs_a, max(x)),
        y = c(0, ys_a, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    if (input$chi2_area == 4) {
      # both tails
      xs_a1 <- seq(min(x), lb, length.out = 400)
      ys_a1 <- dchisq(xs_a1, df = chi2_df)
      xs_a2 <- seq(ub, max(x), length.out = 400)
      ys_a2 <- dchisq(xs_a2, chi2_df)
      polygon(
        x = c(min(x), xs_a1, lb),
        y = c(0, ys_a1, 0),
        col = col2,
        border = NA
      )
      polygon(
        x = c(ub, xs_a2, max(x)),
        y = c(0, ys_a2, 0),
        col = col2,
        border = NA
      )
      # to make the curve more visible
      lines(x, y,
            lwd = 4,
            col = col1)
    }
    
    par(op)
  })
  # Reset button t area
  observeEvent(input$reset_chi2_area, {
    updateNumericInput(session, "chi2_df2", value = 1)
    updateNumericInput(session, "chi2_area", value = 1)
    updateNumericInput(session, "lbound_chi2", value = 1)
    updateNumericInput(session, "ubound_chi2", value = 3)
  })
}
