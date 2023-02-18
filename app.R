library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)

theme_set(theme_bw())
p_plot <- seq(0, 1, length.out = 500)

ui <- fluidPage(
  titlePanel("Proportion of PMx using Bayes"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        "Prior distribution parameters:",
        numericInput("alpha",
          "alpha:",
          min = 1e-4,
          max = 500,
          value = 1.5
        ),
        numericInput("beta",
          "beta:",
          min = 1e-4,
          max = 500,
          value = 10
        )
      ),
      numericInput("n",
        "Number of modellers:",
        min = 1,
        max = 500,
        value = 1
      ),
      numericInput("x",
        "Number using Bayes:",
        min = 0,
        max = 500,
        value = 0
      )
    ),


    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("densities"),
      h4(htmlOutput("summary"))
    )
  )
)

server <- function(input, output) {
  
  df <- reactive({
    # prior
    alpha <- input$alpha
    beta <- input$beta
    
    # data
    x <- input$x
    n <- input$n
    
    # posterior
    alpha_post <- alpha + x
    beta_post <- beta + (n - x)
    
    df <- tibble(
      p = p_plot,
      Prior = dbeta(p_plot, alpha, beta),
      Likelihood = dbinom(round(p_plot * n), size = n, prob = x / n),
      Posterior = dbeta(p_plot, alpha_post, beta_post)
    ) %>% 
      mutate(across(-p, ~ .x / max(.x))) %>% 
      pivot_longer(-p)
    
    df
  })
    
  output$densities <- renderPlot({
    df() %>% 
      ggplot(aes(p, value, colour = name)) +
      geom_line(lwd = 2) +
      labs(
        x = "Proportion of modellers using Bayes",
        y = "Density",
        colour = NULL
      ) +
      theme(text = element_text(size = 20))
    
  })
  
  output$summary <- renderText({
    # prior
    alpha <- input$alpha
    beta <- input$beta
    
    # data
    x <- input$x
    n <- input$n
    
    # posterior
    alpha_post <- alpha + x
    beta_post <- beta + (n - x)
    
    ci_lo <- signif(qbeta(0.025, alpha_post, beta_post), 3)
    ci_hi <- signif(qbeta(0.975, alpha_post, beta_post), 3)
    ci_text <- glue("95% CI: ({ci_lo}, {ci_hi})")
    
    prob50 <- signif(1 - pbeta(0.5, alpha_post, beta_post), 3)
    prob50_text <- glue("Probability that proportion > 0.5: {prob50}")
    
    c(ci_text, "<br>", prob50_text)
  })
}

shinyApp(ui = ui, server = server)
