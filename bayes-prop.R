library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

p_plot <- seq(0, 1, length.out = 100)

# prior
alpha <- 1.5
beta <- 10

df <- tibble(
  p = p_plot,
  Prior = dbeta(p_plot, alpha, beta),
) %>% 
  mutate(across(-p, ~ .x / max(.x))) %>% 
  pivot_longer(-p) %>% 
  print()


df %>% 
  ggplot(aes(p, value, colour = name)) +
  geom_line()

df %>% 
  ggplot(aes(p, value, colour = name)) +
  geom_line(lwd = 2) +
  labs(
    x = "Proportion of modellers using Bayes",
    y = "Density",
    colour = NULL
  ) +
  theme(text = element_text(size = 20), legend.position = "none")
