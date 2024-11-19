pacman::p_load(
  rio,
  here,
  skimr,
  tidyverse,
  lmtest,
  sandwich,
  broom
)

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

## import simulated data
a <- read_csv(
  here("data", "example_data.csv")
)

head(a)

## simple linear regression
mod1 <- lm(y ~ . - id, data = a)

summary(mod1)

qq_plot <- ggplot(mod1, aes(sample = rstandard(mod1))) + 
    geom_qq() + 
    stat_qq_line() +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

ggsave(
  filename = here("figures", "qqplot_lm.pdf"),
  plot = qq_plot,
  width = 12, height = 12, units = "cm"
)