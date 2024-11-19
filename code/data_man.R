pacman::p_load(
  rio,
  here,
  skimr,
  tidyverse,
  lmtest,
  sandwich,
  broom,
  mvtnorm
)

thm <- theme_classic() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

expit <- function(x) {
  1 / (1 + exp(-x))
}

## generate some data:
##### ten continuous confounders, standard normal
mean_c <- rep(0, 10)
sigma_c <- diag(1, 10)
c <- mvtnorm::rmvnorm(n = 5000, mean = mean_c, sigma = sigma_c)

head(c)

GGally::ggpairs(
  data.frame(c)
)

#### one binary exposure
ps_parms <- c(-2, rep(log(1.75), ncol(c)))
x <- rbinom(n = 5000, size = 1, prob = expit(cbind(1, c) %*% ps_parms))

table(x)
mean(x)

#### one continuous outcome
mu_parms <- c(180, 5, rep(2, ncol(c)))
y <- cbind(1, x, c) %*% mu_parms + rnorm(n = 5000)

output_data <- data.frame(
  id = 1:5000, y, x, c
)

names(output_data)[4:13] <- paste0("c", 1:10)

head(output_data)

write_csv(
  output_data,
  here("data", "example_data.csv")
)