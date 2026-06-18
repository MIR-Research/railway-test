n = 200
df <- data.frame(yr = rep(c(2020:2024),n/5),
                 tilled = rep(c("Y","N"),n/2),
                 corn = rpois(n,1.5),
                 wheat = rnorm(n,18,1),
                 soy = rnorm(n,10,2),
                 rice = rnorm(n,13,1.5),
                 rye = rnorm(n,7,1)
)

df %>%
  tidyr::pivot_longer(cols = names(df[-c(1,2)]),
                      names_to = "crop",
                      values_to = "yield") -> df
