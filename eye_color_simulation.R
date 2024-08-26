#### Simulation and analysis for eye color project

set.seed(428)

library(tidyverse)
library(ggthemes)

sample_eyes <- c("Blue", "Hazel", "Brown", "Brown")

negbinom <- function() {
  x <- sample(sample_eyes, 3)
  while (length(unique(x)) < 3) { 
    x <- c(x, sample(sample_eyes, 1))
  }
  n <- length(x)
  blue_portion <- sum(x == "Blue") / n
  brown_portion <- sum(x == "Brown") / n
  hazel_portion <- sum(x == "Hazel") / n
  blue_last <- ifelse(last(x) == "Blue", 1, 0)
  brown_last <- ifelse(last(x) == "Brown", 1, 0)
  hazel_last <- ifelse(last(x) == "Hazel", 1, 0)
  things.to.return <- c(n, blue_portion, brown_portion, hazel_portion, blue_last, brown_last, hazel_last)
  return(things.to.return)
  }

ave_kid <- t(replicate(1000000, negbinom())) %>%
  as.data.frame() %>%
  rename(number = V1, prop_bl = V2, prop_br = V3, prop_hz = V4, blue_last = V5, brown_last = V6, hazel_last = V7)

summary(ave_kid)
quantile(ave_kid$number, c(0, .5, .55, .6, .65, .95))
median(ave_kid$number)
mean(ave_kid$number)
max(ave_kid$number)
sum(ave_kid$number)


plot1 <-  ggplot(data = ave_kid) +
  geom_smooth(aes(x = number, y = prop_bl), color = "blue") +
  geom_smooth(aes(x = number, y = prop_br), color = "brown") +
  geom_smooth(aes(x = number, y = prop_hz), color = "#617C58") +
  geom_hline(yintercept = mean(ave_kid$prop_br), color = "brown") +
  geom_hline(yintercept = mean(ave_kid$prop_bl), color = "blue") +
  geom_hline(yintercept = mean(ave_kid$prop_hz), color = "#617C58") +
  labs(x = "Total Number of Children in Family", y = "Prop. of Children with Eye Color", caption = "Blue Ave = 0.27818, Brown Ave = 0.4436, Hazel Ave = 0.27817") +
  theme_minimal()

plot2 <- ggplot(data = ave_kid) +
  geom_density(aes(x = number), bw = .5, fill = "royalblue") +
  geom_vline(xintercept = mean(ave_kid$number), linewidth = .6) +
  labs(x = "Total Number of Children in Family", y = "Density") +
  theme_minimal() +
  annotate(geom = "text", x = 11.4, y = .35, label = "Mean = 5.004936", size = 3)

plot3 <- ggplot(data = ave_kid) +
  geom_density(aes(x = number), bw = .5, fill = "royalblue") +
  geom_vline(xintercept = quantile(ave_kid$number, c(0, .95)), linewidth = .7, color = "maroon") +
  geom_vline(xintercept = mean(ave_kid$number)) +
  labs(x = "Total Number of Children in Family", y = "Density") +
  theme_minimal() +
  annotate(geom = "text", x = 19, y = .35, label = "95th Percentile = 12", size = 3)

plot4 <- ggplot(data = ave_kid) +
  geom_density(aes(x = number), bw = .5, fill = "royalblue") +
  geom_vline(xintercept = max(ave_kid$number), linewidth = .7, color = "#228B22") +
  geom_vline(xintercept = mean(ave_kid$number)) +
  geom_vline(xintercept = quantile(ave_kid$number, c(0, .95)), linewidth = .7, color = "maroon") +
  labs(x = "Total Number of Children in Family", y = "Density") +
  theme_minimal() +
  annotate(geom = "text", x = 50.5, y = .35, label = "Maximum = 56", size = 3)
