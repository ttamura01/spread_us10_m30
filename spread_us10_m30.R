setwd("/Users/takayukitamura/Documents/R_Computing/us_rates/spread_us10_m30")
library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)
library(scales)

## download data from csv file
rates <- read.csv("/Users/takayukitamura/Documents/R_Computing/us_rates/data/us_10y_30y.csv") %>% 
  select(-X)

## add the rarest data 
updates <- tribble(~date, ~mortgage30, ~treasury10,
                   "2024-06-27", 6.86, 4.44,
                   "2024-07-03", 6.95, 4.36,
                   "2024-07-11", 6.89, 4.22,
                   "2024-07-18", 6.77, 4.20,
                   "2024-07-25", 6.78, 4.28,
                   "2024-08-01", 6.73, 4.09,
                   "2024-08-08", 6.47, 3.98,
                   "2024-08-15", 6.49, 3.92,
                   "2024-08-22", 6.46, 3.86,
                   "2024-08-29", 6.35, 3.87)

##Combine the two data
rates <- rbind(rates, updates)
rates$date <- as.Date(rates$date, format = "%Y-%m-%d")
sapply(rates, class)

## Add spread to the data frame
spread <- rates$mortgage30 - rates$treasury10

rates$spread <- spread
head(rates, 10)
tail(rates, 10)

rates %>% 
  arrange(-spread)

dim(rates)

##understand the data
rates <- rates %>% 
  filter(date>="1971-01-01") 

rates %>% 
  summarise(minimum = min(spread, round(2)),
            median = median(spread, round(2)),
            mean = mean(spread, round(2)),
            maximum = max(spread, round(2)),
            "25th percentile" = quantile(spread,0.25), 
            "75th percentile" = quantile(spread, 0.75),
            sd = sd(spread, round(3)))


##transform the data to longer
long_data <- rates %>% 
  pivot_longer(cols = -date, names_to = "yield", values_to = "percentage")

##historical yields and spread
long_data %>% 
  ggplot(aes(x =date, y = percentage, color = yield)) +
  geom_line(show.legend = TRUE) +
  labs(title = "US 30-year mortgage yield spread over 10-year treasury notes",
       caption = "FRED(Federal Reserve Economic Data), WSJ",
       x = NULL, y = NULL) +
  theme(
    legend.key = element_blank(),
    legend.title = element_blank()
  )

ggsave("figures/us_30y_10y_spread.png", height = 4.5, width = 6)  

rates %>% 
  ggplot(aes(x = date, y = spread))+
  geom_line() +
  labs(title = "US 30-year mortgage yield spread over 10-year treasury notes",
       caption = "FRED(Federal Reserve Economic Data), WSJ",
       x = NULL, y = NULL)



##historical spread with latest data
rates$latest_data <- ifelse(rates$date == max(rates$date), TRUE, FALSE)

rates %>% 
  ggplot(aes(x = date, y = spread, color = latest_data, fill = latest_data)) +
  geom_col(show.legend = FALSE) +
  scale_colour_manual(breaks = c(TRUE, FALSE),
                    values = c("red", "#AAAAAA")) +
  geom_text(data = subset(rates, latest_data == TRUE),
            aes(label = glue("{spread}%")), vjust=-.5, hjust = 0.5, color = "red") +
  labs(title = "US 30-year mortgage spread over 10-year treasury yield",
       x = NULL, y = NULL)

ggsave("figures/historical_spread_latest_data.png", height = 4.5, width = 5)
  
##boxplot

rates$year <- format(rates$date, "%Y")
sapply(rates, class)
rates$year = as.numeric(rates$year)

rates$period <- cut(rates$year,
                    breaks = c(1970, 2008, 2009, 2019, 2024),
                    labels = c("1971-2008", "2009-2019", "2020-2021", "2022-2024"))

d <- ggplot(rates, aes(x = period, y = spread, fill = period)) +
  geom_boxplot(show.legend = FALSE) +
  # facet_wrap(~period) +
  scale_y_continuous(limits = c(-0.5, 6),
                     breaks = seq(0, 6, 2),
                     labels = label_comma(accuracy = 0.1)) +
  labs(x = NULL,
       y = "spread")


d +  labs(title = "30 years mortgage yield spread over 10 years treasuary notes",
          subtitle = "The spread widened since 2022, with unwinding of MBS by Fed?", 
          y = "spread",
          x = NULL) +
  theme(plot.title.position = "plot",
        plot.subtitle = element_text(face = "italic"))

ggsave("figures/boxplots of spread indicating some impacts from Fed's MBS unwinding.png", width = 5, height = 4)

## Add z-score on the data_frame
z_score <- (spread - mean(spread))/sd(spread)

rates$z_score <- z_score
head(rates)
tail(rates)
model <- lm(mortgage30 ~ treasury10, data = rates)
lm(model)
summary(model)

predicted_30y <- predict(model, data.frame(treasury10 = c(4.5)))
attributes(model)

cor(rates$treasury10, rates$mortgage30)
model$coefficients
model$residuals
model$effects
model$rank
model$fitted.values

coefficients(model)
confint(model)
anova(model)

rate_stats <- rates %>% 
  summarise(
    mean_rate = mean(spread),
    SD_rate = sd(spread),
    med_est = median(spread),
    conf_25 = quantile(spread, 0.25),
    conf_75 = quantile(spread, 0.75),
    conf_low = quantile(spread, 0.025),
    conf_high = quantile(spread, 0.975)
  )

conf_low <- rate_stats %>% 
  select(conf_low)
conf_high <- rate_stats %>% 
  select(conf_high)

# Plot summary

latest_spread <- slice_tail(rates)[4]

slice_tail(rates)[4]


e <- ggplot(rates, aes(x = spread)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30, alpha = .3) +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate),
             size = 1, color = "red") +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate + SD_rate),
             linetype = "dashed", color = "red") +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate - SD_rate),
             linetype = "dashed", color = "red") +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate + SD_rate*2),
             linetype = "dotted", color = "red") +
  geom_vline(data = rate_stats,
             aes(xintercept = mean_rate - SD_rate*2),
             linetype = "dotted", color = "red") +
  scale_x_continuous(limits=c(NA, 4),
                     breaks = c(0, 4, 2)) +
  labs(title = "The 30-year mortgage spread over 10y treasury yield",
       subtitle = glue("Current spread at {latest_spread}% is outside of the 95% confidence interval(=2SD)"),
       x = "spread(%)",
       caption = "Source: FRED(Federal Reserve Economic Data") +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    plot.subtitle = element_text(face = "italic"
  ))

e

# Add labels
z1 <-  round((mean(rates$spread) + sd(rates$spread)),2)
z2 <- round((mean(rates$spread) + sd(rates$spread)*2),2)
z3 <- round((mean(rates$spread) - sd(rates$spread)),2)
z4 <- round((mean(rates$spread) - sd(rates$spread)*2),2)

e + geom_text(aes(x = 1.7, y = 450, label = round(rate_stats$mean_rate, 2))) +
  geom_text(aes(x = z1, y = 400, label = z1)) +
  geom_text(aes(x = z2, y = 350, label = z2)) +
  geom_text(aes(x = z3, y = 400, label = z3)) +
  geom_text(aes(x = z4, y = 350, label = z4)) 

ggsave("figures/30y_mortgage_spread_10TB.png", height = 5, width = 5)

e + geom_text(aes(x = 1.7, y = 470, label = glue("mean = {round(rate_stats$mean_rate, 2)}"))) +
  geom_text(aes(x = z1, y = 400, label = glue("mean + SD = {z1}"))) +
  geom_text(aes(x = z2, y = 350, label = glue("mean + 2SD = {z2}"))) +
  geom_text(aes(x = z3, y = 400, label = glue("mean - SD = {z3}"))) +
  geom_text(aes(x = z4, y = 350, label = glue("mean - 2SD = {z4}"))) 

ggsave("figures/30y_mortgage_spread_10TB.png", height = 5, width = 5)


f <- ggplot(rates, aes(x = spread)) +
  geom_histogram(fill = "blue", color = "white") +
  geom_rect(data = rate_stats,
            aes(
              ymin = 0, ymax = Inf,
              x = med_est, xmin = conf_low, xmax = conf_high),
            alpha = 0.2, fill = "green"
  ) +
  geom_vline(data = rate_stats, aes(
    xintercept = med_est),
    size = 1, color = "red"
  ) +
  labs(title = "Histogram - how the spread is distributed",
  subtitle = glue("95% confidence interval is the stread between {conf_low}% and {conf_high}%"),
  caption = "FRED(Federal Reserve Econimic Data"
  ) +
  theme(
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic")
  )
f
ggsave("figures/histogram_stread.png", width = 5, height = 5)





