library(fmpapi)
library(tidyverse)
library(arrow)
library(TTR)
library(furrr)
library(slider)
library(patchwork)

source("R/utils.R")


symbol <- "SSY"
dchart <- fmp_daily_prices(symbol, start_date = "2021-03-01") %>%
  group_by(symbol) %>%
  arrange(date) %>%
  format_data()


d1 <- dchart %>%
  filter(
    date >= as.Date("2021-03-01"),
  #  date <= as.Date("2021-01-03")
  )

d1_change <- d1 %>%
  filter(
    change_per > 0
  )

d1_ema <- d1 %>%
  select(
    symbol, date, starts_with("close_ema_0"), -matches("lag")
  ) %>%
  pivot_longer(cols =  starts_with("close_ema_0"))


p_ema <-
  d1_ema %>%
  ggplot() +
    # geom_vline(
    #   data = d1_change,
    #   aes(xintercept = date),
    #   color = "#50C878", alpha = 0.5, size = 2
    # ) +
  geom_line(
    aes(x = date, y = value, color = name), size = 0.8
  ) +
  scale_x_date(labels = scales::label_date_short()) +
  labs(y = "Closing Price", x = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_colour_viridis_d()


p_sorted <-
  d1 %>%
  ggplot(
    aes(x = date)
  ) +
  geom_area(aes(y = mean_sorted_per_34), fill = "dark blue", size = 1, alpha = 0.5) +
  geom_area(aes(y = mean_sorted_per), fill = "purple", size = 1, alpha = 0.5) +
  # geom_hline((aes(yintercept = 0)), color = "dark grey", size = 1) +
  scale_x_date(labels = scales::label_date_short()) +
  labs(y = "EMA Sort Percentage", x = "") +
  theme_minimal()


p_candle <-
  d1 %>%
  ggplot(aes(x = date, y = close)) +
  tidyquant::geom_candlestick(
    aes(open = open, high = high, low = low, close = close),
    colour_up = "#50C878",
    colour_down = "#CD5C5C",
    fill_up = "#50C878",
    fill_down = "#CD5C5C",
    alpha = 1
  ) +
  geom_line(
    aes(y =close_ema_021), size = 1
  ) +
  geom_line(
    aes(y =close_ema_008), color = "#50C878", size = 1
  ) +
  scale_x_date(labels = scales::label_date_short()) +
  labs(y = "Closing Price", x = "") +
  theme_minimal() +
  theme(legend.position = "none")



p_macd <-
  d1 %>%
  ggplot(
    aes(x = date)
  ) +
  geom_line(aes(y = macd_signal), color = "gold", size = 1) +
  geom_line(aes(y = macd_line), color = "purple", size = 1) +
  geom_hline((aes(yintercept = 0)), color = "dark grey", size = 1) +
  scale_x_date(labels = scales::label_date_short()) +
  labs(y = "MACD", x = "") +
  theme_minimal()


p_stoch <-
  d1 %>%
  ggplot(
    aes(x = date)
  ) +
  geom_hline((aes(yintercept = 80))) +
  geom_hline((aes(yintercept = 50)), linetype = 2) +
  geom_hline((aes(yintercept = 20))) +
    geom_line(aes(y = stoch_k), color = "#2693d1", size = 1) +
    geom_line(aes(y = stoch_d), color = "#ffb627", size = 1) +
  scale_x_date(labels = scales::label_date_short()) +
  labs(y = "Stochastic", x = "") +
  theme_minimal()



p_vol <-
  d1 %>%
  ggplot(aes(x = date, y = volume)) +
  geom_col(aes(fill = volume_ema_spike)) +
  geom_line(aes(y = volume_ema_013), size = 1, color = "black") +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_fill_manual(values = c("grey", "#50C878" )) +
  theme_minimal() +
  labs(y = "Volume", x = "") +
  scale_x_date(labels = scales::label_date_short()) +
  theme(legend.position = "none")


(p_ema / p_candle / (p_stoch / p_sorted)/ (p_macd / p_vol)) + plot_annotation(
  title = symbol
)


































