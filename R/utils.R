library(fmpapi)
library(tidyverse)
library(arrow)
library(furrr)
library(slider)


percent_change <- function(final, initial) ((final - initial) / initial) * 100
# lagged_diff <- function(x, n) log(x) - log(lag(x))
# euc_dist <- function(x, y) as.numeric(rbind(x,y))


# wrappers for TTR functions ----------------------------------------------
nice_ema <- function(x, n) {

  out <- try(TTR::EMA(x, n = n), silent = TRUE)
  if (class(out) == "try-error") out <- as.numeric(rep_len(NA, length(x)))

  out
}

nice_atr <- function(high, low, close) {

  out <- try(as.data.frame(TTR::ATR(cbind(high, low, close),  maType="EMA"))$atr, silent = TRUE)
  if (class(out) == "try-error") out <- as.numeric(rep_len(NA, length(close)))

  out
}


nice_cci <- function(high, low, close, ...) {

  out <- try(TTR::CCI(cbind(high, low, close),  ...), silent = TRUE)
  if (class(out) == "try-error") out <- as.numeric(rep_len(NA, length(close)))

  out
}

nice_obv <- function(close, volume) {

  out <- try(TTR::OBV(close, volume), silent = TRUE)
  if (class(out) == "try-error") out <- as.numeric(rep_len(NA, length(close)))

  out
}

nice_macd_line <- function(close, ...) {

  out <- try(as.data.frame(TTR::MACD(close, maType="EMA", ...))$macd, silent = TRUE)
  if (class(out) == "try-error") out <- as.numeric(rep_len(NA, length(close)))

  out
}

nice_macd_signal <- function(close, ...) {

  out <- try(as.data.frame(TTR::MACD(close, maType="EMA"))$signal, silent = TRUE)
  if (class(out) == "try-error") out <- as.numeric(rep_len(NA, length(close)))

  out
}

nice_stoch_k <- function(high, low, close, ...) {

  out <- try(as.data.frame(TTR::stoch(cbind(high, low, close), maType="EMA", ...))$fastK, silent = TRUE)
  if (class(out) == "try-error") out <- as.numeric(rep_len(NA, length(close)))

  out
}

nice_stoch_d <- function(high, low, close, ...) {

  out <- try(as.data.frame(TTR::stoch(cbind(high, low, close), maType="EMA", ...))$fastD, silent = TRUE)
  if (class(out) == "try-error") out <- as.numeric(rep_len(NA, length(close)))

  out
}




# lists of mutate functions -----------------------------------------------
ema_fns_full <-
  list(
    ema_001 = ~nice_ema(., n = 1),
    ema_002 = ~nice_ema(., n = 2),
    ema_003 = ~nice_ema(., n = 3),
    ema_005 = ~nice_ema(., n = 5),
    ema_008 = ~nice_ema(., n = 8),
    ema_013 = ~nice_ema(., n = 13),
    ema_021 = ~nice_ema(., n = 21),
    ema_034 = ~nice_ema(., n = 34),
    ema_055 = ~nice_ema(., n = 55),
    ema_089 = ~nice_ema(., n = 89),
    ema_144 = ~nice_ema(., n = 144),
    ema_233 = ~nice_ema(., n = 233),
    ema_377 = ~nice_ema(., n = 377),
    ema_610 = ~nice_ema(., n = 610)
  )


ema_fns_near_and_mid <-
  list(
    ema_001 = ~nice_ema(., n = 1),
    ema_002 = ~nice_ema(., n = 2),
    ema_003 = ~nice_ema(., n = 3),
    ema_005 = ~nice_ema(., n = 5),
    ema_008 = ~nice_ema(., n = 8),
    ema_013 = ~nice_ema(., n = 13),
    ema_021 = ~nice_ema(., n = 21),
    ema_034 = ~nice_ema(., n = 34),
    ema_055 = ~nice_ema(., n = 55)
  )


lag_fns <-
  list(
    lag_001 = ~ lag(., n = 1),
    lag_002 = ~ lag(., n = 2),
    lag_003 = ~ lag(., n = 3),
    lag_005 = ~ lag(., n = 5),
    lag_008 = ~ lag(., n = 8),
    lag_013 = ~ lag(., n = 13),
    lag_021 = ~ lag(., n = 21),
    lag_034 = ~ lag(., n = 34),
    lag_055 = ~ lag(., n = 55)
  )










format_data <- function(d) {

  if(!"adj_close" %in% names(d)) d$adj_close <- d$close

  d1 <-
  d %>%
    select(
      symbol,
      date,
      open,
      adj_close,
      low,
      high,
      change_percent,
      volume
    ) %>%
    rename(
      close = adj_close,
      change_per = change_percent
    ) %>% group_by(symbol) %>%
    arrange(symbol, date) %>%
    mutate(
      change_per_over_close_001_days_ago = percent_change(close, lag(close, 1)),
      change_per_over_close_002_days_ago = percent_change(close, lag(close, 2)),
      change_per_over_close_003_days_ago = percent_change(close, lag(close, 3)),
      change_per_over_close_005_days_ago = percent_change(close, lag(close, 5)),
      change_per_over_close_008_days_ago = percent_change(close, lag(close, 8)),
      change_per_over_close_013_days_ago = percent_change(close, lag(close, 13)),
      change_per_over_close_021_days_ago = percent_change(close, lag(close, 21)),
      change_per_over_close_034_days_ago = percent_change(close, lag(close, 34)),
      change_per_over_close_055_days_ago = percent_change(close, lag(close, 55)),
      change_per_over_close_089_days_ago = percent_change(close, lag(close, 89)),
      change_per_over_close_144_days_ago = percent_change(close, lag(close, 144)),
      change_per_over_close_233_days_ago = percent_change(close, lag(close, 233)),
      change_per_over_close_377_days_ago = percent_change(close, lag(close, 377)),
      change_per_over_close_005_days_ago_ema_08 = nice_ema(change_per_over_close_005_days_ago, 8),
      change_per_over_close_013_days_ago_ema_21 = nice_ema(change_per_over_close_013_days_ago, 21),
      change_per_over_close_021_days_ago_ema_34 = nice_ema(change_per_over_close_021_days_ago, 34),
      change_per_over_close_021_days_ago_ema_34_lag13 = lag(change_per_over_close_021_days_ago_ema_34, 13),
      change_per_over_close_021_days_13day_increase = change_per_over_close_021_days_ago_ema_34 - change_per_over_close_021_days_ago_ema_34_lag13,
      change_per_over_close_021_days_13day_increase_lag8 = lag(change_per_over_close_021_days_13day_increase, 8),
      change_per_over_close_021_days_13day_acceration_increase = change_per_over_close_021_days_13day_increase - change_per_over_close_021_days_13day_increase_lag8,
      smoothed_velocity = change_per_over_close_021_days_ago_ema_34,
      smoothed_acceration = change_per_over_close_021_days_13day_increase,
      smoothed_jerk = change_per_over_close_021_days_13day_acceration_increase,
      macd_line = nice_macd_line(close),
      macd_signal = nice_macd_signal(close),
      stoch_k = nice_stoch_k(high, low, close) * 100,
      stoch_d = nice_stoch_d(high, low, close) * 100,
      atr = nice_atr(high, low, close),
      atrp = (atr / close) * 100,
      obv = nice_obv(close, volume),
      cci = nice_cci(high, low, close, maType="SMA"),
      cci_ema_02 = nice_ema(cci, n = 2)
    ) %>%
    mutate(
      across(starts_with("close"), ema_fns_full),
      across(matches("volume|atr|obv"), ema_fns_near_and_mid),
      across(matches("atrp_ema|obv_ema"), lag_fns),
      volume_direction = if_else(close > open, "up", "down"),
      volume_ema_diff = volume_ema_003 - volume_ema_034,
      volume_ema_diff_per = ((volume_ema_003 - volume_ema_034) / volume_ema_034) * 100,
      volume_ema_spike = volume_ema_diff >= volume_ema_034,
      total_spikes_last_001 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 1),
      total_spikes_last_002 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 2),
      total_spikes_last_003 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 3),
      total_spikes_last_005 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 5),
      total_spikes_last_008 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 8),
      total_spikes_last_013 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 13),
      total_spikes_last_021 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 21),
      total_spikes_last_034 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 34),
      total_spikes_last_055 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 55),
      total_spikes_last_089 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 89),
      total_spikes_last_144 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 144),
      total_spikes_last_233 = slide_dbl(volume_ema_spike, ~sum(.x, na.rm = TRUE), .before = 233)
    )


  d1 <- d1 %>%
    mutate(
      jump_10p_21 = nice_ema(slide_dbl(change_per_over_close_001_days_ago, ~mean(.x >= 10, na.rm = TRUE) * 100, .before = 21), 21),
      jump_10p_ = nice_ema(slide_dbl(change_per_over_close_001_days_ago, ~mean(.x >= 10, na.rm = TRUE) * 100, .before = 13), 8),
      jump_13p_ = nice_ema(slide_dbl(change_per_over_close_001_days_ago, ~mean(.x >= 13, na.rm = TRUE) * 100, .before = 13), 8),
      jump_20p = nice_ema(slide_dbl(change_per_over_close_001_days_ago, ~mean(.x >= 20, na.rm = TRUE) * 100, .before = 13), 8),
      lag_close_ema_002 =  close_ema_002 / lag(close_ema_002, n = 3),
      lag_close_ema_005 =  close_ema_005 / lag(close_ema_005, n = 8),
      lag_close_ema_013 =  close_ema_013 / lag(close_ema_013, n = 13),
      lag_close_ema_233 =  close_ema_233 / lag(close_ema_233, n = 34),
      lag_close_ema_089 =  close_ema_089 / lag(close_ema_089, n = 13),
      lag3_macd_line = lag(macd_line, 3),
      lag3_macd_ratio = macd_line / lag3_macd_line,
      ema_close_001_above_002 = close_ema_001 >  close_ema_002,
      ema_close_002_above_003 = close_ema_002 >  close_ema_003,
      ema_close_003_above_005 = close_ema_003 >  close_ema_005,
      ema_close_005_above_008 = close_ema_005 >  close_ema_008,
      ema_close_008_above_013 = close_ema_008 >  close_ema_013,
      ema_close_013_above_021 = close_ema_013 >  close_ema_021,
      ema_close_021_above_034 = close_ema_021 >  close_ema_034,
      ema_close_034_above_055 = close_ema_034 >  close_ema_055,
      ema_close_055_above_089 = close_ema_055 >  close_ema_089,
      ema_close_089_above_144 = close_ema_089 >  close_ema_144,
      ema_close_144_above_233 = close_ema_144 >  close_ema_233,
      ema_close_233_above_377 = close_ema_233 >  close_ema_377,
      ema_close_377_above_610 = close_ema_377 >  close_ema_610,
      close_ema_001_lag1 =  lag(close_ema_001),
      close_ema_002_lag1 =  lag(close_ema_002),
      close_ema_003_lag1 =  lag(close_ema_003),
      close_ema_005_lag1 =  lag(close_ema_005),
      close_ema_008_lag1 =  lag(close_ema_008),
      close_ema_013_lag1 =  lag(close_ema_013)
    ) %>%  rowwise() %>%
    mutate(
      close_sorted_all_increase = all(
        close_ema_001_lag1 <= close_ema_001,
        close_ema_002_lag1 <= close_ema_002,
        close_ema_003_lag1 <= close_ema_003,
        close_ema_005_lag1 <= close_ema_005,
        close_ema_008_lag1 <= close_ema_008,
        close_ema_013_lag1 <= close_ema_013,
        na.rm = TRUE
      ),
      close_sorted_all = all(
        ema_close_001_above_002,
        ema_close_002_above_003,
        ema_close_003_above_005,
        ema_close_005_above_008,
        ema_close_008_above_013,
        ema_close_013_above_021,
        ema_close_021_above_034,
        ema_close_034_above_055,
        ema_close_055_above_089,
        ema_close_089_above_144,
        ema_close_144_above_233,
        ema_close_233_above_377,
        ema_close_377_above_610,
        na.rm = TRUE
      ),
      close_sorted_near_term = all(
        ema_close_001_above_002,
        ema_close_002_above_003,
        ema_close_003_above_005,
        ema_close_005_above_008,
        na.rm = TRUE
      ),
      close_sorted_all_no_long_term = all(
        ema_close_001_above_002,
        ema_close_002_above_003,
        ema_close_003_above_005,
        ema_close_005_above_008,
        ema_close_008_above_013,
        ema_close_013_above_021,
        ema_close_021_above_034,
        na.rm = TRUE
      ),
      close_sorted_mid_term = all(
        ema_close_008_above_013,
        ema_close_013_above_021,
        ema_close_021_above_034,
        ema_close_034_above_055,
        ema_close_055_above_089,
        na.rm = TRUE
      ),
      close_sorted_to_89 = all(
        ema_close_001_above_002,
        ema_close_002_above_003,
        ema_close_003_above_005,
        ema_close_005_above_008,
        ema_close_008_above_013,
        ema_close_013_above_021,
        ema_close_021_above_034,
        ema_close_034_above_055,
        ema_close_055_above_089,
        #ema_close_089_above_144,
        #ema_close_144_above_233,
        na.rm = TRUE
      )
    ) %>%
    group_by(symbol) %>%
    mutate(
      mean_sorted_per = nice_ema(slide_dbl(close_sorted_to_89, ~mean(.x, na.rm = TRUE) * 100, .before = 21), n = 13),
      mean_sorted_per_34 = nice_ema(slide_dbl(close_sorted_all_no_long_term , ~mean(.x, na.rm = TRUE) * 100, .before = 13), n = 8)
    )


  d1
}



# multithread downloads ---------------------------------------------------

# build half with fmpapi:::get_base_url('cloud')
build_fmp_urls <- function(symbol, start_date = NULL, end_date = NULL, last_n = NULL, ...) {
  endpoint <- 'historical-price-full'

  query_list <- list(
    from = start_date,
    to   = end_date,
    timeseries = last_n
  )

  if (!is.null(start_date) && is.null(end_date)) query_list$to <- Sys.Date()
  if (!is.null(last_n)) {
    query_list$from  <- NULL
    query_list$to  <- NULL
  }

  request_urls <- fmpapi:::build_request_urls(symbol, endpoint = endpoint, query_list = query_list, ...)

  request_urls
}


update_fmp_data <- function(request_urls, ...) {

  con <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'stocks',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'postgres')

  map(request_urls, ~update_fmp_data_worker(.x, con = con))
}


update_fmp_data_worker <- function(request_url, con, ...) {

  tick <- Sys.time()

  d <- fmpapi:::get_request_content(request_url, endpoint = 'historical-price-full')

  if (nrow(d) > 0 & "symbol" %in% names(d)) {
    d <- format_data(d)
    DBI::dbWriteTable(con, 'price_data', d, append = TRUE)
  }

  tock <- Sys.time()
  total_time <- as.numeric(tock - tick, "secs")
  if (total_time < 1) Sys.sleep(1 - total_time) # poor mans rate limiter

  TRUE
}



# multithread profiles ----------------------------------------------------

build_fmp_profile_urls <- function(symbol, ...) {
  endpoint <- "profile"

  request_urls <- fmpapi:::build_request_urls(symbol, endpoint = endpoint, ...)

  request_urls
}


update_fmp_profile <- function(request_urls, ...) {

  con <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'stocks',
                        host = 'localhost',
                        port = 5432,
                        user = 'postgres',
                        password = 'postgres')

  map(request_urls, ~update_fmp_profile_worker(.x, con = con))
}


update_fmp_profile_worker <- function(request_url, con, ...) {

  tick <- Sys.time()

  d <- fmpapi:::get_request_content(request_url)

  if (nrow(d) > 0 & "symbol" %in% names(d)) {
    DBI::dbWriteTable(con, 'company_profile', d, append = TRUE)
  }

  tock <- Sys.time()
  total_time <- as.numeric(tock - tick, "secs")
  if (total_time < 1.5) Sys.sleep(1.5 - total_time) # poor mans rate limiter

  TRUE
}

