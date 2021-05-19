library(fmpapi)
library(tidyverse)
library(arrow)
library(furrr)
library(slider)
library(DBI)


n_workers <- 16
plan(multisession, workers = n_workers)

source("R/utils.R")

con <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'stocks',
                      host = 'localhost',
                      port = 5432,
                      user = 'postgres',
                      password = 'postgres')



company_info <- fmp_screen_stocks(
  volumeMoreThan = 500000
) %>%
  filter(
    sector %in% c("Technology", "Healthcare", "Energy", "Consumer Cyclical"),
    !exchange_short_name %in% c("TSX", "EURONEXT"),
    #price < 1000,
    price >= 0.50,
   # beta > 1 | beta == 0,
    is_etf == FALSE,
    is_actively_trading == TRUE
  ) %>%
  select(
    symbol, company_name, sector, industry, beta) %>%
  mutate(
    symbol_label = glue::glue("{symbol} - {company_name}")
  )

#names(company_info) <- c("symbol", paste0("info_", names(company_info)[2:ncol(company_info)]))

dbWriteTable(con, "company_info", company_info, overwrite = TRUE)


symbols <- company_info$symbol
symbols <- split(sample(symbols), cut(seq_along(symbols), 2, labels = FALSE))


# download company profiles -----------------------------------------------
# fmp_profile_urls <- build_fmp_profile_urls(unlist(symbols[[1]], use.names = FALSE))
# cloud_profile_urls <- build_fmp_profile_urls(unlist(symbols[[2]], use.names = FALSE), base_url = fmpapi:::get_base_url("cloud"))
#
# fmp_profile_urls <- split(sample(fmp_profile_urls), cut(seq_along(fmp_profile_urls), n_workers/2, labels = FALSE))
# cloud_profile_urls <- split(sample(cloud_profile_urls), cut(seq_along(cloud_profile_urls), n_workers/2, labels = FALSE))
#
# profile_url_list <- sample(c(fmp_profile_urls, cloud_profile_urls))
#
# dbSendQuery(con, 'DROP TABLE IF EXISTS company_profile')
#
# tick <- Sys.time()
# d_out <- future_map(profile_url_list, ~update_fmp_profile(.x), .progress = TRUE)
# tock <- Sys.time()
# print(tock - tick)



# download price history --------------------------------------------------
fmp_urls <- build_fmp_urls(unlist(symbols[[1]], use.names = FALSE), start_date = "2019-01-01")
cloud_urls <- build_fmp_urls(unlist(symbols[[2]], use.names = FALSE), start_date = "2019-01-01", base_url = fmpapi:::get_base_url("cloud"))

fmp_urls <- split(sample(fmp_urls), cut(seq_along(fmp_urls), n_workers/2, labels = FALSE))
cloud_urls <- split(sample(cloud_urls), cut(seq_along(cloud_urls), n_workers/2, labels = FALSE))

url_list <- sample(c(fmp_urls, cloud_urls))

dbSendQuery(con, 'DROP TABLE IF EXISTS price_data')

tick <- Sys.time()
d_out <- future_map(url_list, ~update_fmp_data(.x), .progress = TRUE)
tock <- Sys.time()

dbSendQuery(con, "CREATE INDEX date_idx ON price_data (date)")
dbSendQuery(con, "CREATE INDEX symbol_idx ON price_data (symbol)")
dbSendQuery(con, "CREATE INDEX symbol_date_idx ON price_data (symbol, date)")



print(tock - tick)
