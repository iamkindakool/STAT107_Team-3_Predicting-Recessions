install.packages(c("fredr", "httr", "jsonlite", "ggplot2"))


library(fredr)
library(httr)
library(jsonlite)
library(ggplot2)


# FRED
fredr_set_key("c859345561941fbef52080a4712c116b")

# BLS  (name it BLS_KEY)
Sys.setenv(BLS_KEY = "60c128da9f8442de96caa1279e03d078")


usrec <- fredr(series_id = "USREC")
head(usrec)

payload <- list(
  seriesid = list("LNS14000000"),
  startyear = "2020",
  endyear   = "2024",
  ## we can go with a bigger sample but I used this as a test
  registrationkey = Sys.getenv("BLS_KEY")
)

res  <- POST("https://api.bls.gov/publicAPI/v2/timeseries/data/", body = payload, encode = "json")
stop_for_status(res)
json <- content(res, as = "parsed", type = "application/json")

# Parse to data.frame (fixed parentheses/commas)
dat <- json$Results$series[[1]]$data
df <- data.frame(
  year   = as.integer(sapply(dat, `[[`, "year")),
  period =              sapply(dat, `[[`, "period"),
  value  = as.numeric(sapply(dat, `[[`, "value")),
  stringsAsFactors = FALSE
)

# Keep monthly only and add Date
df <- df[grepl("^M\\d\\d$", df$period), ]
df$date <- as.Date(sprintf("%s-%02d-01", df$year, as.integer(sub("^M", "", df$period))))
df <- df[order(df$date), c("date","value")]

# Save
write.csv(df, "unemployment_rate.csv", row.names = FALSE)

getwd()  # shows the folder where the CSV was written
