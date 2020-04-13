library (readr)
library(plyr)
library(plotly)
library(dplyr)


# get the data from the john hopkins github repository (data provided by who)
confirmed <-
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

confirmed_data <-
  as.data.frame(read_csv(url(confirmed), col_types = cols()))

# there are same country multiple times (specially the countries big and with multiple regions), create unique name
# by merging country name and region name
confirmed_data$`Country/Region` <-
  data.table::fifelse(
    is.na(confirmed_data$`Province/State`),
    confirmed_data$`Country/Region`,
    paste0(
      confirmed_data$`Country/Region`,
      " ",
      "(",
      confirmed_data$`Province/State`,
      ")"
    )
  )

deaths <-
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

death_data <-
  as.data.frame(read_csv(url(deaths), col_types = cols()))

death_data$`Country/Region` <-
  data.table::fifelse(
    is.na(death_data$`Province/State`),
    death_data$`Country/Region`,
    paste0(
      death_data$`Country/Region`,
      " ",
      "(",
      death_data$`Province/State`,
      ")"
    )
  )

recovered <-
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

recover_data <-
  as.data.frame(read_csv(url(recovered), col_types = cols()))

recover_data$`Country/Region` <-
  data.table::fifelse(
    is.na(recover_data$`Province/State`),
    recover_data$`Country/Region`,
    paste0(
      recover_data$`Country/Region`,
      " ",
      "(",
      recover_data$`Province/State`,
      ")"
    )
  )


# get the global summary
confirmed_cases <- sum(confirmed_data[, ncol(confirmed_data)])
deaths <- sum(death_data[, ncol(death_data)])
recovered <- sum(recover_data[, ncol(recover_data)])

# filter only bangladesh data
bangladesh_confirmed <-
  filter(confirmed_data, confirmed_data[, 2] == "Bangladesh")

bangladesh_death <-
  filter(death_data, death_data[, 2] == "Bangladesh")

bangladesh_recover <-
  filter(recover_data, recover_data[, 2] == "Bangladesh")

bangladesh_data <-
  bind_rows(bangladesh_confirmed, bangladesh_death, bangladesh_recover)

# keep only the required columns
bangladesh_data <-
  select(bangladesh_data, c(5:ncol(bangladesh_data)))

# turn the data long from wide
bangladesh_data <- as.data.frame(t(bangladesh_data))

# rename the columns
bangladesh_data <-
  rename(
    bangladesh_data,
    Confirmed = V1,
    Deaths = V2,
    Recovered = V3
  )

# get the dates and format the column
bangladesh_data$Date <- row.names(bangladesh_data)

bangladesh_data$Date <-
  as.Date(bangladesh_data$Date, format = "%m/%d/%y")

bangladesh_data <- na_if(bangladesh_data, 0)


# plot bangladesh time series
fig <-
  plot_ly(
    bangladesh_data,
    x = ~ Date,
    y = ~ Confirmed,
    name = 'Confirmed Cases in Bangladesh',
    type = 'scatter',
    mode = 'lines'
  ) %>% layout(title = "Corona Cases in Bangaldesh", yaxis = list(title = "Count"))
fig <-
  fig %>% add_trace(y = ~ Deaths,
                    name = 'Deaths in Bangladesh',
                    mode = 'lines')
fig <-
  fig %>% add_trace(y = ~ Recovered,
                    name = 'Recovered Cases in Bangladesh',
                    mode = 'lines')


# plot the summary bar plot
figB <- plot_ly(
  x = c("Confirmed", "Deaths", "Recovered"),
  y = unlist(c(bangladesh_data[nrow(bangladesh_data), c(1:3)])),
  name = paste("Data as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]),
  type = "bar"
) %>% layout(xaxis = list(title = paste(
  "Situation of Bangladesh as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]
)))

# plot the global bar plot
figG <- plot_ly(
  x = c("Confirmed", "Deaths", "Recovered"),
  y = c(confirmed_cases, deaths, recovered),
  name = paste("Global Situation as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]),
  type = "bar",
  marker = list(color = "#F95959"),
) %>% layout(xaxis = list(title = paste("Global Situation as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)])))


# compare the situation of bangladesh with other countries
comparison <- function(countries) {
  # filter the data of the given countries
  world_confirmed <-
    filter(confirmed_data,
           confirmed_data[, 2] %in% c("Bangladesh", countries))
  
  world_death <-
    filter(death_data, death_data[, 2] %in% c("Bangladesh", countries))
  
  world_recover <-
    filter(recover_data, recover_data[, 2] %in% c("Bangladesh", countries))
  
  # bring the data into right format
  # confirmed cases
  world_confirmed <- as.data.frame(t(world_confirmed))
  
  world_conf <- world_confirmed[5:nrow(world_confirmed),]
  
  names(world_conf) <- unlist(c(world_confirmed[2, ]))
  
  world_conf$Date <- row.names(world_conf)
  
  world_conf$Date <-
    as.Date(world_conf$Date, format = "%m/%d/%y")
  
  world_conf[, 1:(ncol(world_conf) - 1)] <-
    data.frame(apply(world_conf[, 1:(ncol(world_conf) - 1)], 2, as.numeric))
  
  ## get the date of first confirmed case
  wc <- world_conf[, 1:(ncol(world_conf) - 1)]
  
  a <- apply(wc, 1, sum)
  
  w <- which(a > 0)[1]
  
  mindate <- world_conf$Date[w]
  
  # get rid of the 0 values with NA
  world_conf_mod <- na_if(world_conf, 0)
  
  
  # fatality
  world_death <- as.data.frame(t(world_death))
  
  world_dead <- world_death[5:nrow(world_death),]
  
  names(world_dead) <- unlist(c(world_death[2, ]))
  
  world_dead$Date <- row.names(world_dead)
  
  world_dead$Date <-
    as.Date(world_dead$Date, format = "%m/%d/%y")
  
  world_dead[, 1:(ncol(world_dead) - 1)] <-
    data.frame(apply(world_dead[, 1:(ncol(world_dead) - 1)], 2, as.numeric))
  
  ## get the date of the first death
  wd <- world_dead[, 1:(ncol(world_dead) - 1)]
  
  d <- apply(wd, 1, sum)
  
  wd <- which(d > 0)[1]
  
  mindateD <- world_dead$Date[wd]
  
  # get rid of the 0 values with NA
  world_dead_mod <- na_if(world_dead, 0)
  
  
  # recovered cases
  world_recover <- as.data.frame(t(world_recover))
  
  world_recov <- world_recover[5:nrow(world_recover),]
  
  names(world_recov) <- unlist(c(world_recover[2, ]))
  
  world_recov$Date <- row.names(world_recov)
  
  world_recov$Date <-
    as.Date(world_recov$Date, format = "%m/%d/%y")
  
  world_recov[, 1:(ncol(world_recov) - 1)] <-
    data.frame(apply(world_recov[, 1:(ncol(world_recov) - 1)], 2, as.numeric))
  
  ## get the date of first recovered case
  wr <- world_recov[, 1:(ncol(world_recov) - 1)]
  
  r <- apply(wr, 1, sum)
  
  wr <- which(r > 0)[1]
  
  mindateR <- world_recov$Date[wr]
  
  # get rid of the 0 values with NA
  world_recov_mod <- na_if(world_recov, 0)
  
  
  # plot the comparison time series
  # confirmed cases
  fig_confirm <-
    plot_ly(
      world_conf_mod,
      x = ~ Date,
      y = ~ world_conf_mod[, 1],
      name = names(world_conf_mod)[1],
      type = "scatter",
      mode = "lines",
      width = 1050
    ) %>% layout(
      title = "Cumulative Confirmed Cases",
      xaxis = list(range = c(mindate, max(
        world_conf_mod$Date
      ))),
      yaxis = list(title = "Cumulative Confirmed Cases")
    )
  
  for (trace in colnames(world_conf_mod)[2:(ncol(world_conf_mod) - 1)]) {
    fig_confirm <-
      fig_confirm %>% plotly::add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
  }
  
  # fatalities
  fig_dead <-
    plot_ly(
      world_dead_mod,
      x =  ~ Date,
      y =  ~ world_dead_mod[, 1],
      name = names(world_dead_mod)[1],
      type = "scatter",
      mode = "lines",
      width = 1050
    ) %>% layout(
      title = "Cumulative Deaths",
      xaxis = list(range = c(mindateD, max(
        world_dead_mod$Date
      ))),
      yaxis = list(title = "Cumulative Deaths")
    )
  
  for (trace in colnames(world_dead_mod)[2:(ncol(world_dead_mod) - 1)]) {
    fig_dead <-
      fig_dead %>% plotly::add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
  }
  
  # recovered cases
  fig_recov <-
    plot_ly(
      world_recov_mod,
      x =   ~ Date,
      y =  ~ world_recov_mod[, 1],
      name = names(world_recov_mod)[1],
      type = "scatter",
      mode = "lines",
      width = 1050
    ) %>% layout(
      title = "Cumulative Recovered Cases",
      xaxis = list(range = c(mindateR, max(
        world_recov_mod$Date
      ))),
      yaxis = list(title = "Cumulative Recovered Cases")
    )
  
  for (trace in colnames(world_recov_mod)[2:(ncol(world_recov_mod) - 1)]) {
    fig_recov <-
      fig_recov %>% plotly::add_trace(y = as.formula(paste0("~`", trace, "`")), name = trace)
  }
  
  # comparative chart since first detected case
  max_case <-
    c(1:max(colSums(world_conf[, 1:(ncol(world_conf) - 1)] != 0)))
  
  fig_confirm_S <-
    plot_ly(
      world_conf,
      x = ~ max_case,
      y = ~ c(world_conf[, 1][world_conf[, 1] != 0],
              rep(NA, (
                length(max_case) - length(world_conf[, 1][world_conf[, 1] != 0])
              ))),
      name = names(world_conf)[1],
      type = "scatter",
      mode = "lines",
      width = 1050
    ) %>% layout(
      title = "Cumulative Confirmed Cases since the First Case was Confirmed",
      xaxis = list(range = c(1, length(max_case)), title = "Days Since First Confirmed Case"),
      yaxis = list(title = "Cumulative Confirmed Cases")
    )
  
  for (trace in colnames(world_conf)[2:(ncol(world_conf) - 1)]) {
    fig_confirm_S <-
      fig_confirm_S %>% plotly::add_trace(y = c(world_conf[, trace][world_conf[, trace] !=
                                                                      0], rep(NA, (
                                                                        length(max_case) - length(world_conf[, trace][world_conf[, trace] != 0])
                                                                      ))), name = trace)
  }
  
  # comparative death chart since first detected case
  max_case_D <-
    c(1:max(colSums(world_dead[, 1:(ncol(world_dead) - 1)] != 0)))
  
  fig_confirm_D <-
    plot_ly(
      world_dead,
      x = ~ max_case_D,
      y = ~ c(world_dead[, 1][world_dead[, 1] != 0],
              rep(NA, (
                length(max_case_D) - length(world_dead[, 1][world_dead[, 1] != 0])
              ))),
      name = names(world_dead)[1],
      type = "scatter",
      mode = "lines",
      width = 1050
    ) %>% layout(
      title = "Cumulative Deaths since the First Fatality Occured",
      xaxis = list(range = c(1, length(max_case_D)), title = "Days Since First Death"),
      yaxis = list(title = "Cumulative Deaths")
    )
  
  for (trace in colnames(world_dead)[2:(ncol(world_dead) - 1)]) {
    fig_confirm_D <-
      fig_confirm_D %>% plotly::add_trace(y = c(world_dead[, trace][world_dead[, trace] !=
                                                                      0], rep(NA, (
                                                                        length(max_case_D) - length(world_dead[, trace][world_dead[, trace] != 0])
                                                                      ))), name = trace)
  }
  
  # get the recover to death ratio data frame and get the plot
  recov_death <-
    world_recov[, 1:(ncol(world_recov) - 1)] / world_dead[, 1:(ncol(world_dead) - 1)]
  recov_death[is.na(recov_death)] <- 0
  recov_death[recov_death == Inf] <- 0
  recov_death$Date <- row.names(recov_death)
  recov_death$Date <-
    as.Date(recov_death$Date, format = "%m/%d/%y")
  
  
  fig_Ratio <-
    plot_ly(
      recov_death,
      x = ~ max_case_D,
      y = ~ c(recov_death[, 1][recov_death[, 1] != 0],
              rep(NA, (
                length(max_case_D) - length(recov_death[, 1][recov_death[, 1] != 0])
              ))),
      name = names(recov_death)[1],
      type = "scatter",
      mode = "lines",
      width = 1050
    ) %>% layout(
      title = "Cumulative Recovery to Cumulative Death since the First Fatality Occured",
      xaxis = list(range = c(1, length(max_case_D)), title = "Days Since First Death"),
      yaxis = list(title = "Cumulative Recovery to Cumulative Death")
    )
  
  for (trace in colnames(recov_death)[2:(ncol(recov_death) - 1)]) {
    fig_Ratio <-
      fig_Ratio %>% plotly::add_trace(y = c(recov_death[, trace][recov_death[, trace] !=
                                                                   0], rep(NA, (
                                                                     length(max_case_D) - length(recov_death[, trace][recov_death[, trace] != 0])
                                                                   ))), name = trace)
  }
  
  # generate the case fatality rate plot (dead/confirmed) daily and cumulative basis
  dead_conf <-
    world_dead[, 1:(ncol(world_dead) - 1)] / world_conf[, 1:(ncol(world_conf) - 1)] * 100
  dead_conf[is.na(dead_conf)] <- 0
  dead_conf[dead_conf == Inf] <- 0
  dead_conf$Date <- row.names(dead_conf)
  dead_conf$Date <-
    as.Date(dead_conf$Date, format = "%m/%d/%y")
  
  # create the daily death series
  dead_daily <-
    data.frame(sapply(world_dead[, 1:(ncol(world_dead) - 1)], function(x)
      diff(x)))
  
  # create the daily confirmed series
  conf_daily <-
    data.frame(sapply(world_conf[, 1:(ncol(world_conf) - 1)], function(x)
      diff(x)))
  
  # create the daily death to conf series
  dead_conf_daily <- dead_daily / conf_daily * 100
  
  # cleanse the data
  dead_conf_daily[is.na(dead_conf_daily)] <- 0
  
  dead_conf_daily[dead_conf_daily == Inf] <- 0
  
  dead_conf_daily[dead_conf_daily < 0] <- 0
  
  # now generate the plots
  fig_cfr <- list()
  
  for (i in 1:(ncol(dead_conf_daily))) {
    d_trace <- which(dead_conf_daily[, i] != 0)[1]
    
    fig_cfr[[i]] <-
      plotly_build(
        plot_ly(
          dead_conf,
          x = ~ max_case_D,
          y = ~ tail(dead_conf[, i], length(max_case_D)),
          name = paste("Cumulative CFR", names(dead_conf_daily)[i]),
          type = "scatter",
          mode = "lines",
          width = 1050
        ) %>% layout(
          showlegend = T,
          title = "Case Fatality Rate (%)",
          xaxis = list(range = c(1, colSums(dead_conf != 0)[i]), title = "Days Since First Seath"),
          yaxis = list(title = names(dead_conf_daily)[i])
        ) %>% add_trace(
          y =  ~ tail(dead_conf_daily[, i], length(max_case_D)),
          name = paste("Daily CFR", names(dead_conf_daily)[i])
        )
      )
  }
  
  fig_cfr_print <-
    subplot(
      fig_cfr,
      nrows = ncol(dead_conf_daily),
      titleY = T,
      titleX = T
    )
  
  return(
    list(
      fig_confirm,
      fig_dead,
      fig_recov,
      fig_confirm_S,
      fig_confirm_D,
      fig_Ratio,
      fig_cfr_print
    )
  )
  
}

# name the desired countries
countries <- c("India", "Pakistan", "South Africa")

# run the function
comparison(countries)

fig
figB
figG
