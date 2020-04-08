shinyServer(function(input, output) {
  corona_visual <- function(countries) {
    # plot the global bar plot
    figG <- plot_ly(
      x = c("Confirmed", "Deaths", "Recovered"),
      y = c(confirmed_cases, deaths, recovered),
      text = c(confirmed_cases, deaths, recovered),
      textposition = "auto",
      name = paste("Global Situation as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]),
      type = "bar",
      marker = list(color = "#0072B2"),
      width = 800
    ) %>% layout(xaxis = list(title = paste(
      "Global Situation as of", bangladesh_data[nrow(bangladesh_data), ncol(bangladesh_data)]
    )))
    
    # plot the global time series
    global_time <-
      plot_ly(
        global_conf_u,
        x = ~ Date,
        y = ~ global_conf_u$global,
        name = "Confirmed",
        type = "scatter",
        mode = "lines",
        width = 800
      ) %>% layout(title = "COVID-19 TIME SERIES",
                   yaxis = list(title = "Count"))
    
    global_time <-
      global_time %>% plotly::add_trace(
        global_death_u,
        x =  ~ Date,
        y =  ~ global_death_u$global,
        name = "Death"
      )
    
    global_time <-
      global_time %>% add_trace(
        global_recov_u,
        x =  ~ Date,
        y =  ~ global_recov_u$global,
        name = "Recovery"
      )
    
    # recovery to death ratio of the world
    global_recov_dead <-
      plot_ly(
        global_recov_u,
        x = ~ Date,
        y = ~ global_recov_u$global / global_death_u$global * 100,
        name = "Global Recovery to Death Ratio",
        type = "scatter",
        mode = "lines",
        width = 800
      ) %>% layout(title = "Global Recovery to Death Ratio",
                   yaxis = list(title = "Recovery to Death (%)"))
    
    
    # plot cfr of the world
    global_death_u <-
      mutate(global_death_u, daily_case = c(NA, diff(global_death_u[, ncol(global_death_u)])))
    global_conf_u <-
      mutate(global_conf_u, daily_case = c(NA, diff(global_conf_u[, ncol(global_conf_u)])))
    
    global_cfr <-
      plot_ly(
        global_conf_u,
        x = ~ Date,
        y = ~ global_death_u$global / global_conf_u$global * 100,
        name = "Cumulative CFR",
        type = "scatter",
        mode = "lines",
        width = 800
      ) %>% layout(title = "Case Fatality Rate (%)",
                   yaxis = list(title = "CFR (%)"))
    
    global_cfr <-
      global_cfr %>% plotly::add_trace(
        global_conf_u,
        x =  ~ Date,
        y =  ~ global_death_u$daily_case / global_conf_u$daily_case * 100,
        name = "Daily CFR"
      )
    
    # % change over 3 days
    global_conf_u <-
      mutate(global_conf_u, change = c(rep(NA, 3), diff(global, 3) / global[1:(nrow(global_conf_u) -
                                                                                 3)]))
    global_death_u <-
      mutate(global_death_u, change = c(rep(NA, 3), diff(global, 3) / global[1:(nrow(global_death_u) -
                                                                                  3)]))
    
    global_p <-
      plot_ly(
        global_conf_u,
        x = ~ Date,
        y = ~ change * 100,
        name = "New Cases",
        type = "scatter",
        mode = "lines",
        width = 800
      ) %>% layout(title = "% Change over 3 Days",
                   yaxis = list(title = "% Change"))
    
    global_p <-
      global_p %>% plotly::add_trace(
        global_death_u,
        x =  ~ Date,
        y =  ~ global_death_u$change * 100,
        name = "Deaths"
      )
    
    
    # compare the situation of bangladesh with other countries
    # filter the data of the given countries
    world_confirmed <-
      filter(confirmed_data,
             confirmed_data[, 2] %in% c("Bangladesh", countries))
    
    world_death <-
      filter(death_data, death_data[, 2] %in% c("Bangladesh", countries))
    
    world_recover <-
      filter(recover_data,
             recover_data[, 2] %in% c("Bangladesh", countries))
    
    # bring the data into right format
    # confirmed cases
    world_confirmed <- as.data.frame(t(world_confirmed))
    
    world_conf <- world_confirmed[5:nrow(world_confirmed), ]
    
    names(world_conf) <- unlist(c(world_confirmed[2,]))
    
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
    
    world_dead <- world_death[5:nrow(world_death), ]
    
    names(world_dead) <- unlist(c(world_death[2,]))
    
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
    
    world_recov <- world_recover[5:nrow(world_recover), ]
    
    names(world_recov) <- unlist(c(world_recover[2,]))
    
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
        width = 800
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
        width = 800
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
        width = 800
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
        width = 800
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
        width = 800
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
    
    # get the recover toe death ratio data frame and get the plot
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
        width = 800
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
    
    
    # create the daily confirmed case series
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
            x = ~ c(1:colSums(dead_conf != 0)[i]),
            y = ~ c(dead_conf[, i][dead_conf[, i] != 0]),
            name = paste("Cumulative CFR", names(dead_conf_daily)[i]),
            type = "scatter",
            mode = "lines",
            width = 800,
            line = list(color = "#56B4E9")
          ) %>% layout(
            showlegend = T,
            title = "Case Fatality Rate (%)",
            xaxis = list(range = c(1, colSums(dead_conf != 0)[i]), title = "Days Since First Death"),
            yaxis = list(title = names(dead_conf_daily)[i])
          ) %>% add_trace(
            y =  ~ dead_conf_daily[d_trace:nrow(dead_conf_daily), i],
            name = paste("Daily CFR", names(dead_conf_daily)[i]),
            line = list(color = "#D55E00")
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
        figG = figG,
        global_time = global_time,
        global_recov_dead = global_recov_dead,
        global_cfr = global_cfr,
        global_p = global_p,
        fig_confirm = fig_confirm,
        fig_dead = fig_dead,
        fig_recov = fig_recov,
        fig_confirm_S = fig_confirm_S,
        fig_confirm_D = fig_confirm_D,
        fig_Ratio = fig_Ratio,
        fig_cfr_print = fig_cfr_print
      )
    )
    
  }
  
  # generate the outputs
  
  output$figG <-
    renderPlotly({
      corona_visual(input$countries)$figG
    })
  
  output$global_time <-
    renderPlotly({
      corona_visual(input$countries)$global_time
    })
  
  output$global_recov_dead <-
    renderPlotly({
      corona_visual(input$countries)$global_recov_dead
    })
  
  output$global_cfr <-
    renderPlotly({
      corona_visual(input$countries)$global_cfr
    })
  
  output$global_p <-
    renderPlotly({
      corona_visual(input$countries)$global_p
    })
  
  output$fig_confirm <-
    renderPlotly({
      corona_visual(input$countries)$fig_confirm
    })
  
  output$fig_dead <-
    renderPlotly({
      corona_visual(input$countries)$fig_dead
    })
  
  output$fig_recov <-
    renderPlotly({
      corona_visual(input$countries)$fig_recov
    })
  
  output$fig_confirm_S <-
    renderPlotly({
      corona_visual(input$countries)$fig_confirm_S
    })
  
  output$fig_confirm_D <-
    renderPlotly({
      corona_visual(input$countries)$fig_confirm_D
    })
  
  output$fig_Ratio <-
    renderPlotly({
      corona_visual(input$countries)$fig_Ratio
    })
  
  output$fig_cfr_print <-
    renderPlotly({
      corona_visual(input$countries)$fig_cfr_print
    })
})