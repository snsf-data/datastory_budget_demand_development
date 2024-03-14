# ---------------------------------------------------------------------------- #
# Data Analyst: Gabriel Okasa
# Script: utility functions to outsource repetitive tasks
# ---------------------------------------------------------------------------- #

# define function that calculates all desired descriptive statistics
get_descriptive_stats <- function(data_main) {
  # -------------------------------------------------------------------------- #
  # inputs:
  # - data_main: tibble dataframe with main data for which stats are calculated
  # -------------------------------------------------------------------------- #
  
  # start with the main dataset
  data_main_stats <- data_main %>%
    # summarize data by calculating the statistics for quantities of interest
    summarize(
      # total sum of amount requested
      TotalAmountRequested = sum(AmountRequested, na.rm = TRUE),
      # total sum of project years
      TotalYears = sum(DurationRequestedMonth/12, na.rm = TRUE),
      # total number of submissions
      TotalNumberSubmitted = n(),
      # amount per submission
      AmountPerSubmission = TotalAmountRequested/TotalNumberSubmitted,
      # amount per project year
      AmountPerYear = TotalAmountRequested/TotalYears,
      # average duration in months
      AverageDurationMonth = mean(DurationRequestedMonth, na.rm = TRUE),
      # number of approved proposals
      NumberApproved = sum(IsApproved, na.rm = TRUE),
      # calculate success rate as share of approved proposals
      SuccessRate = (NumberApproved/TotalNumberSubmitted)*100,
      # sum the total amount granted
      TotalAmountGranted = sum(AmountGranted, na.rm = TRUE),
      # calculate the funding rate as sum of granted / sum of requested
      FundingRate = (TotalAmountGranted/TotalAmountRequested)*100,
      # average team size
      AverageTeamSize = mean(TeamSize, na.rm = TRUE))
  
  # -------------------------------------------------------------------------- #
  # return the summarized data
  return(data_main_stats)
  # -------------------------------------------------------------------------- #
}

# ---------------------------------------------------------------------------- #
# define function that imputes the data for Eccelenza/PRIMA
impute_data <- function(data_main, data_imputed, year) {
  # -------------------------------------------------------------------------- #
  # inputs:
  # - data_main: tibble dataframe with main data to be imputed
  # - data_imputed: tibble dataframe with imputed eccelenza/prima data
  # - year: year for which the eccelenza data should be imputed
  # -------------------------------------------------------------------------- #
  
  # start with the main dataset that should be imputed
  data_main_imputed <- data_main %>%
    # mutate original data by imputing average values for given year
    mutate(
      # total sum of amount requested: add average value for year
      TotalAmountRequested = if_else(
        CallDecisionYearReporting == year,
        (TotalAmountRequested + data_imputed$TotalAmountRequested),
        TotalAmountRequested),
      # total sum of project years: add average value for year
      TotalYears = if_else(
        CallDecisionYearReporting == year,
        (TotalYears + data_imputed$TotalYears),
        TotalYears),
      # total number of submissions: add average value for year
      TotalNumberSubmitted = if_else(
        CallDecisionYearReporting == year,
        (TotalNumberSubmitted + data_imputed$TotalNumberSubmitted),
        TotalNumberSubmitted),
      # amount per submission: update the share with imputed values
      AmountPerSubmission = TotalAmountRequested/TotalNumberSubmitted,
      # amount per project year stay same: update the share with imputed values
      AmountPerYear = TotalAmountRequested/TotalYears,
      # average duration in months: proportional average imputation
      AverageDurationMonth = if_else(
        CallDecisionYearReporting == year,
        (((TotalNumberSubmitted*AverageDurationMonth) +
            (data_imputed$TotalNumberSubmitted*
               data_imputed$AverageDurationMonth))/
           (TotalNumberSubmitted + data_imputed$TotalNumberSubmitted)),
        AverageDurationMonth),
      # number of approved proposals: add average value for year
      NumberApproved = if_else(
        CallDecisionYearReporting == year,
        (NumberApproved + data_imputed$NumberApproved),
        NumberApproved),
      # success rate: update the share with imputed values
      SuccessRate = (NumberApproved/TotalNumberSubmitted)*100,
      # sum the total amount granted: add average value for year
      TotalAmountGranted = if_else(
        CallDecisionYearReporting == year,
        (TotalAmountGranted + data_imputed$TotalAmountGranted),
        TotalAmountGranted),
      # funding rate: update the share with imputed values
      FundingRate = (TotalAmountGranted/TotalAmountRequested)*100,
      # average team size: proportional average imputation
      AverageTeamSize = if_else(
        CallDecisionYearReporting == year,
        (((TotalNumberSubmitted*AverageTeamSize) +
            (data_imputed$TotalNumberSubmitted*
               data_imputed$AverageTeamSize))/
           (TotalNumberSubmitted + data_imputed$TotalNumberSubmitted)),
        AverageTeamSize)
    )
  
  # -------------------------------------------------------------------------- #
  # return the imputed data
  return(data_main_imputed)
  # -------------------------------------------------------------------------- #
}

# ---------------------------------------------------------------------------- #
# define function that scales the output statistics
scale_output_stats <- function(data_main) {
  # -------------------------------------------------------------------------- #
  # inputs:
  # - data_main: tibble dataframe with main data for which stats are scaled
  # -------------------------------------------------------------------------- #
  
  # start with the main dataset
  data_main_scaled <- data_main %>%
    # scale the amounts to millions and thousands appropriatelly
    mutate(
      # in MCHF
      TotalAmountRequested = TotalAmountRequested / 1000000,
      # in kCHF
      AmountPerYear = AmountPerYear / 1000,
      # in kCHF
      AmountPerSubmission = AmountPerSubmission / 1000,
      # in MCHF
      TotalAmountGranted = TotalAmountGranted / 1000000)
  
  # -------------------------------------------------------------------------- #
  # return the scaled data
  return(data_main_scaled)
  # -------------------------------------------------------------------------- #
}

# ---------------------------------------------------------------------------- #
# define function that produces the plots for all quantities of interest
get_plots <- function(plot_data, plot_types, lang = "en", mobile = FALSE) {
  # -------------------------------------------------------------------------- #
  # inputs:
  # - plot_data: tibble dataframe with plot data for which plots are generated
  # - plot_types: vector of strings defning the names of variables for plotting
  # - lang: string defining the language, must be one of "en", "de", "fr"
  # - mobile: logical, to determine if plots should be adapted to mobile version
  # -------------------------------------------------------------------------- #
  # define plots
  stats_plot <- lapply(plot_types, function(plot_idx) {

    # ------------------------------------------------------------------------ #
    # define first variable parts depending on the plot type
    plot_settings <- get_plot_settings(plot_type = plot_idx, lang = lang)
    
    # ------------------------------------------------------------------------ #
    # get the ggplot object
    plot_object <- get_ggplot(plot_data = plot_data,
                              plot_type = plot_idx,
                              plot_settings = plot_settings,
                              mobile = mobile)
    
    # ------------------------------------------------------------------------ #
    # get the interactive ggirage plot
    girafe_plot_object <- get_ggirafe(ggplot_object = plot_object)
    
    # return interactive plot
    return(girafe_plot_object)
  })
  
  # -------------------------------------------------------------------------- #
  # add names to the plots
  names(stats_plot) <- plot_types
  # return the list of plots
  return(stats_plot)
  # -------------------------------------------------------------------------- #
}

# ---------------------------------------------------------------------------- #
# define function that produces the ggplot object for the main figure
get_ggplot_main <- function(plot_data, mobile = FALSE) {
  # -------------------------------------------------------------------------- #
  # inputs:
  # - plot_data: tibble dataframe with plot data for which plots are generated
  # - mobile: logical, to determine if plots should be adapted to mobile version
  # -------------------------------------------------------------------------- #
  ggplot_object <- ggplot(plot_data, aes(
    # define aesthetics
    x = CallDecisionYearReporting,
    y = TotalAmount,
    color = FundingSource,
    group = FundingSource,
    # Define tooltip text for ggiraph
    tooltip = glue(
      "{FundingSource}<br>",
      "{x_name}: {CallDecisionYearReporting}<br>",
      "{y_lab}: <b>{print_num(round(TotalAmount, 1))}</b>"
    ),
    # Highlight all of the points with the same color when hovering
    # over it (ggiraph)
    data_id =  rownames(plot_data))) +
    # add line plot with points for observations
    geom_line(show.legend = FALSE, linewidth = 1.5) +
    geom_point_interactive(size = 2.5) +
    # place legend
    guides(color = guide_legend(nrow = if_else(mobile, 2, 1))) +
    # add axes of the plot
    labs(x = "", y = y_lab) +
    # add snsf color scheme
    scale_color_manual(values = get_datastory_scheme()) +
    # scale the x axis min max
    scale_x_continuous(breaks = seq(min(plot_data$CallDecisionYearReporting),
                                    max(plot_data$CallDecisionYearReporting),
                                    if_else(mobile, 2, 1))) +
    # scale the y axis
    scale_y_continuous(limits = c(0, max(plot_data$TotalAmount) + 400),
                       breaks = seq(0, max(plot_data$TotalAmount) + 500, 500)) +
    # and apply snsf theme
    get_datastory_theme(tick_axis = "x", gridline_axis = "y", family = "Theinhardt")
  
  # depending on mobile version, specify legend size
  if (mobile) {
    # define larger sizes of text for axes and legends
    ggplot_object <- ggplot_object +
      # specify legend position
      theme(legend.position = "top",
            axis.title.y = element_text(size = 16,
                                        margin = margin(0, 10, 0, 0)),
            axis.title.x = element_text(size = 16,
                                        margin = margin(0, 0, 10, 0)),
            axis.text.y = element_text(size = 14.5),
            axis.text.x = element_text(size = 14.5),
            legend.text = element_text(size = 16))
  } else {
    # define standard sizes of text for axes and legends
    ggplot_object <- ggplot_object +
      # specify legend position
      theme(legend.position = "top",
            axis.title.y = element_text(size = 12,
                                        margin = margin(0, 10, 0, 0)),
            axis.title.x = element_text(size = 12,
                                        margin = margin(0, 0, 10, 0)),
            axis.text.y = element_text(size = 10.5),
            axis.text.x = element_text(size = 10.5),
            legend.text = element_text(size = 10.5))
  }
  
  # -------------------------------------------------------------------------- #
  # return the ggplot object
  return(ggplot_object)
  # -------------------------------------------------------------------------- #
}

# ---------------------------------------------------------------------------- #
# define function that produces the ggplot object for all subfigures
get_ggplot <- function(plot_data, plot_type, plot_settings, mobile) {
  # -------------------------------------------------------------------------- #
  # inputs:
  # - plot_data: tibble dataframe with plot data for which plots are generated
  # - plot_type: string defining the name of variables for plotting
  # - plot_settings:list of plot settings such as axis labels and spacing
  # - mobile: logical, to determine if plots should be adapted to mobile version
  # -------------------------------------------------------------------------- #
  
  # and extract the plot settings parts
  x_name <- plot_settings$x_name
  y_lab <- plot_settings$y_lab
  n_limit <- plot_settings$n_limit
  n_break <- plot_settings$n_break
  
  # -------------------------------------------------------------------------- #
  # create ggplot object base don the settings
  ggplot_object <- plot_data %>%
    # rename plot_idx to inject the tidy evaluation
    rename(plot_type := {{ plot_type }} ) %>%
    # define the ggplot object
    ggplot(aes(
      # define aesthetics
      x = CallDecisionYearReporting,
      y = plot_type,
      color = FundingInstrument,
      group = FundingInstrument)
    ) +
    # add line plot with points for observations
    geom_line(show.legend = FALSE, linewidth = 1.5) +
    geom_point_interactive(
      aes(
        tooltip = glue(
          "{FundingInstrument}<br>",
          "{x_name}: {CallDecisionYearReporting}<br>",
          "{y_lab}: <b>{print_num(round(plot_type, 1))}</b>"
        ),
        # Highlight all of the points with the same color when hovering
        # over it (ggiraph)
        data_id =  rownames(plot_data)
      ),
      size = 2.5
    ) +
    # place legend
    guides(color = guide_legend(nrow = 1)) +
    # add axes of the plot
    labs(x = "", y = y_lab) +
    # add snsf color scheme
    scale_color_manual(values = c(get_datastory_scheme()[4],
                                  get_datastory_scheme()[5],
                                  snsf_grays[1])) +
    # scale the x axis min max
    scale_x_continuous(breaks = seq(min(plot_data$CallDecisionYearReporting),
                                    max(plot_data$CallDecisionYearReporting),
                                    if_else(mobile, 2, 1))) +
    # scale the y axis
    scale_y_continuous(limits = c(0, max(plot_data[[plot_type]]) + n_limit),
                       breaks = seq(0, max(plot_data[[plot_type]]) + n_limit,
                                    n_break)) +
    # and apply snsf theme
    get_datastory_theme(tick_axis = "x", gridline_axis = "y", family = "Theinhardt")
  
    # depending on mobile version, specify legend size
    if (mobile) {
      # define larger sizes of text for axes and legends
      ggplot_object <- ggplot_object +
        # specify legend position
        theme(legend.position = "top",
              axis.title.y = element_text(size = 14,
                                          margin = margin(0, 10, 0, 0)),
              axis.title.x = element_text(size = 14,
                                          margin = margin(0, 0, 10, 0)),
              axis.text.y = element_text(size = 12.5),
              axis.text.x = element_text(size = 12.5),
              legend.text = element_text(size = 12.5))
    } else {
      # define standard sizes of text for axes and legends
      ggplot_object <- ggplot_object +
        # specify legend position
        theme(legend.position = "top",
              axis.title.y = element_text(size = 12,
                                          margin = margin(0, 10, 0, 0)),
              axis.title.x = element_text(size = 12,
                                          margin = margin(0, 0, 10, 0)),
              axis.text.y = element_text(size = 10.5),
              axis.text.x = element_text(size = 10.5),
              legend.text = element_text(size = 10.5))
    }
  
  # -------------------------------------------------------------------------- #
  # return the ggplot object
  return(ggplot_object)
  # -------------------------------------------------------------------------- #
}


# ---------------------------------------------------------------------------- #
# define function that produces the ggplot object
get_ggirafe <- function(ggplot_object) {
  # -------------------------------------------------------------------------- #
  # inputs:
  # - ggplot_object: objetc of class ggplot as basis for interactive plot
  # -------------------------------------------------------------------------- #
  
  # Create ggiraph object for interactive graph
  girafe_plot_object <- girafe(
    # pass in the ggplot object
    ggobj = ggplot_object,
    # define height
    height_svg = 4,
    # and further options
    options = list(
      opts_toolbar(saveaspng = FALSE),
      opts_hover(
        css =
          paste0("font-family:", font, ";fill:#c95b40;stroke:white;")),
      opts_tooltip(
        css = 
          str_replace(
            get_ggiraph_tooltip_css(),
            "(font-family:')([\\w\\s]*)(';)",
            paste0("\\1", font, "\\3")
          ),
        opacity = 0.8,
        delay_mouseover = 0,
        delay_mouseout = 0
      )
    )
  )
  
  # -------------------------------------------------------------------------- #
  # return the ggirafe interactive plot
  return(girafe_plot_object)
  # -------------------------------------------------------------------------- #
}

# ---------------------------------------------------------------------------- #
# define function that produces the plot settings for all quantities of interest
get_plot_settings <- function(plot_type, lang = "en") {
  # -------------------------------------------------------------------------- #
  # inputs:
  # - plot_type: string defining the name of variables for plotting
  # - lang: string defining the language, must be one of "en", "de", "fr"
  # -------------------------------------------------------------------------- #
  
  if (plot_type == "TotalAmountRequested") {
    # for total amounts requested, depending on language
    y_lab <- switch(lang,
                    en = "CHF million",
                    de = "Mio. CHF",
                    fr = "mio CHF")
    n_limit <- 400
    n_break <- 500
    
  } else if (plot_type == "AmountPerYear") {
    # for amounts per proposal year
    y_lab <- switch(lang,
                    en = "CHF thousand",
                    de = "tausend CHF",
                    fr = "mille CHF")
    n_limit <- 50
    n_break <- 50
    
  } else if (plot_type == "TotalNumberSubmitted") {
    # for total number of submissions
    y_lab <- switch(lang,
                    en = "Submissions",
                    de = "Gesuche",
                    fr = "Requêtes")
    n_limit <- 1100
    n_break <- 1000
    
  } else if (plot_type == "AmountPerSubmission") {
    # for amounts requested per submission
    y_lab <- switch(lang,
                    en = "CHF thousand",
                    de = "tausend CHF",
                    fr = "mille CHF")
    n_limit <- 100
    n_break <- 100
    
  } else if (plot_type == "AverageDurationMonth") {
    # for average duration in months
    y_lab <- switch(lang,
                    en = "Months",
                    de = "Monate",
                    fr = "Mois")
    n_limit <- 6
    n_break <- 10
    
  } else if (plot_type == "SuccessRate") {
    # for success/funding rates
    y_lab <- switch(lang,
                    en = "Share in %",
                    de = "Anteil in %",
                    fr = "Part en %")
    n_limit <- 5
    n_break <- 10
    
  } else if (plot_type == "FundingRate") {
      # for success/funding rates
      y_lab <- switch(lang,
                      en = "Share in %",
                      de = "Anteil in %",
                      fr = "Part en %")
      n_limit <- 11
      n_break <- 10
    
  } else if (plot_type == "AverageTeamSize") {
    # for average team size
    y_lab <- switch(lang,
                    en = "Persons",
                    de = "Personen",
                    fr = "Personnes")
    n_limit <- 1
    n_break <- 1
    
  }
  
  # -------------------------------------------------------------------------- #
  # x_name is always the same, just in different languages
  x_name <- switch(lang,
                   en = "Year",
                   de = "Jahr",
                   fr = "Année")
  
  # -------------------------------------------------------------------------- #
  # wrap the results into a named list
  settings <- list(x_name, y_lab, n_limit, n_break)
  names(settings) <- c("x_name", "y_lab", "n_limit", "n_break")
  # return the list parameters for plotting
  return(settings)
  # -------------------------------------------------------------------------- #
}

