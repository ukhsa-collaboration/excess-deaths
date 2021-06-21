mon_fri  <- function(x) {
  mondays <- scales::fullseq(x, "1 week")
  fridays <- mondays - lubridate::days(3)
  sort(c(mondays, fridays))
}

fri <- function(x) {
  mondays <- scales::fullseq(x, "1 week")
  fridays <- mondays - lubridate::days(3)
  fridays <- sort(c(fridays))
  if ((length(fridays) %% 2) == 0) {
    fridays <- fridays[c(FALSE, TRUE)]
  } else {
    fridays <- fridays[c(TRUE, FALSE)]
  }
  
  return(fridays)
}

cumulative_excess_deaths <- function(death_data, area_name, cause_name = "all cause",
                                     date_field, model_field, deaths_field, covid_field,
                                     start_date = as.Date("2020-03-20"), line_size = 1,
                                     caption = "", include_covid_ribbon = TRUE, subtitle = "",
                                     include_text_boxes = FALSE, include_monday_final_point = FALSE,
                                     end_date) {
  
  if (cause_name == "all cause") {
    title <- paste0("Cumulative deaths since ",
                    format(start_date, "%d %B %Y"),
                    ", by date of registration, ", area_name)
  } else {
    title <- paste0("Cumulative deaths since ",
                    format(start_date, "%d %B %Y"),
                    ", by date of registration, ", 
                    cause_name, ", ", area_name)  
  }
  title <- stringr::str_wrap(title, 85)
  
  death_data <- death_data %>%
    filter({{ date_field }} <= end_date)
  
  if (include_monday_final_point == FALSE) {
    final_day <- death_data %>%
      filter(!(wday({{ date_field }}) %in% c(7, 1, 2))) %>%
      filter({{ date_field }} == max({{ date_field }})) %>%
      pull({{ date_field }}) %>%
      unique()
  } else {
    final_day <- death_data %>%
      filter(!(wday({{ date_field }}) %in% c(7, 1))) %>%
      filter({{ date_field }} == max({{ date_field }})) %>%
      pull({{ date_field }}) %>%
      unique()
  }
  
  death_data <- death_data %>%
    filter({{ date_field }} >= start_date,
           {{ date_field }} <= final_day)
  
  excess_deaths_data <- death_data %>%
    mutate(area = area_name,
           model_cumulative = cumsum({{ model_field }}),
           deaths_cumulative = cumsum({{ deaths_field }}),
           covid_cumulative = cumsum({{ covid_field }})) %>%
    dplyr::select({{ date_field }}, model_cumulative, deaths_cumulative, covid_cumulative, area)
  
  
  # create label for the annotation
  excess_deaths_label <- excess_deaths_data %>%
    filter({{ date_field }} == max({{ date_field }})) %>%
    mutate(y = (model_cumulative + deaths_cumulative) / 2,
           label = paste0(scales::comma(round_correct(deaths_cumulative - model_cumulative, 0)),
                          " excess deaths"),
           x = final_date + 1) %>%
    dplyr::select(x, y, label)
  
  if (include_text_boxes == TRUE) {
    # create label for final point
    final_point_label <- excess_deaths_data %>%
      filter({{ date_field }} == max({{ date_field }})) %>%
      mutate(label_actual = paste(scales::comma(round_correct(deaths_cumulative, 0)),
                                  "registered deaths"),
             label_expected = paste(scales::comma(round_correct(model_cumulative, 0)),
                                    "expected deaths"),
             max_deaths = pmax(deaths_cumulative, model_cumulative),
             y_actual = case_when(
               (abs(model_cumulative - deaths_cumulative) / max_deaths) > 0.2 ~ 
                  deaths_cumulative, # ie, the excess deaths is greater than 20% of the maximum of modelled/registered
               deaths_cumulative < model_cumulative ~ 
                 deaths_cumulative - (max_deaths * 0.1),
               TRUE ~ 
                 deaths_cumulative + (max_deaths * 0.1)),
             y_expected = case_when(
               (abs(model_cumulative - deaths_cumulative) / max_deaths) > 0.2 ~ 
                 model_cumulative, # ie, the excess deaths is greater than 20% of the maximum of modelled/registered
               model_cumulative < deaths_cumulative ~ 
                 model_cumulative - (pmax(deaths_cumulative, model_cumulative) * 0.1),
               TRUE ~ 
                 model_cumulative + (pmax(deaths_cumulative, model_cumulative) * 0.1)),
             x = final_day + 1)
    label_actual <- final_point_label %>%
      dplyr::select(x, y = y_actual, label = label_actual)
    
    final_point_label <- final_point_label %>%
      dplyr::select(x, y = y_expected, label = label_expected) %>%
      bind_rows(label_actual)
    
    excess_deaths_label <- bind_rows(excess_deaths_label,
                                     final_point_label)
  }
  
  
  friday_labels <- excess_deaths_data %>%
    filter(!is.na(model_cumulative)) %>%
    filter({{ date_field }} == max({{ date_field }}))
  if (nrow(friday_labels) > 0) {
    friday_labels <- friday_labels %>%
      mutate(proportion_covid = covid_cumulative / (deaths_cumulative - model_cumulative),
             proportion_covid = case_when(
               proportion_covid > 1 ~ ">100%", 
               proportion_covid < 0 ~ "-",
               TRUE ~ scales::percent(proportion_covid, accuracy = 0.1)),
             ratio = formatC(deaths_cumulative / model_cumulative, digits = 2, format = "f"),
             label = case_when(
               {{ date_field }} == max({{ date_field }}) ~ paste0(ratio,
                                                                  " times the expected deaths"),
               TRUE ~ paste0("x", ratio, " (", proportion_covid, ")")))
  }
  
  if (include_covid_ribbon == TRUE & 
      nrow(friday_labels) > 0) {
    if (friday_labels$proportion_covid != ">100%") {
      friday_labels <- friday_labels %>%
        mutate(label = case_when(
          proportion_covid != "-" ~ paste0(label, ";\n",
                                           scales::comma(round_correct(covid_cumulative, 0), accuracy = 1),
                                           " deaths with COVID-19\nmentioned (",
                                           proportion_covid,
                                           " of excess)"),
          TRUE ~ label))
    } else {
      friday_labels <- friday_labels %>%
        mutate(label = case_when(
          proportion_covid != "-" ~ paste0(label, ";\n",
                                           scales::comma(round_correct(covid_cumulative, 0), accuracy = 1),
                                           " deaths with COVID-19 mentioned"),
          TRUE ~ label))
    }
    
  }
  
    
  excess_deaths_data <- excess_deaths_data %>%
    # left_join(covid_cumulative, by = intersect(names(.), names(covid_cumulative))) %>%
    mutate(covid_cumulative = deaths_cumulative - covid_cumulative,
           registered_deaths_label = case_when(
             {{ date_field }} < (Sys.Date() - 10) ~ "Registered deaths",
             TRUE ~ "Registered deaths (provisional)"))
  
  excessdeaths_chart <- ggplot(excess_deaths_data, aes(x = {{ date_field }}))
  
  if (include_covid_ribbon == TRUE) {
    excessdeaths_chart <- excessdeaths_chart +
      geom_ribbon(aes(ymin = covid_cumulative,
                      ymax = deaths_cumulative,
                      alpha = "1"),
                  fill = brewer_phe(names = TRUE)["peach"]) +
      scale_alpha_manual(name = "",
                         values = 0.85,
                         breaks = "1",
                         label = "COVID-19 mentioned\non death certificate")
    
  }
  
  excessdeaths_chart <- excessdeaths_chart +
    geom_line(aes(y = model_cumulative,
                  linetype = "1",
                  colour = "1",
                  group = area),
              size = line_size) +
    geom_line(aes(y = deaths_cumulative,
                  linetype = "2",
                  colour = "2"),
              size = line_size) +
    geom_text(data = excess_deaths_label,
                    aes(x = x, 
                        y = y,
                        label = label),
                    hjust = 0,
              size = 5)
  
  if (nrow(friday_labels) > 0) {
    offset <- as.numeric((end_date - start_date) / 2)
    excessdeaths_chart <- excessdeaths_chart +
      geom_text(data = friday_labels,
                aes(label = label,
                    y = deaths_cumulative),
                nudge_x = -offset,
                hjust = 1,
                size = 5) +
      geom_segment(data = friday_labels,
                   aes(xend = {{ date_field }} - offset + 1,
                       y = deaths_cumulative, 
                       yend = deaths_cumulative))
  }
    
  excessdeaths_chart <- excessdeaths_chart +
    theme_phe("phe") +
    scale_linetype_manual(name = "",
                          values = c("dashed", "solid"),
                          breaks = c("1", "2"),
                          labels = c("Modelled (expected) deaths", "Registered deaths")) +
    scale_colour_manual(name = "",
                        values = c("black", "#0072B2"),
                        breaks = c("1", "2"),
                        labels = c("Modelled (expected) deaths", "Registered deaths")) +
    scale_x_date(date_labels = "%b %d",
                 breaks = fri) +
    scale_y_continuous(label = scales::comma) +
    labs(y = "Cumulative registered deaths",
         x = "Date of registration",
         title = title,
         caption = caption,
         subtitle = subtitle) +
    theme(legend.position = "bottom",
          plot.margin = unit(c(1, 10, 1, 1), "lines"),
          axis.text = element_text(size = rel(1.1))) +
    coord_cartesian(clip = 'off') # prevents labels that spill off the side of the chart from being truncated
  
  return(excessdeaths_chart)
}


weekly_deaths_simple <- function(death_data, area_name, cause_name = "all cause",
                                 date_field, model_field, deaths_field, covid_field, 
                                 start_date = as.Date("2020-03-20"),
                                 caption = "", subtitle = "", end_date, show_inset = FALSE) {
  
  if (grepl("^ \\(", subtitle)) subtitle <- ""
  
  if (cause_name == "all cause") {
    title <- paste("Weekly excess deaths by date of registration,", area_name)
  } else {
    title <- paste0("Weekly excess deaths by date of registration, ", cause_name, ", ", area_name)  
  }
  title <- stringr::str_wrap(title, 85)
  
  data <- death_data %>% 
    filter({{ date_field }} <= end_date,
           {{ date_field }} >= start_date) %>%
    rename(weekly_registered = {{ deaths_field }},
           weekly_model = {{ model_field }},
           weekly_covid = {{ covid_field }}) %>% 
    mutate(excess_deaths = weekly_registered - weekly_model)
  
  label_deaths <- data %>%
    dplyr::select({{ date_field }}, weekly_registered, excess_deaths) %>%
    mutate(y = excess_deaths / 2,
           label = scales::comma(round_correct(excess_deaths, 0), accuracy = 1)) %>%
    dplyr::select({{ date_field }}, y, label)
  
  covid_proportions <- data %>%
    dplyr::select({{ date_field }}, excess_deaths, weekly_covid) %>%
    mutate(deficit = case_when(
            excess_deaths < 0 ~ excess_deaths,
            TRUE ~ 0),
           non_covid = case_when(
            excess_deaths > weekly_covid & excess_deaths > 0 ~ excess_deaths - weekly_covid,
            TRUE ~ 0),
           covid = case_when(
            weekly_covid > excess_deaths & excess_deaths > 0 ~ excess_deaths,
            excess_deaths < 0 ~ 0,
            TRUE ~ weekly_covid)) %>%
    dplyr::select({{ date_field }}, non_covid, covid, deficit) %>%
    pivot_longer(cols = non_covid:deficit, 
                 names_to = "type",
                 values_to = "deaths") %>%
    group_by({{ date_field }}) %>%
    mutate(cutoff = sum(abs(deaths))) %>%
    ungroup() %>%
    mutate(label = case_when(
            deaths == cutoff & type == "covid" ~ NA_character_, # this is when covid = excess
            abs(deaths) > max(cutoff) * 0.09 ~ scales::comma(round_correct(deaths, 0), accuracy = 1),
            TRUE ~ NA_character_),
           type = case_when(
            type == "covid" ~ "COVID-19 mentioned\non death certificate",
            type == "non_covid" ~ "Other deaths",
            TRUE ~ "Fewer deaths\nthan expected"))
  
  label_ratio <- data %>%
    dplyr::select({{ date_field }}, weekly_model, weekly_registered, excess_deaths, weekly_covid) %>%
    mutate(y = case_when(
      excess_deaths > 0 ~ excess_deaths + (diff(range(0, range(excess_deaths))) * 0.1),
      TRUE ~ excess_deaths - (diff(range(0, range(excess_deaths))) * 0.1)),
      label = case_when(
        weekly_covid > excess_deaths ~ paste0(scales::comma(round_correct(excess_deaths, 0), accuracy = 1),
                                              "*"),
        TRUE ~ scales::comma(round_correct(excess_deaths, 0), accuracy = 1)),
      label = paste0(label, 
                     "\n(x",
                     format(round_correct(weekly_registered / weekly_model, 2), nsmall = 2),
                     ")")) %>%
    dplyr::select({{ date_field }}, y, label)
  

  # create data to present the covid death labels at bottom of the chart --------
  covid_labels <- data %>%
    dplyr::select({{ date_field }}, weekly_covid, excess_deaths) %>%
    mutate(weekly_covid = scales::comma(round_correct(weekly_covid, 0), accuracy = 1))
  
  y_position <- covid_proportions %>% 
    group_by(date) %>% 
    summarise(excess_deaths = sum(deaths), .groups = "drop") %>% 
    pull(excess_deaths) %>% 
    range()
  
  y_position_numbers <- y_position[1] - (diff(y_position) * 0.5)
  y_position_title <- y_position[1] - (diff(y_position) * 0.45)
  y_position_asterisk <- y_position[1] - (diff(y_position) * 0.58)
  
  covid_header <- covid_labels %>% 
    distinct({{ date_field }}) %>% 
    filter({{ date_field }} == min({{ date_field }})) %>% 
    mutate(y = y_position_title,
           label = "Weekly number of deaths with COVID-19 mentioned on death certificate")
  
  asterisk <- covid_labels %>% 
    distinct({{ date_field }}) %>% 
    filter({{ date_field }} == max({{ date_field }})) %>% 
    mutate(y = y_position_asterisk,
           label = "* = weeks where the total excess was less than the number of deaths with a mention\nof COVID-19, indicating fewer deaths from other causes than expected.")
  

  # create fill colours -----------------------------------------------------
  fill_labels <- c("COVID-19 mentioned\non death certificate",
                   "Other deaths",
                   "Fewer deaths\nthan expected")
  
  cbPalette <- c("#E69F00", "#FFFFFF", "gray50")
  names(cbPalette) <- fill_labels
  
  # starting ggplot ---------------------------------------------------------
  weekly_deaths <- ggplot(covid_proportions, aes(x = {{ date_field }}, y = deaths)) +
    geom_col(aes(fill = type),
             alpha = 0.75,
             colour = "black") +
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5)) +
    geom_text(data = label_ratio,
              aes(y = y, 
                  label = label)) + 
    geom_text(data = covid_header,
              aes(y = y,
                  x = {{ date_field }},
                  label = label),
              hjust = 0,
              nudge_x = -2)
  if (sum(covid_labels$weekly_covid > covid_labels$excess_deaths) > 0) {
    weekly_deaths <- weekly_deaths +
      geom_text(data = covid_labels,
                aes(label = weekly_covid,
                    y = y_position_numbers,
                    colour = "dummy")) +
      guides(colour = guide_legend(override.aes = list(label = "*",
                                                       size = 6))) +
      scale_colour_manual(name = "",
                          values = "black",
                          breaks = "dummy",
                          labels = str_wrap("weeks where the total excess was less than the number of deaths with a mention of COVID-19, indicating fewer deaths from other causes than expected", 55))
    
  } else {
    weekly_deaths <- weekly_deaths +
      geom_text(data = covid_labels,
                aes(label = weekly_covid,
                    y = y_position_numbers))
  }
  weekly_deaths <- weekly_deaths +
    theme_phe("phe") +
    scale_fill_manual(name = "",
                      values = cbPalette,
                      labels = fill_labels,
                      breaks = fill_labels,
                      drop = FALSE) +
    scale_x_date(date_labels = "%b %d",
                 breaks = fri) +
    scale_y_continuous(label = scales::comma,
                       expand = expansion(mult = c(0.05, 0.1))) +
    labs(y = "Weekly number of excess deaths",
         x = "Week ending",
         title = title,
         caption = caption,
         subtitle = subtitle) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = rel(1.05)),
          legend.key.size = unit(1.5,"line"),
          axis.text = element_text(size = rel(1.1)),
          plot.title = element_text(hjust = 0.5))
  
  if (show_inset == TRUE) {
    covid_proportions <- covid_proportions %>%
      filter({{ date_field }} == as.Date("2020-04-03")) %>%
      mutate(label_annotation = case_when(
        deaths == 0 ~ "",
        type == "COVID-19 mentioned\non death certificate" ~ "number of deaths with a mention\nof COVID-19 on the death certificate",
        type  == "Other deaths" ~ "number of excess deaths\nwithout a mention of COVID-19\non the death certificate",
        type  == "Fewer deaths\nthan expected" ~ "number of deaths below\nwhat is expected\nat this time of year",
        TRUE ~ "ERROR"))
    
    label_ratio <- label_ratio %>%
      filter({{ date_field }} == as.Date("2020-04-03")) %>%
      mutate(label_annotation = "total excess deaths\n(the ratio of deaths = registered / expected)")
    
    weekly_deaths_inset <- ggplot(covid_proportions, aes(x = {{ date_field }}, y = deaths)) +
      geom_col(aes(fill = type),
               width = 0.3,
               alpha = 0.75,
               colour = "black") +
      geom_text(aes(label = label), 
                position = position_stack(vjust = 0.5),
                size = 3.5) +
      geom_text(aes(label = label_annotation,
                    x = {{ date_field }} + 0.2), 
                position = position_stack(vjust = 0.5),
                hjust = 0,
                size = 3.5) +
      geom_text(data = label_ratio,
                aes(y = y, 
                    label = label),
                size = 3.5) +
      geom_text(data = label_ratio,
                aes(y = y, 
                    label = label_annotation,
                    x = {{ date_field }} + 0.2),
                hjust = 0,
                size = 3.5) +
      theme_phe("phe") +
      scale_fill_manual(name = "",
                        values = cbPalette,
                        labels = fill_labels,
                        breaks = fill_labels,
                        drop = FALSE) +
      scale_x_date(date_labels = "%b %d",
                   breaks = fri) +
      scale_y_continuous(label = scales::comma) +
      labs(y = "",
           x = "",
           title = "How to read this chart",
           subtitle = "") +
      theme(legend.position = "none",
            plot.title = element_text(size = rel(1),
                                      colour = "black"),
            plot.background = element_rect(colour = "black"),
            plot.margin = unit(c(0, 0.75, 0, 0), unit = "npc"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.grid = element_blank()) +
      coord_cartesian(clip = 'off') # prevents labels that spill off the side of the chart from being truncated
    
    weekly_deaths <- weekly_deaths +
      theme(plot.margin = unit(c(0, 0.15, 0, 0), unit = "npc")) +
      coord_cartesian(clip = 'off') # prevents labels that spill off the side of the chart from being truncated
    
    weekly_deaths <- cowplot::ggdraw() +
      cowplot::draw_plot(weekly_deaths) +
      cowplot::draw_plot(weekly_deaths_inset, x = 0.82, y = 0.65, width = 0.11, height = 0.3)
    
  }
  
  return(weekly_deaths)
  
}

cumulative_compare <- function(death_data, area_name,
                               date_field, model_field, deaths_field, covid_field,
                               axis_field,
                               start_date = as.Date("2020-03-01"),
                               line_size = 1, caption = "", subtitle = "",
                               end_date, facet_scales = "free_y", wrap_length = 20, 
                               include_covid_ribbon = TRUE, axis_title = "",
                               include_table = TRUE, dispersion_parameter, display_cis = FALSE,
                               breakdown_by_ucod = FALSE) {
  
  if (grepl("^ \\(", subtitle)) subtitle <- ""
  
  
  
  if (is.factor(pull(death_data, !! sym(axis_field)))) {
    axis_levels <- levels(death_data[[axis_field]])  
  } else {
    axis_levels <- unique(death_data[[axis_field]]) %>% 
      sort()
  }
  axis_levels <- tools::toTitleCase(axis_levels) %>%
    rev() %>%
    str_wrap(wrap_length)
  
  if (length(dispersion_parameter) == 1) {
    dispersion_parameter <- setNames(rep(dispersion_parameter, length(axis_levels)),
                                     nm = axis_levels)
  } else {
    names(dispersion_parameter) <- str_wrap(tools::toTitleCase(names(dispersion_parameter)), 
                                            wrap_length)
  }
  
  data <- death_data %>%
    filter({{ date_field }} >= start_date,
           {{ date_field }} <= end_date) %>%
    mutate(!! sym(axis_field) := as.character(!! sym(axis_field)),
           !! sym(axis_field) := tools::toTitleCase(!! sym(axis_field)),
           !! sym(axis_field) := str_wrap(!! sym(axis_field), wrap_length),
           !! sym(axis_field) := factor(!! sym(axis_field),
                                      levels = axis_levels)) %>%
    group_by(!! sym(axis_field))
  
  
  if (breakdown_by_ucod) {
    
    data <- data %>%
      summarise(registered_deaths = sum({{ deaths_field }}),
                expected_deaths = sum({{ model_field }}),
                covid_deaths = sum({{ covid_field }}), 
                `Specific cause deaths (underlying cause)` = sum(ucod_disease),
                `COVID-19 deaths (underlying cause)` = sum(ucod_covid),
                .groups = "drop")
    
  } else {
    
  data <- data %>%
    summarise(registered_deaths = sum({{ deaths_field }}),
              expected_deaths = sum({{ model_field }}),
              covid_deaths = sum({{ covid_field }}), 
              .groups = "drop")
  }
  
  data <- data %>%
    split(.[[axis_field]]) %>% 
    map_df(~ add_prediction_intervals(.x, 
                                      modelled_field = expected_deaths, 
                                      dispersion_parameter = dispersion_parameter[.x[[axis_field]]])) %>% 
    mutate(significance = case_when(
              registered_deaths > upb | registered_deaths < lpb ~ "Significant",
              TRUE ~ "Not significant"),
           excess_deaths = registered_deaths - expected_deaths,
           ratio = registered_deaths / expected_deaths)
  if (display_cis == TRUE) {
    data <- data %>% 
      mutate(ratio_label = paste0("x", format(round_correct(ratio, 2), nsmall = 2)))
    caption_ratio <- caption
    ratio_labels <- data %>%
      dplyr::select(!! sym(axis_field), ratio, ratio_label, upb, lpb, registered_deaths, expected_deaths) %>%
      mutate(upb = (registered_deaths + (upb - expected_deaths)) / expected_deaths,
             lpb = (registered_deaths - (expected_deaths - lpb)) / expected_deaths,
             x = case_when(
              ratio > 1 ~ (upb) + (diff(range(1, upb, lpb)) * 0.15),
              TRUE ~ (lpb) - (diff(range(1, upb, lpb)) * 0.15)))
    
  } else {
    data <- data %>% 
      mutate(ratio_label = case_when(
        significance == "Significant" ~ paste0("x", format(round_correct(ratio, 2), nsmall = 2)),
        TRUE ~ NA_character_))
    if (sum(data$significance == "Not significant") > 0) {
      caption_ratio <- paste0("Ratio not displayed where registered deaths are not significantly different from expected deaths",
                              caption)
    } else {
      caption_ratio <- caption
    }
    ratio_labels <- data %>%
      dplyr::select(!! sym(axis_field), ratio, ratio_label, significance) %>%
      mutate(x = case_when(
        significance == "Not significant" ~ 1,
        ratio > 1 ~ (ratio) + (max(max(ratio), abs(min(ratio))) * 0.06),
        TRUE ~ (ratio) - (max(max(ratio), abs(min(ratio))) * 0.06)))
    
  }
           
  
  
  
  excess_deaths_labels <- data %>%
    dplyr::select(!! sym(axis_field), excess_deaths) %>%
    mutate(x = case_when(
            excess_deaths > 0 ~ (excess_deaths) + (max(max(excess_deaths), abs(min(excess_deaths))) * 0.11),
            TRUE ~ (excess_deaths) - (max(max(excess_deaths), abs(min(excess_deaths))) * 0.11)),
           excess_deaths_label = scales::comma(round_correct(excess_deaths, 0), accuracy = 1))
  
  proportions <- data %>%
    dplyr::select(!! sym(axis_field), covid_deaths, excess_deaths, expected_deaths)
  
  if (include_covid_ribbon == TRUE) {
    bar_levels <- c("covid", "non_covid", "deficit")
    bar_labels <- c("COVID-19 mentioned\non death certificate", 
                    "Other deaths",
                    "Fewer deaths\nthan expected")
    
    proportions <- proportions %>%
      mutate(deficit = case_when(
        excess_deaths < 0 ~ excess_deaths,
        TRUE ~ 0),
        non_covid = case_when(
          excess_deaths > 0 & excess_deaths > covid_deaths ~ excess_deaths - covid_deaths,
          TRUE ~ 0),
        covid = case_when(
          covid_deaths > excess_deaths & excess_deaths > 0 ~ excess_deaths,
          excess_deaths < 0 ~ 0,
          TRUE ~ covid_deaths)) %>%
      dplyr::select(!! sym(axis_field), non_covid, covid, deficit) %>%
      pivot_longer(cols = non_covid:deficit, 
                   names_to = "type",
                   values_to = "value")
    
    proportions_for_ratios <- proportions %>%
      dplyr::select(!! sym(axis_field), type, value) %>%
      filter(type != "deficit") %>%
      group_by(!! sym(axis_field)) %>%
      mutate(proportions = value / sum(value)) %>%
      ungroup() %>%
      dplyr::select(-value)
    
    if (display_cis == FALSE) {
      ratio_positive <-  data %>%
        mutate(ratio = case_when(
          significance == "Not significant" ~ NA_real_,
          TRUE ~ ratio))
    } else {
      ratio_positive <- data
    }
    
    ratio_positive <- ratio_positive %>% 
      dplyr::select(!! sym(axis_field), ratio) %>%
      filter(ratio > 1) %>%
      mutate(ratio = ratio - 1) %>%
      left_join(proportions_for_ratios, by = intersect(names(.), names(proportions_for_ratios))) %>%
      mutate(value = ratio * proportions,
             label = case_when(
               proportions != 0 ~ scales::percent(round_correct(proportions, 3), accuracy = 0.1),
               TRUE ~ NA_character_)) %>%
      dplyr::select(!! sym(axis_field), type, value, label)
    
    ratio_positive_dummy <- ratio_positive %>%
      distinct(!! sym(axis_field)) %>%
      mutate(type = "dummy",
             value = 1,
             label = NA_character_)
    
    if (display_cis == FALSE) {
      ratio_negative <-  data %>%
        mutate(ratio = case_when(
          significance == "Not significant" ~ NA_real_,
          TRUE ~ ratio))
    } else {
      ratio_negative <- data
    }
    
    ratio_negative <- ratio_negative %>%
      dplyr::select(!! sym(axis_field), ratio) %>%
      filter(ratio < 1) %>%
      mutate(dummy = ratio, 
             deficit = 1 - ratio) %>%
      dplyr::select(-ratio) %>%
      pivot_longer(dummy:deficit,
                   names_to = "type",
                   values_to = "value") %>%
      mutate(label = NA_character_)
    
    ratio <- bind_rows(ratio_positive,
                       ratio_positive_dummy,
                       ratio_negative) %>%
      complete(!! sym(axis_field), 
               type = c(bar_levels, "dummy"),
               fill = list(value = 0,
                           label = NA)) %>%
      group_by(!! sym(axis_field)) %>%
      mutate(cutoff = case_when(
              sum(value) == 0 ~ NA_real_,
              sum(value) == 1 ~ sum(value[type == "dummy"]),
              TRUE ~ sum(value))) %>%
      ungroup()
    
    if (display_cis == TRUE) {
      ratio <- ratio %>% 
        mutate(label = NA_character_)
    } else {
      ratio <- ratio %>% 
        mutate(label = case_when(
          type %in% c("dummy", "deficit") ~ label,
          value > (diff(range(1, range(cutoff, na.rm = TRUE))) * 0.168) ~ label,
          TRUE ~ NA_character_))
      
    }
    ratio <- ratio %>% 
      mutate(type = factor(type,
                           levels = c(bar_levels, "dummy"),
                           labels = c(bar_labels, ""))) %>%
      arrange(!! sym(axis_field), type)
      
    chart_fill <- setNames(c("#E69F00", "#FFFFFF", "gray50"),
                           nm = bar_labels)
    
    chart_fill_ratio <- setNames(c("#F0E442", "#0072B2", "gray50", NA),
                                 nm = c(bar_labels, ""))
    chart_colours_ratio <- setNames(c(rep("black", 3), NA),
                                    nm = c(bar_labels, ""))
    
  } else {
    bar_levels <- c("excess", "deficit")
    bar_labels <- c("Excess deaths",
                    "Fewer deaths\nthan expected")
    
    ratio_label_names <- c("Excess deaths", "Fewer deaths\nthan expected", "")
    
    chart_colours_ratio <- setNames(c("black", NA, NA),
                                    nm = ratio_label_names)
    
    ratio_levels <- c("ratio", "deficit", "dummy")
    
    
    proportions <- proportions %>%
      mutate(deficit = case_when(
        excess_deaths < 0 ~ excess_deaths,
        TRUE ~ 0),
        excess = case_when(
          excess_deaths > 0 ~ excess_deaths,
          TRUE ~ 0)) %>%
      dplyr::select(!! sym(axis_field), deficit, excess) %>%
      pivot_longer(cols = deficit:excess, 
                   names_to = "type",
                   values_to = "value")
    
    if (display_cis == FALSE) {
      ratio <-  data %>%
        mutate(ratio = case_when(
          significance == "Not significant" ~ NA_real_,
          TRUE ~ ratio))
    } else {
      ratio_negative <- data
    }
    
    ratio <- ratio %>%
      dplyr::select(!! sym(axis_field), ratio) %>%
      mutate(
        dummy = case_when(
          ratio > 1 ~ 1,
          TRUE ~ ratio
        ),
        deficit = case_when(
          ratio < 1 ~ 1 - ratio,
          TRUE ~ 0
        ),
        ratio = case_when(
          ratio > 1 ~ ratio - 1,
          TRUE ~ 0
        )
      ) %>%
      pivot_longer(c(dummy, ratio, deficit), names_to = "type",
                   values_to = "value") %>%
      mutate(label = NA) %>%
      mutate(type = factor(type,
                           levels = ratio_levels,
                           labels = ratio_label_names))
    
    chart_fill <- setNames(c("#FFFFFF", "gray50"),
                           nm = bar_labels)
      
    chart_fill_ratio <- setNames(c("white", "gray50", NA),
                                 nm = ratio_label_names)
    chart_colours_ratio <- setNames(c("black", "black", NA),
                                    nm = ratio_label_names)
  }
  
  proportions <- proportions %>%
    mutate(type = factor(type,
                         levels = bar_levels,
                         labels = bar_labels)) %>%
    group_by(!! sym(axis_field)) %>%
    mutate(cutoff = sum(value)) %>%
    ungroup() %>%
    mutate(label = case_when(
              type == "COVID-19 mentioned\non death certificate" & value == cutoff ~ NA_character_,
              abs(value) < (diff(range(0, range(cutoff))) * 0.1) ~ NA_character_,
              value < 0 ~ NA_character_,
              TRUE ~scales::comma(round_correct(value, 0), accuracy = 1)),
           facet = "Total deaths")
    
  ratio_x_limit <- min(c(data$ratio * 0.8, 0.9))
  
  if (subtitle == "") {
    subtitle <- paste0(format(start_date, "%d %b %Y"),
                       " - ",
                       format(end_date, "%d %b %Y"))
  } else {
    subtitle <- paste0(subtitle, "; ", 
                       format(start_date, "%d %b %Y"),
                       " - ",
                       format(end_date, "%d %b %Y"))
  }

  # begin total plots ----------------------------------------------------------
  cumulative_compare_num <- ggplot(proportions, 
                                   aes(x = value, y = !! sym(axis_field))) +
    geom_col(aes(fill = type),
             colour = "black") +
    geom_text(aes(label = label,
                  group = type),
              position = position_stack(vjust = 0.5))
  if (include_covid_ribbon == TRUE) {
    cumulative_compare_num <- cumulative_compare_num +
      geom_label(data = excess_deaths_labels,
                 aes(x = x,
                     label = excess_deaths_label),
                 label.size = 0)
  }
  cumulative_compare_num <- cumulative_compare_num +
    scale_fill_manual(name = "",
                      values = chart_fill) +
    scale_x_continuous(label = scales::comma) +
    labs(x = "Excess number of registered deaths",
         y = axis_title,
         title = str_wrap(paste("Excess Deaths in", area_name),
                          35),
         caption = caption,
         subtitle = subtitle) +
    theme_phe("phe") +
    theme(legend.position = "bottom")

  # begin ratio plots ----------------------------------------------------------
  cumulative_compare_ratio <- ggplot(ratio, 
                                     aes(x = value, y = !! sym(axis_field))) +
    geom_col(aes(fill = type,
                 colour = type)) +
    geom_text(aes(label = label,
                  group = type),
              position = position_stack(vjust = 0.5))
  if (display_cis == TRUE) {
    ci_data <- data %>% 
      dplyr::select(!! sym(axis_field), lpb, upb, registered_deaths, expected_deaths) %>% 
      mutate(upb = (registered_deaths + (upb - expected_deaths)) / expected_deaths,
             lpb = (registered_deaths - (expected_deaths - lpb)) / expected_deaths,
             value = registered_deaths / expected_deaths)
    cumulative_compare_ratio <- cumulative_compare_ratio +
      geom_errorbarh(data = ci_data,
                     aes(xmin = lpb,
                         xmax = upb),
                     height = 0.5)
  }
  cumulative_compare_ratio <- cumulative_compare_ratio +
    geom_label(data = ratio_labels,
               aes(x = x,
                   label = ratio_label),
               label.size = 0) +
    scale_fill_manual(name = "",
                      values = chart_fill_ratio) +
    scale_colour_manual(name = "",
                        values = chart_colours_ratio) +
    coord_cartesian(xlim = c(ratio_x_limit, NA)) +
    labs(x = "Ratio of registered deaths to expected deaths",
         y = axis_title,
         title = str_wrap(paste("Ratio of Registered Deaths to Expected Deaths in", 
                                area_name),
                         30),
         caption = caption_ratio,
         subtitle = subtitle) +
    theme_phe("phe") +
    theme(legend.position = "bottom")

  # begin gt table ----------------------------------------------------------
  if (include_table == TRUE) {
    table <- data %>%
      arrange(desc(!! sym(axis_field)))
    
    if (display_cis == TRUE) {
      table <- table %>% 
        mutate(upb = excess_deaths + (upb - expected_deaths),
               lpb = excess_deaths - (expected_deaths - lpb)) %>% 
        dplyr::select(!!sym(axis_title) := !! sym(axis_field),
                      `Registered deaths` = registered_deaths,
                      `Expected deaths` = expected_deaths,
                      `Ratio registered / expected` = ratio,
                      `Excess deaths` = excess_deaths,
                      `Upper CI` = upb,
                      `Lower CI` = lpb,
                      `COVID-19 deaths` = covid_deaths) 
        
    } else if (display_cis == FALSE) {
        
      table <- table %>% 
        mutate(ratio = case_when(
          significance == "Not significant" ~ NA_real_,
          TRUE ~ ratio)) 
      
      if (breakdown_by_ucod) {
        table <- table %>% 
          dplyr::select(!!sym(axis_title) := !! sym(axis_field),
                        `Registered deaths` = registered_deaths,
                        `Expected deaths` = expected_deaths,
                        `Specific cause deaths (underlying cause)`,
                        `COVID-19 deaths (underlying cause)`,
                        `Excess deaths` = excess_deaths,
                        `Ratio registered / expected` = ratio)
      } else {
        table <- table %>% 
          dplyr::select(!!sym(axis_title) := !! sym(axis_field),
                        `Registered deaths` = registered_deaths,
                        `Expected deaths` = expected_deaths,
                        `Ratio registered / expected` = ratio,
                        `Excess deaths` = excess_deaths,
                        `COVID-19 deaths` = covid_deaths) 
      }  
      
    }
    
    if (breakdown_by_ucod) {
      table <- table %>%
        gt() %>%
        fmt_number(
          columns = vars(`Registered deaths`, `Expected deaths`, `Excess deaths`, `Specific cause deaths (underlying cause)`, `COVID-19 deaths (underlying cause)`),
          decimals = 0,
        ) %>%
        fmt_number(
          columns = vars(`Ratio registered / expected`),
          decimals = 2
        ) %>%
        fmt_missing(
          columns = vars(`Ratio registered / expected`),
          missing_text = "-"
        ) %>%
        tab_footnote(
          footnote = "registered deaths were not significantly different from expected deaths for the time period",
          locations = cells_body(
            columns = vars(`Ratio registered / expected`),
            rows = is.na(`Ratio registered / expected`))
        ) %>%
        opt_footnote_marks(marks = c("*", "+"))
    } else {
      table <- table %>% 
        mutate(`COVID-19 deaths as % excess` = case_when(
          `Excess deaths` < 0 ~ "-",
          `COVID-19 deaths` > `Excess deaths` ~ ">100%",
          TRUE ~ scales::percent(round_correct(`COVID-19 deaths` / `Excess deaths`, 3), accuracy = 0.1))) %>%
        gt() %>%
        fmt_number(
          columns = vars(`Registered deaths`, `Expected deaths`, `Excess deaths`,`COVID-19 deaths`),
          decimals = 0,
        ) %>%
        fmt_number(
          columns = vars(`Ratio registered / expected`),
          decimals = 2
        ) %>%
        cols_align(
          align = "right",
          columns = vars(`COVID-19 deaths as % excess`)
        ) %>%
        tab_footnote(
          footnote = "the total excess was less than the number of deaths with a mention of COVID-19, indicating fewer deaths from other causes than expected",
          locations = cells_body(
            columns = vars(`COVID-19 deaths as % excess`),
            rows = `COVID-19 deaths as % excess` == ">100%")
        ) %>%
        fmt_missing(
          columns = vars(`Ratio registered / expected`),
          missing_text = "-"
        ) %>%
        tab_footnote(
          footnote = "registered deaths were not significantly different from expected deaths for the time period",
          locations = cells_body(
            columns = vars(`Ratio registered / expected`),
            rows = is.na(`Ratio registered / expected`))
        ) %>%
        tab_style(
          style = list(
            cell_fill(color = "#edbb4c")
          ),
          locations = cells_body(
            columns = vars(`COVID-19 deaths as % excess`),
            rows = `COVID-19 deaths as % excess` == ">100%")
        ) %>%
        opt_footnote_marks(marks = c("*", "+"))
    }
      
      
    if (display_cis == TRUE) {
      table <- table %>% 
        fmt_number(
          columns = vars(`Upper CI`, `Lower CI`),
          decimals = 1,
        )
    }
    
    return(list(cumulative_chart_num = cumulative_compare_num,
                cumulative_chart_ratio = cumulative_compare_ratio,
                cumulative_table = table))
  }
  
  return(list(cumulative_chart_num = cumulative_compare_num,
              cumulative_chart_ratio = cumulative_compare_ratio))
}
