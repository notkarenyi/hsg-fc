# setup -------------------------------------------------------------------

setup <- function(data_path) {
  #' Read and clean data

  df <- read.csv("2024.csv")
  hours <- read.csv("hours.csv")

  df <- df %>%
    group_by(org) %>%
    mutate(
      totalrequested = sum(amount),
      org = str_wrap(org, 60)
    ) %>%
    ungroup() %>%
    mutate(
      org = factor(org, levels = rev(unique(org)), ordered = T)
    )

  # if we don't know how much they spent, assume received plus some rollover
  df[is.na(df$actual), "actual"] <- df[is.na(df$actual), "totalreceived"] + df[is.na(df$actual), "externalreceived"] + round(df[is.na(df$actual), "totalrollover"] * .2)

  # recalculate total requested for only those events held
  df <- df %>%
    filter(actual != 0) %>%
    group_by(org) %>%
    mutate(totalrequested = sum(amount, na.rm = T))

  # cap actual spent by amount we granted
  df <- mutate(df, actualhsg = min(totalreceived, actual))

  return(df)
}

barstyle <- function(p, dist = 200, dollars = T) {
  #' Define styles for bar graphs

  p <- p +
    xlab("") +
    ylab("") +
    coord_flip() +
    theme_classic(base_family = "serif") +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_line(color = "gray80"),
      plot.margin = unit(c(10, 10, 10, 10), "pt")
    )

  if (dollars) {
    p <- p +
      scale_y_continuous(
        breaks = seq(0, 50000, by = dist),
        labels = scales::dollar_format()
      )
  }

  return(p)
}


pointstyle <- function(p, dist = 200, dollars = T) {
  #' Define styles for scatter plots

  p <- p +
    theme_classic(base_family = "serif") +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_line(color = "gray80"),
      panel.grid.major.y = element_line(color = "gray80"),
      plot.margin = unit(c(10, 10, 10, 10), "pt"),
      aspect.ratio = 1
    )

  if (dollars) {
    p <- p +
      scale_x_continuous(
        breaks = seq(0, 50000, by = dist),
        labels = scales::dollar_format()
      ) +
      scale_y_continuous(
        breaks = seq(0, 50000, by = 200),
        labels = scales::dollar_format()
      )
  }

  return(p)
}

# graphs ------------------------------------------------------------------

allocations_by_org <- function(quarter, caption = "") {
  #' Plots total amounts allocated by HSG Finance Committee to each organization.

  p <- df %>%
    group_by(org) %>%
    summarize(total = mean(totalreceived)) %>%
    ggplot(aes(org, total)) +
    geom_bar(stat = "identity", show.legend = F, fill = "#800000") +
    labs(
      title = paste0("   Allocations by Organization, ", quarter),
      subtitle = "   Dotted line represents average allocation",
      caption = caption
    ) +
    geom_hline(
      yintercept = mean(summarize(group_by(df, org), m = mean(totalreceived, na.rm = T))[["m"]], na.rm = T),
      linetype = "dotted",
      size = .6
    )

  barstyle(p)
}

rollover_by_org <- function(quarter) {
  #' Plots amount of external funding + rollover by organization.

  p <- df %>%
    group_by(org) %>%
    summarize(leftover = mean(totalreceived, na.rm = T) + mean(externalreceived, na.rm = T) + mean(totalrollover, na.rm = T) - mean(actual, na.rm = T)) %>%
    ggplot(aes(org, leftover)) +
    geom_bar(stat = "identity", show.legend = F, fill = "#800000") +
    labs(
      title = paste0("   Rollover by Organization, ", quarter),
      subtitle = "   Dotted line represents average allocation",
      caption = ""
    ) +
    geom_hline(
      yintercept = mean(summarize(group_by(df, org), m = mean(totalreceived, na.rm = T))[["m"]], na.rm = T),
      linetype = "dotted",
      size = .6
    )

  barstyle(p, dist = 1000)
}

compare_rollover_allocation <- function(quarter) {
  #' Plots amount of external funding + rollover by allocation per organization.
  #' Attempts to establish a correlation, since rollover is one of the considerations in total allocation

  rollover <- df %>%
    group_by(org) %>%
    summarize(
      rollover = mean(totalrollover + externalreceived),
      received = mean(totalreceived)
    )

  p <- rollover %>%
    ggplot(aes(rollover, received)) +
    geom_point(color = "#800000") +
    labs(
      title = paste0("   Allocations Compared to Rollover +\n   External Funding by Organization, ", quarter)
      # subtitle="   Points represent rollover/external amount"
    ) +
    xlab("Rollover + external funding") +
    ylab("Received")
  # geom_smooth(method="lm")

  # not significant
  # lm(received~rollover, rollover) %>% summary()

  pointstyle(p, dist = 2000)
}

planned_events <- function(quarter, caption = "") {
  p <- df %>%
    group_by(org) %>%
    summarize(
      total = mean(eventsactual),
      planned = mean(events.planned)
    ) %>%
    filter(!is.na(total)) %>%
    ggplot() +
    geom_bar(
      aes(org, planned, fill = Category),
      stat = "identity",
      fill = "#C16622"
    ) +
    geom_bar(
      aes(org, total, fill = Category),
      stat = "identity",
      fill = "#800000"
    ) +
    labs(
      title = paste0("   Planned vs. Actual Number of Events Held\n   by Organization, ", quarter),
      caption = paste0("   ", caption)
    )

  barstyle(p, dollars = F) +
    scale_y_continuous(breaks = seq(0, 20, 2))
}

compare_allocation_events <- function(caption = "") {
  # is receiving a low amount of funding correlated with hosting less events than planned?
  df$devents <- df$events.planned - df$eventsactual
  lm(totalreceived ~ devents, df) %>% summary()

  p <- df %>%
    ggplot(aes(totalreceived, devents)) +
    geom_point(color = "#800000") +
    xlab("Allocation") +
    ylab("Number of events canceled") +
    labs(caption = caption)

  pointstyle(p, dollars = F)
}

spending_by_event_type <- function(quarter) {
  p <- df %>%
    group_by(org, category) %>%
    summarize(
      # we are missing detailed information for actual spending, so impute:
      # scale the amount requested by actual spending
      adjusted = sum(amount, na.rm = T) / mean(totalrequested, na.rm = T) * mean(actualhsg, na.rm = T)
    ) %>%
    group_by(category) %>%
    summarize(adjusted = round(sum(adjusted, na.rm = T))) %>%
    filter(!is.na(adjusted), category != "") %>%
    mutate(
      category = fct_reorder(str_to_sentence(category), adjusted)
    ) %>%
    ggplot(aes(category, adjusted)) +
    geom_bar(stat = "identity", show.legend = F, fill = "#800000") +
    labs(
      title = paste0("    HSG Funds Spending by Event Type, ", quarter),
      subtitle = "     Estimated from amount requested per category"
    )

  barstyle(p, dist = 1000)
}

spending_by_item_type <- function(quarter) {
  p <- df %>%
    group_by(org, type) %>%
    summarize(
      # scale the amount requested by
      adjusted = sum(amount, na.rm = T) / mean(totalrequested, na.rm = T) * mean(actualhsg, na.rm = T)
    ) %>%
    group_by(type) %>%
    summarize(adjusted = round(sum(adjusted, na.rm = T))) %>%
    filter(!is.na(adjusted), type != "") %>%
    mutate(
      type = fct_reorder(str_to_sentence(type), adjusted)
    ) %>%
    ggplot(aes(type, adjusted)) +
    geom_bar(stat = "identity", show.legend = F, fill = "#800000") +
    labs(
      title = paste0("    HSG Funds Spending by Item Type, ", quarter),
      subtitle = "     Estimated from amount requested per item type"
    )

  barstyle(p, dist = 2000)
}
