# setup -------------------------------------------------------------------

setup <- function(quarter) {
  #' Read and clean data

  actual <- read.csv(paste0(current_quarter, ".csv"))
  # standardize org names
  actual <- update_names(actual)

  # add totals requested and planned from last quarter
  planned <- read.csv(paste0(report_quarter, ".csv")) %>%
    update_names()

  planned <- planned %>%
    select(-expenditureactual, -attendactual, -eventsactual)
  actual <- actual %>%
    group_by(org) %>%
    summarize(
      expense.name = expense.name,
      expenditureactual = expenditureactual,
      category = category,
      attendactual = mean(attendactual),
      eventsactual = mean(eventsactual)
    ) %>%
    distinct(org, category, expense.name, expenditureactual, attendactual, eventsactual)

  df <- bind_rows(planned, actual)

  # add totals allocated for last quarter
  allocated <- read_excel(paste0(report_quarter, " Allocations.xlsx")) %>%
    remove_empty("rows")
  names(allocated)[1] <- "org"
  allocated <- update_names(allocated) %>%
    select(org, `Total Allocated`)

  df <- left_join(df, allocated)
  df$totalreceived <- df$`Total Allocated`
  # df$totalreceived[is.na(df$totalreceived)] <- 0
  names(df) <- tolower(names(df))

  # make org categorical and ordered
  df <- df %>%
    mutate(
      org = factor(org, levels = rev(unique(org)), ordered = T)
    )

  # recalculate total requested for only those events held
  df <- df %>%
    group_by(org) %>%
    mutate(
      totalrequested = sum(amount, na.rm = T),
      totalreceived = mean(totalreceived, na.rm = T)
    )

  # cap actual spent by amount we granted
  df$expenditureactualhsg <- NA
  df[!is.na(df$expenditureactual), "expenditureactualhsg"] <- rowMins(as.matrix(df[!is.na(df$expenditureactual), c("totalreceived", "expenditureactual")]))

  df <- df %>%
    mutate(collab = any(collabhso, collabrso)) %>%
    select(-collabhso, -date, -collabrso, -x, -event.off.campus, -`total allocated`, -rollover)

  return(df)
}

barstyle <- function(p, dist = 200, dollars = T) {
  #' Define styles for bar graphs

  p <- p +
    xlab("") +
    ylab("") +
    coord_flip() +
    theme_classic(
      # base_family = "sans-serif"
    ) +
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
    theme_classic(
      # base_family = "sans-serif"
    ) +
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

count_x <- function(variable, func=mean) {
  df <- df[,c("org",variable)]
  names(df) <- c("org","v")
  x <- df %>%
    group_by(org) %>%
    summarize(n=func(v,na.rm=T)) %>%
    select(n) %>%
    sum(na.rm=T) %>%
    round() %>%
    comma()
  return(x)
}

allocations_by_org <- function(quarter, caption = "") {
  #' Plots total amounts allocated by HSG Finance Committee to each organization.

  p <- df %>%
    group_by(org) %>%
    filter(!is.na(totalreceived), totalreceived != 0) %>%
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


rollover_by_org <- function(quarter, dist = 1000) {
  #' Plots amount of external funding + rollover by organization.

  p <- df %>%
    group_by(org) %>%
    summarize(leftover = mean(totalreceived, na.rm = T) + mean(externalreceived, na.rm = T) + mean(totalrollover, na.rm = T) - mean(expenditureactual, na.rm = T)) %>%
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

  barstyle(p, dist)
}

compare_rollover_allocation <- function(quarter, dist = 4000) {
  #' Plots amount of external funding + rollover by allocation per organization.
  #' Attempts to establish a correlation, since rollover is one of the considerations in total allocation

  p <- df %>%
    group_by(org) %>%
    summarize(
      totalrollover = mean(totalrollover + externalreceived, na.rm = T),
      received = mean(totalreceived, na.rm = T)
    ) %>%
    ggplot(aes(totalrollover, received)) +
    geom_point(color = "#800000") +
    labs(
      title = paste0("   Allocations Compared to Rollover +\n   External Funding by Organization, ", quarter)
      # subtitle="   Points represent rollover/external amount"
    ) +
    xlab("Rollover + external funding") +
    ylab("Received")
  # geom_smooth(method="lm")

  # not significant
  # lm(received~totalrollover, totalrollover) %>% summary()

  pointstyle(p, dist)
}

planned_attendance <- function(quarter, caption = "", dist = 20, limit = 1000) {
  p <- df %>%
    filter(!(is.na(expectedattendees) & is.na(attendactual))) %>%
    group_by(org) %>%
    summarize(
      expectedattendees = mean(expectedattendees, na.rm = T),
      attendactual = mean(attendactual, na.rm = T)
    ) %>%
    ggplot() +
    geom_bar(
      aes(org, expectedattendees, fill = Category),
      stat = "identity",
      fill = "#C16622"
    ) +
    geom_bar(
      aes(org, attendactual, fill = Category),
      stat = "identity",
      fill = "#800000"
    ) +
    labs(
      title = paste0("   Planned vs. Actual Attendance\n   by Organization, ", quarter),
      caption = paste0("   ", caption)
    )

  barstyle(p, dist = dist, dollars = F) +
    coord_flip(ylim = c(0, limit))
}


planned_events <- function(quarter, caption = "") {
  p <- df %>%
    group_by(org) %>%
    summarize(
      total = mean(eventsactual, na.rm = T),
      planned = mean(eventsplanned, na.rm = T)
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

planned_spending <- function(quarter, caption = "", dist = 2000, limit = 100000) {
  p <- df %>%
    group_by(org) %>%
    summarize(
      expenditureactual = mean(expenditureactual, na.rm = T),
      # assume they will use some of their rollover
      expenditureplanned = sum(amount, na.rm = T)
    ) %>%
    filter(!is.na(expenditureactual)) %>%
    ggplot() +
    geom_bar(
      aes(org, expenditureplanned, fill = Category),
      stat = "identity",
      fill = "#C16622"
    ) +
    geom_bar(
      aes(org, expenditureactual, fill = Category),
      stat = "identity",
      fill = "#800000"
    ) +
    labs(
      title = paste0("   Planned vs. Actual Spending\n   by Organization, ", quarter),
      caption = paste0("   ", caption)
    )

  barstyle(p, dist = dist) +
    coord_flip(ylim = c(0, limit))
}

compare_allocation_events <- function(caption = "") {
  # is receiving a low amount of funding correlated with hosting less events than planned?
  df$devents <- df$eventsplanned - df$eventsactual
  # lm(totalreceived ~ devents, df) %>% summary()

  p <- df %>%
    ggplot(aes(totalreceived, devents)) +
    geom_point(color = "#800000") +
    xlab("Allocation") +
    ylab("Number of events canceled") +
    labs(
      caption = caption,
      title = "Difference between events planned and implemented,\nby allocation amount"
    )

  pointstyle(p, dollars = F)
}

spending_by_event_type <- function(quarter, caption = "", dist = 2000) {
  p <- df %>%
    filter(!is.na(expenditureactualhsg), category != "") %>%
    group_by(org, category) %>%
    summarize(expenditureactual = sum(expenditureactualhsg, na.rm = T)) %>%
    # distinct(category, type, org) %>%
    group_by(category) %>%
    summarize(expenditureactual = sum(expenditureactual, na.rm = T)) %>%
    mutate(
      category = fct_reorder(str_to_sentence(category), expenditureactual)
    ) %>%
    ggplot(aes(category, expenditureactual)) +
    geom_bar(stat = "identity", show.legend = F, fill = "#800000") +
    labs(
      title = paste0("    HSG Funds Spending by Event Type, ", quarter),
      subtitle = "     ",
      caption = caption
    )

  barstyle(p, dist)
}

orgs_by_event_type <- function(quarter, caption = "", limit = 10000, dist = 1000) {
  p <- df %>%
    distinct(org, category, `expense.name`, expenditureactualhsg) %>%
    group_by(org, category) %>%
    filter(!is.na(expenditureactualhsg), category != "") %>%
    summarize(expenditureactual = sum(expenditureactualhsg, na.rm = T)) %>%
    mutate(
      category = fct_reorder(str_to_sentence(category), expenditureactual)
    ) %>%
    ggplot(aes(org, expenditureactual, fill = category)) +
    geom_bar(stat = "identity",color="white") +
    labs(
      title = paste0("   HSG Funds Spending by Event Type,\n   ", quarter),
      subtitle = "     ",
      caption = caption
    ) +
    scale_fill_paletteer_d("ggthemes::Color_Blind", guide = guide_legend())

  barstyle(p, dist) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    coord_flip(ylim = c(0, limit))
}

spending_by_item_type <- function(quarter, caption = "", dist = 2000) {
  p <- df %>%
    group_by(org, type) %>%
    summarize(
      # scale the proportion of item type requested by amount received
      adjusted = sum(amount, na.rm = T) / mean(totalrequested, na.rm = T) * mean(totalreceived, na.rm = T)
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
      subtitle = "     Estimated from amount requested per item type",
      caption = caption
    )

  barstyle(p, dist)
}
