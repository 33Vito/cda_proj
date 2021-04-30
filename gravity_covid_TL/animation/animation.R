source("R_functions.R")
library(animation)
# library(gganimate)

animation::saveVideo({
  # for (date_i in seq.Date(ymd(20200301), ymd(20201231), length.out = 5)) {
  for (date_i in unique(filter(confirmed_cases, 
                               as.character(postcode) %in% SYD_POA$POA_NAME16)$notification_date)[
    seq(1, length(unique(filter(confirmed_cases, 
                                as.character(postcode) %in% SYD_POA$POA_NAME16)$notification_date)), 2)]) {
    print(
    (confirmed_cases %>%
      filter(notification_date <= as.Date(date_i)) %>%
      filter(as.character(postcode) %in% SYD_POA$POA_NAME16) %>%
      count(postcode, name="Total_cases") %>%
      rename(POA_NAME16 = postcode) %>%
      mutate(POA_NAME16 = fct_reorder(as.factor(POA_NAME16), Total_cases)) %>%
      plot_map_TL(SYD_POA, "POA_NAME16", "Total_cases",
                  paste0("Accumulated confirmed Covid-19 cases by POA (SYD Metro), ", as.Date(date_i)),
                  show_count = T, label_size = 3.5, fill_scale = c(0,135))
    ) / 
      (confirmed_cases %>%
      filter(as.character(postcode) %in% SYD_POA$POA_NAME16) %>%
      count(notification_date, name="Total_cases") %>% 
      mutate(Accumulated_cases = cumsum(Total_cases)) %>% 
      mutate(Accumulated_case_sofar = ifelse(notification_date <= as.Date(date_i), Accumulated_cases, NA)) %>% 
      mutate(date_point = ifelse(notification_date == as.Date(date_i), Accumulated_cases, NA)) %>%
      ggplot(aes(x=notification_date, y=Accumulated_cases)) + 
      geom_line(linetype = 2) + 
      geom_line(aes(y=Accumulated_case_sofar)) + 
      geom_point(aes(y=date_point), col="darkred", size=3) +
      ggl()) + plot_layout(heights = c(4, 1))
    )
}},
video.name = paste0("./animation/accumulated_case_plot_map_TL.mp4"),
interval = .1, ani.height = 1000, ani.width = 1200)


# confirmed_cases %>%
#   filter(notification_date <= date_i) %>% 
#   filter(as.character(postcode) %in% SYD_POA$POA_NAME16) %>%
#   count(postcode, name="Total_cases") %>%
#   rename(POA_NAME16 = postcode) %>%
#   mutate(POA_NAME16 = fct_reorder(as.factor(POA_NAME16), Total_cases)) %>%
#   plot_map_TL(SYD_POA, "POA_NAME16", "Total_cases",
#               paste0("Accumulated confirmed Covid-19 cases by POA (SYD Metro), ", date_i),
#               show_count = T, label_size = 2)