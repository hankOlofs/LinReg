#' Visualizing airport delays
#'
#' @return
#' @export
#' 
#' @importFrom dplyr %>% 
#'
#' @examples
visualize_airport_delays <- function() {
  a <- nycflights13::airports[, -(5:8)]
  b <- nycflights13::flights[, c("dep_delay", "origin")]
  
  airport_delays <-
    dplyr::right_join(a, b, by = c("faa" = "origin"))
  
  mean_delay <- (airport_delays %>%
                   group_by(faa) %>%
                   dplyr::summarize(mean_delay = mean(dep_delay, na.rm = TRUE), .groups = "keep"))
  
  airport_delays <-
    dplyr::left_join(airport_delays, mean_delay, by = "faa")
  airport_delays <- dplyr::select(airport_delays, -c(dep_delay)) %>%
    dplyr::distinct(faa, lat, lon, mean_delay)
  
  
  # a bar chart
  p1 <- ggplot(data=airport_delays, aes(x = faa, y = mean_delay)) +
    geom_bar(stat="identity",  fill="steelblue") +
    geom_text(aes(label=round(mean_delay, 1)), vjust=-0.3, size=3.5) +
    theme_minimal()
  
  print(p1)
  
  # a scatter plot
  p2 <- ggplot(data=airport_delays, aes(x = lon, y = lat)) +
    geom_point(aes(fill = mean_delay, size = 10), shape = 21) +
    geom_text(aes(label = faa), vjust=-0.9, size=3.5) +
    theme_minimal()

  print(p2)
  
  nyc <- map_data("nyc")
 p3  <- ggplot2::ggplot() + 
    geom_map(
      data=nyc, 
      map=nyc,
      aes(map_id=region)) +
   geom_point(data=airport_delays, aes(x = lon, y = lat, fill = mean_delay, size = 10), shape = 21) +
   geom_text(data=airport_delays, aes(x = lon, y = lat, label = faa), vjust=-0.9, size=3.5)
  
  print(p3)
  
}
