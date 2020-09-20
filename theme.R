theme_liu <- function() {
  font <- ""
  liu_col <- c(paste0("#",c("00B9E7","FF7B53", "9B97DC", "17C7D2", "00CFB5", "FEF06F", "7D91A2")))
  
  theme_minimal(base_family = font) %+replace%
    theme(
      plot.background = element_rect(fill = "#B9EEF1"),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "dark gray"),
      panel.grid.minor = element_blank(),
      legend.position = "Bottom"
      #8BE3E8
    )
}



