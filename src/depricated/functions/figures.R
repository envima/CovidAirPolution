plotly_figure = function(d, l){
  fig = plot_ly()
  
  fig = fig %>%
    add_trace(data = d[[l]], x = ~date, y = ~pm_mean,
              name = paste("PM 2.5", names(d[l])),
              mode = 'lines+markers', line = list(color = "red"), marker = list(color = "red"))
  
  fig = fig %>%
    add_trace(data = d[[l]], x = ~date, y = ~new_cases,
              yaxis = "y2", name = paste("New cases", names(d[l])),
              mode = 'lines+markers', line = list(color = "blue"), marker = list(color = "blue"))
  
  fig = fig %>% layout(title= names(d[l]),
                       yaxis = list(title = "PM 2.5 mean"),
                       yaxis2 = list(overlaying = "y", side = "right", title = "new cases"))
  fig
}