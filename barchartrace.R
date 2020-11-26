library(dplyr)
library(echarts4r)

date <- Sys.Date() - 1 #most recent data is yesterday's
covid <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",date,".xlsx")

download.file(covid,destfile = "./covid.xlsx",mode = "wb")
covid <- readxl::read_xlsx("covid.xlsx")
covid$month <- stringr::str_pad(covid$month, width = 2, side = 'left', pad = '0')
covid$day <- stringr::str_pad(covid$day, width = 2, side = 'left', pad = '0')
covid <- covid %>% mutate(day = paste0(year, "-", month, "-", day)) %>% 
  group_by(countriesAndTerritories) %>% arrange(day) %>% 
  mutate(cumulative_cases = cumsum(cases)) %>% ungroup() %>%
  group_by(day) %>% arrange(-cumulative_cases) %>% slice(1:20) %>% mutate(order = letters[1:20])

dates <- unique(covid$day)
titles <- list()
for (i in 1:length(dates)){
  titles[[i]] <- list(text = paste0("Covid cases ",dates[i]), textStyle = list(fontSize = 14))
}

covid %>% 
  group_by(day) %>%
  e_charts(order, timeline=TRUE, reorder=TRUE) %>% 
  e_bar(cumulative_cases, bind=countriesAndTerritories, label=list(show=TRUE, formatter = "{b}", position = "insideBottomLeft")) %>%
  e_timeline_opts(
    show = FALSE,
    autoPlay = TRUE,
    playInterval=200,
    tooltip = list(formatter = '{c}')
  ) %>% 
  e_timeline_serie(title = titles) %>%
  e_animation(duration.update=150) %>%
  e_flip_coords() %>%
  e_x_axis(axisLine=list(show=FALSE), axisTick=list(show=FALSE), axisLabel=list(show=TRUE), position = "top") %>%
  e_y_axis(axisLabel=list(show=FALSE), axisLine=list(show=FALSE), axisTick=list(show=FALSE), inverse=TRUE) 


