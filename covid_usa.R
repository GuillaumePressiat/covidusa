

library(readr)
library(tidyverse)


# (source from https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us)
# and us.census data ( https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv)
# with the geojson (https://raw.githubusercontent.com/jgoodall/us-maps/master/geojson/state.geo.json).


# download history
list_dates <- seq(lubridate::as_date('2020-04-12'), lubridate::as_date(Sys.Date() - 1), by = 1) %>%
  format(., '%m-%d-%Y')
f_url_github <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/'
files_list <- paste0(f_url_github, list_dates, '.csv')

# files_list %>%
#   purrr::map(function(x)httr::GET(x, httr::write_disk(paste0('data_covid/', basename(x)))))

#dat_cov <- list_dates %>% purrr::map_df(function(x)mutate(readr::read_csv(paste0('data_covid/', x, '.csv')), day_file = lubridate::mdy(x)))

# saveRDS(dat_cov, 'dat_cov.Rds')

dat_cov <- read_rds('dat_cov.Rds')

dat_cov <- dat_cov %>% 
  # readr::read_csv('07-17-2020.csv') %>% 
  select(day_file, UID, Confirmed, People_Hospitalized, Deaths, People_Tested, Recovered) %>% 
  mutate(state_id = substr(UID, 7,8)) %>% 
  filter(state_id != "", !is.na(state_id))

date_update <- max(list_dates) %>%
  paste0('data_covid/', ., '.csv') %>%
  readr::read_csv() %>%
  summarise(d = max(Last_Update)) %>%
  arrange(desc(d)) %>%
  pull() %>%
  .[1] %>%
  format(., '%Y-%m-%d -- %Hh%Mm')

library(leaflet)
library(sf)
library(rmapshaper)
library(dplyr, warn.conflicts = FALSE)
library(smoothr)
library(shiny)


data.p <- sf::st_read("state.geo.json") %>%
   ms_simplify(keep = 0.007) %>%
   #smooth(method = "chaikin") %>%
   select(STATEFP10, geometry, NAME10, STUSPS10)

#st_write(data.p, 'states_light.geojson', delete_dsn = TRUE)
data.p <- st_read('states_light.geojson')

# View(head(data.p))
# plot(data.p)

# pops <- readr::read_csv('SCPRC-EST2019-18+POP-RES.csv') %>%
#   select(STATE, POPESTIMATE2019)
# 
# 
# data <- data.p %>%
#   left_join(dat_cov, by = c('STATEFP10' = 'state_id')) %>%
#   left_join(pops, by = c('STATEFP10' = 'STATE'))
# 
# 
# data <- data %>%
#   mutate(popup = lapply(paste0(STATEFP10, " - ", NAME10, " : ", prettyNum(Confirmed, big.mark = ","), '<br>', day_file), htmltools::HTML))
# 
# pal_fun <- colorNumeric(scico::scico(n = 300, palette = "tokyo", direction = - 1, end = 0.85), data$Confirmed, na.color = 'grey90')
# 
# data <- sf::st_transform(data,sp::CRS('+proj=longlat +datum=WGS84'))
# 
# 
# # Just one map one day
# # tictoc::tic()
# # leaflet(data  %>%
# #           filter(day_file == lubridate::as_date('2020-07-17'))) %>%
# #   #addTiles() %>%
# #   addProviderTiles("CartoDB", options = providerTileOptions(opacity = 1, minZoom = 3, maxZoom = 5), group = "Open Street Map") %>%
# #   setView(lng = -100, lat = 40, zoom = 3) %>%
# #   addPolygons(color = 'white', weight = 1.4,
# #               group = 'base',
# #               fillColor = ~pal_fun(Confirmed),
# #               fillOpacity = 1, stroke = 2,
# #               label = ~ popup) %>%
# #   addLegend("bottomleft", pal = pal_fun, values = ~Confirmed,
# #             title = 'Confirmed', opacity = 1)
# # tictoc::toc()
# 
# 
# ui <- bootstrapPage(
# 
#   tags$head(
#     tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
#     tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
#     #includeHTML("meta.html"),
#     tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
#                 type="text/javascript")),
# 
#   leafletOutput("covid", width = "100%", height = "100%"),
# 
#   absolutePanel(
#     bottom = 20, left = 40, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 300px;",
#     titlePanel("USA | Covid"),
#     # br(),
#     em('data is available mouse on hover'),
#     sliderInput("jour",h3(""),
#                 min = min(dat_cov$day_file), max = max(dat_cov$day_file), step = 1,
#                 value = max(dat_cov$day_file),
#                 animate = animationOptions(interval = 1700, loop = FALSE)),
# 
# 
# 
#     shinyWidgets::prettyRadioButtons('sel_data', 'data',
#                                      choices = c('Confirmed', 'People Hospitalized', 'People Tested', 'Deaths', 'Recovered'),
#                                      selected = 'Confirmed',
#                                      shape = "round", animation = "jelly",plain = TRUE,bigger = FALSE,inline = FALSE),
#     shinyWidgets::prettySwitch('pop', "Ratio / 100 000 inhabitants", FALSE),
#     #em(tags$small("*à noter sur ce ratio : un patient peut être hospitalisé plus d'une fois")),
#     #em(tags$small(br(), "Pour les décès, il s'agit de ceux ayant lieu à l'hôpital")),
#     h5(tags$a(href = 'http://github.com/GuillaumePressiat', 'Guillaume Pressiat'), ' & ',
#        tags$a(href = 'http://github.com/Halfbakedpanda', 'Halfbakedpanda')),
# 
#     h5(em('Last update : ' , date_update)),
# 
#     #br(),
#     tags$small(
#       tags$li(tags$a(href = 'https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us', 'COVD-19 data from CSSE github')),
#               tags$li(tags$a(href = 'https://raw.githubusercontent.com/jgoodall/us-maps/master/geojson/state.geo.json', 'Geojson US Map')),
#                tags$li(tags$a(href = 'https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv', 'Populations Us Census')),
# 
#                tags$li(tags$a(href = 'http://www.fabiocrameri.ch/resources/ScientificColourMaps_FabioCrameri.png', 'Scientific colour maps'), ' with ',
#                        tags$a(href = 'https://cran.r-project.org/web/packages/scico/index.html', 'scico package'))))
# 
# 
# )
# 
# server <- function(input, output) {
# 
#   # Confirmed, People_Hospitalized, Deaths, People_Tested
#   get_data <- reactive({
#     temp <- data[which(data$day_file == input$jour),]
#     if (input$sel_data == "Confirmed"){
#       temp$val <- temp$Confirmed
#     } else if (input$sel_data == "People Hospitalized"){
#       temp$val <- temp$People_Hospitalized
#     } else if (input$sel_data == "People Tested"){
#       temp$val <- temp$People_Tested
#     } else if (input$sel_data == "Deaths"){
#       temp$val <- temp$Deaths
#     } else if (input$sel_data == "Recovered"){
#       temp$val <- temp$Recovered
#     }
# 
# 
# 
#     temp$label <- prettyNum(temp$val, big.mark = ',')
# 
#     if (input$pop){
#       temp$val <- (temp$val * 100000) / temp$POPESTIMATE2019
#       temp$label <- paste0(temp$label, '<br><em>', round(temp$val,1), ' / 100 000 inhab.</em><br>', prettyNum(temp$POPESTIMATE2019, big.mark = ','), ' inhabitants')
#     }
# 
# 
#     return(temp)
# 
#   })
# 
#   values_leg <- reactive({
#     temp <- data
#     if (input$sel_data == "Confirmed"){
#       temp$leg <- temp$Confirmed
#     } else if (input$sel_data == "People Hospitalized"){
#       temp$leg <- temp$People_Hospitalized
#     } else if (input$sel_data == "People Tested"){
#       temp$leg <- temp$People_Tested
#     } else if (input$sel_data == "Deaths"){
#       temp$leg <- temp$Deaths
#     } else if (input$sel_data == "Recovered"){
#       temp$leg <- temp$Recovered
#     }
# 
#     if (input$pop){
#       temp$leg <- (temp$leg * 100000) / temp$POPESTIMATE2019
#     }
#     temp <- temp$leg
#     # if (input$log){
#     # temp <- log(temp)
#     # temp[temp < 0] <- 0
#     # }
#     return(temp)
#   })
# 
#   leg_title <- reactive({
#     if (input$pop){
#       htmltools::HTML('Nb for<br>100,000<br>inhab.')
#     } else{
#       'Nb'
#     }
#   })
# 
#   output$covid <- renderLeaflet({
#     leaflet(data = data.p) %>%
#       addProviderTiles("CartoDB", options = providerTileOptions(opacity = 1, minZoom = 3, maxZoom = 6), group = "Open Street Map") %>%
#       setView(lng = -100, lat = 40, zoom = 3) %>%
#       addPolygons(group = 'base',
#                   fillColor = NA,
#                   color = 'white',
#                   weight = 2.5)  %>%
#       addLegend(pal = pal(), values = values_leg(), opacity = 1, title = leg_title(),
#                 position = "topright", na.label = 'No&nbsp;data', )
#   })
# 
# 
# 
#   pal <- reactive({
# 
#     if (input$sel_data != "Recovered"){
#       return(colorNumeric(scico::scico(n = 300, palette = "tokyo", direction = - 1, end = 0.85), values_leg(), na.color = '#c1c1d7'))
#     } else {
#       return(colorNumeric(scico::scico(n = 300, palette = "oslo", direction = - 1, begin = 0.2, end = 0.85), domain = values_leg(), na.color = '#808080'))
#     }
#   })
# 
# 
#   observe({
#     if(input$jour == min(dat_cov$day_file)){
#       data <- get_data()
#       leafletProxy('covid', data = data) %>%
#         clearGroup('polygons') %>%
#         addPolygons(group = 'polygons',
#                     fillColor = ~pal()(val),
#                     fillOpacity = 1,
#                     stroke = 2,
#                     color = 'white',
#                     weight = 2.5, label = ~ lapply(paste0("<b>", STATEFP10, " - ", NAME10, "</b><br>",day_file, ' : ', label), htmltools::HTML))
#     } else {
#       data <- get_data()
#       leafletProxy('covid', data = data) %>%
#         #clearGroup('polygons') %>%
#         addPolygons(group = 'polygons',
#                     fillColor = ~pal()(val),
#                     fillOpacity = 1,
#                     stroke = 2,
#                     color = 'white',
#                     weight = 2.5, label = ~ lapply(paste0("<b>", STATEFP10, " - ", NAME10, "</b><br>",day_file, ' : ', label), htmltools::HTML))
#     }
# 
#   })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)

