library(shiny)
library(leaflet)
library(DT)


# Define UI for application that draws a histogram
# navbarPage('天災風險地圖', id="main",
#                 tabPanel("水災害",
#                          verticalLayout(
#                            sidebarPanel(
#                              dateInput("date1", "Date:", value = "2020-07-31")
#                            ),
#                          mainPanel(
#                            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
#                            leafletOutput("bbmap", height=1000)
#                            )
#                          )
#                          )
# )

navbarPage('天災風險地圖', id="main",
           tabPanel("水災害",
                    #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                    div(class="outer",
                        tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                        leafletOutput("bbmap", width ="100%" , height="100%"),
                    absolutePanel(fixed = TRUE,
                                  draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  dateInput("date1", "日期:", value = "2020-07-31")
                    )
                    )

           )
)


