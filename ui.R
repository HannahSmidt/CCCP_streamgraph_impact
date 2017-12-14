#devtools::install_github("hrbrmstr/streamgraph")
library("streamgraph")
library("shiny")
library("shinyjs")

appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
width: 100%;
text-align: center;
color: #000000;
}
"

bootstrapPage(
  theme = "animate.min.css",
  useShinyjs(),
  inlineCSS(appCSS),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  div(id = "loading-content",
      fluidPage(
        h2(class = "animated infinite pulse", "Loading data..."))),
  streamgraphOutput("streamPlot")
)
