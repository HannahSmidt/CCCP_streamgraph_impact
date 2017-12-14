# Shiny App
library("dplyr")
library("shiny")
library("shinyjs")


source("process-ucdp-data.R", local = TRUE)

function(input, output, session) {
  
  # Process data for particulat stream graph
  dataStreamgraph_impact <- dataStreamgraph %>%
    filter(deaths < 300000)  %>% # without Rwandan genocide
    select(conflictSettings, year, deathsCivilians, deathsCombatants) %>%
    rename(deaths.Civilians = deathsCivilians,
           deaths.Combatants = deathsCombatants) %>%
    reshape(varying= c("deaths.Civilians", "deaths.Combatants")
            , idvar=c("conflictSettings", "year")
            , times=c("Civilians", "Nonstate", "Combatants")
            , direction="long") %>%
    mutate(conflictImpact = paste(conflictSettings, time, sep=": ") )
  head( dataStreamgraph_impact )
  
  # Choose colors for streams
  sg_fill_manual_colors <- c("#C0392B", "#F1948A" 
                             ,"#7FB3D5", "#2980B9"
                             ,"#F7DC6F", "#B7950B"
                             ,"#C39BD3", "#884EA0"
                             ,"#73C6B6", "#1E8449"
                             ,"#AAB7B8", "#797D7F" )  
  # Render streamgraph
  output$streamPlot <- renderStreamgraph({
    
    shinyjs::hide(id = "loading-content",
                  anim = TRUE,
                  animType = "fade")
    
    dataStreamgraph_impact %>%
      group_by(conflictImpact, year) %>%
      streamgraph(key = "conflictImpact", value = "deaths", date = "year"
                  , offset="zero", interpolate="linear") %>%
      sg_axis_x(tick_interval = c(1990, 2014), tick_units = 5)  %>%
      sg_fill_manual(sg_fill_manual_colors) %>%
      sg_legend(show=TRUE, label="Setting & Victim: ") 
  })
}

