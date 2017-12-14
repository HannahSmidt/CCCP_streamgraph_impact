## Task: Make streamgraph to illustrate how impact and actor dimension
##  vary across countries and over time
##  and how the different components vary
## Author: Hannah Smidt
## Date 13.12.2017

library(dplyr)
#devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)


#### Load UCDP data for state conflict ####
download.file(url = "http://ucdp.uu.se/downloads/ged/ged171-RData.zip", destfile = "data-raw/ucdp-data.zip")
unzip(exdir = "./data-raw/ucdp-data", zipfile = "./data-raw/ucdp-data.zip")
load("data-raw/ucdp-data/ged171.Rdata")
gedData <- ged171@data # Syria is not included

#### Choose focal countries and settings #####
focus_country_names <- c("Colombia",
                         "Venezuela",
                         "Rwanda",
                         "Burundi",
                         "DR Congo (Zaire)",
                         "Afghanistan",
                         "Pakistan",
                         "Iraq",
                         "Mexico",
                         "Myanmar (Burma)")

gedData$conflictSettings <- NA
gedData$conflictSettings[gedData$country=="Afghanistan"] <-"Afghanistan / Pakistan"
gedData$conflictSettings[gedData$country=="Pakistan"] <-"Afghanistan / Pakistan"
gedData$conflictSettings[gedData$country=="DR Congo (Zaire)"] <-"Great Lakes Region"
gedData$conflictSettings[gedData$country=="Rwanda"] <-"Great Lakes Region"
gedData$conflictSettings[gedData$country=="Burundi"] <-"Great Lakes Region"
gedData$conflictSettings[gedData$country=="Venezuela"] <-"Colombia / Venezuela"
gedData$conflictSettings[gedData$country=="Colombia"] <-"Colombia / Venezuela"
gedData$conflictSettings[gedData$country=="Iraq"] <-"Iraq"
gedData$conflictSettings[gedData$country=="Myanmar (Burma)"] <-"Myanmar"
gedData$conflictSettings[gedData$country=="Mexico"] <-"Mexico"


##### Make actor data ####
focus_actors_a <- gedData %>%
  filter(country %in% focus_country_names) %>%
  select(conflictSettings, country, year, side_a) %>%
  distinct() %>%
  rename(side = side_a)

focus_actors_b <- gedData %>%
  filter(country %in% focus_country_names) %>%
  select(conflictSettings, country, year, side_b) %>%
  distinct() %>%
  rename(side = side_b)

focus_actor <- rbind( as.data.frame(focus_actors_a), as.data.frame(focus_actors_b) )

focus_actor_numbers <- focus_actor %>%
    mutate(stateActors = grepl("Government", side) ) %>%
    group_by(conflictSettings, year, stateActors) %>%
    summarise(Count= length(stateActors))

focus_actor_numbers <- as.data.frame(focus_actor_numbers)
focus_actor_numbers$stateActors <- ifelse(focus_actor_numbers$stateActors==T, 1,0)

focus_actor_numbers_reshaped <- reshape(focus_actor_numbers, v.names = "Count"
                                             ,idvar = c("conflictSettings", "year"), timevar = "stateActors", direction = "wide")

actor_dataframe <- focus_actor_numbers_reshaped %>%
                          mutate(Count.2 = Count.1-1) %>%
                          rename("NonstateActors" = Count.0,
                                 "StateActors" = Count.1,
                                 "ExternalActors" = Count.2) %>%
                          mutate(SovereignStateActors = 1)

actor_dataframe <- as.data.frame(actor_dataframe)
actor_dataframe <- actor_dataframe[!duplicated( actor_dataframe[,c("conflictSettings", "year")] ),]
actor_dataframe$allActors <-  actor_dataframe$NonstateActors + actor_dataframe$StateActors
actor_dataframe$allActors[is.na(actor_dataframe$allActors)] <-0


#### Transform data for civilian casualties ####
focus_impact <- gedData %>%
  filter(country %in% focus_country_names) %>%
  select(country, conflictSettings, year, deaths_a, deaths_b, deaths_civ) %>%
  group_by(conflictSettings, year) %>%
  mutate(deathsCombatants = sum(deaths_a + deaths_b)
         , deathsCivilians = sum(deaths_civ) ) 
  
impact_dataframe <- as.data.frame(focus_impact)
impact_dataframe <- impact_dataframe[!duplicated( impact_dataframe[,c("conflictSettings", "year")] ), ]
impact_dataframe$deaths <- impact_dataframe$deathsCivilians + impact_dataframe$deathsCombatants


#### Merge actor and impact data ####
dataStreamgraph<- plyr::join_all(list(actor_dataframe, impact_dataframe)
                         , by = c("conflictSettings", "year"), type="full")





# STREAMGRAPH: All deaths across cases                      
dataStreamgraph %>%
  filter(deaths < 300000)  %>% # without rwandan genocide
  group_by(conflictSettings, year) %>%
  streamgraph(key = "conflictSettings", value = "deaths", date = "year"
              , offset="zero", interpolate="linear") %>%
  sg_axis_x(tick_interval = c(1990, 2014), tick_units = 5)  %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="Settings: ") 

# STREAMGRAPH: All actors across cases
dataStreamgraph %>%
  filter(deaths < 300000)  %>% # without rwandan genocide
  group_by(country, year) %>%
  streamgraph(key = "conflictSettings", value = "allActors", date = "year"
              , offset="zero", interpolate="linear") %>%
  sg_axis_x(tick_interval = c(1990, 2014), tick_units = 5)  %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="Settings: ") 


# STREAMFRAPH: Types of actors across all cases
dataStreamgraph_actors <- dataStreamgraph %>%
                    filter(deaths < 300000)  %>% # without rwandan genocide
                    select(conflictSettings, year, SovereignStateActors, NonstateActors, ExternalActors) %>%
                    rename(Actors.SovereignState = SovereignStateActors,
                           Actors.Nonstate = NonstateActors,
                           Actors.External = ExternalActors) %>%
                    reshape(varying= c("Actors.SovereignState", "Actors.Nonstate", "Actors.External")
                                    , idvar=c("conflictSettings", "year")
                                    , times=c("SovereignState", "Nonstate", "External")
                                    , direction="long") %>%
                    mutate(names = plyr::mapvalues(time,c("SovereignState","Nonstate","External"),
                                                c("State governments","Non-state actors","External gov. allies") ) ) %>%
                    mutate(conflictActors = paste(conflictSettings, names, sep=": ") )

sg_fill_manual_colors <- c("#C0392B", "#F1948A", "#E74C3C"
                              ,"#7FB3D5", "#2980B9", "#3498DB"
                              ,"#F7DC6F", "#B7950B", "#F5B041"
                              ,"#C39BD3", "#884EA0", "#8E44AD"
                              ,"#73C6B6", "#1E8449", "#2ECC71"
                              ,"#AAB7B8", "#797D7F", "#7F8C8D" )
                              
dataStreamgraph_actors %>%
  group_by(conflictActors, year) %>%
  streamgraph(key = "conflictActors", value = "Actors", date = "year"
              , offset="zero", interpolate="linear") %>%
  sg_axis_x(tick_interval = c(1990, 2014), tick_units = 5)  %>%
  sg_fill_manual(sg_fill_manual_colors) %>%
  sg_legend(show=TRUE, label="Setting & Actor: ") 


# STREAMGRAPH: Different victims across cases
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


sg_fill_manual_colors <- c("#C0392B", "#F1948A" 
                           ,"#7FB3D5", "#2980B9"
                           ,"#F7DC6F", "#B7950B"
                           ,"#C39BD3", "#884EA0"
                           ,"#73C6B6", "#1E8449"
                           ,"#AAB7B8", "#797D7F" )

dataStreamgraph_impact %>%
  group_by(conflictImpact, year) %>%
  streamgraph(key = "conflictImpact", value = "deaths", date = "year"
              , offset="zero", interpolate="linear") %>%
  sg_axis_x(tick_interval = c(1990, 2014), tick_units = 5)  %>%
  sg_fill_manual(sg_fill_manual_colors) %>%
  sg_legend(show=TRUE, label="Setting & Victim: ") 
