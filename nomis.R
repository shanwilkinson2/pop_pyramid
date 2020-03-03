library(nomisr)
library(dplyr)
library(janitor)
library(ggplot2)
library(forcats)
library(stringr)
library(plotly)
library(glue)

# metadata needed to pull data from nomis ####################################################

  # helpfile here
    # https://cloud.r-project.org/web/packages/nomisr/readme/README.html
  
  # search for indicators with population in 
    nomis_search <- nomis_search(name = "*population*")
  
  # various items of metadata to get parameters
    nomis_meta <- nomis_get_metadata(id = "NM_31_1", "sex", "TYPE")
    nomis_meta <- nomis_get_metadata(id = "NM_31_1", "geography", "TYPE")
    nomis_meta <- nomis_get_metadata(id = "NM_31_1", "measures", "TYPE")
    nomis_meta <- nomis_get_metadata(id = "NM_31_1")
    
  # bolton geography code, maxing out on rows otherwise
    bolton_geog_codes <- nomis_codelist(id= "NM_31_1", "geography", search = "*bolton*")  
    nomis_codelist(id= "NM_31_1", "geography", search = "*england*")  

# pull total population figure ###############################################################  
    
  # bolton population latest, all age, measures = number ie. 20100 (not %	ie 20301)  
    # bolton = 1816133652
    nomis_data <- nomis_get_data(id = "NM_31_1", date = "latest", 
                                 geography = 1816133652, sex = 7, age = 0, measures = 20100)

# data for population pyramid ################################################################ 
  
  # get data
    # bolton & england latest, 5 year age bands, % (not number)  
      # bolton = 1816133652
      # england = 2092957699
      age_breakdown <- nomis_get_data(id = "NM_31_1", date = "latest", 
                                   geography = c(1816133652, 2092957699), sex = c(5, 6), measures = 20301) %>%
        clean_names() %>%
        filter(age_code != 0) # excludes all ages
  
    # prepare data
      age_breakdown2 <- age_breakdown %>%
        # reduce number of columns
          select(geography_name, sex_name, age_name, obs_value) %>%
        # change name label to be shorter
          dplyr::mutate(age_name = str_replace(age_name, "Aged ", ""),
                        age_name = str_replace(age_name, " years", ""),
                        age_name = str_replace(age_name, " year", ""),
                        age_name = as.factor(age_name),
                        age_name = fct_relevel(age_name, "under 1", "1 - 4", "5 - 9", "10 - 14", "15 - 19", 
                                               "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44",
                                               "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", 
                                               "70 - 74", "75 - 79", "80 - 84", "85 and over"),
                        ) %>%
        filter(!age_name %in% c("0 - 15", "16 - 59/64", "16 - 64", "18 - 24", "65 and over")) %>%
        # drop extra levels that were filtered out above
          mutate(age_name = fct_drop(age_name))

# population pyramid - ggplot ###############################################################            

  # myplot <- 
    ggplot() + 
    # swap x & y axes
      coord_flip() + 
    # columns - Bolton 
      geom_col(data = age_breakdown2[age_breakdown2$geography_name == "Bolton",],
        aes(x = age_name, y = ifelse(sex_name == "Male", -obs_value, obs_value), fill = sex_name)) +
    # line - England
      geom_line(data = age_breakdown2[age_breakdown2$geography_name == "England",], 
                aes(x = age_name, 
                    y = ifelse(sex_name == "Male", -obs_value, obs_value), 
                    group = sex_name, colour = sex_name),
                size = 1) +
    # sort out x axis (rotated y) 
      # percent is e.g. 20 for 20% not 0.2 
      # abs ie. absolute takes away +- symbol
      # adds % symbol
        scale_y_continuous(labels = function(x) paste0(abs(x),"%")) +
      # change label of key
    # axis labels
      labs(x = "Age group", y = "Proportion of total" ) + 
    theme_minimal() +
    scale_colour_manual(name = "England", 
                        values = c("#99a89e", # f - Bolton brand darker grey
                                   "#FFB300")) + # m - Bolton brand yellow
    scale_fill_manual(name = "Bolton",
                      values = c("#FFB300", # f - Bolton brand yellow
                                 "#99a89e")) # m - Bolton brand grey 

  #    ggplotly(myplot)
      
# population pyramid - plotly ###############################################################       

      
  plot_ly() %>% 
        # Bolton male bar - so can seperately label on legend
        add_bars(data = age_breakdown2[age_breakdown2$geography_name == "Bolton" & 
                                         age_breakdown2$sex_name== "Male",],
                 x = ~ifelse(sex_name == "Male", -obs_value, obs_value), 
                 y = ~age_name, 
                 color = ~sex_name,
                 orientation = 'h', 
                 hoverinfo = 'text', 
                  text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
                 colors = c("#FFB300", #  - Bolton brand yellow
                            "#99a89e"), #  - Bolton brand grey
                 legendgroup = "Bolton",
                 name = "Bolton male"
                 ) %>% 
        # Bolton female bar 
        add_bars(data = age_breakdown2[age_breakdown2$geography_name == "Bolton" & 
                                         age_breakdown2$sex_name== "Female",],
                 x = ~ifelse(sex_name == "Male", -obs_value, obs_value), 
                 y = ~age_name, 
                 color = ~sex_name,
                 orientation = 'h', 
                 hoverinfo = 'text', 
                  text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
                 colors = c("#FFB300", #  - Bolton brand yellow
                            "#99a89e"), #  - Bolton brand grey
                 legendgroup = "Bolton",
                 name = "Bolton female"
        ) %>% 
        add_paths(data = age_breakdown2[age_breakdown2$geography_name == "England" & 
                                          age_breakdown2$sex_name== "Male" ,] %>%
                    group_by(sex_name),
                  color = ~sex_name, # 2 lines 
                  x = ~ifelse(sex_name == "Male", -obs_value, obs_value),
                  y = ~age_name,
                  hoverinfo = 'text', 
                    text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
                  orientation = 'h',
                  line = list(color="#FFB300"), # bolton brand yellow
                  legendgroup = "England", 
                  name = "England male"
                  ) %>%
        # female england line
        add_paths(data = age_breakdown2[age_breakdown2$geography_name == "England" &
                                          age_breakdown2$sex_name == "Female" ,] %>%
                    group_by(sex_name),
                  color = ~sex_name,
                  x = ~ifelse(sex_name == "Male", -obs_value, obs_value),
                  y = ~age_name,
                  hoverinfo = 'text', 
                    text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
                  orientation = 'h',
                  line = list(color="#99a89e"), # bolton brand grey
                  legendgroup = "England", 
                  name = "England female"
        ) %>%
        layout(bargap = 0.1, barmode = 'overlay',
               xaxis = list(tickmode = 'array', tickvals = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10),
                            ticktext = c('10%', "8%", '6%', "4%", "2%", '0', "2%", '4%', "6%", "8%", '10%'))) %>%
        layout(xaxis = list(title = "<b>Proportion of total</b>"), 
               yaxis = list(title = "<b>Age group</b>"),
               # legend title not working
               legend = list(title = list(text = "Tree")
                             )
               )
        
        

