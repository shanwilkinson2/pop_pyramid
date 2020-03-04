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
        # put main breaks (for tick mark labels) at chosen points 
        scale_y_continuous(breaks = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10),
         # percent is e.g. 20 for 20% not 0.2 
         # abs ie. absolute takes away +- symbol
         # adds % symbol
            labels = function(x) paste0(abs(x),"%")) +
      # change label of key
    # axis labels
      labs(x = "Age group", y = "Proportion of total" ) + 
    theme_minimal() +
    scale_colour_manual(name = "England", 
                        values = c("#99a89e", # f - Bolton brand darker grey
                                   "#FFB300")) + # m - Bolton brand yellow
    scale_fill_manual(name = "Bolton",
                      values = c("#FFB300", # f - Bolton brand yellow
                                 "#99a89e"))  # m - Bolton brand grey 


  #    ggplotly(myplot)
      
# population pyramid - plotly ###############################################################       

      
  plot_ly() %>% 
    # Bolton bars 
        add_bars(data = age_breakdown2[age_breakdown2$geography_name == "Bolton",],
      # multiply by -1 if male to separate genders
        x = ~ifelse(sex_name == "Male", -obs_value, obs_value), 
        y = ~age_name, 
        color = ~sex_name,
      # rotate bars to horizontal
        orientation = 'h', 
      # popup text
        hoverinfo = 'text', 
          text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
        colors = c("#FFB300", #  - Bolton brand yellow
                    "#99a89e"), #  - Bolton brand grey
        legendgroup = "Bolton",
      # name for legend
        name = ~paste("Bolton", sex_name)
         ) %>% 
    # England male line
        # can get the m/f lines in 2 different colours using color = ~ sex_name but then same colour as bars
        # or manually colour, but then both lines are the same colour. 
        # so split into m/ f lines. 
        add_paths(data = age_breakdown2[age_breakdown2$geography_name == "England" & 
                  age_breakdown2$sex_name== "Male" ,] %>%
                  # group by so draws line linking gender not each point in order
                  group_by(sex_name),
          color = ~sex_name, # creates 2 lines 
          x = ~ifelse(sex_name == "Male", -obs_value, obs_value),
          y = ~age_name,
        # popup text
          hoverinfo = 'text', 
            text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
          orientation = 'h',
          line = list(color="#FFB300"), # bolton brand yellow
          legendgroup = "England", 
        # name for legend
          name = "England male"
        ) %>%
    # England female line
        add_paths(data = age_breakdown2[age_breakdown2$geography_name == "England" &
                  age_breakdown2$sex_name == "Female" ,] %>%
                  # group by so draws line linking gender not each point in order
                  group_by(sex_name),
            color = ~sex_name,
            x = ~ifelse(sex_name == "Male", -obs_value, obs_value),
            y = ~age_name,
          # popup text
            hoverinfo = 'text', 
              text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
            orientation = 'h',
            line = list(color="#99a89e"), # bolton brand grey
            legendgroup = "England", 
          # name for legend
            name = "England female"
        ) %>%
    # adjust layout of chart    
      layout(bargap = 0.1, 
             # stops m/ f being offset
              barmode = 'overlay',
            xaxis = list(title = "<b>Proportion of total</b>", 
                         # create fake axis labelling to get rid of minus sign & add percent sign
                           tickmode = 'array', 
                           tickvals = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10),
                           ticktext = c('10%', "8%", '6%', "4%", "2%", '0', "2%", '4%', "6%", "8%", '10%')),
             yaxis = list(title = "<b>Proportion of total</b>"),
             # legend title not working
             legend = list(title = list(text = "Title"))
             ) 
               
      
# population pyramid - plotly - simpler if don't care about colouring ###################################       
      
      
      plot_ly() %>% 
        # Bolton bars 
        add_bars(data = age_breakdown2[age_breakdown2$geography_name == "Bolton",],
                 # multiply by -1 if male to separate genders
                 x = ~ifelse(sex_name == "Male", -obs_value, obs_value), 
                 y = ~age_name, 
                 color = ~sex_name,
                 # rotate bars to horizontal
                 orientation = 'h', 
                 # popup text
                 hoverinfo = 'text', 
                 text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
                 colors = c("#FFB300", #  - Bolton brand yellow
                            "#99a89e"), #  - Bolton brand grey
                 name = ~paste("Bolton",sex_name)
        ) %>% 
        # England lines
        add_paths(data = age_breakdown2[age_breakdown2$geography_name == "England",] %>%
                    # group by so draws line linking gender not each point in order
                    group_by(sex_name),
                  color = ~sex_name, # creates 2 lines but same colour as bars
                  x = ~ifelse(sex_name == "Male", -obs_value, obs_value),
                  y = ~age_name,
                  # popup text
                  hoverinfo = 'text', 
                  text = ~glue("{geography_name} {sex_name}<br>{age_name}: <b>{abs(round(obs_value, digits = 2))}%</b>"),
                  orientation = 'h',
                  line = list(color=("black")),
                  # hide on legend as both the same colour anyway, 
                  # does mean you can't take it off chart using legend tho
                  showlegend = FALSE 
                  ) %>%
      # adjust layout of chart    
        layout(bargap = 0.1, 
               # stops m/ f being offset
               barmode = 'overlay',
               xaxis = list(title = "<b>Proportion of total</b>", 
                            # create fake axis labelling to get rid of minus sign & add percent sign
                            tickmode = 'array', 
                            tickvals = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10),
                            ticktext = c('10%', "8%", '6%', "4%", "2%", '0', "2%", '4%', "6%", "8%", '10%')),
               yaxis = list(title = "<b>Proportion of total</b>"),
               # legend title not working
               legend = list(title = list(text = "Title"))
        )         
        

