library(nomisr)
library(plyr)
library(dplyr)
library(janitor)
library(ggplot2)
library(forcats)
library(stringr)


# helpfile here
# https://cloud.r-project.org/web/packages/nomisr/readme/README.html

# search for indicators with population in 
  nomis_search <- nomis_search(name = "*population*")

# various items of metadata 
  nomis_meta <- nomis_get_metadata(id = "NM_31_1", "sex", "TYPE")
  nomis_meta <- nomis_get_metadata(id = "NM_31_1", "geography", "TYPE")
  nomis_meta <- nomis_get_metadata(id = "NM_31_1", "measures", "TYPE")
  nomis_meta <- nomis_get_metadata(id = "NM_31_1")
  
# bolton geography code, maxing out on rows otherwise
  bolton_geog_codes <- nomis_codelist(id= "NM_31_1", "geography", search = "*bolton*")  
  nomis_codelist(id= "NM_31_1", "geography", search = "*england*")  

# population figure ###########################################################################  
    
  # bolton population latest, all age, measures = number ie. 20100 (not %	ie 20301)  
    nomis_data <- nomis_get_data(id = "NM_31_1", date = "latest", 
                                 geography = 1816133652, sex = 7, age = 0, measures = 20100)

# data for population pyramid ################################################################ 
  
  # get data
    # bolton & england latest, 5 year age bands, % (not number)  
      age_breakdown <- nomis_get_data(id = "NM_31_1", date = "latest", 
                                   geography = c(1816133652, 2092957699), sex = c(5, 6), measures = 20301) %>%
        clean_names() %>%
        filter(age_code != 0) # excludes all ages
  
    # prepare data
      age_breakdown2 <- age_breakdown %>%
        select(geography_name, sex_name, age_sortorder, age_name, obs_value) %>%
        dplyr::mutate(age_name = str_replace(age_name, "Aged ", ""),
                      age_name = str_replace(age_name, " years", ""),
                      age_name = str_replace(age_name, " year", ""),
                      age_name = as.factor(age_name),
                      age_name = fct_reorder(age_name, age_sortorder), 
                      age_name = fct_relevel(age_name, "under 1", "1 - 4", "5 - 9", "10 - 14", "15 - 19", 
                                             "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44",
                                             "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", 
                                             "70 - 74", "75 - 79", "80 - 84", "85 and over"),
                      ) %>%
        filter(!age_name %in% c("0 - 15", "16 - 59/64", "16 - 64", "18 - 24", "65 and over"))
      
  # plot
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
                        group = sex_name),
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
        scale_fill_manual(name = "Gender",
                          values = c("#009639", # Bolton brand dark green
                                     "#FFB300")) # Bolton brand yellow 




