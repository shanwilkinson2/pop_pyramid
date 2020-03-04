library(nomisr)
library(dplyr)
library(janitor)
library(ggplot2)
library(forcats)
library(stringr)

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
  
