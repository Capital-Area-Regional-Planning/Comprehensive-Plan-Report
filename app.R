library(dplyr)
library(here)
library(sf)
library(shiny)
library(officedown)
library(tidycensus)
library(stringr)
library(tidyr)
library(ggplot2)
library(mschart) 
library(officer)
library(flextable)

# To deploy, run: rsconnect::deployApp()

ui <- fluidPage(
    selectInput("muni_choices", "Select a municipality:", muni_choice_list),
   # selectInput("neighbor_choices", "Select neighboring municipalities:", muni_choice_list, multiple = T),
    downloadButton("download_word_document", "Download")
  )
  
server <- function(input, output) {
    output$download_word_document <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function() {
        paste0("Report-", gsub(" ", "-", input$muni_choices), "-", Sys.Date(), ".docx")
      },
      content = function(file) {
        withProgress(value = 0, message = 'Creating report, please wait!',  {
        #find the census name of the selected geography
        muni_chosen <- muni_crosswalk %>% 
          filter(muni_name == input$muni_choices) %>% 
          select(census_name) %>% 
          paste()
        
        #the filtered census data
        data_muni_f <- data_muni %>%
          filter(NAME == muni_chosen) %>%
          select(-GEOID) %>%
          mutate(NAME = input$muni_choices)
        
        #for testing
        # data_muni_f <- data_muni %>%
        #   filter(NAME == "Albion town, Dane County, Wisconsin") %>%
        #   select(-GEOID) %>%
        #   mutate(NAME = "Town of Albion")

        all_data <- data_muni_f %>% 
          #join all the data into one df
          bind_rows(data_county,
                    data_state) %>%
          #transform to easily pull data for tables
          pivot_longer(2:ncol(data_muni_f)) %>% 
          mutate(value = round(value, 2)) %>%
          rename(name = NAME,
                 Variable = name) %>% 
          pivot_wider(values_from = value)
        
        data_moe_f <- moe_report_muni %>%
          filter(NAME == muni_chosen) %>%
          mutate(NAME = input$muni_choices) %>% 
          select(-NAME, -Census)
          
        
        # Set up parameters to pass to Rmd document
        params <- list(muni = input$muni_choices,
                       data = all_data,
                       moe_reports = list(muni = data_moe_f, county = moe_report_county %>% select(-NAME, -Census), state = moe_report_state %>%  select(-NAME, -Census)))
        
        rmarkdown::render("template.Rmd", output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
    )
  }
  

shinyApp(ui, server)

