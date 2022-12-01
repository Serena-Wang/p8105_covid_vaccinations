# Collect links to all historical versions of converage-by-demo.csv
query_base_file <- "people/coverage-by-demo.csv"
page = 1
links <- data.frame()
while (TRUE) {
  request <- httr::GET(
    'https://api.github.com/repos/nychealth/covid-vaccine-data/commits',
    query = list(path = query_base_file, 
                 sort = "author-date",
                 page = as.character(page)))
  
  if (request$status_code > 400) {
    break
  }
  
  content <- httr::content(request)
  
  if(length(content) == 0){
    break
  }
  
  for (i in 1:length(content)){
    
    curr_date = as.Date(sub("\\T.*", "", content[[i]]$commit$author$date))
    
    if (!(curr_date %in% links$date)){
      a_commit <- data.frame(date = content[[i]]$commit$author$date,
                             sha = content[[i]]$sha) %>%
        tidyr::separate(date, into = c("date", "time"), sep = "T") %>%
        dplyr::mutate(time =  sub("\\Z.*", "",time),
                      date = as.Date(date),
                      file_link = paste0(
                        "https://raw.githubusercontent.com/nychealth/covid-vaccine-data/",
                        sha,"/",query_base_file)) %>%
        dplyr::select(-sha, -time)
      
      
      links <- links %>% dplyr::bind_rows(a_commit)
      } 
    }
  page <- page + 1
}

write_csv(links, "./Data/coverage_by_demo_links.csv")

# read in all files 
coverage_by_demo_full <- map_dfr(
  links$file_link, 
  function(file_link){
    file <- read_csv(file = file_link)
    if("COUNT_ADDITIONAL_CUMULATIVE" %in% colnames(file)){
      file <- file %>%
        mutate(COUNT_ADDITIONAL_CUMULATIVE = 
                               as.numeric(COUNT_ADDITIONAL_CUMULATIVE))
    }
    
    if("COUNT_FULLY_CUMULATIVE" %in% colnames(file)){
      file <- file %>%
        mutate(COUNT_FULLY_CUMULATIVE = 
                 as.numeric(COUNT_FULLY_CUMULATIVE))
    }
    
    if("COUNT_PARTIALLY_CUMULATIVE" %in% colnames(file)){
      file <- file %>%
        mutate(COUNT_PARTIALLY_CUMULATIVE = 
                 as.numeric(COUNT_PARTIALLY_CUMULATIVE))
    }
    
    if("COUNT_1PLUS_CUMULATIVE" %in% colnames(file)){
      file <- file %>%
        mutate(COUNT_1PLUS_CUMULATIVE = 
                 as.numeric(COUNT_1PLUS_CUMULATIVE))
    }
    return (file)
  })

write_csv(coverage_by_demo_full, "./Data/coverage_by_demo_full.csv")
      