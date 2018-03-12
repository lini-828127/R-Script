GOanno_From_icugi <- function(filepath, filename) {
  # load package
  library(rvest)
  library(tidyverse)
  library(xml2)
  library(data.table)
  
  # set workspace
  setwd(filepath)
  url <- dir()
  filenumber <- length(url)
  result <- data.table()
  for (i in 1:filenumber) {
    # read data into R and Extrac the Annotate Part(web)
    web <- read_html(url[i]) %>% html_node(., "#terms-tripal-data-pane") 
    
    # GeneID 
    CsaID <- str_replace_all(url[i], ".html", "") %>% str_trim(., side = "both")
    
    # Calculate the Annotate part number(nap)
    nap <- html_text(web) %>% str_count(., "Vocabulary:")
    
    # Annotate 1st's rows (ra)
    rat1 <- html_nodes(web, "#tripal_feature-table-terms-0") %>% 
      html_table(trim = TRUE) %>% .[[1]] %>% nrow()
    
    # Judge the number of Annotation part 
    if(nap == 1 && rat1 != 0){
      table_0_caption <- html_nodes(web, "#tripal_feature-table-terms-0") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_0 <- html_nodes(web, "#tripal_feature-table-terms-0") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table0 <- data.table(CsaID = rep(CsaID, nrow(table_0)), 
                                table_0, 
                                Annotation = rep(table_0_caption, nrow(table_0)))
      result <- bind_rows(result, anno_table0)
    } 
    if(nap == 2){
      table_0_caption <- html_nodes(web, "#tripal_feature-table-terms-0") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_0 <- html_nodes(web, "#tripal_feature-table-terms-0") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table0 <- data.table(CsaID = rep(CsaID, nrow(table_0)), 
                                table_0, 
                                Annotation = rep(table_0_caption, nrow(table_0)))
      
      table_1_caption <- html_nodes(web, "#tripal_feature-table-terms-1") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_1 <- html_nodes(web, "#tripal_feature-table-terms-1") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table1 <- data.table(CsaID = rep(CsaID, nrow(table_1)), 
                                table_1, 
                                Annotation = rep(table_1_caption, nrow(table_1)))
      result <- bind_rows(result, anno_table0, anno_table1)
    }
    if (nap == 3) {
      table_0_caption <- html_nodes(web, "#tripal_feature-table-terms-0") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_0 <- html_nodes(web, "#tripal_feature-table-terms-0") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table0 <- data.table(CsaID = rep(CsaID, nrow(table_0)), 
                                table_0, 
                                Annotation = rep(table_0_caption, nrow(table_0)))
      
      table_1_caption <- html_nodes(web, "#tripal_feature-table-terms-1") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_1 <- html_nodes(web, "#tripal_feature-table-terms-1") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table1 <- data.table(CsaID = rep(CsaID, nrow(table_1)), 
                                table_1, 
                                Annotation = rep(table_1_caption, nrow(table_1)))
      table_2_caption <- html_nodes(web, "#tripal_feature-table-terms-2") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_2 <- html_nodes(web, "#tripal_feature-table-terms-2") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table2 <- data.table(CsaID = rep(CsaID, nrow(table_2)), 
                                table_2, 
                                Annotation = rep(table_2_caption, nrow(table_2)))
      result <- bind_rows(result, anno_table0, anno_table1, anno_table2)
    }
    
    if (nap == 4) {
      table_0_caption <- html_nodes(web, "#tripal_feature-table-terms-0") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_0 <- html_nodes(web, "#tripal_feature-table-terms-0") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table0 <- data.table(CsaID = rep(CsaID, nrow(table_0)), 
                                table_0, 
                                Annotation = rep(table_0_caption, nrow(table_0)))
      
      table_1_caption <- html_nodes(web, "#tripal_feature-table-terms-1") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_1 <- html_nodes(web, "#tripal_feature-table-terms-1") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table1 <- data.table(CsaID = rep(CsaID, nrow(table_1)), 
                                table_1, 
                                Annotation = rep(table_1_caption, nrow(table_1)))
      table_2_caption <- html_nodes(web, "#tripal_feature-table-terms-2") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_2 <- html_nodes(web, "#tripal_feature-table-terms-2") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table2 <- data.table(CsaID = rep(CsaID, nrow(table_2)), 
                                table_2, 
                                Annotation = rep(table_2_caption, nrow(table_2)))
      table_3_caption <- html_nodes(web, "#tripal_feature-table-terms-3") %>% html_nodes(., "caption") %>% 
        html_text() %>% str_replace_all(., "Vocabulary: ", "") %>% str_trim() 
      table_3 <- html_nodes(web, "#tripal_feature-table-terms-3") %>% html_table(trim = TRUE) %>% .[[1]]
      anno_table3 <- data.table(CsaID = rep(CsaID, nrow(table_3)), 
                                table_3, 
                                Annotation = rep(table_3_caption, nrow(table_3)))
      result <- bind_rows(result, anno_table0, anno_table1, anno_table2, anno_table3)
    }
    
  }
  write.table(result, file = filename, quote = FALSE, row.names = FALSE, sep = "\t")
  rm(list = ls())
}