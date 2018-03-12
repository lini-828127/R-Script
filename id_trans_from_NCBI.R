
ID_trans_Entrez2Csa <- function(filepath, filename) {
  library(rvest)
  library(tidyverse)
  library(xml2)
  library(data.table)
  setwd(filepath)
  url <- dir(filepath)
  i <- 1
  result <- data.table()
  
  while(i < length(url)){
    EntrezGene <- str_replace_all(url[i], ".html", "")
    webpage <- read_html(url[i])
    CsaID <- html_nodes(webpage, "#summaryDl > dd:nth-child(6)") %>% 
      xml_text() %>% 
      str_replace_all(., "_", "")
    
    NCBI_des <- html_nodes(webpage, "#summaryDl > dd:nth-child(4)") %>% 
      xml_text() %>% 
      str_replace_all(., "_", "")
    
    tmp <- data.table(EntrezGene = EntrezGene,
                      CsaID = CsaID,
                      NCBI_description = NCBI_des)
    result <- rbind(result, tmp)
    i <- i+1
  }
  write.table(result, file = filename, quote = FALSE, row.names = FALSE, sep = "\t")
}

trans(filepath = filepath, filename = filename)