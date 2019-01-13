#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#                  Jonathan Quintana - Data Driven Securty                     #
#                                                                              #
#******************************************************************************#

# install.packages("xml2")
# install.packages("tidyr")
library(xml2)
library(tidyr)
library(stringr)


GetCPEItems <- function(cpe.raw) {
  # transform the list to data frame
  cpe_df <- data.frame (
    title = xml_text(xml_find_all(cpe.raw, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]")),
    cpe_name <- xml_text(xml_find_all(cpe.raw, "//*[cpe-23:cpe23-item]/*/@name")), 
    stringsAsFactors = F
  )
  # return data frame
  return(cpe_df)
}

CleanCPEs <- function(cpes){

  # data manipulation
  columns <- c("Cpe", "Version", "Part", "Vendor", "Product", "Version", "Update", "Edition", "Language", "SW_Edition", "Target_sw", "Target_hw", "Other")
  cpes$cpe_name <- str_replace_all(cpes$cpe_name, "\\\\:", ";")
  cleaned_cpe <- separate(data = cpes, col = cpe_name, into = columns, sep = ":", remove = F)
  
  cleaned_cpe$Cpe <- as.factor(cleaned_cpe$Cpe)
  cleaned_cpe$Version <- as.factor(cleaned_cpe$Version)
  cleaned_cpe$Part <- as.factor(cleaned_cpe$Part)
  cleaned_cpe$Vendor <- as.factor(cleaned_cpe$Vendor)
  cleaned_cpe$Product <- as.factor(cleaned_cpe$Product)
  cleaned_cpe$Language <- as.factor(cleaned_cpe$Language)
  cleaned_cpe$SW_Edition <- as.factor(cleaned_cpe$SW_Edition)
  cleaned_cpe$Target_sw <- as.factor(cleaned_cpe$Target_sw)
  cleaned_cpe$Target_hw <- as.factor(cleaned_cpe$Target_hw)
  cleaned_cpe$Other <- as.factor(cleaned_cpe$Other)
  
  return(cleaned_cpe)
}


ParseCPEData <- function() {
  
  #compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
  #cpes_filename <- "cpes.zip"
  #download.file(compressed_cpes_url, cpes_filename)
  #unzip(zipfile = cpes_filename)
  cpe.file <- "./official-cpe-dictionary_v2.3.xml"
    
  # load cpes as xml file
  cpes <- xml2::read_xml(x = cpe.file)
  
  # get CPEs
  cpes <- GetCPEItems(cpes)

  # transform, clean, arrange parsed cpes as data frame
  df <- CleanCPEs(cpes)

  # return data frame
  return(df)
}



