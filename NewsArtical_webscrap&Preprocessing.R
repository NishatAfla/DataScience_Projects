library(rvest)
library(tm)
library(textclean)
library(tidyverse)
library(hunspell)
library(textstem)
library(SnowballC)
library(english)
library(stringr)

install.packages("rvest")
install.packages("tm")
install.packages("textclean")
install.packages("tidyverse")
install.packages("hunspell")
install.packages("textstem")
install.packages("SnowballC")
install.packages("english")
install.packages("stringr")


# Scrape Links
base_url <- "https://www.thedailystar.net/business"
limit <- 10
all_links <- c()
page_num <- 1

while (length(all_links) < limit) {
  current_page_url <- paste0(base_url, "?page=", page_num)
  main_page <- read_html(current_page_url)
  
  links <- main_page %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    unique() %>% 
    .[grepl("/business/news/", .)]
  
  links <- paste0("https://www.thedailystar.net", links)
  all_links <- unique(c(all_links, links))
  
  next_page <- main_page %>% 
    html_nodes(".pager-show-more-next a") %>% 
    html_attr("href")
  
  if (length(next_page) == 0) break
  page_num <- page_num + 1
}

article_links <- head(all_links, limit)

# Scrape Contents
articles <- list()
for (url in article_links) {
  article_page <- read_html(url)
  content <- article_page %>% 
    html_nodes("article p") %>%
    html_text(trim = TRUE)
  
  if (length(content) == 0) {
    content <- article_page %>% 
      html_nodes("div.content p") %>%
      html_text(trim = TRUE)
  }
  
  articles[[url]] <- paste(content, collapse = " ")
}

articles_df <- data.frame(content = unlist(articles), stringsAsFactors = FALSE)
write.csv(articles_df, "DailyStar_Business_Unprocessed.csv", row.names = FALSE)

# Text processing functions


convert_number_to_word_fp <- function(text) {
  str_replace_all(text, "\\d+(\\.\\d+)?", function(x) {
    num <- as.numeric(x)
    int_part <- floor(num)
    dec_part <- num - int_part
    
    int_words <- as.character(as.english(int_part))
    dec_words <- ifelse(dec_part > 0, paste("point", as.character(as.english(dec_part * 100))), "")
    
    paste(int_words, dec_words)
  })
}

clean_text <- function(text) {
  text %>%
    # Convert to ASCII and remove HTML tags

    iconv(to = "ASCII", sub = "") %>%
    gsub("<[^>]+>", "", .) %>%
    # Remove non-printable characters and normalize whitespace
    str_replace_all("[[:cntrl:]]", " ") %>%
    str_squish() %>%
    
    # Convert numbers to words and remove punctuation
   
    str_remove_all("[[:punct:]]") %>%
    
    # Convert to lowercase and trim whitespace
    tolower() %>%
    str_trim()
}
last_clean <- function(text) {
  text <- str_remove_all(text, "[[:punct:]&&[^\\$]]") # Removes punctuation except dollar signs
  text <- trimws(text) # Removes leading and trailing whitespace
  return(text)
}

tokenize_text <- function(text) {
  unlist(strsplit(text, "\\s+"))
}




for (i in 1:ncol(articles_df)) {
  
  # Convert numbers to words
  articles_df[[i]] <- sapply(articles_df[[i]], convert_number_to_word_fp)
  
  # Clean and preprocess text
  cleaned_text <- sapply(articles_df[[i]], clean_text)
  cleaned_text <- replace_contraction(cleaned_text)
  cleaned_text <- replace_emoji(cleaned_text)
  cleaned_text <- gsub("\\s{2,}", " ", cleaned_text)
  
  # Remove stopwords
  stop_words <- stopwords("en")
  no_stopwords <- removeWords(cleaned_text, stop_words)
  spell_checked_text <- sapply(no_stopwords, function(x) {
    words <- unlist(strsplit(x, " "))
    corrected_words <- sapply(words, function(word) {
      suggestions <- hunspell(word)
      if (length(suggestions) > 0) word else suggestions[[1]][1]
    })
    paste(corrected_words, collapse = " ")
  })
  
  spell_checked_text <- gsub("\\s{2,}", " ", no_stopwords)
  
  stemmed_text <- wordStem(spell_checked_text)
  final_text <- lemmatize_words(stemmed_text)
  
  final_text <- gsub("\\s{2,}", " ", trimws(final_text))
  final_text <- sapply(final_text, last_clean)
  articles_df[[i]] <- final_text
}



write.csv(articles_df, "DailyStar_Business_Processed_post.csv", row.names = FALSE)

write.csv(article_links, "aricle_links.csv", row.names = FALSE)



