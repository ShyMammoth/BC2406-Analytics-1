# Part 1: Acquisition - Line # 5 - 1577
# Part 2a: Fit - Department-Industry Personality Analysis - Line # 1579 - 1704
# Part 2b: Fit - Applicant's Personality Analysis - Line # 1706 - 1789
# Part 3: Attrition - Line # 1792 - 2018

# ==============================================================================================================
# Part 1: Acquisition
# ==============================================================================================================

# Quickly install prerequisite packages
packages <- c("tidyverse", "rvest", "xml2", "dplyr", "tidytext", "ggplot2",
              "data.table", "tm", "tidyr", "textstem", "wordcloud", "igraph",
              "ggraph", "topicmodels", "caret", "doSNOW", "cluster", "dendextend",
              "ggdendro", "quanteda", "pdftools", "tm")
install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(install)

# Importing prerequisite packages
library(tidyverse)
library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(data.table)
library(tm)
library(tidyr)
library(textstem)
library(wordcloud)
# library(igraph)
library(ggraph)
library(topicmodels)
library(caret)
library(doSNOW)
library(cluster)
library(dendextend)
library(ggdendro)
library(quanteda)
library(pdftools)
library(tm)

# ==============================================================================================================
# Web-Scraping
# ==============================================================================================================


# We will performing web-scraping on indeed.com, a site that allows web-scraping as stated in their terms of service
# To perform for 3 predefined roles, namely "Data Scientist", "Data Analyst", and "Data Engineer"
# Each dataset will take approximately 10 minutes to be scraped
# Note to not perform this operation back-to-back, too fast as well

# Firstly, Data Scientist
url = "https://www.indeed.com.sg/jobs?q=Data+Scientist&l="
page <- xml2::read_html(url)
# Manually searched for last-page results (62 total pages as of 02/11/2019)
page_result_start <- 10 # starting page 
page_result_end <- 610 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)
d_scientist <- data.frame()
for(i in seq_along(page_results)) {
  
  first_page_url <- "https://www.indeed.com.sg/jobs?q=Data%20Scientist"
  url <- paste0(first_page_url, "&start=", page_results[i])
  page <- xml2::read_html(url)
  # Sleep for 2 seconds per page to prevent "Error in open.connection(con, "rb") : Timeout was reached"
  Sys.sleep(2)
  
  # Job Title
  job_title <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("title")
  
  # Links of Job Title
  links <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("href")
  
  # Obtain Job Description from link in Job Title
  job_description <- c()
  for(i in seq_along(links)) {
    
    url <- paste0("https://www.indeed.com.sg/", links[i])
    page <- xml2::read_html(url)
    
    job_description[[i]] <- page %>%
      rvest::html_nodes("div")  %>% 
      rvest::html_nodes(xpath = '//*[@class="jobsearch-jobDescriptionText"]') %>% 
      rvest::html_text() %>%
      stringi::stri_trim_both()
  }
  
  head(d_scientist)
  
  # Dataframe of a page
  df <- data.frame(job_title, job_description)
  
  # Dataframe of all results
  d_scientist <- rbind(d_scientist, df)
}
nrow(d_scientist) # 550 of 711 total JDs scraped as of 02/11/2019
write.csv(d_scientist, "C:/Users/Tony/Desktop/d_scientist1.csv", row.names = FALSE)



# Secondly, Data Analyst
url = "https://www.indeed.com.sg/jobs?q=Data+Analyst&l="
page <- xml2::read_html(url)
# Manually searched for last-page results (100 total pages as of 02/11/2019)
page_result_start <- 10 # starting page 
page_result_end <- 1000 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)
d_analyst <- data.frame()
for(i in seq_along(page_results)) {
  
  first_page_url <- "https://www.indeed.com.sg/jobs?q=Data+Analyst"
  url <- paste0(first_page_url, "&start=", page_results[i])
  page <- xml2::read_html(url)
  # Sleep for 2 seconds per page to prevent "Error in open.connection(con, "rb") : Timeout was reached"
  Sys.sleep(2)
  
  # Job Title
  job_title <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("title")
  
  # Links of Job Title
  links <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("href")
  
  # Obtain Job Description from link in Job Title
  job_description <- c()
  for(i in seq_along(links)) {
    
    url <- paste0("https://www.indeed.com.sg/", links[i])
    page <- xml2::read_html(url)
    
    job_description[[i]] <- page %>%
      rvest::html_nodes("div")  %>% 
      rvest::html_nodes(xpath = '//*[@class="jobsearch-jobDescriptionText"]') %>% 
      rvest::html_text() %>%
      stringi::stri_trim_both()
  }
  
  head(d_analyst)
  
  # Dataframe of a page
  df <- data.frame(job_title, job_description)
  
  # Dataframe of all results
  d_analyst <- rbind(d_analyst, df)
}

nrow(d_analyst) # 1559 of 2494 total JDs scraped as of 02/11/2019
write.csv(d_analyst, "C:/Users/Tony/Desktop/d_analyst.csv", row.names = FALSE)



# Lastly, Data Engineer
url = "https://www.indeed.com.sg/jobs?q=Data+Engineer&l="
page <- xml2::read_html(url)
# Manually searched for last-page results (100 total pages as of 02/11/2019)
page_result_start <- 10 # starting page 
page_result_end <- 1000 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)
d_engineer <- data.frame()
for(i in seq_along(page_results)) {
  
  first_page_url <- "https://www.indeed.com.sg/jobs?q=Data+Engineer"
  url <- paste0(first_page_url, "&start=", page_results[i])
  page <- xml2::read_html(url)
  # Sleep for 2 seconds per page to prevent "Error in open.connection(con, "rb") : Timeout was reached"
  Sys.sleep(2)
  
  # Job Title
  job_title <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("title")
  
  # Links of Job Title
  links <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("href")
  
  # Obtain Job Description from link in Job Title
  job_description <- c()
  for(i in seq_along(links)) {
    
    url <- paste0("https://www.indeed.com.sg/", links[i])
    page <- xml2::read_html(url)
    
    job_description[[i]] <- page %>%
      rvest::html_nodes("div")  %>% 
      rvest::html_nodes(xpath = '//*[@class="jobsearch-jobDescriptionText"]') %>% 
      rvest::html_text() %>%
      stringi::stri_trim_both()
  }
  
  head(d_engineer)
  
  # Dataframe of a page
  df <- data.frame(job_title, job_description)
  
  # Dataframe of all results
  d_engineer <- rbind(d_engineer, df)
}

nrow(d_engineer) # 1001 of 4112 total JDs scraped as of 02/11/2019
write.csv(d_engineer, "C:/Users/Tony/Desktop/d_engineer.csv", row.names = FALSE)


# Web-Scrape programming languages
url1 = "https://en.wikipedia.org/wiki/List_of_programming_languages"
page1 <- xml2::read_html(url1)
prog_lang <- page1 %>% 
  rvest::html_nodes("div ul li a") %>% 
  rvest::html_attr("title")
writeLines(prog_lang, "~/The Documents/School/BC2406 - Visual & Predict/Project/Project/Text Mining/prog_lang.txt")

# ==============================================================================================================
# Pre-Processing of Web-Scraped Data
# ==============================================================================================================

setwd("~/The Documents/School/BC2406 - Visual & Predict/Project/Project/Text Mining")
d_scientist <- fread("d_scientist.csv")
d_analyst <- fread("d_analyst.csv")
d_engineer <- fread("d_engineer.csv")
skills <- readLines("skills.txt")

# Cleaning data scientist
d_scientist <- as.character(d_scientist$job_description) # Converting dataset to character 
d_scientist <- gsub("[\r\n]", "", d_scientist) # Removing line breaks 
d_scientist <- gsub("[[:punct:]]", "", d_scientist) # Removing punctuations
d_scientist <- gsub("[^[:alpha:]]", " ", d_scientist) # Converting to alphabetical format, as numbers is not meaningful to find useful words for resume
d_scientist <- iconv(d_scientist, 'utf-8', 'ascii', sub='') # Removing characters of UTF-8 (e.g. Ábcdêãçoàúü) by converting to Ascii
d_scientist <- trimws(d_scientist) # Trim leading/trail whitespaces
d_scientist <- tolower(d_scientist) # Convert all to lower casing

# Cleaning data analyst
d_analyst <- as.character(d_analyst$job_description) # Converting dataset to character 
d_analyst <- gsub("[\r\n]", "", d_analyst) # Removing line breaks 
d_analyst <- gsub("[[:punct:]]", "", d_analyst) # Removing punctuations
d_analyst <- gsub("[^[:alpha:]]", " ", d_analyst) # Converting to alphabetical format, as numbers is not meaningful to find useful words for resume
d_analyst <- iconv(d_analyst, 'utf-8', 'ascii', sub='') # Removing characters of UTF-8 (e.g. Ábcdêãçoàúü) by converting to Ascii
d_analyst <- trimws(d_analyst) # Trim leading/trail whitespaces
d_analyst <- tolower(d_analyst) # Convert all to lower casing

# Cleaning data engineer
d_engineer <- as.character(d_engineer$job_description) # Converting dataset to character 
d_engineer <- gsub("[\r\n]", "", d_engineer) # Removing line breaks 
d_engineer <- gsub("[[:punct:]]", "", d_engineer) # Removing punctuations
d_engineer <- gsub("[^[:alpha:]]", " ", d_engineer) # Converting to alphabetical format, as numbers is not meaningful to find useful words for resume
d_engineer <- iconv(d_engineer, 'utf-8', 'ascii', sub='') # Removing characters of UTF-8 (e.g. Ábcdêãçoàúü) by converting to Ascii
d_engineer <- trimws(d_engineer) # Trim leading/trail whitespaces
d_engineer <- tolower(d_engineer) # Convert all to lower casing


# ==============================================================================================================
# Data Cleaning
# ==============================================================================================================


# Bigrams (1 token = 2 words make most meaningful sense in this context) 
# e.g. "big data", "artificial intelligence" and "machine learning"

ds <- tibble(text = d_scientist)
ds <-  ds %>%
  mutate(docnumber = row_number()) %>% 
  unnest_tokens(bigram, text, token ="ngrams", n = 2) 

da <- tibble(text = d_analyst)
da <-  da %>%
  mutate(docnumber = row_number()) %>% 
  unnest_tokens(bigram, text, token ="ngrams", n = 2) 

de <- tibble(text = d_engineer)
de <-  de %>%
  mutate(docnumber = row_number()) %>% 
  unnest_tokens(bigram, text, token ="ngrams", n = 2) 


# Separate bigram for cleaning (all while maintaining bigram's structural integrity)
ds_sep <- ds %>%
  separate(bigram, c("word1", "word2"), sep = " ")

da_sep <- da %>%
  separate(bigram, c("word1", "word2"), sep = " ")

de_sep <- de %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Clean via filtering stop words
ds_sep
ds_sep <- ds_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
row_number(ds_sep)
ds_sep

da_sep
da_sep <- da_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
da_sep

de_sep
de_sep <- de_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
de_sep

# Clean via filtering freq 1 (often not meaningful words, e.g. "aaa")
ds_sep %>% 
  count(word1, sort = TRUE) %>% 
  arrange(n) %>% 
  print(n=20)

ds_sep
ds_word1_lowcount <- ds_sep %>% count(word1, sort = TRUE) %>% filter(n <= 1, .preserve=TRUE)
ds_word2_lowcount <- ds_sep %>% count(word2, sort = TRUE) %>% filter(n <= 1, .preserve=TRUE)
ds_sep <- ds_sep %>%
  filter(!word1 %in% ds_word1_lowcount$word1) %>%
  filter(!word2 %in% ds_word2_lowcount$word2) %>% print()

da_word1_lowcount <- da_sep %>% count(word1, sort = TRUE) %>% filter(n <= 1, .preserve=TRUE)
da_word2_lowcount <- da_sep %>% count(word2, sort = TRUE) %>% filter(n <= 1, .preserve=TRUE)
da_sep <- da_sep %>%
  filter(!word1 %in% ds_word1_lowcount$word1) %>%
  filter(!word2 %in% ds_word2_lowcount$word2) %>% print()

de_word1_lowcount <- de_sep %>% count(word1, sort = TRUE) %>% filter(n <= 1, .preserve=TRUE)
de_word2_lowcount <- de_sep %>% count(word2, sort = TRUE) %>% filter(n <= 1, .preserve=TRUE)
de_sep <- de_sep %>%
  filter(!word1 %in% de_word1_lowcount$word1) %>%
  filter(!word2 %in% de_word2_lowcount$word2) %>% print()


# Clean via filtering high char words (often incorrectly processed) via trial and error
ds_sep %>% 
  mutate(word1_chars = nchar(word1)) %>% 
  mutate(word2_chars = nchar(word2)) %>% 
  arrange(desc(word1_chars)) %>%
  print(n=50)

ds_temp <- ds_sep %>%
  filter(nchar(word1) < 20) %>%
  filter(nchar(word2) < 20)

ds_temp %>% 
  mutate(word1_chars = nchar(word1)) %>% 
  mutate(word2_chars = nchar(word2)) %>% 
  arrange(desc(word1_chars)) %>%
  print(n=50)
# Still observing error prone words, thus, to continue reducing character size allowed

ds_temp <- ds_sep %>%
  filter(nchar(word1) < 16) %>%
  filter(nchar(word2) < 16)

ds_temp %>% 
  mutate(word1_chars = nchar(word1)) %>% 
  mutate(word2_chars = nchar(word2)) %>% 
  arrange(desc(word1_chars)) %>%
  print(n=50)

ds_sep <- ds_sep %>%
  filter(nchar(word1) < 16) %>%
  filter(nchar(word2) < 16) %>% print()
# Majority of words are valid, thus, to stop cleaning via this method as it is a damaging method

da_temp <- da_sep %>%
  filter(nchar(word1) < 20) %>%
  filter(nchar(word2) < 20) 

da_temp %>% 
  mutate(word1_chars = nchar(word1)) %>% 
  mutate(word2_chars = nchar(word2)) %>% 
  arrange(desc(word1_chars)) %>%
  print(n=50)
# Still observing error prone words, thus, to continue reducing character size allowed


da_temp <- da_sep %>%
  filter(nchar(word1) < 16) %>%
  filter(nchar(word2) < 16)

da_temp %>% 
  mutate(word1_chars = nchar(word1)) %>% 
  mutate(word2_chars = nchar(word2)) %>% 
  arrange(desc(word1_chars)) %>%
  print(n=50)

da_sep <- da_sep %>%
  filter(nchar(word1) < 16) %>%
  filter(nchar(word2) < 16) %>% print()
# Majority of words are valid, thus, to stop cleaning via this method as it is a damaging method


de_temp <- de_sep %>%
  filter(nchar(word1) < 20) %>%
  filter(nchar(word2) < 20) %>%

de_temp %>% 
  mutate(word1_chars = nchar(word1)) %>% 
  mutate(word2_chars = nchar(word2)) %>% 
  arrange(desc(word1_chars)) %>%
  print(n=50)
# Still observing error prone words, thus, to continue reducing character size allowed

de_temp <- de_sep %>%
  filter(nchar(word1) < 16) %>%
  filter(nchar(word2) < 16)

de_temp %>% 
  mutate(word1_chars = nchar(word1)) %>% 
  mutate(word2_chars = nchar(word2)) %>% 
  arrange(desc(word1_chars)) %>%
  print(n=50)

de_sep <- de_sep %>%
  filter(nchar(word1) < 16) %>%
  filter(nchar(word2) < 16) %>% print()
# Majority of words are valid, thus, to stop cleaning via this method as it is a damaging method

# Clean via lemmatizing (e.g. run, runs, running, ran has the same meaning)
ds_sep <- ds_sep %>% 
  distinct() %>% 
  mutate(word1 = lemmatize_words(word1)) %>% 
  mutate(word2 = lemmatize_words(word2)) %>% print()

da_sep <- da_sep %>% 
  distinct() %>% 
  mutate(word1 = lemmatize_words(word1)) %>% 
  mutate(word2 = lemmatize_words(word2)) %>% print()

de_sep <- de_sep %>% 
  distinct() %>% 
  mutate(word1 = lemmatize_words(word1)) %>% 
  mutate(word2 = lemmatize_words(word2)) %>% print()

# Dictionary for lemmatize_words function substitutes "data" for "datum". Thus, to explicitly revert this.
ds_sep$word1 <- gsub("datum", "data", ds_sep$word1)
ds_sep$word2 <- gsub("datum", "data", ds_sep$word2)
da_sep$word1 <- gsub("datum", "data", da_sep$word1)
da_sep$word2 <- gsub("datum", "data", da_sep$word2)
de_sep$word1 <- gsub("datum", "data", de_sep$word1)
de_sep$word2 <- gsub("datum", "data", de_sep$word2)

# Uniting separated bigrams
ds <- ds_sep %>%
  unite(bigram, word1, word2, sep = " ")

da <- da_sep %>%
  unite(bigram, word1, word2, sep = " ")

de <- de_sep %>%
  unite(bigram, word1, word2, sep = " ")

# ==============================================================================================================
# General Text Analysis
# ==============================================================================================================

# By count
ds %>% count(bigram, sort = TRUE) 
da %>% count(bigram, sort = TRUE) 
de %>% count(bigram, sort = TRUE) 

# By prop (since scraped dataset does not have same length)
ds_prop <- ds %>%
  count(bigram, sort = TRUE) %>% 
  mutate(doc = "ds") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=10)
da_prop <- da %>%
  count(bigram, sort = TRUE) %>% 
  mutate(doc = "da") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=10)
de_prop <- de %>%
  count(bigram, sort = TRUE) %>% 
  mutate(doc = "de") %>%  mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=10)


merged <- dplyr::union(ds_prop, da_prop)
merged <- dplyr::union(merged, de_prop)
# merged %>% print(n=50)
# merged %>% filter(bigram=="computer science")

merged <- merged %>%
  group_by(doc) %>% 
  top_n(10) %>%
  ungroup() %>% 
  arrange(doc, proportion) %>%
  mutate(bigram = reorder(bigram, freq)) %>%
  mutate(order = row_number())


ggplot(merged, aes(order, proportion, fill = bigram)) +
geom_col(stat = "identity", show.legend = FALSE) + xlab(NULL) + coord_flip() + facet_wrap(~doc, scales = "free_y") +
scale_x_continuous(
  breaks = merged$order,
  labels = merged$bigram,
  expand = c(0,0)) +
labs(y = "Proportion of token used per job role", x = NULL)


# Word Cloud
set.seed(2019)
ds %>%
  count(bigram) %>%
  with(wordcloud(bigram, n, max.words = 100, min.freq = 1, random.order=FALSE, rot.per = 0.35, colors=brewer.pal(8,"Dark2")))
da %>%
  count(bigram) %>%
  with(wordcloud(bigram, n, max.words = 100, min.freq = 1, 
                 scale=c(1.5, .5), random.order=FALSE, rot.per = 0.35, colors=brewer.pal(8,"Dark2")))
de %>%
  count(bigram) %>%
  with(wordcloud(bigram, n, max.words = 100, min.freq = 1, 
                 scale=c(1.5, .5), random.order=FALSE, rot.per = 0.35, colors=brewer.pal(8,"Dark2")))


# Employ igraph to create graph network
library(igraph)
ds_graph <- ds_sep %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 30) %>%
  graph_from_data_frame()

da_graph <- da_sep %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 120) %>%
  graph_from_data_frame()

de_graph <- de_sep %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 50) %>%
  graph_from_data_frame()

# Each terms are treated as nodes, and connected to other nodes with directional arrow
set.seed(2019)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(ds_graph, layout = "fr") +
  geom_edge_link(aes(), show.legend = TRUE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(da_graph, layout = "fr") +
  geom_edge_link(aes(), show.legend = TRUE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(de_graph, layout = "fr") +
  geom_edge_link(aes(), show.legend = TRUE, arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# Document-Term-Matrix
merged_dtm <- merged %>% 
  cast_dtm(doc, bigram, freq)
merged_matrix <- as.matrix(merged_dtm)

dim(merged_matrix) # 3 documents, 98998 tokens
View(merged_matrix[1:3, 1:100]) # Show first 100 tokens
colnames(merged_matrix)[1:100]


# Find inherent topics with LDA
merged_lda <- LDA(merged_dtm, k = 2, control = list(seed = 2019))
merged_lda

merged_topics <- tidy(merged_lda, matrix = "beta")
merged_topics

merged_top_terms <- merged_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

merged_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
# Through the use of LDA, it has separated demographic terms from technical terms 


# Removing sparse terms
merged_dtm <- removeSparseTerms(merged_dtm, 0.1)
merged_dtm
freq <- sort(colSums(as.matrix(merged_dtm)), decreasing=TRUE)   
# findFreqTerms(dtm, lowfreq=10)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  
ggplot(subset(wf, freq>300), aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))   


# Clustering
# Agglomerative Clustering
m <- as.matrix(t(merged_dtm))
d <- dist(m, method = "euclidean")
groups <- hclust(d, method="ward.D")

# Plot dendrogram (cannot observe anything)
ggdendrogram(groups, rotate = TRUE, theme_dendro = FALSE)
# plot(groups, cex=0.9, hang=-1)
# rect.hclust(groups, k=5)


# Exploring Sub-Dendrograms
dev.new(width=5, height=4)
dend <- d %>% 
  hclust %>% 
  as.dendrogram %>%  
  set("labels_to_character") %>% 
  color_branches(k=6)
labels_cex(dend) <- 0.8
# labels(dend)
# length(labels(dend))

dend_list <- get_subdendrograms(dend, 5)
par(mfrow = c(2,3))
# png("plotdendogram.png",width=1600,height=800)
plot(dend, main = "Original dendrogram", cex.lab = 0.75, cex.axis = 0.75)
sapply(dend_list, plot)

  # Reset 
dev.off()
par(mfrow=c(1,1))


# ==============================================================================================================
# Targeted Analysis 
# Programming Languages
# ==============================================================================================================

# Find programming languages
prog_lang <- readLines("prog_lang.txt")

# Cleaning programming languages 
prog_lang <- prog_lang[36:794] # Poorly-scraped data
prog_lang <- gsub("[\r\n]", "", prog_lang) # Removing line breaks 
prog_lang <- trimws(prog_lang) # Trim leading/trail whitespaces
prog_lang <- gsub("[[:punct:]]", "", prog_lang) # Removing punctuations
prog_lang <- gsub(" ", "", prog_lang) # Removing spaces
prog_lang <- tolower(prog_lang) # Convert all to lower casing
prog_lang <- gsub("programminglanguage", "", prog_lang) # Non-meaningful contextual words
prog_lang <- gsub("processing", "", prog_lang) # Non-meaningful contextual words
prog_lang <- gsub("language", "", prog_lang) # Non-meaningful contextual words
prog_lang <- prog_lang[c(1:700, 726:759)]  # Poorly-scraped data
prog_lang

prog_lang <- tibble(prog_lang)
prog_lang <-  prog_lang %>%
  unnest_tokens(prog_lang, prog_lang) # Output, Input

total_ds <- c(ds_sep$word1, ds_sep$word2)
total_ds <- tibble(prog_lang = total_ds)
total_da <- c(da_sep$word1, da_sep$word2)
total_da <- tibble(prog_lang = total_da)
total_de <- c(de_sep$word1, de_sep$word2)
total_de <- tibble(prog_lang = total_de)


# ====================================================================================================================
# Inner join with programming languages

ds_prog_skills <- inner_join(prog_lang, total_ds)
da_prog_skills <- inner_join(prog_lang, total_da)
de_prog_skills <- inner_join(prog_lang, total_de)
ds_prog_skills <- ds_prog_skills %>%
  filter(!prog_lang %in% stop_words$word)
da_prog_skills <- da_prog_skills %>%
  filter(!prog_lang %in% stop_words$word)
de_prog_skills <- de_prog_skills %>%
  filter(!prog_lang %in% stop_words$word)

# ====================================================================================================================
# Frequent word counts

ds_prog_skills_prop <- ds_prog_skills %>%
  count(prog_lang, sort = TRUE) %>% 
  mutate(doc = "ds") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% top_n(10) %>% print()
da_prog_skills_prop <- da_prog_skills %>%
  count(prog_lang, sort = TRUE) %>% 
  mutate(doc = "da") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% top_n(10) %>% print()
de_prog_skills_prop <- de_prog_skills %>%
  count(prog_lang, sort = TRUE) %>% 
  mutate(doc = "de") %>%  mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% top_n(10) %>% print()

ds_prog_skills_prop %>%
  top_n(10) %>%
  mutate(prog_lang = reorder(prog_lang, proportion)) %>%
  ggplot(aes(prog_lang, proportion, fill = prog_lang)) +
  geom_col() + xlab(NULL) + coord_flip() + 
  labs (y = "Proportion of top 10 programming languages (DS)")

  da_prog_skills_prop %>%
  top_n(10) %>%
  mutate(prog_lang = reorder(prog_lang, proportion)) %>%
  ggplot(aes(prog_lang, proportion, fill = prog_lang)) +
  geom_col() + xlab(NULL) + coord_flip() + 
  labs (y = "Proportion of top 10 programming languages (DA)")
  
de_prog_skills_prop %>%
  top_n(10) %>%
  mutate(prog_lang = reorder(prog_lang, proportion)) %>%
  ggplot(aes(prog_lang, proportion, fill = prog_lang)) +
  geom_col() + xlab(NULL) + coord_flip() + 
  labs (y = "Proportion of top 10 programming languages (DE)")

merged <- dplyr::union(ds_prog_skills_prop, da_prog_skills_prop)
merged <- dplyr::union(merged, de_prog_skills_prop)


# Visualisation
merged %>%
  group_by(doc) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(prog_lang = reorder(prog_lang, proportion)) %>%
  ggplot(aes(prog_lang, proportion, fill = prog_lang)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Proportion",
       x = "Top 10 programming langauges as a whole") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

merged %>%
  group_by(doc) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(prog_lang = reorder(prog_lang, proportion)) %>%
  ggplot(aes(prog_lang, proportion, fill = prog_lang)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~doc, scales = "free_y") +
  labs(y = "Proportion",
       x = "Top 10 programming languages") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Selection
merged %>% 
  filter(prog_lang=="python") %>% 
  arrange(desc(proportion))


# Programming Languages Document-Term-Matrix
prog_lang_dtm <- merged %>% 
  cast_dtm(doc, prog_lang, freq)
prog_lang_matrix <- as.matrix(prog_lang_dtm)

dim(prog_lang_matrix) # 3 documents, 16 tokens
View(prog_lang_matrix) 
colnames(prog_lang_matrix)


# ==============================================================================================================
# Data Science Tools
# Resource : https://data-flair.training/blogs/data-science-tools/
# ==============================================================================================================

ds_tools <- c("sas", "apache", "spark", "bigml", "d3", "matlab", "excel", "ggplot", "tableau", 
              "jupyter", "matplotlib", "ntlk", "sklearn", "tensorflow", "weka")
ds_tools <- tibble(ds_tools)

# ====================================================================================================================
# Inner join with data science tools

colnames(total_ds) <- "ds_tools" 
colnames(total_da) <- "ds_tools" 
colnames(total_de) <- "ds_tools" 

ds_dstools <- inner_join(ds_tools, total_ds)
da_dstools <- inner_join(ds_tools, total_da)
de_dstools <- inner_join(ds_tools, total_de)

# ====================================================================================================================
# Frequent word counts

ds_dstools_prop <- ds_dstools %>%
  count(ds_tools, sort = TRUE) %>% 
  mutate(doc = "ds") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print()

da_dstools_prop <- da_dstools %>%
  count(ds_tools, sort = TRUE) %>% 
  mutate(doc = "da") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print()

de_dstools_prop <- de_dstools %>%
  count(ds_tools, sort = TRUE) %>% 
  mutate(doc = "de") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print()


ds_dstools_prop %>%
  top_n(10) %>%
  mutate(ds_tools = reorder(ds_tools, proportion)) %>%
  ggplot(aes(ds_tools, proportion, fill = ds_tools)) +
  geom_col() + xlab(NULL) + coord_flip() + 
  labs(y="Proportion of top 10 data-science tools (DS)")

da_dstools_prop %>%
  top_n(10) %>%
  mutate(ds_tools = reorder(ds_tools, proportion)) %>%
  ggplot(aes(ds_tools, proportion, fill = ds_tools)) +
  geom_col() + xlab(NULL) + coord_flip() + 
  labs(y="Proportion of top 10 data-science tools (DA)")


de_dstools_prop %>%
  top_n(10) %>%
  mutate(ds_tools = reorder(ds_tools, proportion)) %>%
  ggplot(aes(ds_tools, proportion, fill = ds_tools)) +
  geom_col() + xlab(NULL) + coord_flip() + 
  labs(y="Proportion of top 10 data-science tools (DE)")


merged <- dplyr::union(ds_dstools_prop, da_dstools_prop)
merged <- dplyr::union(merged, de_dstools_prop)

# Visualisation
merged %>%
  group_by(doc) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(ds_tools = reorder(ds_tools, proportion)) %>%
  ggplot(aes(ds_tools, proportion, fill = ds_tools)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Proportion",
       x = "Top 10 data-science tools as a whole") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


merged %>%
  group_by(doc) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(ds_tools = reorder(ds_tools, proportion)) %>%
  ggplot(aes(ds_tools, proportion, fill = ds_tools)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~doc, scales = "free_y") +
  labs(y = "Proportion of top 10 data science tools",
       x = NULL) +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# Selection
merged %>% 
  filter(ds_tools=="spark") %>% 
  arrange(desc(proportion))


# Data Science Tools Document-Term-Matrix
dstools_dtm <- merged %>% 
  cast_dtm(doc, ds_tools, freq)
dstools_matrix <- as.matrix(dstools_dtm)

dim(dstools_matrix) # 3 documents, 12 tokens
colnames(dstools_matrix) # List 12 tokens
View(dstools_matrix) 


# ==============================================================================================================
# Technical Skills
# Resource : http://nirvacana.com/thoughts/2013/07/08/becoming-a-data-scientist/
# ==============================================================================================================

# fundamentals <- c("matrices", "algebra", "hash", "binarytree", "cap", "relational", "db", "join", "tabular", "dataframes", "series",
#                   "sharding", "olap", "multidimensional", "etl", "reporting", "bi", "json", "xml", "nosql", "regex",
#                   "vendor", "landscape", "env")
# statistics <- c("uci", "repo", "descriptive", "statistics", "histogram", "percentile", "outlier", "probability", "bayes", "cdf",
#                 "normal", "poisson", "gaussian", "distribution", "skewness", "anova", "density", "limit", "monte", "carlo",
#                 "hypothesis", "chisquare", "estimation", "confidence", "interval", "kernel", "density", "regression", "covariance",
#                 "correlation", "pearson", "causation", "fit", "euclidean", "distance")
# machinelearning <- c("machine", "learning", "categorical", "continuous", "supervised", "unsupervised", "reinforced", "learning",
#                      "traintest", "classify", "decision", "tree", "predict", "lift", "overfit", "underfit", "bias", "variance",
#                      "boosting", "naive", "knn", "logistic", "linear", "perceptron", "hierarchical", "clustering", "sentimental",
#                      "neural", "tagging")
# textmining <- c("vocabulary", "mapping", "ntlk", "weka", "mahout", "extraction", "associaton", "vector",
#                 "term", "document", "frequency", "matrix", "uima", "recognition", "corpus")
# visualisation <- c("visualisation", "histogram", "boxplot", "uni", "bi", "multivariate", "viz", "ggplot", "histogram", "pie", "tree", "map",
#                    "scatter", "spatial", "survey", "timeline", "decision", "d3", "infovis", "manyeyes", "tableau")
# bigdata <- c("map", "reduce", "hadoop", "hdfs", "replication", "cloudera", "hortonworks",
#              "nodes", "mir", "sqoop", "flume", "scribe", "pig", "chukwa", "weblog", "mahout", "zookeeper", "avro", "storm",
#              "rhadoop", "rmr", "cassandra", "mongodb", "neo4j")
# dataingestion <- c("format", "discovery", "acquisition", "integration", "fusion", "transformation", "enrichment", "survey",
#                    "openrefine", "etl")
# datamunging <- c("principal", "component", "stratified", "sampling", "denoising", "extraction", "binning", "sparse", "estimators",
#                  "unbiased", "missing", "values", "scrubbing", "normalization", "dimensionality", "numerosity", "reduction")
# techskills <- c(fundamentals, statistics, machinelearning, textmining, visualisation, bigdata, dataingestion, datamunging)

techskills <- c("classification", "regression", "statistics", "visualisation", "extraction", "discovery")
techskills <- tibble(techskills)

# ====================================================================================================================
# Inner join with technical skills

colnames(techskills) <- "tskills" 
colnames(total_ds) <- "tskills" 
colnames(total_da) <- "tskills" 
colnames(total_de) <- "tskills" 

ds_tskills <- inner_join(techskills, total_ds)
da_tskills <- inner_join(techskills, total_da)
de_tskills <- inner_join(techskills, total_de)

# ====================================================================================================================
# Frequent word counts

ds_tskills_prop <- ds_tskills %>%
  count(tskills, sort = TRUE) %>% 
  mutate(doc = "ds") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=50)

da_tskills_prop <- da_tskills %>%
  count(tskills, sort = TRUE) %>% 
  mutate(doc = "da") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=50)

de_tskills_prop <- de_tskills %>%
  count(tskills, sort = TRUE) %>% 
  mutate(doc = "de") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=50)


ds_tskills_prop %>%
  mutate(tskills = reorder(tskills, proportion)) %>%
  ggplot(aes(tskills, proportion, fill = tskills)) +
  geom_col() + xlab(NULL) + coord_flip() +
  labs(y = "Proportion of technical skills (DS)")

da_tskills_prop %>%
  mutate(tskills = reorder(tskills, proportion)) %>%
  ggplot(aes(tskills, proportion, fill = tskills)) +
  geom_col() + xlab(NULL) + coord_flip() +
  labs(y = "Proportion of technical skills (DA)")

de_tskills_prop %>%
  mutate(tskills = reorder(tskills, proportion)) %>%
  ggplot(aes(tskills, proportion, fill = tskills)) +
  geom_col() + xlab(NULL) + coord_flip() +
  labs(y = "Proportion of technical skills (DE)")

merged <- dplyr::union(ds_tskills_prop, da_tskills_prop)
merged <- dplyr::union(merged, de_tskills_prop)

# Visualisation
merged %>%
  group_by(doc) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(tskills = reorder(tskills, proportion)) %>%
  ggplot(aes(tskills, proportion, fill = tskills)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Proportion",
       x = "Technical skills analysed as a whole") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


merged %>%
  group_by(doc) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(tskills = reorder(tskills, proportion)) %>%
  ggplot(aes(tskills, proportion, fill = tskills)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~doc, scales = "free_y") +
  labs(y = "Proportion of technical skills",
       x = NULL) +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# Selection
merged %>% 
  filter(tskills=="visualisation") %>% 
  arrange(desc(proportion))


# Data Science Tools Document-Term-Matrix
tskills_dtm <- merged %>% 
  cast_dtm(doc, tskills, freq)
tskills_matrix <- as.matrix(tskills_dtm)

dim(tskills_matrix) # 3 documents, 5 tokens
colnames(tskills_matrix) # List 5 tokens
View(tskills_matrix) 

# ==============================================================================================================
# Soft Skills
# Resource : https://resumegenius.com/blog/resume-help/soft-skills
# ==============================================================================================================

softskill <- c("communicate", "teamwork", "interpersonal", "creative", "leadership", "responsible", "discipline", "initiative")
softskill <- tibble(softskill)

# ====================================================================================================================
# Inner join with data science tools

colnames(total_ds) <- "softskill" 
colnames(total_da) <- "softskill" 
colnames(total_de) <- "softskill" 

ds_softskill <- inner_join(softskill, total_ds)
da_softskill <- inner_join(softskill, total_da)
de_softskill <- inner_join(softskill, total_de)

# ====================================================================================================================
# Frequent word counts

ds_softskill_prop <- ds_softskill %>%
  count(softskill, sort = TRUE) %>% 
  mutate(doc = "ds") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=12)

da_softskill_prop <- da_softskill %>%
  count(softskill, sort = TRUE) %>% 
  mutate(doc = "da") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=12)

de_softskill_prop <- de_softskill %>%
  count(softskill, sort = TRUE) %>% 
  mutate(doc = "de") %>% mutate(freq = n) %>% mutate(total = sum(n)) %>% mutate(proportion = n / sum (n)) %>% 
  select(-n) %>% print(n=12)


ds_softskill_prop %>%
  mutate(softskill = reorder(softskill, proportion)) %>%
  ggplot(aes(softskill, proportion, fill = softskill)) +
  geom_col() + xlab(NULL) + coord_flip() +
  labs(y = "Proportion of soft skills (DS)")

da_softskill_prop %>%
  mutate(softskill = reorder(softskill, proportion)) %>%
  ggplot(aes(softskill, proportion, fill = softskill)) +
  geom_col() + xlab(NULL) + coord_flip()

de_softskill_prop %>%
  mutate(softskill = reorder(softskill, proportion)) %>%
  ggplot(aes(softskill, proportion, fill = softskill)) +
  geom_col() + xlab(NULL) + coord_flip()

merged <- dplyr::union(ds_softskill_prop, da_softskill_prop)
merged <- dplyr::union(merged, de_softskill_prop)

# Visualisation
merged %>%
  group_by(doc) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(softskill = reorder(softskill, proportion)) %>%
  ggplot(aes(softskill, proportion, fill = softskill)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL,
       x = "Soft skills analysed as a whole") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


merged %>%
  group_by(doc) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(softskill = reorder(softskill, proportion)) %>%
  ggplot(aes(softskill, proportion, fill = softskill)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~doc, scales = "free_y") +
  labs(y = "Proportion of soft skills",
       x = NULL) +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# Selection
merged %>% 
  filter(softskill=="communication") %>% 
  arrange(desc(proportion))


# Soft Skill Document-Term-Matrix
softskill_dtm <- merged %>% 
  cast_dtm(doc, softskill, freq)
softskill_matrix <- as.matrix(softskill_dtm)

dim(softskill_matrix) # 3 documents, 8 tokens
colnames(softskill_matrix) # List 8 tokens
View(softskill_matrix) 

# ==============================================================================================================
# Application 
# 1. Data Science
# ==============================================================================================================

library(quanteda)
library(pdftools)
library(tm)


# Analysing a folder of resumes (Data Scientist)
setwd("C:/Users/Tony/Desktop/Resume/Data Scientist")
ds_files <- list.files(pattern = "pdf$")
ds_files

# Apply pdf_text function
ds_text <- lapply(ds_files, pdf_text)
length(ds_text) # 20 resumes
lapply(ds_text, length) # List of pages per resume
head(ds_text) # Read pdf-converted text files

# Create corpus (collection of documents of pdf)
ds_corp <- Corpus(URISource(ds_files),
               readerControl = list(reader = readPDF))
class(ds_corp) # VCorpus (Volatile Corpus)

ds_dtm <- DocumentTermMatrix(ds_corp)
ds_matrix <- as.matrix(ds_dtm)
ds_matrix[1:5, 1:5] # First 5 terms

# ==============================================================================================================
# Programming Skills

# Obtain list of programming skills (Data Scientist)
top_ds_prog_skills <- ds_prog_skills_prop$prog_lang
top_ds_prog_skills


# Filtering dataset with list of programming skills
ds_cols <- colnames(ds_matrix)
ds_prog_matrix1 <- ds_matrix[, ds_cols %in% top_ds_prog_skills]
ds_prog_df <- as.data.frame(ds_prog_matrix1) %>% print()


# Compute new proportion of programming languages that are only mentioned 
ds_prog_skills_fitted <- colnames(ds_prog_df)
ds_prog_skills_prop1 <- ds_prog_skills_prop[ds_prog_skills_prop$prog_lang %in% ds_prog_skills_fitted, ] %>% print()
ds_prog_skills_prop1 <- ds_prog_skills_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()


# Compute log-weighted scoring
ds_prog_score <- sapply(ds_prog_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
ds_prog_score1 <- sapply(colnames(ds_prog_score), function(x) ds_prog_skills_prop1$proportion[ds_prog_skills_prop1$prog_lang == x]*ds_prog_score[,x]) %>% print()

# Compute total programming score
ds_prog_score1 <- round(cbind(ds_prog_score1, total = rowSums(ds_prog_score1)), 2) %>% print() 
ds_prog_score1 <- data.frame(ds_prog_score1)
rownames(ds_prog_score1) <- rownames(ds_prog_df)
ds_prog_score1

# ==============================================================================================================
# Data Science Tools

# Proportion of data science tools
ds_dstools_prop

# Filtering dataset with list of data science tools
ds_cols <- colnames(ds_matrix)
ds_dstools_matrix1 <- ds_matrix[, ds_cols %in% ds_dstools_prop$ds_tools]
ds_dstools_df <- as.data.frame(ds_dstools_matrix1) %>% print()

# Compute new proportion of data science tools that are only mentioned 
ds_dstools_fitted <- colnames(ds_dstools_df)
ds_dstools_prop1 <- ds_dstools_prop[ds_dstools_prop$ds_tools %in% ds_dstools_fitted, ] %>% print()
ds_dstools_prop1 <- ds_dstools_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()

# Compute log-weighted scoring
ds_ds_score <- sapply(ds_dstools_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
ds_ds_score1 <- sapply(colnames(ds_ds_score), function(x) ds_dstools_prop1$proportion[ds_dstools_prop1$ds_tools == x]*ds_ds_score[,x]) %>% print()

# Compute total data science tools score
ds_ds_score1 <- round(cbind(ds_ds_score1, total = rowSums(ds_ds_score1)), 2) %>% print() 
ds_ds_score1 <- data.frame(ds_ds_score1)
rownames(ds_ds_score1) <- rownames(ds_dstools_df)
ds_ds_score1

# ==============================================================================================================
# Technical Skills

# Proportion of technical skills
ds_tskills_prop

# Filtering dataset with list of technical skills
ds_cols <- colnames(ds_matrix) 
ds_tskills_matrix1 <- ds_matrix[, ds_cols %in% ds_tskills_prop$tskills]
ds_tskills_df <- as.data.frame(ds_tskills_matrix1) %>% print()

# No need for computation of new proportion as all tokens are mentioned

# Compute log-weighted scoring
ds_tskills_score <- sapply(ds_tskills_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
ds_tskills_score1 <- sapply(colnames(ds_tskills_score), function(x) ds_tskills_prop$proportion[ds_tskills_prop$tskills == x]*ds_tskills_score[,x]) %>% print()

# Compute total technical skills score
ds_tskills_score1 <- round(cbind(ds_tskills_score1, total = rowSums(ds_tskills_score1)), 2) %>% print() 
ds_tskills_score1 <- data.frame(ds_tskills_score1)
rownames(ds_tskills_score1) <- rownames(ds_tskills_df)
ds_tskills_score1


# ==============================================================================================================
# Soft Skills

# Proportion of technical skills
ds_softskill_prop

# Filtering dataset with list of technical skills
ds_cols <- colnames(ds_matrix) 
ds_softskill_matrix1 <- ds_matrix[, ds_cols %in% ds_softskill_prop$softskill]
ds_softskill_df <- as.data.frame(ds_softskill_matrix1) %>% print()

# Compute new proportion of data science tools that are only mentioned 
ds_softskill_fitted <- colnames(ds_softskill_df)
ds_softskill_prop1 <- ds_softskill_prop[ds_softskill_prop$softskill %in% ds_softskill_fitted, ] %>% print()
ds_softskill_prop1 <- ds_softskill_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()

# Compute log-weighted scoring
ds_softskill_score <- sapply(ds_softskill_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
ds_softskill_score1 <- sapply(colnames(ds_softskill_score), function(x) ds_softskill_prop1$proportion[ds_softskill_prop1$softskill == x]*ds_softskill_score[,x]) %>% print()

# Compute total data science tools score
ds_softskill_score1 <- round(cbind(ds_softskill_score1, total = rowSums(ds_softskill_score1)), 2) %>% print() 
ds_softskill_score1 <- data.frame(ds_softskill_score1)
rownames(ds_softskill_score1) <- rownames(ds_softskill_df)
ds_softskill_score1

# ==============================================================================================================
# Total Score
ds_total <- data.frame(p.lang = ds_prog_score1$total,
                       ds.tool = ds_ds_score1$total,
                       t.skill = ds_tskills_score1$total, 
                       softskill = ds_softskill_score1$total)
ds_total <- cbind(ds_total, grandtotal = rowSums(ds_total))
rownames(ds_total) <- rownames(ds_softskill_df)
ds_total
ds_total[order(-ds_total$grandtotal), ]
# Ranked and sorted according to total score




# ==============================================================================================================
# Application 
# 2. Data Analyst
# ==============================================================================================================

library(quanteda)
library(pdftools)
library(tm)


# Analysing a folder of resumes (Data Analyst)
setwd("C:/Users/Tony/Desktop/Resume/Data Analyst")
da_files <- list.files(pattern = "pdf$")
da_files

# Apply pdf_text function
da_text <- lapply(da_files, pdf_text)
length(da_text) # 20 resumes
lapply(da_text, length) # List of pages per resume
head(da_text) # Read pdf-converted text files

# Create corpus (collection of documents of pdf)
da_corp <- Corpus(URISource(da_files),
                  readerControl = list(reader = readPDF))
class(da_corp) # VCorpus (Volatile Corpus)

da_dtm <- DocumentTermMatrix(da_corp)
da_matrix <- as.matrix(da_dtm)
da_matrix[1:5, 1:10]

# ==============================================================================================================
# Programming Skills

da_prog_skills_prop 

# Obtain list of programming skills (Data Scientist)
top_da_prog_skills <- da_prog_skills_prop$prog_lang
top_da_prog_skills


# Filtering dataset with list of programming skills
da_cols <- colnames(da_matrix)
da_prog_matrix1 <- da_matrix[, da_cols %in% top_da_prog_skills]
da_prog_df <- as.data.frame(da_prog_matrix1) %>% print()


# Compute new proportion of programming skills that are only mentioned 
da_prog_skills_fitted <- colnames(da_prog_df)
da_prog_skills_prop1 <- da_prog_skills_prop[da_prog_skills_prop$prog_lang %in% da_prog_skills_fitted, ] %>% print()
da_prog_skills_prop1 <- da_prog_skills_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()


# Compute log-weighted scoring
da_prog_score <- sapply(da_prog_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
da_prog_score1 <- sapply(colnames(da_prog_score), function(x) da_prog_skills_prop1$proportion[da_prog_skills_prop1$prog_lang == x]*da_prog_score[,x]) %>% print()

# Compute total programming score
da_prog_score1 <- round(cbind(da_prog_score1, total = rowSums(da_prog_score1)), 2) %>% print() 
da_prog_score1 <- data.frame(da_prog_score1)
rownames(da_prog_score1) <- rownames(da_prog_df)
da_prog_score1

# ==============================================================================================================
# Data Science Tools

# Proportion of data science tools
da_dstools_prop

# Filtering dataset with list of data science tools
da_cols <- colnames(da_matrix)
da_dstools_matrix1 <- da_matrix[, da_cols %in% da_dstools_prop$ds_tools]
da_dstools_df <- as.data.frame(da_dstools_matrix1) %>% print()

# Compute new proportion of data science tools that are only mentioned 
da_dstools_fitted <- colnames(da_dstools_df)
da_dstools_prop1 <- da_dstools_prop[da_dstools_prop$ds_tools %in% da_dstools_fitted, ] %>% print()
da_dstools_prop1 <- da_dstools_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()

# Compute log-weighted scoring
da_ds_score <- sapply(da_dstools_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
da_ds_score1 <- sapply(colnames(da_ds_score), function(x) da_dstools_prop1$proportion[da_dstools_prop1$ds_tools == x]*da_ds_score[,x]) %>% print()

# Compute total data science tools score
da_ds_score1 <- round(cbind(da_ds_score1, total = rowSums(da_ds_score1)), 2) %>% print() 
da_ds_score1 <- data.frame(da_ds_score1)
rownames(da_ds_score1) <- rownames(da_dstools_df)
da_ds_score1

# ==============================================================================================================
# Technical Skills

# Proportion of technical skills
da_tskills_prop

# Filtering dataset with list of technical skills
da_cols <- colnames(da_matrix) 
da_tskills_matrix1 <- da_matrix[, da_cols %in% da_tskills_prop$tskills]
da_tskills_df <- as.data.frame(da_tskills_matrix1) %>% print()

# Compute new proportion of data science tools that are only mentioned 
da_tskills_fitted <- colnames(da_tskills_df)
da_tskills_prop1 <- da_tskills_prop[da_tskills_prop$tskills %in% da_tskills_fitted, ] %>% print()
da_tskills_prop1 <- da_tskills_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()

# Compute log-weighted scoring
da_tskills_score <- sapply(da_tskills_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
da_tskills_score1 <- sapply(colnames(da_tskills_score), function(x) da_tskills_prop$proportion[da_tskills_prop$tskills == x]*da_tskills_score[,x]) %>% print()

# Compute total technical skills score
da_tskills_score1 <- round(cbind(da_tskills_score1, total = rowSums(da_tskills_score1)), 2) %>% print() 
da_tskills_score1 <- data.frame(da_tskills_score1)
rownames(da_tskills_score1) <- rownames(da_tskills_df)
da_tskills_score1


# ==============================================================================================================
# Soft Skills

# Proportion of technical skills
da_softskill_prop

# Filtering dataset with list of technical skills
da_cols <- colnames(da_matrix) 
da_softskill_matrix1 <- da_matrix[, da_cols %in% da_softskill_prop$softskill]
da_softskill_df <- as.data.frame(da_softskill_matrix1) %>% print()

# Compute new proportion of data science tools that are only mentioned 
da_softskill_fitted <- colnames(da_softskill_df)
da_softskill_prop1 <- da_softskill_prop[da_softskill_prop$softskill %in% da_softskill_fitted, ] %>% print()
da_softskill_prop1 <- da_softskill_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()

# Compute log-weighted scoring
da_softskill_score <- sapply(da_softskill_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
da_softskill_score1 <- sapply(colnames(da_softskill_score), function(x) da_softskill_prop1$proportion[da_softskill_prop1$softskill == x]*da_softskill_score[,x]) %>% print()

# Compute total data science tools score
da_softskill_score1 <- round(cbind(da_softskill_score1, total = rowSums(da_softskill_score1)), 2) %>% print() 
da_softskill_score1 <- data.frame(da_softskill_score1)
rownames(da_softskill_score1) <- rownames(da_softskill_df)
da_softskill_score1

# ==============================================================================================================
# Total Score
da_total <- data.frame(p.lang = da_prog_score1$total,
                       ds.tool = da_ds_score1$total,
                       t.skill = da_tskills_score1$total, 
                       softskill = da_softskill_score1$total)
da_total <- cbind(da_total, grandtotal = rowSums(da_total))
rownames(da_total) <- rownames(da_softskill_df)
da_total
da_total[order(-da_total$grandtotal), ]
# Ranked and sorted according to total score


# ==============================================================================================================
# Application 
# 3. Data Engineer
# ==============================================================================================================

library(quanteda)
library(pdftools)
library(tm)


# Analysing a folder of resumes (Data Analyst)
setwd("C:/Users/Tony/Desktop/Resume/Data Engineer")
de_files <- list.files(pattern = "pdf$")
de_files

# Apply pdf_text function
de_text <- lapply(de_files, pdf_text)
length(de_text) # 20 resumes
lapply(de_text, length) # List of pages per resume
head(de_text) # Read pdf-converted text files

# Create corpus (collection of documents of pdf)
de_corp <- Corpus(URISource(de_files),
                  readerControl = list(reader = readPDF))
class(de_corp) # VCorpus (Volatile Corpus)

de_dtm <- DocumentTermMatrix(de_corp)
de_matrix <- as.matrix(de_dtm)
de_matrix[1:5, 1:10]

# ==============================================================================================================
# Programming Skills

de_prog_skills_prop 

# Obtain list of programming skills (Data Scientist)
top_de_prog_skills <- de_prog_skills_prop$prog_lang
top_de_prog_skills


# Filtering dataset with list of programming skills
de_cols <- colnames(de_matrix)
de_prog_matrix1 <- de_matrix[, de_cols %in% top_de_prog_skills]
de_prog_df <- as.data.frame(de_prog_matrix1) %>% print()


# Compute new proportion of programming skills that are only mentioned 
de_prog_skills_fitted <- colnames(de_prog_df)
de_prog_skills_prop1 <- de_prog_skills_prop[de_prog_skills_prop$prog_lang %in% de_prog_skills_fitted, ] %>% print()
de_prog_skills_prop1 <- de_prog_skills_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()


# Compute log-weighted scoring
de_prog_score <- sapply(de_prog_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
de_prog_score1 <- sapply(colnames(de_prog_score), function(x) de_prog_skills_prop1$proportion[de_prog_skills_prop1$prog_lang == x]*de_prog_score[,x]) %>% print()

# Compute total programming score
de_prog_score1 <- round(cbind(de_prog_score1, total = rowSums(de_prog_score1)), 2) %>% print() 
de_prog_score1 <- data.frame(de_prog_score1)
rownames(de_prog_score1) <- rownames(de_prog_df)
de_prog_score1

# ==============================================================================================================
# Data Science Tools

# Proportion of data science tools
de_dstools_prop

# Filtering dataset with list of data science tools
de_cols <- colnames(de_matrix)
de_dstools_matrix1 <- de_matrix[, de_cols %in% de_dstools_prop$ds_tools]
de_dstools_df <- as.data.frame(de_dstools_matrix1) %>% print()

# No need for computation of new proportion as all tokens are mentioned

# Compute log-weighted scoring
de_ds_score <- sapply(de_dstools_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
de_ds_score1 <- sapply(colnames(de_ds_score), function(x) de_dstools_prop$proportion[de_dstools_prop$ds_tools == x]*de_ds_score[,x]) %>% print()

# Compute total data science tools score
de_ds_score1 <- round(cbind(de_ds_score1, total = rowSums(de_ds_score1)), 2) %>% print() 
de_ds_score1 <- data.frame(de_ds_score1)
rownames(de_ds_score1) <- rownames(de_dstools_df)
de_ds_score1

# ==============================================================================================================
# Technical Skills

# Proportion of technical skills
de_tskills_prop

# Filtering dataset with list of technical skills
de_cols <- colnames(de_matrix) 
de_tskills_matrix1 <- de_matrix[, de_cols %in% de_tskills_prop$tskills]
de_tskills_df <- as.data.frame(de_tskills_matrix1) %>% print()

# Compute new proportion of data science tools that are only mentioned 
de_tskills_fitted <- colnames(de_tskills_df)
de_tskills_prop1 <- de_tskills_prop[de_tskills_prop$tskills %in% de_tskills_fitted, ] %>% print()
de_tskills_prop1 <- de_tskills_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()

# Compute log-weighted scoring
de_tskills_score <- sapply(de_tskills_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
de_tskills_score1 <- sapply(colnames(de_tskills_score), function(x) de_tskills_prop$proportion[de_tskills_prop$tskills == x]*de_tskills_score[,x]) %>% print()

# Compute total technical skills score
de_tskills_score1 <- round(cbind(de_tskills_score1, total = rowSums(de_tskills_score1)), 2) %>% print() 
de_tskills_score1 <- data.frame(de_tskills_score1)
rownames(de_tskills_score1) <- rownames(de_tskills_df)
de_tskills_score1


# ==============================================================================================================
# Soft Skills

# Proportion of technical skills
de_softskill_prop

# Filtering dataset with list of technical skills
de_cols <- colnames(de_matrix) 
de_softskill_matrix1 <- de_matrix[, de_cols %in% de_softskill_prop$softskill]
de_softskill_df <- as.data.frame(de_softskill_matrix1) %>% print()

# Compute new proportion of data science tools that are only mentioned 
de_softskill_fitted <- colnames(de_softskill_df)
de_softskill_prop1 <- de_softskill_prop[de_softskill_prop$softskill %in% de_softskill_fitted, ] %>% print()
de_softskill_prop1 <- de_softskill_prop1 %>% mutate(total = sum(freq)) %>% mutate(proportion = freq/total) %>% print()

# Compute log-weighted scoring
de_softskill_score <- sapply(de_softskill_df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()

# Apply proportion scoring function 
de_softskill_score1 <- sapply(colnames(de_softskill_score), function(x) de_softskill_prop1$proportion[de_softskill_prop1$softskill == x]*de_softskill_score[,x]) %>% print()

# Compute total data science tools score
de_softskill_score1 <- round(cbind(de_softskill_score1, total = rowSums(de_softskill_score1)), 2) %>% print() 
de_softskill_score1 <- data.frame(de_softskill_score1)
rownames(de_softskill_score1) <- rownames(de_softskill_df)
de_softskill_score1

# ==============================================================================================================
# Total Score
de_total <- data.frame(p.lang = de_prog_score1$total,
                       ds.tool = de_ds_score1$total,
                       t.skill = de_tskills_score1$total, 
                       softskill = de_softskill_score1$total)
de_total <- cbind(de_total, grandtotal = rowSums(de_total))
rownames(de_total) <- rownames(de_softskill_df)
de_total
de_total[order(-de_total$grandtotal), ]
# Ranked and sorted according to total score

# ==============================================================================================================
# Part 2a : Fit
# Department/Industry Personality Analysis
# ==============================================================================================================

library(data.table)
personality.dt <- fread("Personality.csv")

# ==============================================================================================================
# Extraversion
# ==============================================================================================================

# Extraversion Scores across Departments
# Mean
means_extraversion_dept <- round(tapply(personality.dt$Extraversion, personality.dt$Department, mean), digits = 2)
means_extraversion_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Extraversion)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Extraversion across Departments") 
#ANOVA
aov_extraversion_dept <- aov(personality.dt$Extraversion~personality.dt$Department)
summary(aov_extraversion_dept)

# Extraversion Scores across Industries
# Mean
means_extraversion_industry <- round(tapply(personality.dt$Extraversion, personality.dt$Industry, mean), digits = 2)
means_extraversion_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Extraversion)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Extraversion across Industries") 
# ANOVA
aov_extraversion_industry <- aov(personality.dt$Extraversion~personality.dt$Industry)
summary(aov_extraversion_industry)

# ==============================================================================================================
# Agreeableness
# ==============================================================================================================

# Agreeableness Scores across Departments
# Mean
means_agreeableness_dept <- round(tapply(personality.dt$Agreeableness, personality.dt$Department, mean), digits = 2)
means_agreeableness_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Agreeableness)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Agreeableness across Departments") 
#ANOVA
aov_agreeableness_dept <- aov(personality.dt$Agreeableness~personality.dt$Department)
summary(aov_agreeableness_dept)

# Agreeableness Scores across Industries
# Mean
means_agreeableness_industry <- round(tapply(personality.dt$Agreeableness, personality.dt$Industry, mean), digits = 2)
means_agreeableness_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Agreeableness)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Agreeableness across Industries") 
# ANOVA
aov_agreeableness_industry <- aov(personality.dt$Agreeableness~personality.dt$Industry)
summary(aov_agreeableness_industry)

# ==============================================================================================================
# Openness
# ==============================================================================================================

# Openness Scores across Departments 
# Mean
means_openness_dept <- round(tapply(personality.dt$Openness, personality.dt$Department, mean), digits = 2)
means_openness_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Openness)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Openness across Departments") 
# ANOVA
aov_openness_dept <- aov(personality.dt$Openness~personality.dt$Department)
summary(aov_openness_dept)

# Openness Scores across Industries
# Mean
means_openness_industry <- round(tapply(personality.dt$Openness, personality.dt$Industry, mean), digits = 2)
means_openness_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Openness)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Openness across Industries") 
# ANOVA
aov_openness_industry <- aov(personality.dt$Openness~personality.dt$Industry)
summary(aov_openness_industry)

# ==============================================================================================================
# Neuroticism
# ==============================================================================================================

# Neuroticism Scores across Departments
# Mean
means_neuroticism_dept <- round(tapply(personality.dt$Neuroticism, personality.dt$Department, mean), digits = 2)
means_neuroticism_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Neuroticism)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Neuroticism across Departments") 
# ANOVA
aov_neuroticism_dept <- aov(personality.dt$Neuroticism~personality.dt$Department)
summary(aov_neuroticism_dept)

# Neuroticism Scores across Industries 
# Mean
means_neuroticism_industry <- round(tapply(personality.dt$Neuroticism, personality.dt$Industry, mean), digits = 2)
means_neuroticism_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Neuroticism)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Neuroticism across Industries") 
# ANOVA
aov_neuroticism_industry <- aov(personality.dt$Neuroticism~personality.dt$Industry)
summary(aov_neuroticism_industry)

# ==============================================================================================================
# Conscientiousness
# ==============================================================================================================

# Conscientiousness Scores across Departments
# Mean
means_conscientiousness_dept <- round(tapply(personality.dt$Conscientiousness, personality.dt$Department, mean), digits = 2)
means_conscientiousness_dept
# Boxplot of Mean
ggplot(personality.dt, aes(x = Department, y = Conscientiousness)) + geom_boxplot(aes(fill = Department)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Conscientiousness across Departments") 
# ANOVA
aov_conscientiousness_dept <- aov(personality.dt$Conscientiousness~personality.dt$Department)
summary(aov_conscientiousness_dept)

# Conscientiousness Scores across Industries
# Mean
means_conscientiousness_industry <- round(tapply(personality.dt$Conscientiousness, personality.dt$Industry, mean), digits = 2)
means_conscientiousness_industry
# Boxplot of Mean
ggplot(personality.dt, aes(x = Industry, y = Conscientiousness)) + geom_boxplot(aes(fill = Industry)) + theme(legend.position = "top") +   stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3,show.legend = FALSE) + labs(title = "Scores for Conscientiousness across Industries") 
# ANOVA
aov_conscientiousness_industry <- aov(personality.dt$Conscientiousness~personality.dt$Industry)
summary(aov_conscientiousness_industry)

# ==============================================================================================================
# Part 2b : Fit
# Applicant's Personality Analysis
# ==============================================================================================================
setwd("C:/Users/simch/Desktop/NTU/Year 2 Sem 1/BC2406 Analytics 1/Project")
library(quanteda)


personality <- dictionary(list(Extraversion = c("outgoing", "feel", "people-oriented"),
                               Introversion = c("information", "work", "factual"),
                               Intuition = c("feel", "instinct", "evaluate"),
                               Sensing = c("fact", "actual", "matter"),
                               Thinking = c("fact", "decision", "judge"),
                               Feeling = c("principle", "value", "feel"),
                               Judging = c("neat", "order", "establish"), 
                               Perceiving = c("spontaneous", "open-ended", "adapt")))

library(readtext)
cv <- readtext("C:/Users/simch/Desktop/NTU/Year 2 Sem 1/BC2406 Analytics 1/Project/CV/*.txt",
               docvarsfrom = "filenames",
               docvarnames = c("Type", "Index"),
               dvsep = "_",
               encoding = "UTF-8")

cv.corpus <- corpus(cv)
summary(cv.corpus)

cv.tokens1 <- tokens(cv.corpus, remove_punct = T, remove_numbers = T)
cv.tokens2 <- tokens_remove(cv.tokens1, pattern = stopwords('en'))
cv.tokens3 <- tokens_wordstem(cv.tokens2)
cv.tokens4 <- tokens_tolower(cv.tokens3)

cv.personality <- dfm(cv.tokens4, dictionary = personality)
cv.personality.df <- convert(cv.personality, to = "data.frame")
rownames(cv.personality.df) <- cv.personality.df$document
cv.personality.df$document = NULL

# Compute log-weighted scoring
cv.personality.log <- sapply(cv.personality.df, function(x) ifelse(x > 0, 1 + log(x), 0)) %>%  print()


# Compute total data science tools score
cv.personality.log1 <- data.frame(cv.personality.log)
cv.personality.log1$Introversion_Extroversion <- (cv.personality.log1$Extraversion - cv.personality.log1$Introversion)
cv.personality.log1$Intuition_Sensing <- (cv.personality.log1$Intuition - cv.personality.log1$Sensing)
cv.personality.log1$Thinking_Feeling <- (cv.personality.log1$Thinking - cv.personality.log1$Feeling)
cv.personality.log1$Judging_Perceiving <- (cv.personality.log1$Judging - cv.personality.log1$Perceiving)
cv.personality.log1 <- cv.personality.log1[, -1:-8]
rownames(cv.personality.log1) <- rownames(cv.personality.df)
cv.personality.log1

cv.personality.log1$Personality <- paste(ifelse(cv.personality.log1$Introversion_Extroversion > 0, "I", "E"),
                                         ifelse(cv.personality.log1$Intuition_Sensing > 0, "N", "S"),
                                         ifelse(cv.personality.log1$Thinking_Feeling > 0, "T", "F"),
                                         ifelse(cv.personality.log1$Judging_Perceiving > 0, "J", "P"), sep = "")
library(ggplot2)
library(dplyr)
cv.personality.log1 %>%  arrange(-Introversion_Extroversion) %>% 
  ggplot() +
  geom_col(aes(rownames(cv.personality.log1), 
               Introversion_Extroversion, 
               fill = rownames(cv.personality.log1)
  ), show.legend = FALSE) + xlab(NULL) + coord_flip()

cv.personality.log1 %>%  arrange(-Intuition_Sensing) %>% 
  ggplot() +
  geom_col(aes(rownames(cv.personality.log1), 
               Intuition_Sensing, 
               fill = rownames(cv.personality.log1)
  ), show.legend = FALSE) + xlab(NULL) + coord_flip()

cv.personality.log1 %>%  arrange(-Thinking_Feeling) %>% 
  ggplot() +
  geom_col(aes(rownames(cv.personality.log1), 
               Thinking_Feeling, 
               fill = rownames(cv.personality.log1)
  ), show.legend = FALSE) + xlab(NULL) + coord_flip()

cv.personality.log1 %>%  arrange(-Intuition_Sensing) %>% 
  ggplot() +
  geom_col(aes(rownames(cv.personality.log1), 
               Judging_Perceiving, 
               fill = rownames(cv.personality.log1)
  ), show.legend = FALSE) + xlab(NULL) + coord_flip()


# ==============================================================================================================
# Part 3 : Attrition
# ==============================================================================================================
setwd('C:/Users/ooimi/Documents/BC2406')
library(data.table)
data<-fread("IBM HR Data.csv")
summary(data)

# ==============================================================================================================
# Data Cleaning
# ==============================================================================================================
# Removal of less meaningful data points
data<-data[!data$EmployeeNumber == "TESTING",]
data<-data[!data$EmployeeNumber == "TEST",]
data<-data[!data$EmployeeNumber == "Test",]
data<-data[!data$EmployeeNumber == "Test 456",]
data<-data[!data$`Application ID` == "TESTING",]
data<-data[!data$`Application ID` == "Test",]

# Remove rows with at least 25 of the columns blank
data<-data[!(apply(is.na(data), 1, sum)>25)] 

# Remove shifted rows
data<-data[!data$MaritalStatus=="4",]

# Remove duplicates
data<-data[!duplicated(data),]

# Anomaly
subset(data,TotalWorkingYears<YearsAtCompany) #6 data points
subset(data,NumCompaniesWorked==0 & TotalWorkingYears>YearsAtCompany) #2800 people are at their first company but has more working years than years in the company

# Remove the anomalies
data<-data[!(TotalWorkingYears<YearsAtCompany),]
data<-data[!(NumCompaniesWorked==0 & TotalWorkingYears>YearsAtCompany),]

# Change to categorical variables
data$Gender<-factor(data$Gender)
data$BusinessTravel<-factor(data$BusinessTravel)
data$Attrition<-factor(data$Attrition)
data$Department<-factor(data$Department)
data$EducationField<-factor(data$EducationField)
data$JobRole<-factor(data$JobRole)
data$MaritalStatus<-factor(data$MaritalStatus)
data$OverTime<-factor(data$OverTime)
data$Over18<-factor(data$Over18)
data$`Employee Source`<-factor(data$`Employee Source`)

# Change satisfactions to ordinal categorical variable
data$JobSatisfaction<-factor(data$JobSatisfaction,ordered=T,levels = c(1,2,3,4))
data$RelationshipSatisfaction<-factor(data$RelationshipSatisfaction,ordered=T,levels = c(1,2,3,4))
data$EnvironmentSatisfaction<-factor(data$EnvironmentSatisfaction,ordered=T,levels = c(1,2,3,4))

# Change to numeric
data$DistanceFromHome<-as.numeric(data$DistanceFromHome)
data$PercentSalaryHike<-as.numeric(data$PercentSalaryHike)
data$HourlyRate<-as.numeric(data$HourlyRate)
data$MonthlyIncome<-as.numeric(data$MonthlyIncome)
data$JobLevel<-as.numeric(data$JobLevel)

# Drop columns with no change in variable
data<-data[,Over18:=NULL]
data<-data[,StandardHours:=NULL]
data<-data[,EmployeeCount:=NULL]


# Comparing correlation of data related to salary and JobLevel
library(corrplot)
library(RColorBrewer)
salary<-data[, .(HourlyRate, DailyRate, MonthlyRate, MonthlyIncome, JobLevel)]
m<-cor(salary,use="complete.obs")
corrplot(m, method = "color", col =  brewer.pal(n = 8, name = "RdBu"),
         type = "upper", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

# Drop HourlyRate, DailyRate and MonthlyRate as illogical for correlation between these variables to be so low
data<-data[,HourlyRate:=NULL]
data<-data[,DailyRate:=NULL]
data<-data[,MonthlyRate:=NULL]

# Change blanks to NA in Attrition
data$Attrition[data$Attrition==" "|data$Attrition==""]<-NA
data<-data[, "Attrition":=ifelse(Attrition=="Current employee",1,0)]
data$Attrition<-factor(data$Attrition)

# ==============================================================================================================
# Logistic Regression
# All variables to predict "ATTRITION" 
# ==============================================================================================================

data1<-data.table(data)

#remove irrelevant variables
data1<-data1[,EmployeeNumber:=NULL]
data1<-data1[,`Application ID`:=NULL]

m1<-glm(Attrition ~., family = binomial, data=data1, na.action=na.omit)
summary(m1)

#refitted model
m2<-glm(Attrition ~ Age+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobLevel+JobSatisfaction+NumCompaniesWorked+PercentSalaryHike+RelationshipSatisfaction+StockOptionLevel+TrainingTimesLastYear+WorkLifeBalance+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,family = binomial, data=data1, na.action=na.omit)
summary(m2)
OR<-exp(coef(m2))
OR
OR.CI<-exp(confint(m2))
OR.CI

# ==============================================================================================================
# CART
# Other input variables to predict "SATISFACTIONS
# ==============================================================================================================
# CART library
library(rpart)
library(rpart.plot)
set.seed(2014)
options(digits = 5)

#dataset
data2<-data.table(data)
#remove irrelavant variables
data2<-data2[,Attrition:=NULL]
data2<-data2[,EmployeeNumber:=NULL]
data2<-data2[,`Application ID`:=NULL]


#FOR ENVIRONMENT SATISFACTION
data.env<-data.table(data2)
data.env<-data.env[,JobSatisfaction:=NULL]
data.env<-data.env[,RelationshipSatisfaction:=NULL]
cart.env<- rpart(EnvironmentSatisfaction ~ ., data=data.env, method='class', cp=0)
#rpart.plot(cart.env, nn= T, main = "Maximal Tree in cart.env")
#print(cart.env)
printcp(cart.env)
plotcp(cart.env, main = "Prune Sequence CV Errors")

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap.env <- cart.env$cptable[which.min(cart.env$cptable[,"xerror"]),
                                    "xerror"] + cart.env$cptable[which.min(cart.env$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart.env$cptable[i,j] > CVerror.cap.env) {
  i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt.env = ifelse(i > 1, sqrt(cart.env$cptable[i,1] * cart.env$cptable[i-1,1]), 1)

cart.env.opt <- prune(cart.env, cp = cp.opt.env)
rpart.plot(cart.env.opt, nn= T, main = "EnvironmentSatisfaction Optimal Tree")

cart.env.opt$variable.importance

# Confusion Matrix
tree_predict.env <- predict(cart.env.opt, newdata = data.env, type = 'class')
tree_table.env <- table(data.env$EnvironmentSatisfaction, tree_predict.env)
tree_table.env
round(prop.table(tree_table.env),3)
cat("CART Overall Accuracy Rate (EnvironmentSatisfaction): ", round(0.161+0.161+0.264+0.259,3), "\n")



# FOR JOB SATISFACTION
data.job<-data.table(data2)
data.job<-data.job[,EnvironmentSatisfaction:=NULL]
data.job<-data.job[,RelationshipSatisfaction:=NULL]
cart.job<-rpart(JobSatisfaction ~ ., data=data.job, method='class', cp=0)
#rpart.plot(cart.job, nn= T, main = "Maximal Tree in cart.job")
#print(cart.job)
printcp(cart.job)
plotcp(cart.job, main = "Prune Sequence CV Errors")

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap.job <- cart.job$cptable[which.min(cart.job$cptable[,"xerror"]),
                                    "xerror"] + cart.job$cptable[which.min(cart.job$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart.job$cptable[i,j] > CVerror.cap.job) {
  i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt.job = ifelse(i > 1, sqrt(cart.job$cptable[i,1] * cart.job$cptable[i-1,1]), 1)

cart.job.opt <- prune(cart.job, cp = cp.opt.job)
rpart.plot(cart.job.opt, nn= T, main = "JobSatisfaction Optimal Tree")
cart.job.opt$variable.importance

# Confusion Matrix
tree_predict.job <- predict(cart.job.opt, newdata = data.job, type = 'class')
tree_table.job <- table(data.job$JobSatisfaction, tree_predict.job)
tree_table.job
round(prop.table(tree_table.job),3)
cat("CART Overall Accuracy Rate (JobSatisfaction): ", round(0.187+0.182+0.292+0.298,3), "\n")


# FOR RELATIONSHIP SATISFACTION
data.rns<-data.table(data2)
data.rns<-data.rns[,JobSatisfaction:=NULL]
data.rns<-data.rns[,EnvironmentSatisfaction:=NULL]
cart.rns<- rpart(RelationshipSatisfaction ~ ., data=data.rns, method='class', cp=0)
#rpart.plot(cart.job, nn= T, main = "Maximal Tree in cart.rns")
#print(cart.rns)
printcp(cart.rns)
plotcp(cart.rns, main = "Prune Sequence CV Errors")

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap.rns <- cart.rns$cptable[which.min(cart.rns$cptable[,"xerror"]),
                                    "xerror"] + cart.rns$cptable[which.min(cart.rns$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart.rns$cptable[i,j] > CVerror.cap.rns) {
  i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt.rns = ifelse(i > 1, sqrt(cart.rns$cptable[i,1] * cart.rns$cptable[i-1,1]), 1)

cart.rns.opt <- prune(cart.rns, cp = cp.opt.rns)
rpart.plot(cart.rns.opt, nn= T, main = "RelationshipSatisfaction Optimal Tree")
cart.rns.opt$variable.importance

# Confusion Matrix
tree_predict.rns <- predict(cart.rns.opt, newdata = data.rns, type = 'class')
tree_table.rns <- table(data.rns$RelationshipSatisfaction, tree_predict.rns)
tree_table.rns
round(prop.table(tree_table.rns),3)
cat("CART Overall Accuracy Rate (RelationshipSatisfaction): ", round(0.177+0.198+0.308+0.283,3), "\n")

