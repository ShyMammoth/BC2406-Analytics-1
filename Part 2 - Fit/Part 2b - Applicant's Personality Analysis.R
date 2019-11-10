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