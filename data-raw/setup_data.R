# note that in addition to the packages imported and suggested by tidytext,
# this script requires the devtools and qdapDictionaries packages

library(dplyr)

# emotions dataset --------------------------------------------------------

emotions <- readr::read_tsv("data-raw/NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt.zip",
                            col_names = FALSE, skip = 46)
emotions <- emotions %>% tidyr::spread(X2, X3) %>%
  rename(word = X1) %>%
  select(-negative, -positive) %>%
  tidyr::gather("emotion", "value", -word) %>%
  filter(value == 1) %>%
  select(-value) %>%
  arrange(word)
readr::write_csv(nrc_emotions, "data-raw/emotions.csv")
devtools::use_data(emotions, overwrite = TRUE)

# sentiments dataset ------------------------------------------------------

nrc_lexicon <- readr::read_tsv("data-raw/NRC-emotion-lexicon-wordlevel-alphabetized-v0.92.txt.zip",
                               col_names = FALSE, skip = 46)
nrc_lexicon <- nrc_lexicon %>% tidyr::spread(X2, X3) %>%
  rename(word = X1) %>%
  mutate(polarity = ifelse(negative == 1, -1, ifelse(positive == 1, 1, NA))) %>%
  select(word, polarity) %>%
  filter(!is.na(polarity)) %>%
  mutate(score = abs(polarity), lexicon = "nrc") %>%
  arrange(word)

bing_lexicon1 <- readr::read_lines("data-raw/positive-words.txt",
                                   skip = 35)
bing_lexicon2 <- readr::read_lines("data-raw/negative-words.txt",
                                   skip = 35)
bing_lexicon1 <- data_frame(word = bing_lexicon1, polarity = 1)
bing_lexicon2 <- data_frame(word = bing_lexicon2, polarity = -1)
bing_lexicon <- bind_rows(bing_lexicon1, bing_lexicon2) %>%
  mutate(score = abs(polarity), lexicon = "bing") %>%
  arrange(word)

AFINN_lexicon <- readr::read_tsv("data-raw/AFINN-111.txt",
                                 col_names = FALSE)
AFINN_lexicon <- AFINN_lexicon %>%
  transmute(word = X1, polarity = sign(X2), score = abs(X2) / max(X2), lexicon = "AFINN")

sentiments <- bind_rows(nrc_lexicon, bing_lexicon, AFINN_lexicon) %>%
  filter(!stringr::str_detect(word, "[^[:ascii:]]"))

readr::write_csv(sentiments, "data-raw/sentiments.csv")
devtools::use_data(sentiments, overwrite = TRUE)


# stop_words dataset ------------------------------------------------------

SMART <- data_frame(word = tm::stopwords("SMART"), lexicon = "SMART")
snowball <- data_frame(word = tm::stopwords("en"), lexicon = "snowball")
onix <- data_frame(word = qdapDictionaries::OnixTxtRetToolkitSWL1, lexicon = "onix")

stop_words <- bind_rows(SMART, snowball, onix) %>%
  filter(!stringr::str_detect(word, "[^[:ascii:]]"))

readr::write_csv(stop_words, "data-raw/stop_words.csv")
devtools::use_data(stop_words, overwrite = TRUE)


# parts_of_speech dataset ------------------------------------------------------

parts_of_speech <- readr::read_csv("Noun,N
                                   Plural,p
                                   Noun Phrase,h
                                   Verb (usu participle),V
                                   Verb (transitive),t
                                   Verb (intransitive),i
                                   Adjective,A
                                   Adverb,v
                                   Conjunction,C
                                   Preposition,P
                                   Interjection,!
                                   Pronoun,r
                                   Definite Article,D
                                   Indefinite Article,I
                                   Nominative,o
                                   ", col_names = c("pos", "code"))

# parts of speech
parts_of_speech <- readr::read_delim("data-raw/mobyposi.i.zip", delim = "\xd7",
                                   col_names = c("word", "code")) %>%
  tidyr::unnest(code = stringr::str_split(code, "")) %>%
  inner_join(parts_of_speech, by = "code") %>%
  filter(!stringr::str_detect(word, " ")) %>%
  mutate(word = stringr::str_to_lower(word)) %>%
  select(-code) %>%
  distinct() %>%
  filter(!stringr::str_detect(word, "[^[:ascii:]]"))

readr::write_csv(parts_of_speech, "data-raw/parts_of_speech.csv")
devtools::use_data(parts_of_speech, overwrite = TRUE)
