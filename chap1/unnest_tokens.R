text <- c(
    "Because I could not stop for Death -",
    "He kindly stopped for me -",
    "The Carriage held but just Ourselves -",
    "and Immortality"
)

text

library(dplyr)
library(tidytext)

text_df <- data_frame(line = 1:4, text=text)
text_df

text_df %>%
    unnest_tokens(word, text)

# ak %>% f1 %>% f2 %>% f3 %>% ab


install.packages("dplyr")

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(
        linenumber = row_number(),
        chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))
    ) %>%
    ungroup()

original_books

tidy_books <- original_books %>%
    unnest_tokens(word, text)

tidy_books

data(stop_words)
tidy_books <- tidy_books %>%
    anti_join(stop_words)

count_word <- tidy_books %>%
    count(word, sort = TRUE)

library(ggplot2)

tidy_books %>%
    count(word, sort = TRUE) %>%
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) + 
    geom_col() +
    xlab(NULL) +
    coord_flip()
