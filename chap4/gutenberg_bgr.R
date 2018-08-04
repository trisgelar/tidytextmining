library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset){
    dataset %>%
        unnest_tokens(
            bigram, text, 
            token = "ngrams",
            n = 2
        ) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(
            !word1 %in% stop_words$word,
            !word2 %in% stop_words$word
        ) %>%
        count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams){
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(
            aes(
                edge_alpha = n
            ),
            show.legend = FALSE,
            arrow = a, end_cap = circle(.07, 'inches')
        ) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes
            (label = name), vjust = 1, hjust = 1
        ) +
        theme_void()
}

library(gutenbergr)
kjv <- gutenberg_download(10)

library(stringr)

kjv_bigrams <- kjv %>%
    count_bigrams()

kjv_bigrams %>%
    filter(
        n > 40,
        !str_detect(word1, "\\d"),
        !str_detect(word2, "\\d"),
    
    ) %>%
    visualize_bigrams()

install.packages("widyr")
library(widyr)
data(stop_words)

austen_section_words <- austen_books() %>%
    filter(book == "Pride & Prejudice") %>%
    mutate(section = row_number() %/% 10) %>%
    filter(section > 0) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word)

austen_section_words

word_pairs <- austen_section_words %>%
    pairwise_count(word, section, sort = TRUE)

word_pairs

word_pairs %>%
    filter(item1 == "darcy")

word_cors <- austen_section_words %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, section, sort = TRUE)

word_cors

word_cors %>%
    filter(item1 == "pounds")

word_cors %>%
    filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
    group_by(item1) %>%
    top_n(6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()

set.seed(2016)

word_cors %>%
    filter(correlation > .15) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(
        aes(
            edge_alpha = correlation
        ),
        show.legend = FALSE
    ) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes
        (label = name), repel = TRUE
    ) +
    theme_void()
