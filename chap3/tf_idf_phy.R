library(gutenbergr)
physics <- gutenberg_download(
    c(37729, 14725, 13476, 5001), 
    meta_fields = "author"
)

physics_words <- physics %>%
    unnest_tokens(word, text) %>%
    count(author, word, sort = TRUE) %>%
    ungroup

physics_words

plot_physics <- physics_words %>%
    bind_tf_idf(word, author, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    mutate(author = factor(author, levels = 
        c(
            "Galilei, Galileo",
            "Huygens, Christiaan",
            "Tesla, Nikola",
            "Einstein, Albert"
        )))

plot_physics %>%
    group_by(author) %>%
    top_n(15, tf_idf) %>%
    ungroup %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~author, ncol = 2, scales = "free") +
    coord_flip()

library(stringr)
physics %>%
    filter(str_detect(text, "eq\\.")) %>%
    select(text)

physics %>%
    filter(str_detect(text, "K1")) %>%
    select(text)

physics %>%
    filter(str_detect(text, "AK")) %>%
    select(text)

mystopwords <- data_frame(word = 
    c(
        "eq", "co", "rc", "ac", "ak", "bn",
        "fig", "file", "cg", "cb", "cm"
    ))

physics_words <- anti_join(physics_words, mystopwords, by = "word")

plot_physics <- physics_words %>%
    bind_tf_idf(word, author, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    group_by(author) %>%
    top_n(15, tf_idf) %>%
    ungroup %>%
    mutate(author = factor(author, levels = 
        c(
            "Galilei, Galileo",
            "Huygens, Christiaan",
            "Tesla, Nikola",
            "Einstein, Albert"
        )))

library(ggplot2)
ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
geom_col(show.legend = FALSE) +
labs(x = NULL, y = "tf-idf") +
facet_wrap(~author, ncol = 2, scales = "free") +
coord_flip()