library(gutenbergr)

hgwells <- 
    gutenberg_download(
        c(35, 36, 5230, 159)
    )

View(hgwells)

tidy_hgwells <- hgwells %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) 
    
tidy_hgwells %>%
    count(word, sort = TRUE)

bronte <- 
    gutenberg_download(
        c(1260, 768, 969, 9182, 767)
    )

tidy_bronte = bronte %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

tidy_bronte %>%
    count(word, sort = TRUE)

library(tidyr)

frequency <- bind_rows(
    mutate(tidy_bronte, author = "Brontë Sisters"),
    mutate(tidy_hgwells, author = "H.G. Wells"),
    mutate(tidy_books, author = "Jane Austen") 
) %>%
mutate(word = str_extract(word, "[a-z']+")) %>%
count(author, word) %>%
group_by(author) %>%
mutate(proportion = n / sum(n)) %>%
select(-n) %>%
spread(author, proportion) %>%
gather(author, proportion, "Brontë Sisters": "H.G. Wells")

View(frequency)

library(scales)
ggplot(frequency, 
    aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen`) - proportion)
    ) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    facet_wrap(~author, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Jane Austen", x = NULL)

View(frequency)

cor.test(data = frequency[frequency$author == "Brontë Sisters",], ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], ~ proportion + `Jane Austen`)