library(igraph)

bigram_counts

bigram_graph <- bigram_counts %>%
    filter(n > 20) %>%
    graph_from_data_frame()

bigram_graph

install.packages("ggraph")
library(ggraph)

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes
        (label = name), vjust = 1, hjust = 1
    )


set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
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
    

