showRecommendations <- function(graph) {
    
    if (is.null(graph)) {
        return(NULL)
    }

    vis.nodes <- tibble::as_tibble(tidygraph::activate(graph, nodes))
    vis.edges <- tibble::as_tibble(tidygraph::activate(graph, edges))

    # Nodes
    vis.nodes$id <- vis.nodes$movieID
    vis.nodes$shape <- 'dot'
    vis.nodes$size <- 12
    # vis.nodes$size   <- scales::rescale(vis.nodes$degree, to = c(10, 20))
    vis.nodes$title <- glue::glue('{vis.nodes$name}:\n{vis.nodes$overview}')
    vis.nodes$label <- stringr::str_replace(vis.nodes$name, ' \\(.*', '')
    vis.nodes$group <- factor(vis.nodes$isQuery, c(FALSE, TRUE))
    vis.nodes$mass <- c(1, 10)[vis.nodes$group]
    vis.nodes$borderWidth <- c(1, 2)[vis.nodes$group]
    vis.nodes$borderWidthSelected <- 0
    vis.nodes$color.background = c('#05173d', '#550f0f')[vis.nodes$group] 
    vis.nodes$color.border = c('#ffffff', '#ffffff')[vis.nodes$group]
    vis.nodes$color.highlight.background = '#df8715'
    # vis.nodes$fixed <- c(FALSE, TRUE)[vis.nodes$group]
    vis.nodes$font.color = '#05173d'
    vis.nodes$font.size = 18
    vis.nodes$font.background = '#e2e2e2'
    vis.nodes$shadow = TRUE

    # Edges
    vis.edges$from <- vis.nodes$movieID[vis.edges$from]
    vis.edges$to <- vis.nodes$movieID[vis.edges$to]
    vis.edges$width <- scales::rescale(vis.edges$weight2, to = c(1, 15))
    vis.edges$color <- "#6e6e6e"
    vis.edges$smooth <- TRUE
    vis.edges$shadow <- TRUE

    # Graph
    vis <- visNetwork::visNetwork(vis.nodes, vis.edges, width = "100%", height = "90vh", background = '#e2e2e2') %>% 
        visNetwork::visOptions(
            highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE, hideColor = '#bdbdbd'), 
            nodesIdSelection = list(enabled = TRUE, style = 'visibility: hidden;'), 
            autoResize = TRUE, 
            manipulation = list(enabled = FALSE)
        ) %>%
        visNetwork::visPhysics(solver = 'barnesHut', stabilization = TRUE) %>%
        visNetwork::visLayout(randomSeed = 11)
    if (nrow(vis$x$nodes) < 2) vis <- NA
    return(vis)
}

