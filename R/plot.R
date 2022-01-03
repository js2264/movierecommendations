showRecommendations <- function(graph) {
    
    if (is.null(graph)) {
        return(NULL)
    }

    # Nodes
    vis.nodes <- tibble::as_tibble(tidygraph::activate(graph, nodes))
    vis.nodes$shape <- 'dot'
    vis.nodes$size <- 12
    vis.nodes$title <- vis.nodes$unique_title
    vis.nodes$label <- stringr::str_replace(vis.nodes$unique_title, ' \\(....\\)', '')
    vis.nodes$group <- factor(vis.nodes$isQuery, c(FALSE, TRUE))
    vis.nodes$mass <- c(1, 10)[vis.nodes$group]
    vis.nodes$borderWidth <- c(1, 2)[vis.nodes$group]
    vis.nodes$borderWidthSelected <- 1
    vis.nodes$color.background = c('#05173d', '#550f0f')[vis.nodes$group] 
    vis.nodes$color.border = c('#ffffff', '#ffffff')[vis.nodes$group]
    vis.nodes$color.highlight.background = '#000000'
    vis.nodes$color.highlight.border = '#000000'
    vis.nodes$font.color = '#05173d'
    vis.nodes$font.size = 18
    vis.nodes$font.background = '#e2e2e2'
    vis.nodes$shadow = TRUE

    # Edges
    vis.edges <- tibble::as_tibble(tidygraph::activate(graph, edges))
    vis.edges$from <- vis.nodes$id[vis.edges$from]
    vis.edges$to <- vis.nodes$id[vis.edges$to]
    vis.edges$width <- 5
    vis.edges$color <- "#6e6e6e"
    vis.edges$smooth <- TRUE
    vis.edges$shadow <- TRUE

    # Graph
    vis <- visNetwork::visNetwork(vis.nodes, vis.edges, width = "100%", height = "90vh", background = '#e2e2e2') %>% 
        visNetwork::visOptions(
            highlightNearest = list(enabled = TRUE, degree = 0, hover = FALSE, hideColor = '#bdbdbd'), 
            nodesIdSelection = list(enabled = TRUE, style = 'width: 0px; height: 0px; visibility: hidden;'), 
            autoResize = TRUE, 
            manipulation = list(enabled = FALSE)
        ) %>%
        visNetwork::visPhysics(solver = 'barnesHut', stabilization = TRUE) %>%
        visNetwork::visLayout(randomSeed = 11)
    if (nrow(vis$x$nodes) < 2) vis <- NA
    return(vis)
}

