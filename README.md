# movierecommendations

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![pkgdown](https://github.com/js2264/movierecommendations/workflows/pkgdown/badge.svg)](https://github.com/js2264/movierecommendations/actions)
<!-- badges: end -->

## Get recommendations from TMDB lists (the tedious way)

```r
library(movierecommendations)

# Load TMDB database
tmdb <- tmdb()

# Search a movie to query
searchField('The Aveangers', tmdb, field = 'title')

# Find frequently associated movies
getRecommendations('Friends with Benefits (2011)', tmdb)
getRecommendations(c('Friends with Benefits (2011)', 'Love & Other Drugs (2010)'), tmdb)
getRecommendations(c('Friends with Benefits (2011)', 'Love & Other Drugs (2010)', 'Remember Me (2010)'), tmdb) |> 
    dplyr::group_by(to_title) %>% dplyr::tally() %>% dplyr::arrange(dplyr::desc(n)) %>% as.data.frame()
```

## Get recommendations from movie co-occurrences in TMDB lists (the manual way)

```r
# Load TMDB co-occurrences network
co_occurrences <- co_occurrences()

# Get recommendations for network of co-occurrences
net <- recommendationNetwork(co_occurrences, query = 'Stoker (2013)')
showRecommendations(net)

# Get recommendations for network of co-occurrences
net <- recommendationNetwork(co_occurrences, query = 'Stoker (2013)')
showRecommendations(net)

# Get recommendations for network of co-occurrences, for several movies with custom filters
recommendationNetwork(
    co_occurrences,
    query = c('The Others (2001)', 'Stoker (2013)', 'The Orphanage (2007)', 'The Strangers (2008)', 'The Grudge 2 (2006)'), 
    filters = filters(n_recommendations = 20, th_year_min = 2000, th_year_max = 2020)
) |> showRecommendations()

# Get recommendations for network of co-occurrences, for several movies
net <- recommendationNetwork(query = 'Stoker (2013)')
net <- net |> addMovie(query = 'The Skin I Live In (2011)')
net <- net |> addMovie(query = 'All About My Mother (1999)')
net <- net |> addMovie(query = 'Mama (2013)')
net <- net |> addMovie(query = 'Only Lovers Left Alive (2013)')
showRecommendations(net)
```

## Open up a shiny app (the FUN way)

A fully interactive movie explorer is available!

```r
movieExplorer()
```

![](https://github.com/js2264/movierecommendations/blob/master/inst/extdata/network.png)

![](https://github.com/js2264/movierecommendations/blob/master/inst/extdata/network_options.png)

