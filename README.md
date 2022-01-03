# movierecommendations

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![pkgdown](https://github.com/js2264/movierecommendations/workflows/pkgdown/badge.svg)](https://github.com/js2264/movierecommendations/actions)
<!-- badges: end -->

## Get recommendations from TMDB

```r
library(movierecommendations)

# Load TMDB movie list
tmdb_movies <- tmdb_movies()
tmdb_movies

# Search a movie to query
searchField('stocker', tmdb_movies)

# Get TMDB recommendations
recommendationNetwork('Stoker (2013)', tmdb_movies)
```

## Expand recommendations

```r
# Load TMDB movie list
tmdb_movies <- tmdb_movies()
tmdb_movies

# Get TMDB recommendations
net <- recommendationNetwork('Stoker (2013)', tmdb_movies)
showRecommendations(net)

# Expand recommendations
net <- recommendationNetwork(query = 'Stoker (2013)', tmdb_movies)
net <- net |> addMovie(query = 'Thirst (2009)', tmdb_movies)
net <- net |> addMovie(query = 'Sympathy for Lady Vengeance (2005)', tmdb_movies)
net <- net |> addMovie(query = 'Only Lovers Left Alive (2013)', tmdb_movies)
showRecommendations(net)
```

## Open up a shiny app (the FUN way)

A fully interactive movie explorer is available!

```r
movieExplorer()
```

![](https://github.com/js2264/movierecommendations/blob/master/inst/extdata/network.png)

![](https://github.com/js2264/movierecommendations/blob/master/inst/extdata/network_options.png)

