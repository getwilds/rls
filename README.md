<!-- Don't edit README.md! Edit README.Rmd, then run `make readme` -->

# rls

<!-- badges: start -->
[![Project Status: Concept – Not useable, no support, not open to feedback, unstable API.](https://getwilds.org/badges/badges/concept.svg)](https://getwilds.org/badges/#concept)
[![R-check](https://github.com/getwilds/rls/actions/workflows/R-check.yml/badge.svg)](https://github.com/getwilds/rls/actions/workflows/R-check.yml)
<!-- badges: end -->

Row Level Security stuff

## Installation

Development version


``` r
# install.packages("pak")
pak::pak("getwilds/rls")
```

## Golden path (NOT WORKING YET, README Driven development)

Connect

```r
library(rls)
library(DBI)
library(RPostgres)
con <- dbConnect(Postgres())
```

Create a policy and add it to a table

xxx

## Bugs? Features?

Open an issue on our [issue tracker](https://github.com/getwilds/rls/issues/).

## Contributors

This package follows [Git Flow](https://nvie.com/posts/a-successful-git-branching-model/).

## Code of Conduct

Please note that the rls project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## License

[MIT](LICENSE.md)
