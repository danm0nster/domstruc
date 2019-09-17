# domstruc
Compute Metrics of Dominance Hierarchies

This R package computes various classic and new measures of dominance hierarchies.

## Example

## Installation
Since this package is not yet on CRAN, you have to install it from GitHub. You can do this by installing the devtools package and then install from GitHub:

```
install.packages("devtools")
library(devtools)
install_github("danm0nster/domstruc")
```
Note: This is not be possible when the repository is private. As long as the repository is private you have to supply
`install_github()` with a Personal Access Token (PAT) which you can generate under settings, developer settings on GitHub. With a PAT set up for use with R, you can install from the private repository like this:
```
install_github("danm0nster/domstruc", auth_token = github_pat())
```

For information about how to set up your PAT, see [the documentation](https://usethis.r-lib.org/reference/browse_github_pat.html)

## Overview

## Authors and contributors
The `domstruc`package is written by

* Elizabeth Hobson
* Simon DeDeo
* Dan Mønster
