## AidData for R

The [AidData](http://aiddata.org) project makes available a wealth of information
on development finance. At the core of their database lies the project-level information
about development financing. The `aiddata` package for `R` simplifies the 
access to this database directly from the `R` console.

## Installation

`aiddata` is not (yet) published on CRAN. It needs to be installed directly
from GitHub, using `devtools`:
  
  ```{r, eval = FALSE}

install.packages("devtools")
library(devtools)
install_github("felixhaass/aiddata", dependencies = TRUE)
library(aiddata)

```


## Overview

`aiddata` is a set of functions that are wrapped around [`httr`](http://cran.r-project.org/web/packages/httr/) functions to access and download data from the [AidData API](http://aiddata.org/use-aiddatas-api). 

... to be completed.
