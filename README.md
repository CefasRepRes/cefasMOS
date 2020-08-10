# Installing

The `cefasMOS` package is in development and has not been submitted to CRAN.
You can install it directly from bitbucket (A github analog) using the `devtools` package.

The package should now handle all of it's dependencies.
Other rarely used functions may require additional packages, install these later if you have problems.
Please report any issues to Tom.



```r
install.packages("devtools")

devtools::install_bitbucket("betascoo8/cefasmos")

  # optionally if you want the development version
devtools::install_bitbucket("betascoo8/cefasmos", subdir = "dev")

  # install_bitbucket considers a package built under a different version of R an error
  # (it's not)
  # if it fails to install with such and error try
devtools::install_bitbucket("betascoo8/cefasmos", force = T)
```


```r
library("cefasMOS") # load the package
```

# data.table

Due to the large number of rows we tend to be dealing with `cefasMOS` leans heavily on the `data.table` package.
This is a highly optimised extension of the typical data.frame.
A `data.table` inherits from a `data.frame` such that it *is* a `data.frame` and all of the usual functions that can be used with a `data.frame` will work.
However, it does have some powerful features which allow fast grouping and assignment, and these use different syntax that base R and differs from the `tidyverse` style of data manipulation.
It's suggested you familiarise yourself with the `data.table` package; datacamp provide a useful cheat sheet [here]( https://www.datacamp.com/community/tutorials/data-table-cheat-sheet).

# Extracting SmartBuoy data

The `smartbuoy.timeseries` will draw a single parameter with a zoomable timeseries plot.
It will also by default fetch telemetry data when available.
See `?smartbuoy.timeseries` for more information.


```r
x = smartbuoy.timeseries("TH1", "TEMP", yr = 2012:2014)
head(x$data)
  # x$dygraph
```

To extract QC'd data or raw data use the `smartbuoy.fetch` function.
By default only data where min QC level has been reached is returned, this can be overridden.


```r
lbdat = smartbuoy.fetch(deployment_group = "LIVBAY",
                        after = "2014-01-01", parameters = c("FLUORS", "TOXN"))

  # save the data
fwrite(lbdat, file = "lbdat.csv") # fread is a data.table optimisation of write.csv
```

When pivoting (dcast), make sure to use the data.table version to preserve the data.table and for speed.
from data.table v1.9.6 dcast is a S3 method, i.e. you can use `dcast` rather than `dcast.data.table`.
When there are multiple measurements for the same parameter at the same dateTime we have two options.
pass `fun.aggregate = mean` to average the values, for example take the mean temperature from the optode and FSI.
Alternatively we can keep the duplicates, but we need to add an identifying column first.
Another option is to rename the duplicate variable, e.g. rename a second OBS to "FTU2".


```r
wp = smartbuoy.fetch(deployment = "DOWSING/038", ct_temp_only = F)
  # aggregation not specified so default "length" used.
dcast.data.table(wp, dateTime + deployment ~ par, value.var = "value")

  # taking the mean
dcast.data.table(wp, dateTime + deployment ~ par,
                 value.var = "value", fun.aggregate = mean)

  # keeping the values
wp[,sen := .GRP, by = list(dateTime, deployment, par)]
dcast.data.table(wp, dateTime + deployment + sen ~ par, value.var = "value")
```
