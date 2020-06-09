# CefasMOS

# TODO

### 1. global variable issue
Much of this package uses data.table modify by reference features.
i.e. `DF[, var_name := var_name * 2]`.
`R CHECK` can't cope with these features so will throw a `no visible binding for global variable` warning during package build/check.

The recommended way of dealing with this by the `data.table` team is to bind the variables locally to `NULL` inside the function.

e.g.

```r
my_fn <- function() {
  mpg <- hp <- mpg_div_hp <- NULL
  mtcars <- data.table(mtcars)
  mtcars[, mpg_div_hp := mpg / hp]
  mtcars[]
}

```

However as of May 2020, that hasn't been done.

### 2. Develop ESM2 profiler oxygen lag correction scripts

### 3. Write tests!

### 4. Documentation updates
