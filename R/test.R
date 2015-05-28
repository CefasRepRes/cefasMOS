#' Test Function
#'
#' Tests stuff
#'
#' @details TODO
#' @return whatever
#' @export
tester <- function(){
    require(data.table)
    x = data.table(int = c(0, 5, 1), id = c("MOUSE", "FROG", "DOG"), vec = c(900, 800, 400))
    x = x[,!c("id"), with = F]
    return(x)
}