ferrybox.ADC_to_PAR <- function(y){
    #       (0.5301*exp(((y-32770)/3276.7)*3.3674))/5.008332834
    return( (0.5301*exp(((y-32770)/3276.7)*3.3674))/5.008332834 )
}

par_from_voltage <- function(x, factor, offset){
    return(factor * exp(offset * x))
}