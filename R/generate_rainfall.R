#' Generate a rainfall distirbution
#'
#' This is an internal function used by param() to generate a rainfall timeseries based on input values.
#'
#' @param type character. Used to define the distribution of rainfall. Options are: (1) Constant (2) triangualar (3) input
#' @param depth numeric. Describes the total depth of rainfall in meters
#' @param hours numeric. Decribes the total duration of rainfall in hours
#' @param timestep numeric. Describes the timestep to generate rainfall measurements in seconds
#' @param path character. Describes a path to an input csv or xlsx file if type is 'input'
#'
#' @export
#' @examples
#' constant_ppt = generate_rainfall(type = 'constant', depth = .4, hours = 24, timestep = 1, path = NULL)
#' triangular_ppt = generate_rainfall(type = 'triangular', depth = .4, hours = 24, timestep = 1, path = NULL)
#' nrcs_ppt = generate_rainfall(type = 'input', depth = NULL, hours = NULL, timestep = NULL, path = "nrcs_type2_0.40m.xlsx")
#'
#' @author
#' Mike Johnson



generate_rainfall = function(type = 'input', depth = NULL, hours = NULL, timestep = NULL, path = NULL) {

# Initialize Rainfall -----------------------------------------------------
  rainfall <- NULL

# Catch if inValid input --------------------------------------------------

  if(!isTRUE(type == "triangular" || type == "constant" || type == "input")){
     return(print(cat(paste("Type must be either:", "(A) Triangular", "(B) Constant", "(C) Input (either a csv or xlsx)", sep = "\n")))
     )}

# Catch if not fully parameterized ----------------------------------------

  if(type == 'constant' && is.null(depth) || type == 'triangular' && is.null(depth)) {
    return(print(cat(paste("!!   An input for total rainfall depth in meters is required",
                 "!!   Use the parameter 'depth =' ", sep = "\n"))))}

  if(type == 'constant' && is.null(hours) || type == 'triangular' && is.null(hours)) {
    return(print(cat(paste("!!   An input for total duration of rainfall in hours is required",
                           "!!   Use the parameter 'hours =' ", sep = "\n"))))}

  if(type == 'constant' && is.null(timestep) || type == 'triangular' && is.null(timestep)) {
    return(print(cat(paste("!!   An input for a time step in seconds is required",
                           "!!   Use the parameter 'timestep =' ", sep = "\n"))))}

# Create Runoff --------------------------------------------

  if(type == 'constant'){

    seconds <- hours * 60 * 60
    periods <- seconds/timestep
    rainfall <- vector(mode = 'numeric', length = periods)

    for(i in 1:length(rainfall)){
      rainfall[i] = depth/periods
    }

  } else if (type == 'triangular'){

    seconds <- hours * 60 * 60
    periods <- seconds/timestep
    rainfall <- vector(mode = 'numeric', length = periods)

    for (i in 1:(length(rainfall))/2){
      slope = ((depth/periods)*2)/(periods/2)
      rainfall[i] = slope * i
    }

    for(j in ((length(rainfall)/2)+1):length(rainfall)){
      rainfall[j] = max(rainfall) - (slope * (j - length(rainfall)/2))
    }

  } else if(type == 'input'){

  options(warn = 0)
   suppressWarnings( rainfall <- try(read.csv(path), silent = TRUE))
   suppressWarnings(rainfall <- try(read.xlsx(file <- path, sheetIndex <- 1)))

   periods <- length(rainfall[,1])
   rainfall <- rainfall[,2]

  }

 if(!is.null(rainfall)){
  plot(rainfall, type = 'l', main = "Rainfall Distirbution",
       xlab = paste("Time (time step =", timestep,"second)"), ylab = 'Depth (meters)', col = 'blue', lwd = 3)
  grid()

  return(data.frame(time = seq(1,periods,1), rain = rainfall))

}
}


