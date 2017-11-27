#' Process Model Object
#'
#' Used to process and subset a model run generate with exe(). Here you can define the timesteps of interest and request a dataframe, excel file or graphical output t for each variable at those timesteps.
#' Timesteps can be defined by a vector of hours, minutes or seconds.
#'
#' @param model a model file calcualted with exe()
#' @param hr numeric. Define timesteps of interest in hours. Leave min and sec as NULL
#' @param min numeric. Define timesteps of interest in min. Leave hr and sec as NULL
#' @param sec numeric. Define timesteps of interest in seconds. Leave hr and min as NULL
#' @param plot logical. Request plots be generated for each varaible at each timestep. If TRUE plots will be stored as an element 'plot' in the output list object. Default is FALSE.
#' @param excel logical. Request a formated excel workbook be generated for processed data. Returned workbook contains a README sheet detailing the date, time, and user who generated the workbook; the modeled event and hill slope; and the contents of the remaining 8 sheets.
#'
#' @export
#'
#' @examples
#' constant_sub = process(model = constant_mod, hr = c(0,5,10,15,20,24), min = NULL, sec = NULL, plot = TRUE, excel = FALSE)
#' triangular_sub = process(model = triangular_mod, hr = NULL, min = NULL, sec = c(1200, 6000, 25200, 52200, 68100), plot = TRUE, excel = TRUE)
#'
#' @return
#' A list with elements for:
#'
#' 1) Scenario file
#'
#' 2) Runoff depth (y)
#'
#' 3) Infiltration (F)
#'
#' 4) Wetting Front (zf)
#'
#' 5) Runoff Volume (q)
#'
#' 6) Runoff depth at toe (y_toe)
#'
#' 7) Plots (if Plots = TRUE)
#'
#' @author
#' Mike Johnson

process = function(model = NULL, hr = NULL, min = NULL, sec = NULL, plot = TRUE, excel = FALSE){


if(!is.null(hr)){t = hr * 60 * 60 }

if(!is.null(min)){t = min * 60}

if(!is.null(sec)){t = sec}

# 1. y -----------------------------------------------------------------------

y.df = list()
y.df[[1]] = c(1:dim(model$y)[1])

for(i in 2:(length(t)+1)){
  y.df[[i]] = model$y[,t[i-1]]}

model$y[,t[5]]

  y.df = data.frame(y.df)
  names(y.df) = c("stations", t)
  melt.y = melt(y.df, id.vars = "stations")

# 2. q -----------------------------------------------------------------------
q.df = list()
q.df[[1]] = c(1:dim(model$q)[1])

for(i in 2:(length(t)+1)){
  q.df[[i]] = model$q[,t[i-1]]}

  q.df = data.frame(q.df)
  names(q.df) = c("stations", t)
  melt.q = melt(q.df, id.vars = "stations")

# 3. F -----------------------------------------------------------------------

F.df = list()
F.df[[1]] = c(1:dim(model$F)[1])

for(i in 2:(length(t)+1)){
  F.df[[i]] = model$F[,t[i-1]]}

  F.df = data.frame(F.df)
  names(F.df) = c("stations", t)
  melt.F = melt(F.df, id.vars = "stations")

# 4. zf ----------------------------------------------------------------------

zf.df = list()
zf.df[[1]] = c(1:dim(model$zf)[1])

for(i in 2:(length(t)+1)){
  zf.df[[i]] = model$zf[,t[i-1]]}

  zf.df = data.frame(zf.df)
  names(zf.df) = c("stations", t)
  melt.zf = melt(zf.df, id.vars = "stations")


# 4. toe ----------------------------------------------------------------------

y.toe.df = list()
y.toe.df[[1]] = c(1:length(model$y_toe))
y.toe.df[[2]] = model$y_toe
y.toe.df = data.frame(y.toe.df)
names(y.toe.df) = c("Time", 'y')

data = list(param = model$param,y= y.df, F = F.df,zf = zf.df, q = q.df, toe = y.toe.df)


if(plot == TRUE){
py = ggplot(data=melt.y, aes(x=stations, y=value, group=variable, color = variable)) + geom_line(aes(color = variable))  + labs(x = "Stations (k)", y = "Depth (m)", title = "Runoff (y)", color = "Time since tp (sec)") +
  theme_bw() + xlim(0,305)
pF  = ggplot(data=melt.F, aes(x=stations, y=value, group=variable)) + geom_line(aes(color = variable))  + labs(x = "Stations (k)", y = "Depth (m)", title = "Infiltration (F)", color = "Time since tp (sec)") +
      theme_bw() + xlim(0,305)
pzf = ggplot(data=melt.zf, aes(x=stations, y=value, group=variable)) + geom_line(aes(color = variable)) + labs(x = "Stations (k)", y = "Depth (m)", title = "Wetting Front (zf)", color = "Time since tp (sec)") +
      theme_bw() + xlim(0,305)
pq  = ggplot(data=melt.q, aes(x=stations, y=value, group=variable)) + geom_line(aes(color = variable))  + labs(x = "Stations (k)", y = "Rate (m3/s)", title = "Runoff (q)", color = "Time since tp (sec)") +
      theme_bw() + xlim(0,305)
toe = ggplot(data = y.toe.df, aes(x = Time, y = y)) + geom_line()  + labs(x = "Time", y = "Depth (m)", title = "Runoff Depth (y)", color = "Time since tp (sec)") +
      theme_bw()
rain = ggplot(data = data$param$rain, aes(x = time, y = rain)) + geom_line(aes(group=1), colour="#000099")  + labs(x = "Time", y = "Depth (m)", title = "Rainfall Distribution") +
  theme_bw()

all = grid.arrange(py, pq, toe, pF, pzf, rain, ncol = 3)

plot = list(y = py, F = pF, zf = pzf, toe = toe, rain = rain, q = pq, all = all)
}

if(excel == TRUE){

  excel_build(model = data)

}

return(list(param = model$param, y= y.df, F = F.df,zf = zf.df, q = q.df, toe = y.toe.df, plots = plot))

}





