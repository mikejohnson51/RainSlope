#' Parameterize a Scenario file
#'
#' This function paramaterizes a scenario file for the model via a GUI. This is necessary to ensure the file is formated correctly for a model run. To use, simply declare a varaible and run param().
#'
#' Upon execution, a GUI interface will appear and request the following infomation to define three lists: (1) Hill (2) Soil (3) rainfall
#' If you have already declared a sceanrio file, simply type the variable name in the approapitate heading and click 'Use old run'
#'
#' Hill shape
#'
#' Vertical Units: Provide in units of (m) (i.e. 60)
#'
#' Horizontal Units: Provide in units of (m) (i.e. 300)
#'
#' Width Units: Provide in units of (m) (i.e. 50)
#'
#' Roughness: Provide in units of () (i.e. )
#'
#' Conductivity: Provide in units of () (i.e. )
#'
#' Soil Water Tension: Provide in units of () (i.e. )
#'
#' Porosity: Provide in units of () (i.e. )
#'
#' Water Content: Provide in units of () (i.e. )
#'
#'
#' A user is given three options for defining a rainfall distribution. (1) A constant rate (2) a triangular distribution (3) an input file\n
#'
#' In all cases the user must specify a type as: 'Constant', 'Triangular', or 'Input'
#' For cases 1 and 2 a user must also specify the rainfall
#'
#' Duration: Provide in units of (hours) (i.e. 24 )
#'
#' Depth: Provide in units of (m) (i.e. .4 )
#'
#' Timestep: Provide in units of (seconds) (i.e. 1)
#'
#' For an input storm, a user must specify a path to a .csv or xlsx file
#'
#' Finally, specify a distance (deltaX) and time step (deltaT) for the model to calculate over.
#'
#' Once complete submit the information to store and derive all needed model parameters to the decalred variable. If you have already declared a sceanrio file, simply type the variable name in the approapitate heading and select 'Use old run' to auto populate the scenario file.
#'
#' @return
#'
#' This function will return a three element list (hill, soil, rain) accesable via the '$' operator
#'
#' @export
#' @examples
#' scene1 = param()
#' @author
#' Mike Johnson


param <- function(){

  #Declare Variable Names
    prior <- tclVar("")
   ###########################
    rise <- tclVar("")
    run <- tclVar("")
    width <- tclVar("")
    ###########################
    roughness     <- tclVar("")
    conductivity  <- tclVar("")
    tension       <- tclVar("")
    porosity      <- tclVar("")
    water_content <- tclVar("")
    ###########################
    type <- tclVar("")
    hours <- tclVar("")
    depth <- tclVar("")
    timestep <- tclVar("")
    rain_timestep <- tclVar("")
    distance_bin <- tclVar("")
    path <- tclVar("")

  # Create Query boundaries
    tt <- tktoplevel()

    tkwm.title(tt,"Coupled Runoff Infiltration Model")

  # Define Entries
    prior.entry = tkentry(tt, textvariable=prior)
    ###########################
    y.entry <- tkentry(tt, textvariable=rise)
    x.entry <- tkentry(tt, textvariable=run)
    b.entry <- tkentry(tt, textvariable=width)
    ###########################
    m.entry <- tkentry(tt, textvariable=roughness)
    k.entry <- tkentry(tt, textvariable=conductivity)
    yf.entry <- tkentry(tt, textvariable=tension)
    n.entry <- tkentry(tt, textvariable=porosity)
    v.entry <- tkentry(tt, textvariable=water_content)
    ###########################
    t.entry <- tkentry(tt, textvariable=type)
    h.entry <- tkentry(tt, textvariable=hours)
    d.entry <- tkentry(tt, textvariable=depth)
    dt.entry <- tkentry(tt, textvariable=timestep)
    rts.entry <- tkentry(tt, textvariable=rain_timestep)
    dx.entry <- tkentry(tt, textvariable=distance_bin)
    p.entry <- tkentry(tt, textvariable=path)

  # Allow clear button
    reset <- function() {
      tclvalue(rise)<-""
      tclvalue(run) <-""
      tclvalue(width) <-""
      ###########################
      tclvalue(roughness) <-""
      tclvalue(conductivity) <-""
      tclvalue(tension) <-""
      tclvalue(porosity) <-""
      tclvalue(water_content) <-""
      ###########################
      tclvalue(type) <-""
      tclvalue(hours) <-""
      tclvalue(depth) <-""
      tclvalue(timestep) <-""
      tclvalue(rain_timestep) <-""
      tclvalue(distance_bin) <-""
      tclvalue(path) <-""
      }

    submit <- function() {

      y <- as.numeric(tclvalue(rise))
      x <- as.numeric(tclvalue(run))
      b <- as.numeric(tclvalue(width))
      ###########################
      m <- as.numeric(tclvalue(roughness))
      k <- as.numeric(tclvalue(conductivity))
      yf <- as.numeric(tclvalue(tension))
      n <- as.numeric(tclvalue(porosity))
      v <- as.numeric(tclvalue(water_content))
      ###########################
      t <- as.character(tclvalue(type))
      h <- as.numeric(tclvalue(hours))
      d <- as.numeric(tclvalue(depth))
      dt <- as.numeric(tclvalue(timestep))
      rts <- as.numeric(tclvalue(rain_timestep))
      dx <- as.numeric(tclvalue(distance_bin))
      p <- as.character(tclvalue(path))

      hill <- parent.env(environment())
        hill$x  <- x
        hill$y  <- y
        hill$b  <- b
        ###########################
        hill$m  <- m
        hill$k  <- k
        hill$yf <- yf
        hill$n  <- n
        hill$v  <- v
        ###########################
        hill$t  <- t
        hill$h  <- h
        hill$d  <- d
        hill$dt <- dt
        hill$rts <- rts
        hill$dx <- dx
        hill$p  <- p

      tkdestroy(tt)
    }

    OnOK <- function() {

        prior = eval(parse(text = tclvalue(prior)))

        tclvalue(rise)<- prior$hill$Y
        tclvalue(run) <- prior$hill$X
        tclvalue(width) <- prior$hill$Z
        ###########################
        tclvalue(roughness) <-prior$soil$roughness
        tclvalue(conductivity) <- prior$soil$conductivity
        tclvalue(tension) <-prior$soil$tension
        tclvalue(porosity) <-prior$soil$porosity
        tclvalue(water_content) <-prior$soil$content
        ###########################
        tclvalue(type) <-prior$param$type
        tclvalue(hours) <-prior$param$hours
        tclvalue(depth) <-prior$param$depth
        tclvalue(timestep) <-prior$param$dt
        tclvalue(rain_timestep) <-prior$param$rts
        tclvalue(distance_bin) <-prior$param$dx
        tclvalue(path) <-prior$param$p

      }

    tkgrid(tklabel(tt,text="Use Previous Run?"),   prior.entry, pady = 10, padx =10)

    tkgrid(tklabel(tt,text="Hillslope Properties"),columnspan=2)
      tkgrid(tklabel(tt,text="Vertical Units"),   y.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Horizontal Units"), x.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Width Units"),      b.entry, pady = 10, padx =10)

    tkgrid(tklabel(tt,text="Soil Properties"),columnspan=2)
      tkgrid(tklabel(tt,text="Roughness"),          m.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Conductivity"),       k.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Soil Water Tension"), yf.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Porosity"),           n.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Water Content"),      v.entry, pady = 10, padx =10)

    tkgrid(tklabel(tt,text="Rainfall Parameters"),columnspan=2)
      tkgrid(tklabel(tt,text="Rainfall Type"),            t.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Rainfall Duraction (hrs)"), h.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Total Rainfall Depth (meters)"),     d.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Generate Rainfall at timestep? (sec)"),     rts.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Path"),                     p.entry, pady = 10, padx =10)

    tkgrid(tklabel(tt,text="Model Parameters"),columnspan=2)
      tkgrid(tklabel(tt,text="Distance Step (dx)"),       dx.entry, pady = 10, padx =10)
      tkgrid(tklabel(tt,text="Time step (sec)"),          dt.entry, pady = 10, padx =10)

    submit.but <- tkbutton(tt, text="submit", command=submit)
    # reset.but  <- tkbutton(tt, text="Clear", command=reset)
    OK.but     <-    tkbutton(tt,text="Use old run",command=OnOK)


    tkgrid(submit.but, OK.but)

    tkwait.window(tt)

    hill = data.frame(X = x , Y = y, Z = b, slope = y/x)

    soil = data.frame(roughness = m, conductivity = k, tension = yf , porosity = n , content = v)

    param = data.frame(type = t, hours = h, depth = d, rts = rts, dt = dt, dx = dx, path = toString(p), stringsAsFactors = FALSE) %>%
      mutate(beta = atan(hill$slope)*(180/pi),
             gamma = 1,
             coeff = 5/3,
             alpha = (1/soil$roughness)*gamma^coeff*hill$slope^.5,
             a0 = soil$conductivity*(soil$porosity-soil$content)/cos(beta*pi/180),
             a1 = a0 * yf/cos(beta*pi/180)
             )

    rain = generate_rainfall(type = tolower(as.character(t)), depth = d, hours = h, timestep = rts, path = toString(p))

    if(param$type == 'input'){
      param$depth = sum(rain$rain)
      param$rts = (h*60*60) / length(rain$rain)
    }

    data = list(hill = hill, soil = soil, rain = rain, param = param)

    data$param$tp = time_of_runoff(data)
    data$param$t_int = data$rain$time[length(data$rain$time)] - data$param$tp
    data$param$d_int = (data$hill$X / data$param$dx) + 1

    return(data)

}



