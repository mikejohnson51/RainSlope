#' Determine time of inital runoff
#'
#' This is an internal function used by param() to calcualte time of initial runoff from a rainfall distribution generated with generate_rainfall().
#'
#' @param scenario_file a scenario_file generated with param()
#'
#' @export
#' @examples
#' tp = time_of_runoff(scene1)
#'
#' @author
#' Mike Johnson

time_of_runoff = function(mod){

  if(tolower(mod$param$type) == "constant"){

    r0 = mod$rain$rain[1]

    if(r0 <= mod$soil$conductivity){stop("Stability condition not met: r0 < K0")
    } else {
      tp = (1/r0)*(mod$soil$conductivity*mod$soil$tension*(mod$soil$porosity-mod$soil$content))/((r0 - mod$soil$conductivity)*cos(mod$param$beta*pi/180)*cos(mod$param$beta*pi/180))
    }

  } else if(tolower(mod$param$type) == "triangular"){

    c = (mod$rain$rain[2] - mod$rain$rain[1])

    cos2 = (cos(mod$param$beta*pi/180)^2)
    b0 = c^2 * cos2
    b1 = -1*c*mod$soil$conductivity*cos2
    b2 =  -2*mod$soil$conductivity*mod$soil$tension*(mod$soil$porosity-mod$soil$content)

    f = function(t) b0*t^3 + b1*t^2 + b2
    tp = uniroot.all(f, interval = c(0,length(mod$rain$time)/2))

    plot(f, xlim = c(0, 43200), lwd = 2)
    abline(h = 0, lty = 2)
    abline(v = tp, lty = 2)
    points(tp,0, pch = 16, cex = 1.5)
    text(tp, .00000007, labels = paste0("(",round(tp), ", 0)"), pos = 2)

  } else if(tolower(mod$param$type) == "input"){

    #mod = nrcs
    cos2 = (cos(mod$param$beta*pi/180)^2)


    for(i in 1:length(mod$rain$rain)){

      j= seq(1,i,1)
      Rd = sum(mod$rain$rain[j])
      xxx  = mod$soil$tension /((Rd * cos2)/(mod$soil$porosity-mod$soil$content))

      test   = mod$param$dt * (mod$soil$conductivity + mod$soil$conductivity * xxx)
      mod$rain$rain[i] >= test
      if(mod$rain$rain[i] >= test){break}
      tp = i

    }

  } else{ tp = NULL}


  return(round(tp))
}


