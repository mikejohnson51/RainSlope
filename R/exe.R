#' Execute Model
#'
#' Run the model for a sceanrio file built using param().
#'
#' @param sceanrio_file A three element list built using param()
#' @param save.as logical. If TRUE model data will be saved as an RDS file in the working directory. Default is FALSE
#'
#' @export
#' @examples
#' mod = exe(scene1, save.as = TRUE)
#' @author
#' Mike Johnson

exe = function(scenario_file, save.as = FALSE){

  mod = scenario_file

  #Extract Variables

  dt = mod$param$dt
  a0 = mod$param$a0
  conductivity = mod$soil$conductivity
  a1 = mod$param$a1
  gamma = mod$param$gamma
  alpha = mod$param$alpha
  coeff = mod$param$coeff
  dx = mod$param$dx
  tp = mod$param$tp
  rain = mod$rain$rain
  width = mod$hill$Z
  porosity = mod$soil$porosity
  content = mod$soil$content

  # Initialize model
   t_int = mod$param$t_int
   d_int = mod$param$d_int

   F = matrix(0, nrow = d_int, ncol = t_int)
      F[,1] = .03487
      for( i in 2:t_int){
        F[1,i] = ((a1 * dt) + (F[1, i-1]^2 * dt)) / (F[1, i-1] - (conductivity * dt))
      }

     y  = matrix(1e-26, nrow = d_int, ncol = t_int)
     a  = matrix(0,     nrow = d_int, ncol = t_int)
     f0 = matrix(0,     nrow = d_int, ncol = t_int)
     g  = matrix(0,     nrow = d_int, ncol = t_int)
     b  = matrix(0,     nrow = d_int, ncol = t_int)
     d  = matrix(0,     nrow = d_int, ncol = t_int)
     r  = matrix(0,     nrow = d_int, ncol = t_int)
     q  = matrix(0,     nrow = d_int, ncol = t_int)
     zf = matrix(0,     nrow = d_int, ncol = t_int)

# Execute Model

     for (t in 2:t_int){
       for(x in 2:d_int){

       f0[x, t-1] <- (F[x,t-1] + F[x-1,t-1]) / (2*dt)

        a[x, t-1] <- a0 / (2*f0[x,t-1])

        b[x, t-1] <- (conductivity * dt) +
                      (a1/f0[x,t-1]) + (a0/f0[x,t-1]) * (y[x,t-1]/2)

        g[x, t-1] <- 1 / (gamma + a[x, t-1])

        r[x, t-1] <- (dt * alpha * coeff) *
                      ((y[x, t-1] + y[x-1, t-1])/2) ^ (coeff-1) *
                      ((y[x, t-1] - y[x-1, t-1])/dx)

        d[x, t-1] <- -1*r[x, t-1] + mod$rain$rain[tp + t] + F[x, t-1]

        runoff = 10

          if(y[x, t-1] <= 0){
            runoff = 0
          }else{
            runoff = g[x, t-1] * (y[x, t-1] - F[x, t-1] - b[x, t-1] + d[x, t-1])
          }

          if(runoff > 0){
            y[x, t] = runoff
          }else{
            y[x, t] = 0
          }

          if(y[x,t] <= 0){
            F[x, t] = F[x, t-1] + rain[tp + t] * dt
          }else{
            F[x, t] = F[x, t-1] + (a[x, t-1] * y[x, t]) + b[x, t-1]
          }
       }
     }

     q = alpha * width * y^coeff
     zf = F / (porosity - content)
     y_toe = c(rep(0,  tp), y[d_int,])

     data = list(param = scenario_file, F = F, y = y, q = q, zf = zf, y_toe = y_toe )

     if(save.as){
        filename = paste0(save.as,".rds")
        saveRDS(data, filename)
     }

     return(data)
}






