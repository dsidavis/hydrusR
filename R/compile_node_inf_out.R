
#' Compile NOD_INF.OUT data from all simulations
#'
#' @param outputs.path
#' @param output
#' @param num.of.sims
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
compile.nod_inf.out<- function(outputs.path, output =  NULL, num.of.sims, ...) {

      if(is.null(output) | missing(output)) {
            output = output = c("Head", "Moisture", "K", "C", "Flux",
                                "Sink", "Kappa", "v/KsTop", "Temp")
      }

      simulations = paste0("sim", 1:num.of.sims)
      sim_paths = file.path(outputs.path, simulations)
      cat("reading NOD_INF.OUT...")
      node_out_list = lapply(X = sim_paths, FUN = read.nod_inf, output = output)
      cat("done\n")

      node_out_list = vector("list", length = num.of.sims)
      for(a in 1: num.of.sims){
            cat("reading Node_inf.out for simulation ", a, "...")
            sim_path  = file.path(outputs.path, paste("sim", a, sep = ""))

            node_out = read.nod_inf(simulation.path = sim_path, output = output)

            if(a > 1){
                  last_out = node_out_list[[a-1]]
                  node_out$Time = node_out$Time + last_out$Time[nrow(last_out)]

                  rm(last_out)

            }

            node_out_list[[a]] = node_out

            node_data_compiled = data.frame(do.call("rbind", node_out_list),
                                            row.names = NULL, check.names = FALSE)

            cat("done\n")

      }

      return(node_data_compiled)

}

