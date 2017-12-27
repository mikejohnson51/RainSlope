options(java.parameters = "- Xmx1024m")

#library(xlsx); library(ggplot2); library(tcltk); library(tcltk2); library(tidyverse); library(rootSolve);library(reshape2);library(ggthemes);library(gridExtra);


#source('generate_rainfall.R'); source('param.R'); source('time_to_runoff.R'); source('exe.R'); source('process.R'); source('excel_build.R'); source('r2excel.R')

################################################################################
################################################################################
# ------------------------------- Workflow ----------------------------------- #
################################################################################
# ------------------- Step 1: Create Scenario with param() ------------------- #
# ----------------------- Step 2: Run model with exe() ----------------------- #
# ------------------- Step 3: Subset output with process() ------------------- #
################################################################################
################################################################################
#
#
#
################################################################################
# ------------------- Step 1: Create Scenario with param() ------------------- #
################################################################################

  constant = param()
  saveRDS(scene1, 'constant_scene.rds')
  constant = readRDS("constant_scene.rds")
  #scene_tri = param()
  #nrcs = param()
#test = param()
################################################################################
# ----------------------- Step 2: Run model with exe() ----------------------- #
################################################################################
  #mod1 = exe(scene1, save.as = ")


  #saveRDS(mod1, "constant.rds")
  #mod1 = readRDS("constant.rds")

  #mod_tri = exe(scene_tri)
  #mod_nrcs  = exe(nrcs)

################################################################################
# -------------------- Step 3: Subset output with process() ------------------ #
################################################################################

  #output = process(model = mod1, sec = c(1200, 6000, 39000, 81900),
  #              plot = TRUE, excel = FALSE)

  #tri_output = process(model = mod_tri, sec = c(1200, 6000, 25200, 52200, 68100),
  #                     plot = TRUE, excel = TRUE)

  #plot(nrcs$rain$rain)

