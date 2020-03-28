source("scripts/install_needed_packages.R")     # installing needed packages
source("scripts/load_needed_packages.R")        # loading needed packages
source("scripts/load_needed_functions.R")       # loading all needed functions
source("scripts/create_plan.R")                 # creating the plan

plan                                            # displaying the plan
make(plan)                                      # making the plan
vis_drake_graph(plan)                           # visualizing which parts of the plan are not up to date
readd(raw_data)                                 # accessing targets of the plan
