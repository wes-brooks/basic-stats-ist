## ----load-data------------------------------------------------------------------------------------------
# load the mice_pot and barnacles datasets
mice_pot = read.csv( url( "https://raw.githubusercontent.com/wes-brooks/basic-stats/main/data/mice_pot.csv"))
barnacles = read.csv( url( "https://raw.githubusercontent.com/wes-brooks/basic-stats/main/data/barnacles.csv"))


## ----mice-descriptive-----------------------------------------------------------------------------------
with( mice_pot, by(percent_of_act, group, mean))

## ----barnacles-descriptive------------------------------------------------------------------------------
mean( barnacles$per_m )
