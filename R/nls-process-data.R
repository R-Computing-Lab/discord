# Parsing script lightly modified from NLS Investigator
# Load here package for relative file paths
library(here)

new_data <- read.table(here('data/flu_shot.dat'), sep=' ')
names(new_data) <- c('R0000100',
                     'R0000149',
                     'R0173600',
                     'R0214700',
                     'R0214800',
                     'T2055700',
                     'T2056000',
                     'T3026100',
                     'T3026400',
                     'T3956900',
                     'T3957200',
                     'T4894600',
                     'T4894900',
                     'T5597700',
                     'T5598000')


# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0173600 <- factor(data$R0173600,
                          levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0),
                          labels=c("CROSS MALE WHITE",
                                   "CROSS MALE WH. POOR",
                                   "CROSS MALE BLACK",
                                   "CROSS MALE HISPANIC",
                                   "CROSS FEMALE WHITE",
                                   "CROSS FEMALE WH POOR",
                                   "CROSS FEMALE BLACK",
                                   "CROSS FEMALE HISPANIC",
                                   "SUP MALE WH POOR",
                                   "SUP MALE BLACK",
                                   "SUP MALE HISPANIC",
                                   "SUP FEM WH POOR",
                                   "SUP FEMALE BLACK",
                                   "SUP FEMALE HISPANIC",
                                   "MIL MALE WHITE",
                                   "MIL MALE BLACK",
                                   "MIL MALE HISPANIC",
                                   "MIL FEMALE WHITE",
                                   "MIL FEMALE BLACK",
                                   "MIL FEMALE HISPANIC"))
  data$R0214700 <- factor(data$R0214700,
                          levels=c(1.0,2.0,3.0),
                          labels=c("HISPANIC",
                                   "BLACK",
                                   "NON-BLACK, NON-HISPANIC"))
  data$R0214800 <- factor(data$R0214800,
                          levels=c(1.0,2.0),
                          labels=c("MALE",
                                   "FEMALE"))
  return(data)
}


varlabels <- c("ID# (1-12686) 79",
               "HH ID # 79",
               "SAMPLE ID  79 INT",
               "RACL/ETHNIC COHORT /SCRNR 79",
               "SEX OF R 79",
               "MED TEST IN PAST 24 MO - FLU SHOT 2008",
               "MED TEST IN PAST 24 MO - FLU SHOT 2008",
               "MED TEST IN PAST 24 MO - FLU SHOT 2010",
               "MED TEST IN PAST 24 MO - FLU SHOT 2010",
               "MED TEST IN PAST 24 MO - FLU SHOT 2012",
               "MED TEST IN PAST 24 MO - FLU SHOT 2012",
               "MED TEST IN PAST 24 MO - FLU SHOT 2014",
               "MED TEST IN PAST 24 MO - FLU SHOT 2014",
               "MED TEST IN PAST 24 MO - FLU SHOT 2016",
               "MED TEST IN PAST 24 MO - FLU SHOT 2016"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("CASEID",
                   "HHID",
                   "SAMPLE",
                   "RACE",
                   "SEX",
                   "FLU_M_2008",
                   "FLU_F_2008",
                   "FLU_M_2010",
                   "FLU_F_2010",
                   "FLU_M_2012",
                   "FLU_F_2012",
                   "FLU_M_2014",
                   "FLU_F_2014",
                   "FLU_M_2016",
                   "FLU_F_2016")
  return(data)
}


categories <- qnames(vallabels(new_data))

remove(new_data)
