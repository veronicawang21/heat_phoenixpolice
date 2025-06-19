# heat_phoenixpolice
Association between ambient heat exposure and the number of dispatched calls for police service in Phoenix, AZ

## data files
Phoenix_PopWieghtPRISM: folder contains temperature files
\
  SSC_PhoenixPRISMDAILY_LONGtmax: Daily populated-weighted maximum temperature exposure averaged across Phoenix city boundaries
  \
  SSC_PhoenixPRISMDAILY_LONGtmin: Daily populated-weighted minimum temperature exposure averaged across Phoenix city boundaries
  \
  SSC_PhoenixPRISMDAILY_LONGvpdmax: Daily maximum vapor pressure deficit averaged across Phoenix city boundaries
  \
  SSC_PhoenixPRISMDAILY_LONGpvdmin: Daily minimum vapor pressure deficit averaged across Phoenix city boundaries
  \

phoenix_grouping.csv: categorization of incident call types (also shown in Supplemental Materials Table S1)
\

phoenix_police_og (_upload1 through _upload 6): individual dispatched calls for police service in Phoenix split into 6 files due to large file size
\

## analyses code
1_prep.R: Clean and merge all exposure and outcome files and calculate heat index
\
2_0_analysis_calls.R: Association between temperature/heat index and the overall number of disptached police calls
\
2_1_analysis_warm.R: Association between temperature/heat index and the overall number of dispatched police calls among warm months
\
2_1_analysis_cold.R: Association between temperature/heat index and the overall number of dispatched police calls among cool months
\
3_0_analysis_type.R: Association between temperature/heat index and the number of dispatched police calls by incident type
\
3_1_analysis_type.R: Association between temperature/heat index and the number of dispatched mental health incident-type police calls only
\
4_0_compile_results.R: Create figures
