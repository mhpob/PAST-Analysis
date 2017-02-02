#install.packages("waterData")
#library(waterData)

#CB_discharge <- importDVs("01646580", code = "00060", stat = "00003", sdate = "2014-01-01", edate = "2016-12-31")

# discharge_URL <- tellMeURL("01646580", code=00060, stat = 00003, sdate = 2014-01-01, edate = 2016-12-31)
# discharge_URL
# 'ERROR SENT BY CLIENT WAS SYNTACTICALLY INCORRECT'

library(readxl)
chain_bridge <- read_excel("P:/Wiernicki/Chain Bridge Flow Data.xlsx")

class(chain_bridge$`Date / Time`)

#lubridate::ymd_hms(chain_bridge$`Date / Time`)


# chain_bridge$`Date / Time` <- as.numeric(chain_bridge$`Date / Time`)
# chain_bridge$`Discharge, ft^/s` <- as.numeric(chain_bridge$`Discharge, ft^/s`)
# chain_bridge$`Gage height, ft` <- as.numeric(chain_bridge$`Gage height, ft`)
# chain_bridge$`Temperature, water, degC, 4.1 ft from river bed (middle)` <- as.numeric(chain_bridge$`Temperature, water, degC, 4.1 ft from river bed (middle)`)
# chain_bridge$`Temperature, water, deg C, 1 ft from river bed (bottom)` <- as.numeric(chain_bridge$`Temperature, water, deg C, 1 ft from river bed (bottom)`)
# chain_bridge$`Temperature, water, deg C, 7.1 ft from river bed (top)` <- as.numeric(chain_bridge$`Temperature, water, deg C, 7.1 ft from river bed (top)`)
# chain_bridge$`Specific conductance, wat unf uS/cm @ 25 degC` <- as.numeric(chain_bridge$`Specific conductance, wat unf uS/cm @ 25 degC`)
# chain_bridge$`Nitrate & nitrite water in situ, mg/L as` <- as.numeric(chain_bridge$`Nitrate & nitrite water in situ, mg/L as`)
# chain_bridge$`Turbidity, IR LED light, det ang 90 deg, FNU, From a multiparameter sonde` <- as.numeric(chain_bridge$`Turbidity, IR LED light, det ang 90 deg, FNU, From a multiparameter sonde`)
# chain_bridge$`Dissolved oxygen, mg/L, From multiparameter sonde` <- as.numeric(chain_bridge$`Dissolved oxygen, mg/L, From multiparameter sonde`)
# chain_bridge$`pH, water, unfltrd, field, std units, From multiparameter sonde` <- as.numeric(chain_bridge$`pH, water, unfltrd, field, std units, From multiparameter sonde`)
# chain_bridge$`Temperature, water, degC, From multiparameter sonde` <- as.numeric(chain_bridge$`Temperature, water, degC, From multiparameter sonde`)
# chain_bridge$`Specific conductance, wat unf uS/cm @ 25 deg C, From multiparameter sonde` <- as.numeric(chain_bridge$`Specific conductance, wat unf uS/cm @ 25 deg C, From multiparameter sonde`)

