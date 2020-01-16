# Batttery-failure-diagnostic-model

# The first step for this project is to extract data from the BBOXX SMARTSolar database using SQL language (to make queries)
# Two databases: PostgreSQL and InfluxDB

# Real table and variable names are restricted due to data confidentiality
# 1. PostgreSQL

SELECT [column name: battery test result], [column name: battery product imei], [column name: failure date], [column name: repair id], [column name: battery symptom]
FROM [for example: table of battery test result]
INNER JOIN [for example: table of repair log] ON [matching column names]
WHERE [specify: type of battery] AND [specify: symptom type] AND [specify: battery test result (pass of fail)];

# For example: counting how many pass and fail batteries
SELECT [column name: battery test result] AS “Battery test result”, COUNT(product_imei) AS “Count”
FROM [for example: table of battery test result]
INNER JOIN [for example: table of repair log] ON [matching column names]
WHERE [specify: type of battery] AND [specify: symptom type] AND [specify: battery test result (pass of fail)]
GROUP BY [column name: battery test result]
ORDER BY COUNT(product_imei) DESC;

# Note: more than one tables from BBOXX SMARTSolar database are joined to form a new table for battery failure diagnostic model

# 2. InfluxDB time series database

SELECT mean(voltage) as “voltage” FROM telemetry_rp.telemetry 
WHERE product_imei = [the respective battery IMEI] AND time >= [start date (1 year before failure)] AND time <= [date of failure] GROUP BY time(60m);
