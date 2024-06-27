# PondTemps
Data analysis for pond water temperature forecast model using air temperatures

# Question being addressed
What is the probability that ponds exceed the thermal threshold (CTmax) for salmon within the century?

# Hypothesis


# Initial Goals:
  1. Create a file with all the pond/air temperature for the forecasts of each pond.
     Include a column for date, time, each pond's water temp, each air temp logger with each pond, and the airport temperatures (min/max/mean)
    
  2. Create mean air temp data for each airport logger (max-min)/2
     Get ERA5 monthly data and validate using the min/max data for the airports
     
  3. Use state-space model for the forecast
     Benefits: 1. Gives limits and bounds to the data forecast (temp at step t+1 cannot be more than # degrees away from temp at t)
               2. Will give error that grows through time, where dates farther out have a wider confidence interval than closer to now
  
  4. Models with SNAP data initially (presented at SFS 2024 in Philly)
  
  5. Models with CORDEX data which is more robust


# Steps Covered here
  1. Uploading all the data to GitHub
     Adding in all the raw data files in the PondTemps folder and organizing these
  2. Data file creation (monthly means for the temperature data)
     Getting the data entered into one file
     Getting the ERA5 data incorporated (may wait on this and just use airport temps for now)
  3. Running the State-Space model
     Use code based on what Alyssa sent along for the examples
  4. These were all run with SNAP data for SFS 2024 in Philly
  5. Since have downloaded CORDEX data which is better defined
     Goal is to rerun the SNAP analysis but with the CORDEX Dataa

# Code files here -- presented in the same order as the file folder
  1. 'ComparingAirportERA5Temps.Rmd' 
      Compares observed airport temperatures to ERA five reanalysis temperatures
      Poor match so ignored the ERA5 data and went with the airport temperature data that was empirically measured
      
  2. 'CordexDataSummaries.Rmd' 
      Takes the raster data files from CORDEX download on capurnicus and filters out the data for CRD and YF from RCP 8.5 and RCP 4.5
      Outputs these are .csv files that are used in the models for the CORDEX forecasts
  
  3. 'CreatingCollatedDataFile.Rmd'
      Takes all of the files of data for air and water temperature, measured and forecasted (SNAP here) and puts them into .csv files that can be uploaded into the code that is available for the forecasts for each region and RCP
      Outputs these as .csv files that are used in subsequent modeling code
      
  4. 'IndividualPondModels_CR.Rmd'
      This code is unneeded but kept for now. Will likely delete in the future
      
  5. 'IndividualPondModels_Forecasts45_CR.Rmd'
      This code does the forecasting models for CRD RCP 4.5 using an output file from the Creating Collated Data Rmd file.
      
  6. 'IndividualPondModels_Forecasts45_YF.Rmd'
      This code does the forecasting models for YF RCP 4.5 using an output file from the Creating Collated Data Rmd file.
  
  7. 'IndividualPondModels_Forecasts85_CR.Rmd'
      This code does the forecasting models for CRD RCP 8.5 using an output file from the Creating Collated Data Rmd file.
      
  8. 'IndividualPondModels_Forecasts85_YF.Rmd'
      This code does the forecasting models for YF RCP 8.5 using an output file from the Creating Collated Data Rmd file.
      
  9. 'IndividualPondModels_Observed_YF.Rmd'
      This code is unneeded but kept for now. Will likely delete in the future
  
  10. 'LinearLoopingAcrossPonds_StateSpaceModel.Rmd'
      This code is unneeded but kept for now. Will likely delete in the future
    
  11. 'RegAirWaterTemp_TableValues.Rmd'
      This code gets the linear relationships between teh air and water temperatures that are presented in the tables in the pond temp manuscript draft that was written in June 2024
      
  12. 'SNAP_RCP_DataExtractionOrganization.Rmd'
      This code pulls apart the forecasting data from the SNAP files that are used in the SFS version of this data analysis
      
  13. 'StateSpacePondTempModel - CR.Rmd'
      This code is unneeded but kept for now. Will likely delete in the future
      
  14. 'StateSpacePondTempModel - YF.Rmd'
      This code is unneeded but kept for now. Will likely delete in the future   
