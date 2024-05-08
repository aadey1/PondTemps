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


# Files
  1. Data file creation (monthly means for the temperature data)
     Getting the data entered into one file
     Getting the ERA5 data incorporated (may wait on this and just use airport temps for now)
  2. Running the State-Space model
     Use code based on what Alyssa sent along
