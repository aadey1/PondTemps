---
editor_options: 
  markdown: 
    wrap: 72
---

# Pond Temperature Analysis

Data analysis for pond water temperature forecast model using air
temperatures

Multiple Attempts Made -- Finalized Version is "BiassCorrection" folder
here, but other versions are in the "PastMethods folder here

These include 1. a random walk method based on EFI notes 2. an
individual model method 3. a pooled model method 4. a method using geo
data with the 5 model ensemble (no bias correction) with individual
ponds modeled 5. a method using geo data with the 5 model ensemble (no
bias correction) with pooled ponds modeled

# Manuscript Draft Title

Thermal sensitivity of Alaskan coastal wetlands to climate change

# Question being addressed

How does pond water temperature respond to increasing air temperatures
under future climate scenarios from the IPCC?

A. How do the two regions (YF and CRD) respond to RCP 4.5 and 8.5? Are
there differences in warming patterns across seasons? B. What is the
probability of exceeding the 20 C threshold for salmon growth (20 C)? C.
Not asked but could ask what is the probability of exceeding the thermal
limit (23-25 C)?

# Hypothesis

1.  We hypothesized that coastal wetland ponds would display a strong
    linear relationship to air temperature, allowing the use of simple
    statistical models to simulate the effects of climate change on
    water temperature.

2.  We further hypothesized that ponds would frequently meet or exceed
    the upper thermal threshold or growth constraint of salmon within
    this century based on predicted air temperature changes.

# Goals:

1.  Create a file with all the pond/air temperature for the forecasts of
    each pond. Include a column for date, time, each pond's water temp,
    each air temp logger with each pond, and the airport temperatures
    (min/max/mean)

2.  Create mean air temp data for each airport logger (max-min)/2

3.  Use state-space model for the forecast Benefits: 1. Gives limits and
    bounds to the data forecast (temp at step t+1 cannot be more than \#
    degrees away from temp at t) 2. Will give error that grows through
    time, where dates farther out have a wider confidence interval than
    closer to now

4.  Models with SNAP data initially (presented at SFS 2024 in Philly)

5.  Needed to conduct a bias correction because the winter months were
    far too cold Considered two options A. difference method B. z-score
    method Went with the Z-score method.

# Steps Covered here

1.  Download the SNAP data

-   SNAP website:
-   Use the code file "forecasted_temperature.R" to organize the 5-model
    ensemble and put the data together
-   the plots for the forecast of the air temperature un-biased is in
    here

2.  Bias correction of the SNAP data and data collation

-   Here we chose to do a monthly bias correction because the difference
    between SNAP and the airport air temperatures was greater in the
    colder months than in the warmer months
-   Use the file "SNAP_Extraction_BiasCorrection_MonthlyCorrection.Rmd"
    which will use the SNAP data output of step 1 and the airport data
-   Use the file "SNAP_RCP_DataOrganization" to organize the files
    together after the bias correction is conducted

3.  Pooled model analysis

-   We pooled the models for the analysis here (in pervious versions we
    did individual model assessmenets)

-   Use the file "PooledPondModel.Rmd" to run the pooled model. This
    file includes the code to run the four models (factorial of two
    regions and two scenarios)

4.  Delta Calculations for seasons

-   To compare the seasonal changes across decades before the end of the
    century (2030, 2060, 2090) to the observed temperatures

-   Use the files "Corr_Delta_Seasonality\_[ ].R" in the [ ] is the
    different region (crd or yf) or scenario (45 or 85). These will give
    the seasonal differences by month for each combination

5.  Summary of Model Projections

-   We created summary figures here using the file
    "SummaryModelProjectsH20Temp.Rmd" file

-   This will create the summary water temperature figures that are in
    the manuscript draft
