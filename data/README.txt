This README.txt file was generated on 2022-03-23 by Juan Carlos Villaseñor-Derbez


GENERAL INFORMATION

1. Title of Dataset: Data for: Evaluating conditions for MFAD fisheries development in the Caribbean and Bermuda

2. Author Information
	A. Principal Investigator Contact Information
		Name: Margaret Wilson
		Institution: University of California Santa Barbara
		Email: mwattswilson@gmail.com

	B. Alternate Contact Information
		Name: Juan Carlos Villaseñor-Derbez
		Institution: University of California Santa Barbara
		Email: juancarlos@ucsb.edu

3. Date of data collection (single date, range, approximate date) <suggested format YYYY-MM-DD>: 

4. Geographic location of data collection <latitude, longitute, or city/region, State, Country, as appropriate>: 

5. Information about funding sources that supported the collection of the data: 


SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: NA

2. Links to publications that cite or use the data: https://doi.org/10.3389/fmars.2022.827068

3. Links to other publicly accessible locations of the data: NA

4. Links/relationships to ancillary data sets: NA

5. Was data derived from another source? yes/no
	A. If yes, list source(s): https://gmed.auckland.ac.nz/index.html

6. Recommended citation for this dataset: 


DATA & FILE OVERVIEW

1. File List: 
croped_cost.tif	- Raster file with total costs of FADs (deployment + travel)
country_level_cost_summary_statistics.csv - Spatial summary statistics for the raster file
data_scaled.csv - Scaled socioeconomic indicators used for figure 3


2. Relationship between files, if important: 
- cropped_coast.tif is spatially intersected with country exclusive economic zones to produce zonal stats stored in country_level_cost_summary_statistics.csv

3. Additional related data collected that was not included in the current data package: 

4. Are there multiple versions of the dataset? no


METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: 
Data on ocean bottom depth and current speed were downloaded from: https://gmed.auckland.ac.nz/index.html

2. Methods for processing the data: 
See Methods section in: https://doi.org/10.3389/fmars.2022.827068

3. Instrument- or software-specific information needed to interpret the data: NA

4. Standards and calibration information, if appropriate: NA

5. Environmental/experimental conditions: NA

6. Describe any quality-assurance procedures performed on the data: NA

7. People involved with sample collection, processing, analysis and/or submission: NA


DATA-SPECIFIC INFORMATION FOR: cropped_coast.tif

1. Number of variables: 1

2. Number of cases/rows: dimensions :
        dimensions : 374, 422, 157828  (nrow, ncol, ncell)
        resolution : 0.083, 0.083  (x, y)
        extent     : -90.02271, -54.99671, 4.969117, 36.01112  (xmin, xmax, ymin, ymax)
        crs        : +proj=longlat +datum=WGS84 +no_defs 
        source     : croped_cost.tif 
        names      : croped_cost 
        values     : 2509.035, 18257.31  (min, max)

3. Variable List: croped_cost (Cost of deploying and opperating a Fish Aggregating Device, in US dollars)

4. Missing data codes: 
NA

5. Specialized formats or other abbreviations used: 

DATA-SPECIFIC INFORMATION FOR: country_level_cost_summary_statistics.csv

1. Number of variables: 10

2. Number of cases/rows: 30

3. Variable List: 
      - ISO3 : Standard 3-letter code for each country
      - mean : Mean cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD)
      - median : Median cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD)
      - sd : Standard Deviation cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD)
      - min : Minimum cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD)
      - max : Maximum cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD)
      - pct10 : 10th percentile of cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD)
      - pct25 : 25th percentile of cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD) 
      - pct75 : 75th percentile of cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD)
      - pct09 : 90th percentile of cost of deploying and opperating a Fish Aggregating Device within a country's Exlcusive Economic Zone (in USD)

4. Missing data codes: 
Missing values are represented with 'NA'

5. Specialized formats or other abbreviations used: 

DATA-SPECIFIC INFORMATION FOR: country_level_cost_summary_statistics.csv


1. Number of variables: 26

2. Number of cases/rows: 30

3. Variable List: 
    - name_govt : Country name
    - alpha_3 : Standard 3-letter code for each country
    - energy_ad : Energy adequacy
    - poverty_rate : Poverty rate
    - Exports_percap : Per capita seafood exports
    - Imports_percap : Per capita seafood imports
    - pc_n_tourists : Per capita annual number of tourists
    - reg_strength : Mean regulatory strength for FAD deployment, FAD access, and FAD fishing practices generated from survey responses
    - wgi_corrupt : World Governance Index for corruption (unitless)
    - wgi_goveff : World Governance Index for government effectiveness (unitless)
    - wgi_polstab : World Governance Index for political stability (unitless)
    - wgi_regqual : World Governance Index for regulatory quality (unitless)
    - wgi_rulelaw : World Governance Index for rule of law (unitless)
    - wgi_account : World Governance Index for accountability (unitless)
    - wgi_mean : Mean of all six World Governance Index scores
    - score_govt : Regulatory strength
    - score_wgi : Mean of all six World Governance Index scores
    - score_need : Mean of energy adequacy and poverty rate
    - score_marketability : Mean of per capita imports, exports, and annual tourists
    - n_fads : Number of FADs
    - n_private : Number of FADs reported as private
    - n_public : Number of FADs reported as public
    - vessels_fad : Number of fishing vessels utilizing FADs
    - vessels_tot : Total number of fishing vessels
    - fads_per_totvessel : Number of FADs per vessel
    - fads_per_fadvessel : Number of FADs per FAD-fishing vessel

4. Missing data codes: 
Missing values are represented with 'NA'

5. Specialized formats or other abbreviations used: 
