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
- country_level_cost_summary_statistics.csv is then used to generate data_scaled.csv

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

3. Variable List: croped_cost

4. Missing data codes: 
NA

5. Specialized formats or other abbreviations used: 

DATA-SPECIFIC INFORMATION FOR: country_level_cost_summary_statistics.csv

1. Number of variables: 10

2. Number of cases/rows: 30

3. Variable List: 
    ISO3   Length:30          Class :character   Mode  :character                                                     
     mean  Min.   : 3841      1st Qu.: 7064      Median : 8435      Mean   : 8117    3rd Qu.: 9083    Max.   :11166   
    median Min.   : 4355      1st Qu.: 6669      Median : 7833      Mean   : 7785    3rd Qu.: 9016    Max.   :11493   
      sd   Min.   : 983.7     1st Qu.:2866.1     Median :3957.6     Mean   :3519.7   3rd Qu.:4184.6   Max.   :4475.0  
     min   Min.   :2509       1st Qu.:2617       Median :2673       Mean   :2713     3rd Qu.:2772     Max.   :3276    
     max   Min.   : 4836      1st Qu.:14229      Median :17193      Mean   :15528    3rd Qu.:17608    Max.   :18152   
    pct10  Min.   :2770       1st Qu.:3089       Median :3420       Mean   :3605     3rd Qu.:4015     Max.   :5482    
    pct25  Min.   :2850       1st Qu.:4506       Median :4924       Mean   :5185     3rd Qu.:5735     Max.   :8271    
    pct75  Min.   : 4448      1st Qu.: 9205      Median :11662      Mean   :10861    3rd Qu.:12561    Max.   :14262   
    pct09  Min.   : 4681      1st Qu.:11108      Median :14155      Mean   :13052    3rd Qu.:15206    Max.   :16061

4. Missing data codes: 
NA

5. Specialized formats or other abbreviations used: 

DATA-SPECIFIC INFORMATION FOR: country_level_cost_summary_statistics.csv


1. Number of variables: 26

2. Number of cases/rows: 30

3. Variable List: 

     name_govt          Length:30          Class :character   Mode  :character                                                                        
      alpha_3           Length:30          Class :character   Mode  :character                                                                        
      energy_ad         Min.   :0.0000     1st Qu.:0.5000     Median :0.6923     Mean   :0.6552     3rd Qu.:0.8558     Max.   :1.0000     NA's   :16  
     poverty_rate       Min.   :0.0000     1st Qu.:0.1941     Median :0.2846     Mean   :0.3314     3rd Qu.:0.4126     Max.   :1.0000     NA's   :15  
    Exports_percap      Min.   :0.000000   1st Qu.:0.000430   Median :0.001183   Mean   :0.062371   3rd Qu.:0.005284   Max.   :1.000000   NA's   :12  
    Imports_percap      Min.   :0.0000     1st Qu.:0.1276     Median :0.2885     Mean   :0.3395     3rd Qu.:0.4189     Max.   :1.0000     NA's   :9   
    pc_n_tourists       Min.   :0.00000    1st Qu.:0.03879    Median :0.07305    Mean   :0.15832    3rd Qu.:0.22667    Max.   :1.00000                
     reg_strength       Min.   :0.0000     1st Qu.:0.1667     Median :0.3333     Mean   :0.3715     3rd Qu.:0.5000     Max.   :1.0000     NA's   :6   
     wgi_corrupt        Min.   :0.0000     1st Qu.:0.5345     Median :0.6730     Mean   :0.6453     3rd Qu.:0.8953     Max.   :1.0000     NA's   :13  
      wgi_goveff        Min.   :0.0000     1st Qu.:0.5628     Median :0.6760     Mean   :0.6911     3rd Qu.:0.8798     Max.   :1.0000     NA's   :13  
     wgi_polstab        Min.   :0.0000     1st Qu.:0.6418     Median :0.7521     Mean   :0.6893     3rd Qu.:0.8230     Max.   :1.0000     NA's   :13  
     wgi_regqual        Min.   :0.0000     1st Qu.:0.5817     Median :0.7064     Mean   :0.6630     3rd Qu.:0.8606     Max.   :1.0000     NA's   :13  
     wgi_rulelaw        Min.   :0.0000     1st Qu.:0.3996     Median :0.6281     Mean   :0.5836     3rd Qu.:0.7076     Max.   :1.0000     NA's   :13  
     wgi_account        Min.   :0.0000     1st Qu.:0.7041     Median :0.7823     Mean   :0.7014     3rd Qu.:0.8427     Max.   :1.0000     NA's   :16  
       wgi_mean         Min.   :0.0000     1st Qu.:0.6524     Median :0.7356     Mean   :0.6775     3rd Qu.:0.8400     Max.   :1.0000     NA's   :13  
      score_govt        Min.   :0.0000     1st Qu.:0.1667     Median :0.3333     Mean   :0.3715     3rd Qu.:0.5000     Max.   :1.0000     NA's   :6   
      score_wgi         Min.   :0.0000     1st Qu.:0.6524     Median :0.7356     Mean   :0.6775     3rd Qu.:0.8400     Max.   :1.0000     NA's   :13  
      score_need        Min.   :0.0000     1st Qu.:0.1559     Median :0.2549     Mean   :0.3330     3rd Qu.:0.4384     Max.   :1.0000     NA's   :17  
    score_marketability Min.   :0.0000     1st Qu.:0.1083     Median :0.2619     Mean   :0.3516     3rd Qu.:0.5078     Max.   :1.0000     NA's   :12  
        n_fads          Min.   :   0.0     1st Qu.:   1.5     Median :   8.0     Mean   : 152.2     3rd Qu.:  27.0     Max.   :2500.0     NA's   :7   
      n_private         Min.   :  0        1st Qu.:  0        Median :  0        Mean   : 43        3rd Qu.: 20        Max.   :600        NA's   :9   
       n_public         Min.   : 0.000     1st Qu.: 0.000     Median : 2.000     Mean   : 3.762     3rd Qu.: 5.000     Max.   :24.000     NA's   :9   
     vessels_fad        Min.   :  0.00     1st Qu.:  3.75     Median : 26.00     Mean   : 85.72     3rd Qu.:128.50     Max.   :450.00     NA's   :12  
     vessels_tot        Min.   :   27      1st Qu.:  120      Median :  332      Mean   : 1347      3rd Qu.:  650      Max.   :10700      NA's   :13  
    fads_per_totvessel  Min.   :0.00000    1st Qu.:0.03333    Median :0.10000    Mean   :0.34167    3rd Qu.:0.13750    Max.   :3.03030    NA's   :13  
    fads_per_fadvessel  Min.   :0.006329   1st Qu.:0.145455   Median :0.200000   Mean   :0.750582   3rd Qu.:0.222222   Max.   :4.545455   NA's   :17  

4. Missing data codes: 
NA

5. Specialized formats or other abbreviations used: 
