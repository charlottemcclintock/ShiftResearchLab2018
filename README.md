
## the HOME tool
##### Housing by Occupational Median Earnings

The aim of this project is to develop a tool where users can select an industry or occupation and explore Denver neighborhoods by housing affordability, school quality, and commute time to quickly and more fully inform users on how occupational choice affects quality of life variables. A user selects her industry and occupation, and based on the annual median income for that occupation in the Denver area, she is shown an interactive graph of Denver neighborhoods.

## Data & Sources
* Median Occupational Wages *from the Bureau of Labor Statistics*
    + [2017 BLS Wage Data](https://www.bls.gov/oes/current/oes_nat.htm), data filtered to "Denver-Aurora-Lakewood, CO" area. 
* School Quality *from the Colorado Department of Education*
    + [2017 CMAS English Language Arts and Math Proficiency](https://www.cde.state.co.us/assessment)
    + [2014-2017 High School Graduation Rates](https://www.cde.state.co.us/cdereval/gradratecurrent)
* Average Commute Time *from the American Community Survey*
    + [ACS 2016 (5-Year Estimates)](https://www.socialexplorer.com/tables/ACS2016_5yr/R11709778) from Social Explorer, for the seven county Denver metro area, with tracts matched to neighborhoods. 
* Housing Affordability Data *from each of the seven counties in the Denver area*
    + [Adams County](http://www.adcogov.org/gisdata)
    + [Arapahoe County](http://www.co.arapahoe.co.us/1151/GIS-Data-Download)
    + [Boulder County](https://www.bouldercounty.org/property-and-land/assessor/data-download/)
    + [Broomfield County](http://opendata.broomfield.org/datasets/parcels)
    + [Denver County](https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-parcels)
    + [Douglas County](https://gis-dougco.opendata.arcgis.com/datasets/bdbf1ab37f5a43a694c341a6cb355ae5_1)
    + [Jefferson County](http://data-jeffersoncounty.opendata.arcgis.com/datasets/parcel)

## Methodology

#### Occupational Wages

Median annual occupation wage data comes from the Bureau of Labor Statistics, from the 2017 calendar year, narrowed to the "Denver-Aurora-Lakewood, CO" statistical area. A user can select either industry alone for a broad perspective, or industry and occupation for more narrowly tailored results.

Cleaning script can be found in `bls.clean.R`. 


#### School Quality

School quality is determined by CMAS proficiency in reading and math for the three closest elementary, middle, and high schools. CMAS proficiency data was retrieved from the Colorado Department of Education data portal. Schools were matched to neighborhoods by geographic location through a geospatial join in QGIS.  

CMAS data was aggregated by school level (elementary, middle, high) for each neighborhood, then by neighborhood as a whole to create an average proficiency score for each school.


Cleaning script can be found in `ed.clean.R`. 

#### Average Commute Time

Average commute time is based on 5 year American Community Survey tract level data for average reported commute time. Tracts were then matched to neighborhoods with a matching table. Neighborhood averages were computed as the average of all matched tracts (the difference between average and median was frequently 0, always less than 2 minutes).

Cleaning script can be found in `acs.clean.R`. 

#### Housing Affordability

Parcel data was retrieved from the Adams, Arapahoe, Broomfield, Boulder, Denver, Douglas, and Jefferson County assessor's offices. The tool currently only uses parcel data for single family homes, but will be expanded soon. 

Cleaning script can be found in `housing.clean.R`. 

## Scripts

* Cleaning
    + School Quality: `ed.clean.R`
    + Housing Affordability: `housing.clean.R`
    + Average Commute Time: `acs.clean.R`
    + Median Occupation Wages: `bls.clean.R`
    
* Shiny: in `app`
    + App Script: `app.R`