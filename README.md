# ShiftResearchLab2018
Working to transform data into actionable information that improves the social sector's ability to serve Colorado's low-income communities. 

## About This Project
##### Housing Affordability and Access to Quality Education by Occupation
The aim of this project is to develop a tool where a user selects her occupation from a list, and is shown a graph of neighborhoods in the Denver metropolitan area by percent of houses in a given neighborhood that are afforable given the median occupational wages for a selected occupation, and reading and math proficiency and high school graduation rates for schools in that neighborhood. The points would then also be scaled by average commute time.

## Data & Sources
* Median Occupational Wages *from the Bureau of Labor Statistics*
    + [2017 BLS Wage Data](https://www.bls.gov/oes/current/oes_nat.htm), data filtered to "Denver-Aurora-Lakewood, CO" area. 
* School Quality *from the Colorado Department of Education*
    + [2017 CMAS English Language Arts and Math Proficiency](https://www.cde.state.co.us/assessment)
    + [2014-2017 High School Graduation Rates](https://www.cde.state.co.us/cdereval/gradratecurrent)
* Average Commute Time *from the American Community Survey*
    + [ACS 2016 (5-Year Estimates)](https://www.socialexplorer.com/tables/ACS2016_5yr/R11709778) from Social Explorer, for the seven county Denver metro area, with tracts matched to neighborhoods. 
* Housing Affordability Data *from each of the seven counties in the Denver area*
    + Adams County
    + Arapahoe County
    + Boulder County
    + Broomfield County
    + Denver County
    + Douglas County
    + Jefferson County

## Methodology

#### Occupational Wages

Occupational wage data comes from the Bureau of Labor Statistics, from the 2017 calendar year. The data was filtered to the "Denver-Aurora-Lakewood, CO" area. The tool will utilize the annual median wages data, and the user will be able to select first their industry, and then their occupation, based on BLS occupation and industry classifications.

Cleaning script can be found in `bls.clean.R`. 


#### School Quality

School quality is determined by CMAS proficiency in reading and math and high school graduation rates. Both data sets were retrieved from the Colorado Department of Education data portal. Schools were matched to neighborhoods by geographic location through a geospatial join in QGIS.  

CMAS data was aggregated by grade, then by subject area to create an average proficiency score for each school. School level data was then aggregated by neighborhood to create an average proficiency score for each neighborhood. 3 neighborhoods had high school graduation rates, but no CMAS scores, and 119 neighborhoods had CMAS scores but no high school graduation rates. The measures of central tendency and distributions for the CMAS data and the graduation rate date were different, so rather than take the average, the both data sets were given percentile scores and then an average was taken where both values existed which became the school quality measure. Where only one value existed, that become the school quality measure. 

This is thus a relative metric, with each neighborhoods schools given a score relative to all other neighborhoods in the Denver metro area. 

Qs: Should this be absolute? Is there a better way to combine the data? 

Cleaning script can be found in `ed.clean.R`. 

#### Average Commute Time

Average commute time is based on 5 year American Community Survey tract level data for average reported commute time. Tracts were then matched to neighborhoods with a matching table. Neighborhood averages were computed as the average of all matched tracts (the difference between average and median was frequently 0, always less than 2 minutes).

Cleaning script can be found in `acs.clean.R`. 

#### Housing Affordability

Parcel data was retrieved from the Adams, Arapahoe, Broomfield, Boulder, Denver, Douglas, and Jefferson County assessor's offices. 

Cleaning script can be found in `housing.clean.R`. 

## Scripts

* Cleaning
    + School Quality: `ed.clean.R`
    + Housing Affordability: `housing.clean.R`
    + Average Commute Time: `acs.clean.R`
    + Median Occupation Wages: `bls.clean.R`
    
* Shiny: in `app`
    + App Script: `app.R`
