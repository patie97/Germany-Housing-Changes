# Germany-Housing-Changes

Replication scripts for the thesis "Reshaping Residential Preferences in the Wake of COVID-19: An Examination of Living Space Demand in Germany's Urban Centers and Periphery"
For Replication, the RWI-GEO-RED dataset has to be requested by the RWI is for research-purposes only. It is not allowed to publish them without a license.

Replication steps:


House sales
1. run 1a-hp-sale to create dataset and remove irrelevant variables and preparation for analysis. 
2. run 2a-hp-sale to combine additional data on Work from Home and socioeconomic data.
3. run 3a-hp-sale to create the hedonic price index for houses for sale
4. run 4a-hp-listings to create plots for Days on the Market and active listings

Apartment rents
1.run 1b-ap-rent to create dataset and remove irrelevant variables and preparation for analysis. 
2. run 2b-ap-rent-sale to create the hedonic price index for houses for sale
3. run 4b-ap-listings to create plots for Days on the Market and active listings

Coordinates
1. run Python script coordination-calculation.ipynb to calculate distances of objects for either apartment rents or house sales

Overall analysis
1. run 5a-data-descrip.R to create descriptive statistics
2. run 5b-price-analysis.R to create plots from chapter 5
4. run 5c-distance-analysis.R to create regression results and bid-rent curve from equation in chapter 5
5. run 5d-migration-wfh.R to recreate the plots on WFH and migration
6. run 5e-bid-rent.R to create first bid-rent curve
