# DDL Research Specialist Technical Assignment

**Author:** Margaret Christians 

**Date:** October 18, 2026 

---

## Code
All data processing, merging, data cleaning, and figure generation code was done in Python:  

**[Assignment A Notebook](./Assignment_A.ipynb)**

**[Assignment B Notebook](./Assignment_B.ipynb)**


### Instructions to Run
1. Clone this repository

2. Install required dependencies (in `requirements.txt`)
    
3. Open and run `Assignment_A.ipynb` and/or `Assignment_B.ipynb`

---

## Presentation
The presentation on Assignment B covering spatial visualization, feature selection, data quality, and maintenance is available here:

**[Assignment B Presentation Slides](./DDL-Presentation_Margaret-Christians.pdf)**

---

## Reflections

### Assignment A

Data Sourcing: Downloaded NetZeroTracker data and Forbes2000 data from provided links into data folder.

Forbes and NetZero Overlap: According to my code, there are 880 companies in the Forbes 2000 not included in NetZero. This, however, is only an upper limit because of inconsistencies in the two datasets' formatting of company names.

Matching Errors: Just looking through each DataFrame row by row, I observed many companies that are in both datasets but have differently formatted names. For example, "Walt Disney" in Forbes is formatted as "Walt Disney Co" in NetZero, or "JPMorganChase" in Forbes is formatted as "JP Morgan Chase" in NetZero, and so on. Because there isn't a universal ID code to identify companies across datasets, company name is still our best key column. But as researchers, we have to identify these potential formatting issues to prevent over/underestimations in our analyses. 

### Assignment B

Data Sourcing: The Assignment B notebook downloads the ClimActor 2.0 dataset ZIP from [UNC Dataverse](https://dataverse.unc.edu/dataset.xhtml?persistentId=doi:10.15139/S3/GGWVVQ), extracts it, and loads the files.

Merging and Spatial Visualization: Generated geographic coordinates using the key dictionary's `long_lat` column, then merged chosen CSVs using ClimActor ID to create geospatial visualizations of the most important features.

Data Quality: There were minimal issues with data quality. See presentation for more details and suggested updates.

## AI use

I did not use AI to generate code for Assignment A -- I only used my personal knowledge and notes.

For Assignment B, I labeled all sections of my code where I used AI. I used GitHub Copilot to start, but quickly switched to an external AI (Gemini), which was technically slower because it didn't have access to my entire notebook and I had to adapt its code to my variables and preferences. However, this forced me to gain a much better understanding of ClimActor, as well as geospatial plotting.

All workflow and visualization decisions were made by me.

I made my presentation from scratch, writing all information and constructing all graphical elements, color palettes, etc. myself.

This README was also written entirely by me.