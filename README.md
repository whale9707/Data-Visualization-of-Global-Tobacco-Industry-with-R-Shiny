# [Tobacco Control Research](https://annruilu.shinyapps.io/5063_final_project_Group_C/)

**GR5063 Data Visualization Project Proposal- Group C**

Please kindly check -
- link for our product: https://annruilu.shinyapps.io/5063_final_project_Group_C/
- product introduction video: https://drive.google.com/file/d/1mHhXCAQbSINvUbjIzhX40_EbBBk7nhEd/view?usp=drivesdk

## Team Members:

Hanzhang Hu: hh2921@columbia.edu

Dan Li: dl3466@columbia.edu

Pengyun Li: pl2799@columbia.edu

Rui Lu: rl3191@columbia.edu

## Abstract: 

Tobacco consumption has been one of the biggest public health threats in the world. Recent studies conducted by the World Health Organization (WHO) and the Institute for Health Metrics and Evaluation have shown that around 7.2 million people die prematurely every year from smoking. However, despite the severe harms of tobacco usage, it is impractical to simply ban tobacco production as tobacco generates significant economic benefits such as agricultural employment, tax revenue, foreign exchange earnings, etc. Therefore, policymakers in different countries need to find a balance point between the harms of tobacco consumption on public health and the economic benefits generated from the tobacco industry.

Our team aims to utilize various data visualization techniques on tobacco industry and policy datasets to see the effectiveness and public sentiment of global tobacco control policies from 2008 to 2018.

## Techniques: 

We used R shiny ggplot2, plotly, maps, text analysis (sentiment analysis, word clouds and network analysis) for data visualization.

## Product Overview:

The whole structure of our final product falls into six parts.

- **Project overview**: the main objective of the dashboard and an introduction to our team
- **Tobacco Background**: the background information about the tobacco industry from the perspective of consumption and threat to health
- **Global Tobacco Control Policy**:  detailed introduction to the tobacco control policies all over the world
- **Cost Benefit Analysis**: we developed an algorithm to do the cost benefit analysis for the tobacco industry
- **Text Exploration**:  texts from both New York Times and Reddit to see people’s discussion around this topic
- **Appendix**: the data source for our product

## Data Description:

Here are the datasets we are going to use, and we have already cleaned the datasets for further analysis. Sources of original datasets are shown below.

1. tobacco_production

Tobacco production by country and year
1,207 rows & 5 columns. Size: <1MB. 

Source: http://data.un.org/Data.aspx?q=tobacco&d=ICS&f=cmID%3a25090-0


2. tobacco_use_us

Tobacco usage in the US
111,048 rows & 21 columns. Size: ~30MB. 

Source: https://chronicdata.cdc.gov/


3. tobacco_use_ww

Worldwide tobacco usage
4,023 rows & 7 columns. Size: <1MB. 

Source: https://www.who.int/


4. sales_per_day

Average number of cigarettes sold per day on each year
2,767 rows & 4 columns. Size: <1MB. 

Source: http://www.pnlee.co.uk/ISS.htm


5. death_rates_smoking_age

Early deaths due to smoking per 100,000 individuals 
6,468 rows & 9 columns. Size: <1MB. 

Source: http://ghdx.healthdata.org/gbd-results-tool


6. stop_smoking

Indicators that contribute to an individual to stop smoking
774 rows & 7 columns. Size: <1MB. 

Source: https://apps.who.int/gho/data/node.home


7. us_chronic_resp_disease

Estimates for age-standardized mortality rates by county from chronic respiratory diseases
~3.4 million rows & 8 columns. Size: ~350MB. 

Source: https://www.healthdata.org/
