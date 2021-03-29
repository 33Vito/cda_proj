# Computational Data Analytics Project

## Project proposal

Gravitational homophily clustering to curb the outbreak of Covid-19 

### Problem Statement

Since the outbreak of Covid-19, governments around the world have relied on two key measures to curb local transmission of the disease are 1) contact tracing; 2) extensive testing. These two measures work well if a) the outbreak is discovered earlier; b) the number of confirmed cases is limited, and c) the presence of asymptomatic carriers is rare. However, in many parts of the world, it is extremely unlikely for all these three conditions to be met simultaneously. In most cases, local transmission cannot be contained without extreme measures such as extended periods of lockdown – which result in a heavy toll on the economic performance. Considering this, effective methods **to select only a small subset of the geography or population to enter lockdown or perform group testing** can be immensely helpful for the government to strike a balance between containing the virus and maintaining the economic output. 

In this project, we will explore the merit of using geographic clustering based on homophily and gravitational analysis to identify a small collection of the local areas for the authority to impose stronger measure such as lockdown and group testing to effectively curb the local outbreak of an epidemic disease such as Covid-19, while minimising the impact on economic activities. 

## Data Source (candidates)
Daily confirmed cases of Covid-19 by geography:
•	Our World in Data - https://ourworldindata.org/coronavirus, https://github.com/owid/covid-19-data/tree/master/public/data 
•	COVID-19 data in NSW - https://data.nsw.gov.au/data/dataset/nsw-covid-19-cases-by-location-and-likely-source-of-infection/resource/2776dbb8-f807-4fb2-b1ed-184a6fc2c8aa

Geographic attributes: 
•	Australia census data – ABS 2016 Census Table Builder - https://www.abs.gov.au/websitedbs/D3310114.nsf/home/About+TableBuilder 
•	US census data - https://www.census.gov/data.html 

## Methodology

### The Gravity Model

The spatial diffusion of any epidemic disease, including Covid-19, follows the law of distance decay in geography. The gravity model estimates the level of “connection” between two geographical areas without the need to precisely measure the flow of people. The intuition behind the gravity model is particularly relevant to a lockdown decision – because such decision need to consider both the inflow and outflow of people to and from the source of the outbreak. We can explore various methods of geographical clustering based on 1) distance; 2) travel pattern; and 3) gravity scores. 

### The Homophily Principle

The fundamental principle behind homophily is that “similarity breeds connection”. In the context of curbing the outbreak of an epidemic disease, we can explore the extent to which the level of “connection” estimated from “similarity” between two areas can be informative regarding the spatial diffusion of an a local outbreak.  

### The hybrid approaches

We will explore the benefit of combining the homophily principle and the gravity model in the clustering of geographic regions for the purpose of tracing the spread of the disease. E.g. we can replace the population measure in the gravity model by homophily measures such as the share of a certain ethnical population, we can also tweak the distance measure to incorporate the dissimilarity between two areas. 

## Evaluation and Final Results

The purpose of the analysis is to help local authority **to select a small subset of the geography or population to enter lockdown or perform group testing**, in the presence of a local epidemic outbreak. A natural evaluation metric would be how well the clustering “predicts” the rise of the confirmed cases of Covid-19, following the initial outbreak. However, it is difficult to control additional factors such as the methods adopted by local authorities to prevent the spread of the disease. Therefore, we will limit the evaluation only to data from the early outbreaks where local authorities were not able to implement any measure, and residents are less aware of the risk of infection. 

## Task allocation

| Stages          | Steps                       | Details                  | Allocation |
|-----------------|-----------------------------|--------------------------|------------|
| Data Collection | shapefiles                  | As is                    | Tony       |
|                 | Suburb quickstats           | ABS 2016 census          | Fred       |
|                 | covid-19 NSW/AU             | Individual suburbs       | Fred       |
| Methodology     | The Gravity Model           | Gavity index calculation | Tony       |
|                 | The Homophily Principle     | Clustering               | Fred       |
|                 | The hybrid approaches       | Ensemble methods         | Joint      |
| Evaluation      | Pairwise association mining | TBD                      | TBD        |
|                 | Conditional probability     | TBD                      | TBD        |
|                 | Statistical tests           | TBD                      | TBD        |
|                 | Network methods             | TBD                      | TBD        |
| Writing         | Essay                       | Final report             | Joint      |

