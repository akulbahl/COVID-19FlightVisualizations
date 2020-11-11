# COVID-19 Airline Flight Delays and Cancellations

![Airlines](https://www.globaltimes.cn/Portals/0/attachment/2020/2020-10-20/84220eb8-3383-4611-8f76-4d8fcf0df7a0.jpeg)

COVID-19 has severely crippled the global airline industry with air service reductions widespread throughout 2020. This dataset containing **~11 million flights** will aid those seeking to visualize the impact that the virus has had on the domestic United States airline industry through detailed flight delay and cancellation data. 

## Data

The *United States Department of Transportation's (DOT) Bureau of Transportation Statistics* tracks the on-time performance of domestic flights operated by large air carriers. The data collected is from **January - June 2020** and contains relevant flight information (on-time, delayed, canceled, diverted flights) from the Top 10 United States flight carriers for **~11 million flights**. 

The data files are hosted and freely available on [Kaggle](https://www.kaggle.com/akulbahl/covid19-airline-flight-delays-and-cancellations). 

The flights are segmented by month. The cleaned and segmented data is located in the data folder with full column descriptions in the attached '.txt' file. 

## Usage

View the R Shiny application here. To run locally, download the Kaggle data has been downloaded and merged to one file. Change your working directory:

```Python
setwd("path\to\file")
```

Read in the data:

```Python
read.csv()
```

## Contributing
Pull requests are welcome - please be sure to open an issue to discuss changes in your request.

## License
[MIT](https://choosealicense.com/licenses/mit/)
