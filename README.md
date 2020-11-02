# STA304-PS4-Group-65

## Forecasting the popular vote in the 2020 American presidential elections
In this paper, we use the multilevel regression with post-stratification method to forecast the overall popular vote of the 2020 American presidential elections. 

## Note on file structure
'inputs' folder is where the raw data files should go. There are two data files that were used in this study: (i) post-stratification data from the Census Bureau's American Community Survey (ACS); (ii) Democracy Fund + UCLA Nationscape data (June 2020). We are not allowed to redistribute these data files. Users are expected to obtain the data files with persmission from the official websites.

'scripts' folder contains the R scripts that we used to clean the datasets.

'outputs' is where anything that we created goes. This includes our paper and the Rmd file used to generate the paper, as well as the cleaned data files. Again, we are not allowed to share the data files.

## Suggested steps for recreating our report
1. Download the raw data files from the ACS and the Democracy Fund + UCLA website. Place them in the 'inputs' folder under the 'data' sub-folder.
2. Run the data cleaning scripts that we have provided in the 'scripts' folder. In order for the scripts to run correctly, ensure that the filepath and filenames in the code match with the ones on your system.
3. Ensure that the cleaned data files from step 2 are placed in the 'output' folder in the 'data' sub-folder.
4. In the 'output' folder, you will find the 'paper' sub-folder. Run the .Rmd file there to generate the report.

