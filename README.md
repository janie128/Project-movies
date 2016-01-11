# Data Science Project: Box Office Prediction
### From Movie Reviews to Box Office
  
This project aims to explore the correlation between movie box office and Amazon movie reviews, and develop predictive models for projecting the overall movie box office. Various features from the movie reviews are extracted, including: review ratings distribution statistics, review length, review sentiment score and number of reviews. Machine learning models are trained and used to project box office gross.  
  
There are three main parts to this project: 1) data scraping and cleaning, 2) feature extraction, and 3) prediction modeling. A report detailing the analysis done in this project can be found <a href="http://nbviewer.jupyter.org/github/janie128/Project-movies/blob/master/Box_Office_Predictions.ipynb" target="_blank">here</a>.
  
This project is done in R. All codes can be found in this repository.   
  
#### File descriptions:
`main.R` is the main script that calls for all parts of the analysis.  
`box_office_mojo_scraping.R` conducts web scraping to acquire box office data.  
`clean_explore_BOdata.R` performs data cleaning and exploratory data analysis of box office data.  
`generate_strict_json.py` parses and outputs original Amazon review data into strict JSON format for R parsing.  
`parse_real_title` contains function for title matching between movie titles from box office data and product titles from Amazon review data.  
`clean_reviewsTitles.R` performs data merging and cleaning of review data.  
`feature_extraction.R` extracts features to be used in modeling.  
`word_analysis.R` contains function used in feature extraction to perform text analysis and natural language processing on reviews.  
`machine_learning.R` trains different learning machine learning models for prediction modeling.  
  
#### Data sources:
Movie box office: <a href="http://www.boxofficemojo.com/daily/?view=bymovie&yr=all&sort=title&order=ASC&p=.htm" target="_blank">Box Office Mojo</a>  
Movie reviews (from Amazon): <a href="http://jmcauley.ucsd.edu/data/amazon/" target="_blank">Dr. Julian McAuley's Amazon product data</a>  
Inflation data: <a href="http://www.usinflationcalculator.com/inflation/historical-inflation-rates/" target="_blank">US Inflation Calculator</a>  
Sentiment analysis dictionary: <a href="https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon" target="_blank">Hu and Liu's Opinion Lexicon</a>  
  
#### References:  
"Image-based recommendations on styles and substitutes" J. McAuley, C. Targett, J. Shi, A. van den Hengel; *SIGIR*, 2015  
"Inferring networks of substitutable and complementary products" J. McAuley, R. Pandey, J. Leskovec; *Knowledge Discovery and Data Mining*, 2015  
"Mining and Summarizing Customer Reviews" M. Hu, B. Liu; *Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD-2004)*, Aug 22-25, 2004, Seattle, Washington, USA  
