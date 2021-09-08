# ANZ
 This task is based on a synthesised transaction dataset containing 3 months’ worth of transactions for 100 hypothetical customers. It contains purchases, recurring transactions, and salary transactions.The dataset is designed to simulate realistic transaction behaviours that are observed in ANZ’s real transaction data.

Our first task is to identify annual salary for each customer and to explore correlations between annual salary and various customer attributes (e.g. age). These attributes could be those that are readily available in the data (e.g. age) or those that we construct or derive ourselves (e.g. those relating to purchasing behaviour). Once we found some correlations, our second task is to predict the annual salary for each customer by using the attributes we identified above.

In order to achieve this goal, we will try a number of standard linear and non-linear algorithms to spot-check our problem in R. In general, our analysis can be broken down into 5 steps:

descriptive statistics and data visualization to better understand the data
data pre-processing in order to better expose the structure of the prediction problem to modeling algorithms
evaluation of algorithms
improving results by algorithm tuning and ensemble methods
finalizing the model, making predictions and presenting results
