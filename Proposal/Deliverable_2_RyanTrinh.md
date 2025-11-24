# **Deliverable\_2\_RyanTrinh**

# **Introduction**

## **Background Information**

The performance of the stock market ties in with unemployment and consumer spending, and this is in line with general economic theory. However, I’ve found that recently, the significance of these connections may be shifting. In my recent experience with trading, stock markets have rebounded very quickly even with unemployment rates staying high, and other negative economic factors holding. This makes me curious about whether traditional macro indicators are still reliable in today's modern market conditions. Being able to identify the structural breaks using quantitative metrics can prove successful in determining a market recession, and finding out how significant traditional macro indicators are will help me improve my investment and trading strategy based on my results. I think there is a lot of value in researching traditional market theories and challenging them with the volatile nature and norm-breaking tendencies of the current market conditions of the current decade. It is a parallel with the tech industry, how it improves so rapidly, requiring quick adaptation to innovations; in which the financial markets should be the same.

## 

## **Purpose of Project**

My project aims to test whether the traditional macroeconomic indicators (unemployment rate and consumer spending specifically) can still predict or strongly correlate with stock market returns. This will also explore whether structural breaks exist in the relationship during major economic events such as the 2008 financial crisis, and the 2020 COVID pandemic, the latter time period being what I mainly want to focus on. We will also be studying the consumer spending index (CSI). Consumer spending has gone up by a lot in recent years, and we will test whether this connection has influence on why markets may not be reacting as strongly to traditional macroeconomic indicators. 

## **Significance**

Finding whether the relationship between major macroeconomic indicators and the stock market are still valid during the modern market conditions of the current decade can help us understand what does or does not move the markets. The results of this project may also show key economic relationships important for economists and policymakers, as well as other informed investors. If my findings are in line with my hypothesis that the relationships are breaking down, it can suggest that the markets may be driven more heavily by other factors like monetary policy, consumer reliance, or other factors not included in the datasets I am using.

# 

# **Objectives**

## **Primary:**

- Identify structural breaks in the relationship between unemployment rates and US stock market returns.

## **Secondary:** 

- Compare the relationships between macro factors across different major time periods.  
- Test whether or not unemployment has lost predictive power or correlation in the modern stock market era.  
- Examine the possibility of consumer spending relating to market performance.

# **Methodology**

## **Approach**

- Collect the data from the sources I’ve outlined: Unemployment rate (UNRATE), recession indicator (USREC), consumer spending (PCEC96), and the SP500 (SP500) from the Federal Reserve Economic Data (FRED).  
- Convert the stock index values into monthly returns, maybe shorter time frames for market periods where I want more detail.  
- Run correlation, regression, and rolling regression analysis to test predictive relationships.  
- Test for structural breaks to identify the possibility of shifts in the unemployment rate to market link.

## **Tools and Resources**

- R with quantmod, tidyverse, and strucchange libraries.  
- Federal Reserve Economic Data (FRED) for all datasets.

## **Timeline**

- Week 1: Collect and clean data  
- Week 2-3: Perform exploratory analysis and create descriptive statistics  
- Week 3-4: Perform predictive relationship testing and structural break testing.  
- Week 5: Create write-up draft, then clean for final report and presentation.

# 

# **Expected Outcomes**

## **Deliverables**

- A .zip file and github repo link containing my project with proper file structuring.  
  - Includes .rmd and .nb.html files of the project, graphs and other visual aids, as well as the final presentation in a .ppt or .pdf file.

## **Success Criteria**

- Successfully merging and analyzing the 4 datasets.  
- Evidence proving whether or not structural breaks exist in the unemployment-market relationship.  
- Evidence showing whether or not the predictive relationship between the unemployment-market relationship decays.  
- Visualizations of data that shows the results properly.

# **Challenges and Limitations**

## **Challenges**

The time frames of consumer spending and unemployment data have differing lags, making the alignment of these difficult.

## **Limitations**

- This project only aims to see the effects on US markets, which may not reflect global dynamics.  
- There are more macroeconomic indicators that are not studied in this project, meaning that there could be much more depth not analyzed in this study.  
- This is my first project with a much more macroeconomic focus, where my experience lies strongly only in algorithmic trading and quantitative analysis on financial markets. This limits the quality of my interpretation compared to more professional macroeconomic studies.

# **Conclusion**

This project aims to find whether unemployment is still a significant predictor of US stock market performance, and what the role of consumer spending is in this scenario. By testing for structural breaks across major economic events and different market periods, this project aims to find how useful traditional macroeconomic theory and signals are in current market conditions. I'm curious to see whether my findings are in line with my predictions, and if it could be academically relevant.

# **References**

- Federal Reserve Economic Data (FRED), Federal Reserve Bank of St. Louis.  
- National Bureau of Economic Research (NBER)