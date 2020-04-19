---
title: "Bovespa Screening for Day-Trading strategies with brazilian stocks"
author: 
- name: "Lucas S. Macoris"
  affiliation: "PhD Student at Insper - Institute of Research - São Paulo - Brazil"
  email: "Contact: lucassm4@al.insper.edu.br"
output: html_document
---


## Disclaimer

Disclaimer: the contents expressed herein are exclusively designed for educational purposes and does not represent, in any circumstances, the opinion of **Insper - Institute of Research**. This content should not be viewed as a financial advise. For additional information, contact can be made by email: lucassm4@al.insper.edu.br.


## About this textbook

This is an R Markdown document regarding the **Bovespa Screeening** initiative. This notebook  uses *Yahoo! Finance* data in order to collect signals for Long/Short Operations. Basically, it is comprised of an **R Notebook** that collects daily Bovespa Data, calculates technical indicators, create signals of Long/Short Operations based on the desired criteria and charts each of the stocks that are in line with the desired strategy.

The idea of this project is to provide an automated screening system in which, based on the technical indicators defined by the researcher, one can easily run a screening daily screening process in order to select a bunlde of assets to be trader in long and/or short positions. This notebook implements this by using *Yahoo! Finance!* data on a daily basis from the beginning of 2019, and is managed in order to provide a signal forecast based on the last trading session available in the servers.

For that, we are going to present a simple example using a set of brazilian traded stocks in *Bovespa*. All the stocks are presented in the auxiliary *.csv* file, `Assets.csv`, and can be changed to accomodate any stock available in *Yahoo! Finance* servers.

All the errors, fixes and improvements are fully discussed above, with brief explanations of what has changed. 

**Important Remark: this project is not being updated anymore**. However, it can serve as a initial point for several analysis that aim to address data analysis for a a bundle of assets simultaneosly.

## Log

The actual version of the main file:

1. Implements the resulting output of trading with Bollinger Bands, Relative Strenght Index (RSI) and a customized technical indicator based on the Moving Average Convergence Divergence (MACD).
2. Uses *Yahoo! Finance*, a provider of free, daily updated financial data for brazilian stocks.

Fixes:

- 02.02.2020:

1. Added MACD Oscilattor for the graphs.
2. Created MACD Difference indicator.
3. Fix MACD Difference to account for DiD Estimator around the threshold of Bollinger Bands.

- 11.01.2020:
1. Plots are now created inside a new folder named with `Sys.Date` function output.

## Important Remarks

**Important Remark**: *Yahoo! Finance* generally offers data with splits and dividends adjustments and therefore may not be the same as the brokerage information. In this sense, recommendations must also be analyzed through technical indicators presented on the brokerage account.

Additionally, one can change the log in order to use any other provider of financial data of the same format, such as [*AlphaVantage*](https://www.alphavantage.co/), which also has an API support into the `quantmod` library.

