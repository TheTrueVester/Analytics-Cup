# Analytics Cup 2022/23 – Fraud / Reseller Detection

This repository contains my solution for the **Analytics Cup 2022/2023** (in cooperation with Siemens Advanta Consulting).  
The task was to build an **explainable classification model** that detects whether a customer acts as an **unauthorized reseller** (`Reseller = 1`) or uses the products internally (`Reseller = 0`). The model is evaluated using **Balanced Accuracy (BAC)** on a hidden private test set. 

---

## Repository Structure

- Challenge_Description.pdf — Official challenge description and evaluation setup.   
- Data files (anonymized): classification.csv, customers.csv, business_units.csv, service_map.csv, sales_orders.csv, sales_orders_header.csv, transactions.csv, geo.csv  
- script_version_B.R — Final reproducible pipeline:
  - Load & clean data  
  - Merge transactional tables  
  - Engineer aggregated customer-level features  
  - Handle class imbalance  
  - Train random forest (tidymodels + ranger)  
  - Evaluate via Balanced Accuracy  
  - Output predictions_version_B.csv  
- script_version_A.R — Early model version (offer-status prediction).  
- File_Analysis/ — Exploratory and feature-engineering work.  
- TestData/ — Intermediate dev data.  
- Example submissions: submission-Chen.csv, submission_random.csv.

> Note: Data is anonymized and may only be used under the Analytics Cup rules.
>
> ## Data Model Overview

The project integrates seven relational datasets.  
The diagram below illustrates how the files are connected and which keys are used for joins:

![Data Schema](File_Analysis/D984257D-E96A-47BB-8ABB-D718B79EC696.jpeg)

---

## How to Run the Final Model

### 1. Install the required R packages

    install.packages(c(
      "tidyverse", "lubridate", "summarytools", "ggmap",
      "tidymodels", "dplyr", "randomForest", "ROSE", "ranger", "vip"
    ))

### 2. Set the working directory

    setwd("path/to/Analytics-Cup")

### 3. Run the final script

    source("script_version_B.R")

### 4. The script will:

- Preprocess and merge all datasets  
- Train the random forest model  
- Print Sensitivity, Specificity, Balanced Accuracy  
- Generate the file:

    predictions_version_B.csv

This file contains:

- `id` → Test_set_id  
- `prediction` → predicted reseller label (0 or 1)

---

## Methods & Modeling Notes

### Feature Engineering

- Joins across customers, sales_orders, sales_orders_header, business_units, service_map  
- Aggregated per-customer metrics:
  - Order counts, item totals, net value sums  
  - Document type and delivery state frequencies  
  - Time intervals (e.g., creation → release)  
- Handling mismatched Item_Position values as described in the challenge instructions. 

### Modeling

- Random forest (ranger) with 1000 trees  
- 5-fold cross-validation  
- Optimized for Balanced Accuracy  
- Class imbalance mitigated via downsampling  
- Manual confusion-matrix validation:
  - Sensitivity  
  - Specificity  
  - Balanced Accuracy = (Sensitivity + Specificity) / 2  

### Explainability

- Variable importance using `vip::vip()`  
- Identification of major predictive drivers such as:
  - Order patterns  
  - Service material classes  
  - Sales organization behavior  
  - Net value distributions  
  - Business-unit activity  

---

## Contact

**Lukas Vester**  
Email: lukasvester@gmail.com  
GitHub: https://github.com/TheTrueVester
