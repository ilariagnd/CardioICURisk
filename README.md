# CardioICURIsk
This repository contains the official implementation of the following manuscript: “Interpretability of time-series deep learning models: a study in cardiovascular patients admitted to Intensive Care Unit” by Gandin, Scagnetto, Romani, Barbati.

## Requirements
R 4.0.3

Python 3.7.7

## Getting started
The analysis is based on MIMIC III tables. Access to the database can be requested [here](https://physionet.org/content/mimiciii/1.4/) by researchers that meet the criteria. 

Given the huge size of some of the tables, the selection of cardiovascular patients was perform with a query on MIMIC III database using a database management system  (see ICDM codes in Appendix A of the manuscript). Assuming the filtering has been performed, you will need the following tables:
- PATIENTS_filtered.sas7bdat
- CHARTEVENTS_filtered.sas7bdat
- LABEVENTS_filtered.sas7bdat
- PRESCRIPTIONS_filtered.sas7bdat
- PROCEDUREEVENTS_filtered.sas7bdat
- D_ITEMS.csv.gz
- D_LABITEMS.csv.gz
Put all files in folder ./input/

## Workflow
The program codes can be divided into two steps: data processing, to create the working datasets, and training/validation of the model. The analysis presented in the paper are based on 10 iterations of training/validation in order to avoid sampling bias due to the training-test data split.

### Data processing
Set ./output/ as working directory.

Run in sequence the following R scripts:
- s1.read_raw_data.R
- s2.identify_vars.R
- s3.time_windows.R
- s4.create_datasets.R

You should see in the ./output/ directory both training and validation sets.

### Train and validate the model
Set ./output/ as working directory.

The algorithm requires a set of hyperparameters saved in file as the one in ./output/cv_res_lstm_mod_attention.csv that typically is the results of tuning analysis.

Run the following Python script:
- s5.train_val_model.py

You will find in the /output/ direcroty:
- model's file # o5.fin_model.h5
- model’s predictions # o5.fin_model_pred.csv
- activation weights # o5.fin_model_act.csv
