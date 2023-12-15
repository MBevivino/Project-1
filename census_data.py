#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  8 15:32:11 2023

@author: matthewbevivino
"""

import requests
import pandas as pd
from config import CENSUS_API_KEY

def fetch_census_data():
    # Base URL for the Census API
    BASE_URL = 'https://api.census.gov/data/2019/acs/acs5'

    # Parameters for the API request
    params = {
        'get': 'B19013_001E,B15003_022E,B02001_002E,B01001_026E,B01002_001E,B23025_005E,B25064_001E',
        'for': 'county:*',
        'key': CENSUS_API_KEY
    }

    # Make the API request
    response = requests.get(BASE_URL, params=params)

    # Check if the request was successful
    if response.status_code == 200:
        # Parse the response JSON into a DataFrame
        data = response.json()
        df = pd.DataFrame(data[1:], columns=data[0])

        # Save the data to a CSV file
        df.to_csv('census_data.csv', index=False)
        print("Data retrieved and saved successfully.")
    else:
        print("Failed to retrieve data:", response.status_code)

if __name__ == "__main__":
    fetch_census_data()
