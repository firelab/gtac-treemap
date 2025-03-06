import pandas as pd
from simpledbf import Dbf5

def calculate_percent_clips(file_path, column_name, min_val, max_val):
    # Load the .dbf file
    dbf = Dbf5(file_path)
    df = dbf.to_dataframe()

    # Check if the column exists
    if column_name not in df.columns:
        raise ValueError(f"Column {column_name} not found in the file")

    # Calculating the cumulative count
    df['CumulativeCount'] = df['Count'].cumsum()
    total_count = df['Count'].sum()

    # Find the closest value to the min_val and max_val
    min_clip = df[df[column_name] <= min_val]['CumulativeCount'].max() / total_count
    max_clip = df[df[column_name] >= max_val]['CumulativeCount'].min() / total_count

    return min_clip, max_clip

# Usage
file_path = r'C:\Users\NicholasStorey\Desktop\TreeMap2016.tif.vat.dbf'  # Replace with your file path
column_name = 'CARBON_L'         # Replace with your column name
min_val = 2
max_val = 59

min_clip, max_clip = calculate_percent_clips(file_path, column_name, min_val, max_val)
print(f"Minimum Percent Clip: {min_clip}")
print(f"Maximum Percent Clip: {max_clip}")

