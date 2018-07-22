import pandas as pd
import pickle as pk
import os

BASE_PATH = '/Users/mohammadmahdi/Desktop/tehran_data_parse/crawled_data/'
TARGET_PATH = '/Users/mohammadmahdi/Desktop/tehran_data_parse/'
os.chdir(BASE_PATH)

try:
    os.remove('.DS_Store')
except FileNotFoundError:
    pass

files = os.listdir(BASE_PATH)

trait_list = []
try:
    for file in files:
        file_address = BASE_PATH + file
        with open(file_address, mode='rb') as read_file:
            file_data = pk.load(read_file)[0][2]
            for record in file_data:
                traits = record[3]
                for trait in traits:
                    trait_list.append(trait)
except Exception:
    print(file_address)

unique_trait_list = list(set(trait_list))


result = {}
result['latitude'] = []
result['longitude'] = []
result['type'] = []

for trait in unique_trait_list:
    result[trait] = []


for file in files:
    file_address = BASE_PATH + file
    with open(file_address, mode='rb') as read_file:
        file_data = pk.load(read_file)[0][2]
        for record in file_data:
            result['longitude'].append(record[0])
            result['latitude'].append(record[1])
            result['type'].append(record[2])
            traits = record[3]
            non_traits = list(set(unique_trait_list) - set(traits))
            for trait in traits:
                result[trait].append(traits[trait])
            for trait in non_traits:
                result[trait].append(None)

record_count = len(result['latitude'])

result_df = pd.DataFrame(result)
os.chdir(TARGET_PATH)
result_df.to_csv('final_crawler_result.csv')

