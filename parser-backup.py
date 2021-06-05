import pandas as pd
import sys
import openpyxl

'''
pd.set_option('display.max_rows',None)

pd.set_option('display.max_columns',None)

pd.set_option('display.width',1500)
'''

'''
old_stdout = sys.stdout

log_file = open("output.txt","w")

sys.stdout = log_file

df = pd.read_excel('dataset.xlsx', engine='openpyxl', nrows= 420)

print(df)

sys.stdout = old_stdout

log_file.close()
'''

with open("dataset-original.xlsx", mode="r", encoding="utf-8") as file:
    read_file = pd.read_excel(file, engine='openpyxl', nrows= 420)
    read_file.to_csv (r'dataset.csv', encoding='utf-8', index = None, header=True)

#read_file = pd.read_excel(r'dataset-original.xlsx', encoding='utf-8', engine='openpyxl', nrows= 420)
#read_file.to_csv (r'dataset.csv', encoding='utf-8', index = None, header=True)

