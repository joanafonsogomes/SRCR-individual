import pandas as pd
import sys
import openpyxl
import csv

read_file = pd.read_excel('dataset-original.xlsx', engine='openpyxl', nrows= 420)
read_file.to_csv (r'dataset.csv', encoding='utf-8', index = None, header=False)

# old_stdout = sys.stdout

# log_file = open("output.txt","w")

# sys.stdout = log_file

with open('dataset.csv') as file, open("baseConhecimento","w") as baseC:
        data = csv.reader(file)
        base = baseC.readlines()
        # prev = None
        # value = None
        for row in data:
            base[8] = ("circuitos("+row[0]+", "+
                            
                            row[1]+","+
                            
                            row[2]+",'"+
                            
                            row[3]+"','"+
                            
                            (row[4].strip().split(":"))[0]+"','"+ # o -> ( )
                            
                            row[5]+"','"+
                            
                            row[6]+"',"+
                            
                            row[7]+","+
                            
                            row[8]+","+
                            
                            row[9]+").")