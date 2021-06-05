import pandas as pd
import sys
import openpyxl
import csv
import networkx as net
import random as rand
import json

ruas = dict()

def excelToCSV():
    read_file = pd.read_excel('dataset-original.xlsx', engine='openpyxl', nrows= 419)
    read_file.to_csv (r'dataset.csv', encoding='utf-8', index = None, header=False)

def createDataset():
    old_stdout = sys.stdout
    log_file = open("output.txt","w")
    sys.stdout = log_file

    with open('dataset.csv') as file:
            data = csv.reader(file)
            for row in data:
                print ("ponto("+row[0]+", "+
                                
                                row[1]+","+
                                
                                row[2]+",'"+
                                
                                (row[4].strip().split(":"))[0]+"','"+ 
                                
                                row[5]+"','"+
                                
                                row[6]+"',"+
                                
                                row[7]+","+
                                
                                row[8]+","+
                                
                                row[9]+").")
                ruas[(row[4].strip().split(":"))[0]] = []

    sys.stdout = old_stdout

    log_file.close()

def getRandPontoRecolha():
    return ruas_list[(rand.randint(0,len(ruas_list)-1))]

def createGraph():
    
    old_stdout = sys.stdout
    log_file = open("outputGrafo.pl","w")
    sys.stdout = log_file

    G = net.Graph()
    G.add_nodes_from(ruas_list)

    while not net.is_connected(G):
        G.add_edge(getRandPontoRecolha(),getRandPontoRecolha())
        
    dictPoints = net.to_dict_of_lists(G)

    print("grafo([",end = '')

    count = 0

    for i in dictPoints:
        if(count==0):
            print("'" + i +"'",end = '')
            count=1
        else:
            print(",'"+i + "'",end = '')
    
    print("],[",end = '')

    count2 = 0

    for i in dictPoints:
        for j in dictPoints[i]:
            if(count2==0):
                print("aresta('" + i + "','" + j + "')",end = '')
                count2=1
            else:
                print(",aresta('" + i + "','" + j + "')",end = '')

    print("]).",end = '')

    sys.stdout = old_stdout

    log_file.close()


# Call functions

excelToCSV()
createDataset()
ruas_list = list(ruas.keys())
createGraph()


