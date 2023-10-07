import pandas as pd

tour = pd.read_csv(r"Aufgabe5\data\tour3.txt",skiprows=1,names=["Ort","Jahr","Essentiell?","Distanz von Start"])

print(tour)