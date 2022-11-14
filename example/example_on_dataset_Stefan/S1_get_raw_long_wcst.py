import numpy as np
import os
from pprint import pprint
import pandas as pd

fdb_resp = np.load(os.path.join('DBN','FullDataset.npy'),allow_pickle=True)
dim_map = np.load(os.path.join('DBN','MatchingMat.npy')).astype(np.int32)

def get_csv(index):
    fdb = np.array(list(fdb_resp.tolist().values()))
    fdb = fdb[index]

    new_csv = []
    for k,v in fdb.items():
        # print(v)
        for trial in range(len(v)):
            # print(type(v[trial]))
            tmp = np.insert(v[trial],0,k)

            # print(tmp,dim_map[trial][0])
            tmp = np.append(tmp,dim_map[trial][0])
            tmp = np.append(tmp,dim_map[trial][1])
            tmp = np.append(tmp,dim_map[trial][2])
            new_csv.append(list(tmp))
    # print(new_csv)

    df = pd.DataFrame(new_csv,columns=['subid','feedback','button_pressed','color_rule','shape_rule','number_rule'])
    df['feedback'].sum()


    if index == 0:
        df.to_csv('stefan_wcst_long_health.csv')
    else:
        df.to_csv('stefan_wcst_long_exp.csv')

get_csv(0)
get_csv(1)

