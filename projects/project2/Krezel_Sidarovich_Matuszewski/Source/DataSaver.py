# 
# 
# ====================
INITIALS = 'SMX'

### Should be set to True if parsed_data.csv and meta_data.csv are not present
PARSE_DATA_FIRST = False

### Which data to save
TO_SAVE = {
    "track_r_data":     True,
    # "artist_r_data":    False,
    "track_scores":            True, # requires track_r_data and unique_track_names
    "track_scores_over_time":  True, # requires track_r_data and unique_track_names
    "track_scores_per_period": True  # requires track_r_data and unique_track_names
}
# ====================
#
#

import Analysers
import Utils
import pandas as pd
import os

data = None
meta_data = None
track_r_data = None
unique_track_names = None

def save_track_r_data():
    global track_r_data
    global unique_track_names
    track_r_data, unique_track_names = Analysers.calculate_track_relevance(data, meta_data)

    track_r_data.to_csv(f"Data/Analysed/RelevanceData/{INITIALS}_track_r_data.csv", sep=';')
    with open(f"Data/Analysed/RelevanceData/{INITIALS}_unique_track_names.txt", 'w', encoding='utf-8') as f:
        for i in range(len(unique_track_names)-1):
            f.write(unique_track_names[i]+'\n')
        f.write(unique_track_names[-1])

def save_artist_r_data():
    pass

def save_scores():
    track_scores = Analysers.calculate_scores(track_r_data, unique_track_names)
    track_scores.to_csv(f"Data/Analysed/Score/{INITIALS}_track_weekly.csv", sep=';')

def save_scores_over_time():
    track_scores_over_time = Analysers.calculate_scores_over_time(track_r_data, unique_track_names)
    track_scores_over_time.to_csv(f"Data/Analysed/ScoreOverTime/{INITIALS}_track_weekly.csv", sep=';')

def save_scores_per_period():
    pass

functions = {
    "track_r_data": save_track_r_data,
    # "artist_r_data": save_artist_r_data,
    "track_scores": save_scores,
    "track_scores_over_time": save_scores_over_time,
    "track_scores_per_period": save_scores_per_period,
}


def main():
    global data
    global meta_data
    global track_r_data
    global unique_track_names
    if PARSE_DATA_FIRST:
        if INITIALS[-1] == 'X':
            Utils.ExtendedStreamHistoryParser("Data/Raw/{INITIALS}/", f"{INITIALS}_parsed_data.csv")
    if not TO_SAVE["track_r_data"]:
        track_r_data = pd.read_csv(f"Data/Analysed/RelevanceData/{INITIALS}_track_r_data.csv", sep=';')
        unique_track_names = list()
        with open(f"Data/Analysed/RelevanceData/{INITIALS}_unique_track_names.txt", encoding='utf-8') as f:
            for line in f.read().splitlines():
                unique_track_names.append(line)
    
    data = pd.read_csv(f"Data/Parsed/{INITIALS}_parsed_data.csv", sep=';')
    meta_data = pd.read_csv(f"Data/Parsed/{INITIALS}_meta_data.csv", sep=';')
    for k, v in TO_SAVE.items():
        if v:
            os.system('cls')
            print(f"Saving {k}...") 
            functions[k]()
    
if __name__ == '__main__':
    main()




