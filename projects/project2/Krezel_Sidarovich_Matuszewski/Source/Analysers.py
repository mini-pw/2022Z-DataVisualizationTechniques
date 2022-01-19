from datetime import timezone
from numpy.lib import math, unique
import pandas as pd
import numpy as np
import os

RELEVANCE_SUFFIX = ''
FULL_STREAM_COUNT_SUFFIX = '_fullStreamCount'
STREAM_COUNT_SUFFIX = '_streamCount'

def __get_dates_column(data : pd.DataFrame, date_bin = "week"):
        dates = pd.to_datetime(data["date"])

        min_date = dates.min()
        max_date = dates.max()
        min_date = pd.Timestamp(year=min_date.year, month=min_date.month, day=min_date.day, tz=min_date.tz)
        if date_bin == "day":
            days_delta = (max_date - min_date).days
            dates = [min_date + pd.Timedelta(days=1) for i in range(0, days_delta+1)]
            dates.insert(0, dates[0] - pd.DateOffset(days = 1)) # sentinel
        elif date_bin == "week":
            min_date = min_date - pd.Timedelta(days = min_date.dayofweek)
            max_date = max_date - pd.Timedelta(days = max_date.dayofweek)
            weeks_delta = (max_date - min_date).days // 7
            dates = [min_date + pd.Timedelta(days=i*7) for i in range(0, weeks_delta+1)]
            dates.insert(0, dates[0] - pd.DateOffset(weeks = 1)) # sentinel
        elif date_bin == "month":
            min_date = min_date - pd.Timedelta(days = min_date.day - 1)
            max_date = max_date - pd.Timedelta(days = max_date.day - 1)
            months_delta = (max_date.year - min_date.year) * 12 + max_date.month - min_date.month
            dates = [min_date + pd.DateOffset(months=i) for i in range(0, months_delta+1)]
            dates.insert(0, dates[0] - pd.DateOffset(months = 1)) # sentinel
        
        return dates


def __floor_dates(dates : pd.Series, date_bin = "week"):
    dates = dates.apply(lambda d : pd.Timestamp(year = d.year, month = d.month, day = d.day, tz=d.tz))
    if date_bin == "day":
        return dates
    elif date_bin == "week":
        return dates.apply(lambda d : d - pd.DateOffset(days = d.dayofweek))
    elif date_bin == "month":
        return dates.apply(lambda d : d - pd.DateOffset(days = d.day - 1))


def calculate_track_relevance(data : pd.DataFrame, meta_data : pd.DataFrame, date_bin = "week", stream_count_threshold = 5, playtime_threshold = 30000, dates = None):
    """
    Calculates a dataframe containing relevance of each track over time.
    data        -- stream history
    meta_data   -- stream history meta data
    stream_count_threshold  -- minimum number of streams per track. Default = 2
    playtime_threshold      -- minimum play time per track [ms] (streams below playtime threshold do not count towards stream count). Default = 30000
    date        -- Series-like of dates. If none provided defaults to a list between minimum week and maximum week with weekly steps
    """
    if (dates is None): dates = __get_dates_column(data, date_bin)

    # filter out short streams
    data["ms"] = pd.to_numeric(data["ms"])
    data = data[data["ms"] >= playtime_threshold]

    # filter out tracks that weren not play enough times
    data["track"] = data["track"] + " (" + data["artist"] + ")"
    agg_tmp = data.groupby("track")["track"].agg(count=len).reset_index()
    unique_track_names = agg_tmp.loc[agg_tmp["count"] >= stream_count_threshold,"track"].unique()

    # filter out tracks that could not be metatised (are not present in meta_data)
    indices = pd.Series(unique_track_names).isin(meta_data["trackName"] + " (" + meta_data["primaryArtist"] + ")")
    unique_track_names = unique_track_names[indices]

    # floor dates to start of week
    data["date"] = pd.to_datetime(data["date"])
    data["date"] = __floor_dates(data["date"], date_bin)
    # data["date"] = data["date"].apply(lambda ts : \
    #     pd.Timestamp(year = ts.year, month = ts.month, day = ts.day) - pd.Timedelta(days = ts.dayofweek))

    data = data[data["track"].isin(unique_track_names)]
    
    # prepare data frame
    columns = ["date"]
    # track_columns = unique_track_names
    track_columns = [t + RELEVANCE_SUFFIX for t in unique_track_names]
    track_columns = np.concatenate((track_columns, [t + STREAM_COUNT_SUFFIX for t in unique_track_names]))
    track_columns = np.concatenate((track_columns, [t + FULL_STREAM_COUNT_SUFFIX for t in unique_track_names]))
    columns = np.concatenate((columns, track_columns))
    r_data = pd.DataFrame(columns=columns, index=np.arange(0, len(dates))).fillna(0)
    r_data["date"] = dates

    # populate data frame
    for date_bin in dates[1:]:
        os.system('cls')
        print(date_bin)
        current_streams = data[data["date"] == date_bin]
        # current_streams = current_streams[current_streams["track"].isin(unique_track_names)]
        # for track in unique_track_names:
        for track in current_streams["track"].unique():
            # print(track)
            track_streams = current_streams[current_streams["track"] == track]
            stream_count = len(track_streams)
            # if C == 0:
            #     weeks_since_lats_stream[track] += 1
            # else:
            #     weeks_since_lats_stream[track] = 0

            # L = previous_relevance[track]
            total_play_time = track_streams["ms"].sum()
            duration = meta_data[meta_data["trackName"] + " (" + meta_data["primaryArtist"] + ")"==track].iloc[0]["durationMs"]
            full_stream_count = len(track_streams[track_streams["ms"] >= duration * 0.8]) # margin of error
            relevance = total_play_time/duration

            r_data.loc[r_data["date"]==date_bin, track+RELEVANCE_SUFFIX] = relevance
            r_data.loc[r_data["date"]==date_bin, track+STREAM_COUNT_SUFFIX] = stream_count
            r_data.loc[r_data["date"]==date_bin, track+FULL_STREAM_COUNT_SUFFIX] = full_stream_count

    return r_data, unique_track_names


def calculate_artist_relevance(data : pd.DataFrame, meta_data : pd.DataFrame, date_bin = "week", stream_count_threshold = 25, playtime_threshold = 30000, dates = None):
    """
    Calculates a dataframe containing relevance of each artist over time.
    data        -- stream history
    meta_data   -- stream history meta data
    stream_count_threshold  -- minimum number of streams per artist. Default = 10
    playtime_threshold      -- minimum play time per track [ms] (streams below playtime threshold do not count towards stream count). Default = 30000
    date        -- Series-like of dates. If none provided defaults to a list between minimum week and maximum week with weekly steps
    """

    if (dates is None): dates = __get_dates_column(data, date_bin)

    # filter out short streams
    data["ms"] = pd.to_numeric(data["ms"])
    data = data[data["ms"] >= playtime_threshold]

    # filter out artists that weren not play enough times
    agg_tmp = data.groupby("artist")["artist"].agg(count=len).reset_index()
    unique_artist_names = agg_tmp.loc[agg_tmp["count"] >= stream_count_threshold,"artist"].unique()
    data = data[data["artist"].isin(unique_artist_names)]
    track_r_data, unique_track_names = calculate_track_relevance(data, meta_data, date_bin, 1, 0, dates)
    
    columns = ["date"]
    artist_columns = [a + RELEVANCE_SUFFIX for a in unique_artist_names]
    artist_columns = np.concatenate((artist_columns, [a + STREAM_COUNT_SUFFIX for a in unique_artist_names]))
    artist_columns = np.concatenate((artist_columns, [a + FULL_STREAM_COUNT_SUFFIX for a in unique_artist_names]))
    columns = np.concatenate((columns, artist_columns))

    r_data = pd.DataFrame(columns=columns, index=np.arange(0, len(dates))).fillna(0)
    r_data["date"] = dates

    for artist in unique_artist_names:
        relevance = track_r_data.iloc[:,track_r_data.columns.str.endswith(f"({artist}){RELEVANCE_SUFFIX}")]
        stream_count = track_r_data.iloc[:,track_r_data.columns.str.endswith(f"({artist}){STREAM_COUNT_SUFFIX}")]
        full_stream_count = track_r_data.iloc[:,track_r_data.columns.str.endswith(f"({artist}){FULL_STREAM_COUNT_SUFFIX}")]
        r_data[artist+RELEVANCE_SUFFIX] = relevance.sum(axis=1)
        r_data[artist+STREAM_COUNT_SUFFIX] = stream_count.sum(axis=1)
        r_data[artist+FULL_STREAM_COUNT_SUFFIX] = full_stream_count.sum(axis=1)

    return r_data, unique_artist_names

import time
def calculate_scores(r_data : pd.DataFrame = None, unique_names = None):
    """
    Calculates scores for a given relevance data.
    r_data          -- track/artist relevance data
    unique_names    -- list of unique track/artist names present in r_data
    """
    def get_first_stream_week(name):
        return r_data[name+RELEVANCE_SUFFIX].ne(0).idxmax() # first non zero row
        # because each week is present then the idx is the number of the week
    def calculate_consistency(name):
        # ignore weeks before the first stream up to half a year
        data_len = len(r_data)
        non_zero_data_len = len(r_data[r_data[name] != 0])
        first_stream_week = max(min(get_first_stream_week(name), len(r_data["date"])-26), 1)
        consistency = non_zero_data_len / (data_len-1)
        amortised_consistency = non_zero_data_len / (data_len-first_stream_week)
        return consistency, amortised_consistency
    def calculate_retention_rate(name):
        totalPlaytime = r_data[name].sum()
        streamCount = r_data[name+STREAM_COUNT_SUFFIX].sum()
        if streamCount == 0: return 0 # if only subset of data is analysed then no streams for a track can be present
        return  totalPlaytime / streamCount

    score_data = pd.DataFrame(columns=["name", "retentionRate", "fullStreams", "consistency", "amortisedConsistency", "mean", "score"], \
                                index=np.arange(len(unique_names)))

    for i in range(0, len(unique_names)):
        name = unique_names[i]

        score_data.iloc[i]["name"] = name
        score_data.iloc[i]["retentionRate"] = calculate_retention_rate(name)
        score_data.iloc[i]["fullStreams"] = r_data[name+FULL_STREAM_COUNT_SUFFIX].sum()
        score_data.iloc[i][("consistency", "amortisedConsistency")] = calculate_consistency(name)
        score_data.iloc[i]["mean"] = r_data[name+RELEVANCE_SUFFIX].mean()
        score_data.iloc[i]["score"] =   score_data.iloc[i]["fullStreams"] * \
                                        score_data.iloc[i]["consistency"] ** 2 * \
                                        score_data.iloc[i]["amortisedConsistency"] *\
                                        score_data.iloc[i]["retentionRate"] ** 2
        # break
    return score_data


def calculate_scores_per_period(r_data : pd.DataFrame, unique_names = None):
    pass


def calculate_scores_over_time(r_data: pd.DataFrame, unique_names = None):
    dates = r_data["date"][1:] # remove sentinel

    columns = ["name"] + dates.tolist()
    scores_data = pd.DataFrame(columns = columns, index=np.arange(len(unique_names)))
    scores_data["name"] = unique_names

    for date in dates:
        curr_data = r_data[r_data["date"] <= date]
        scores_data[date] = calculate_scores(curr_data, unique_names)["score"]

    return scores_data


def get_session_lengths(data : pd.DataFrame, stream_gap_threshold = 300):
    stream_gaps = pd.to_datetime(data["date"]).diff()[1:].apply(lambda td : td.seconds + td.microseconds / 10**6)
    stream_gaps = pd.concat([pd.Series([np.inf]), stream_gaps])
    session_lengths = list()
    for i, stream in data.iterrows():
        if stream_gaps[i] > stream_gap_threshold + stream["ms"]/1000:
            session_lengths.append([1, stream["ms"], stream["date"], None])
            continue
        session_lengths[-1][0] += 1
        session_lengths[-1][1] += stream["ms"]
        session_lengths[-1][3] = stream["date"]
    
    return session_lengths


def get_longest_sessions(data : pd.DataFrame, stream_gap_threshold = 300, top_n = 5):
    def get_session_info(session):
        session_start = pd.to_datetime(session[2])
        session_end = pd.to_datetime(session[3])
        try:
            session_data = data[data["date"].between(session_start, session_end, inclusive=True)]
        except Exception as e:
            print(session)
            raise(e)
        total_streams = session[0]
        total_playtime = session[1]
        time_delta = session_end - session_start
        session_duration = time_delta.seconds + time_delta.microseconds / 10**6
        tracks = session_data["track"].tolist()
        return [total_streams, total_playtime, str(session_start), str(session_end), session_duration, tracks]
    
    data["date"] = pd.to_datetime(data["date"])
    session_lengths = get_session_lengths(data, stream_gap_threshold)
    session_lengths.sort(key = lambda s : s[1], reverse = True)
    if top_n == -1: 
        top_n = len(session_lengths)
    
    sessions_info = list()
    for i in range(top_n):
        sessions_info.append(get_session_info(session_lengths[i]))

    return sessions_info


def get_blast_from_the_past(r_data : pd.DataFrame, unique_track_names, date_threshold_coeff = 0.4, top_old = 15, top_new = 30):
    """
    Finds tracks that were on the top at the beggining of the year but are not there now.
    r_data                  -- track relevance data
    unique_track_names      -- track names present in relevance data
    date_threshold_coeff    -- cutoff between 'old' and 'new' top. Default = 0.4
    top_old, top_new        -- cutoff for top tracks. Default = 15, 30,
        
    Example:
    For default values the function finds tracks that were TOP15 in the first 40% of the timespan but were not TOP30 in the last 60%
    """
    dates = r_data["date"]
    max_date = dates[np.floor(date_threshold_coeff * len(dates))]
    old_r_data = r_data.loc[r_data["date"] <= max_date]
    new_r_data = r_data.loc[r_data["date"] > max_date]
    old_top_tracks = calculate_scores(old_r_data, unique_track_names).sort_values(by="score", ascending=False).head(top_old)["name"]
    new_top_tracks = calculate_scores(new_r_data, unique_track_names).sort_values(by="score", ascending=False).head(top_new)["name"]
    return old_top_tracks[~old_top_tracks.isin(new_top_tracks)]


def get_stat_heatmap(data : pd.DataFrame, xAxisFunc = (lambda x : x.hour), yAxisFunc = (lambda x : x.dayofweek), valueFunc = (lambda x : x["track_uri"]), aggFunc = len):
    data = data.copy()
    data["xAxisGroup"] = data["date"].apply(xAxisFunc)
    data["yAxisGroup"] = data["date"].apply(yAxisFunc)
    data["value"] = valueFunc(data)
    return data\
        .groupby(["yAxisGroup", "xAxisGroup"])["value"]\
        .agg(stat = aggFunc).reset_index()\
        .pivot(index="yAxisGroup", columns = "xAxisGroup", values="stat")


def main():
    pass
    # data = pd.read_csv("Data/Parsed/JKX_parsed_data.csv", sep=';').sort_values(by="date")
    # get_longest_sessions(data)
    # meta_data = pd.read_csv("Data/Parsed/JKX_meta_data.csv", sep=';')
    # data = data[data["track"] == "Моя голова винтом (My head is spinning like a screw)"]
    # __get_dates_column(data, "week")
    
    # get_longest_sessions(data)
    # r_data, track_names = calculate_track_relevance(data, meta_data, 25, 30000)
    # get_blast_from_the_past(r_data, track_names)

if __name__ == '__main__':
    main()