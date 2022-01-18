import datetime

import numpy as np
import pandas as pd
import streamlit as st


@st.experimental_memo(show_spinner=False)
def przygotowanieDanychHeatmap(df, startingDate, endingDate):
    data = df

    startingDate = pd.to_datetime(startingDate)
    endingDate = pd.to_datetime(endingDate)

    data = data[["id", "author", "date", "hour"]]

    data.date = data.date.str.split("T").str.get(0)
    data["date"] = pd.to_datetime(data["date"])

    data = data.loc[(data["date"] >= startingDate) & (data["date"] <= endingDate)]

    data.date = [day.strftime('%Y-%m-%d') for day in data.date]
    data = data.loc[data["author"] != ""]
    data = data.groupby(["date", "hour"], as_index=False).agg({"id": "count"})

    days = pd.date_range(start=startingDate, end=endingDate, freq='1D')
    days = days.strftime('%Y-%m-%d')
    days = pd.DataFrame(days)[0].tolist()
    days = sorted(24 * days)

    period = (endingDate - startingDate) // np.timedelta64(1, 'D') + 1

    hours = [i for i in range(24)] * period

    result = {"date": days, "hour": hours}
    result = pd.DataFrame(result)

    result = pd.merge(result, data, how="outer", on=["date", "hour"])
    result["id"] = result["id"].fillna(0)
    result["week_day"] = pd.to_datetime(result["date"])
    result["week_day"] = [datetime.datetime.weekday(date) for date in result["week_day"]]

    result = result.groupby(["week_day", "hour"], as_index=False).agg({"id": "mean"})
    result = result.rename(columns={"id": "sum_mean_id"})

    return result
