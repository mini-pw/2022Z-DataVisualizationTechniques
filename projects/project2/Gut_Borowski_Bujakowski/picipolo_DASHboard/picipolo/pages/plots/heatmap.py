from datetime import datetime
import numpy as np

import pandas as pd
from pandas.api.types import CategoricalDtype

import plotly.express as px
import plotly.graph_objs as go

from pages.plots.utils import load_data

day_names = ['Monday', 'Tuesday', 'Wednesday',
             'Thursday', 'Friday', 'Saturday', 'Sunday']


def preprocess_data(df: pd.DataFrame, me: str) -> pd.DataFrame:
    weekday_type = CategoricalDtype(categories=day_names, ordered=True)

    df['time'] = pd.to_datetime(df['time'])
    df['day_name'] = df['time'].dt.day_name()
    df['day_name'] = df['day_name'].astype(weekday_type)
    df['hour'] = df['time'].dt.hour

    df = df[df['who'] == me]

    return df


def prepare_data(df: pd.DataFrame, start: datetime, end: datetime, me: str) -> pd.DataFrame:
    df = preprocess_data(df, me)

    df = df[(df['time'].dt.date >= start) & (df['time'].dt.date <= end)]

    df = df.groupby(['day_name', 'hour']).size().to_frame('number_of_messages')

    mux = pd.MultiIndex.from_product([day_names, [i for i in range(24)]], names=['day_name', 'hour'])
    add_indices = mux.difference(df.index)

    df_with_missing_indices = pd.DataFrame(index=add_indices, columns=df.columns).fillna(0)
    df = pd.concat([df, df_with_missing_indices])

    df.reset_index(inplace=True)
    df.sort_values(['day_name', 'hour'], inplace=True)

    return df


def create_plotly_array(df: pd.DataFrame) -> np.ndarray:
    number_of_messages = df['number_of_messages'].values
    return np.array_split(number_of_messages, 7)


def create_heatmap(df: pd.DataFrame, start: datetime, end: datetime, me: str) -> go.Figure:
    df = prepare_data(df, start, end, me)
    arr = create_plotly_array(df)

    return px.imshow(arr,
                     labels=dict(x="Hour",
                                 y="Day of Week",
                                 color="Number of messages sent"),
                     y=day_names,
                     x=[str(i) for i in range(24)]
                     )


if __name__ == '__main__':
    data = load_data()

    start = data['time'].min()
    end = datetime.now()

    me = input('Please provide your full name: ')
    fig = create_heatmap(data, start, end, me=me)
    fig.show()
