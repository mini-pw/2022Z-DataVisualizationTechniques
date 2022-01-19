from pathlib import Path
import pandas as pd
from typing import Optional, Tuple


def load_data() -> Optional[pd.DataFrame]:
    file_path = Path(__file__).resolve()
    data_path = file_path.parents[3].joinpath(
        'data', 'user_data', 'parsed', 'messengerData.csv')

    if not data_path.is_file():
        return None

    df = pd.read_csv(data_path, delimiter=';')
    df['time'] = pd.to_datetime(df['time'])
    return df


def load_data_bar_chart() -> Optional[Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]]:
    file_path = Path(__file__).resolve()
    data_path = file_path.parents[3].joinpath(
        'data', 'user_data', 'parsed', 'friends.csv')

    if not data_path.is_file():
        return None

    df_friends_received = pd.read_csv(data_path, delimiter=";")

    file_path = Path(__file__).resolve()
    data_path = file_path.parents[3].joinpath(
        'data', 'user_data', 'parsed', 'friend_requests_received.csv')

    if not data_path.is_file():
        return None

    df_friends_waiting = pd.read_csv(data_path, delimiter=";")

    file_path = Path(__file__).resolve()
    data_path = file_path.parents[3].joinpath(
        'data', 'user_data', 'parsed', 'friend_requests_rejected.csv')

    if not data_path.is_file():
        return None

    df_friends_rejected = pd.read_csv(data_path, delimiter=";")

    file_path = Path(__file__).resolve()
    data_path = file_path.parents[3].joinpath(
        'data', 'user_data', 'parsed', 'friend_requests_sent.csv')

    if not data_path.is_file():
        return None

    df_friends_sent = pd.read_csv(data_path, delimiter=";")

    return df_friends_received, df_friends_waiting, df_friends_rejected, df_friends_sent


def load_data_line_chart(me) -> Optional[pd.DataFrame]:
    file_path = Path(__file__).resolve()
    data_path = file_path.parents[3].joinpath(
        'data', 'user_data', 'parsed', 'messengerData.csv')

    if not data_path.is_file():
        return None

    df = pd.read_csv(data_path, delimiter=";")
    df = df.loc[df["name"] == me]
    df['time'] = pd.to_datetime(df['time']).dt.strftime('%Y-%m-%d')
    df = df.groupby("time").size().reset_index(name='counts')
    df['cumsum'] = df['counts'].cumsum()
    df = df[["time", "cumsum"]]
    return df


def load_path_fonts():
    file_path = Path(__file__).resolve()
    data_path = file_path.parents[3].joinpath('data', 'fonts', 'Symbola.ttf')
    return data_path.as_posix()


def get_img_path(num: int):
    file_path = Path(__file__).resolve()
    data_path = file_path.parents[3].joinpath('data', 'instructions', f'instructions{num}.png')
    return data_path.as_posix()
