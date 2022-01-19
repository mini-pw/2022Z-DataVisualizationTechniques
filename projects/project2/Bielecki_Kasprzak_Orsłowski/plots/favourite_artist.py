from typing import Tuple

import pandas as pd

from Spotipy import get_sp


def favourite_artist(df: pd.DataFrame, size: int = 2) -> Tuple[str, str]:
    """
    Returns a name and an URL to an image of the most listened to artist

    :param size: An integer of value 0, 1 or 2 (default). It determines
        the size of an image (0 - smallest, 2 - biggest).
    :param df: DataFrame with spotify streaming history
    :return: tuple containing artist's name and image URL separated by
        "|" character
    """
    size = 2 - size  # correction -
    # without it, 0 would mean the biggest image, which is counter-intuitive
    sp = get_sp()
    artist_name: str = df[["artistName", "trackName"]] \
        .groupby("artistName") \
        .agg("count") \
        .rename(columns={"trackName": "count"}) \
        .sort_values("count", ascending=False) \
        .reset_index()["artistName"][0]
    image_url: str = sp.search(
        q=artist_name,
        limit=1,
        type="artist"
    )["artists"]["items"][0]["images"][size]['url']
    return artist_name, image_url
