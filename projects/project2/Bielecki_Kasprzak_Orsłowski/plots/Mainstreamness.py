from random import sample

import pandas as pd
import plotly.express as px

from Spotipy import get_sp


def popularity(df):
    """
    :param df: Dataframe with spotify streaming history
    :return: plotly.express figure (histogram plot)
    """
    songs = df
    sp = get_sp()
    print(songs)
    artists = []
    for song in songs.iterrows():
        artists.append(song[1].artistName)
    artists = list(set(artists))
    artists = sample(artists, 100)
    pop = []
    for artistName in artists:
        artist = sp.search(q=artistName, type="artist")
        if not artist["artists"]["items"]:
            pop.append(None)
            continue
        pop.append(artist["artists"]["items"][0]["popularity"])
    data = {'artist': artists,
            'popularity': pop}
    df = pd.DataFrame(data, columns=['artist', 'popularity'])
    df = df.groupby("popularity").count().reset_index()
    fig = px.histogram(df, x="popularity", y="artist", nbins=100,
                       title="Popularity distribution <br><sub>How popular are the artists you listen to?</sup>",
                       labels={"artist": "number of artists"},
                       color_discrete_sequence=['#CB772F'] * len(df))
    fig.update_layout(
        xaxis_title="Number of artists",
        yaxis_title="Popularity",
        font=dict(
            family="Courier New, monospace",
            size=18,
            color="#CB772F"
        ),
        paper_bgcolor='rgba(43, 43, 43, 1)',
        plot_bgcolor='rgba(43, 43, 43, 1)'
    )
    return fig