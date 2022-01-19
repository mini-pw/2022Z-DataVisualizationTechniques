import base64
import io

import dash
import pandas as pd
import wordcloud
from dash import dcc
from dash import html
from dash.dependencies import Input, Output, State

from plots.Mainstreamness import popularity
from plots.favourite_artist import favourite_artist
from plots.song_most_skipped import most_skipped
from plots.when_listening_dist import when_listening_dist
from plots.wordcloud_maker import create_wordcloud
from utils.decorators import all_args_none
from utils.lyrics_getter import get_lyrics
from utils.readingfiles import parse_zip, get_streaming_history

app = dash.Dash(
    __name__,
    suppress_callback_exceptions=True
)
server = app.server


def wrap_with_custom_loading(children, style=None):
    if style is None:
        style = dict()
    return html.Div(
        children=[
            html.Br(),
            dcc.Loading(
                children=children,
                type="circle",
                color="#FFC66D",
            ),
            html.Br(),
        ],
        style=style
    )


app.layout = html.Div([
    # title:
    html.H2(
        "Upload your Spotify data, then choose date range to analyse or leave the default date.",
        style={
            'textAlign': 'center'
        }),
    # data storage:
    html.Div(id="all-data"),
    # upload button:
    dcc.Upload(
        id="upload-data",
        children=html.Button("Upload zipped Spotify data"),
        accept=".zip",
        multiple=False,
        style={
            'textAlign': 'center'
        }
    ),
    # date picker:
    html.Br(),
    html.Div(id="date-picker-div",
             style={
                 'textAlign': 'center'
             }),
    # favourite artist:
    html.Div(id="separator1"),
    wrap_with_custom_loading(
        html.Div(id="favourite-artist",
                 style={
                     'textAlign': 'center'
                 }),
    ),
    # most skipped song:
    html.Div(id="separator2"),
    wrap_with_custom_loading(
        html.Div(id="most-skipped",
                 style={
                     'textAlign': 'center'
                 }),
    ),
    # when do you listen:
    html.Div(id="separator3"),
    wrap_with_custom_loading(
        html.Div(id="when-listening",
                 style={
                     'textAlign': 'center'
                 })
    ),
    # popularity distribution:
    html.Div(id="separator4"),
    wrap_with_custom_loading(
        html.Div(id="popularity-dist",
                 style={
                     'textAlign': 'center'
                 })
    ),
    # wordcloud:
    html.Div(id="separator5"),
    html.Div(id="slider-div",
             style={
                 'textAlign': 'center',
             }),
    wrap_with_custom_loading(
        html.Div(id="wordcloud",
                 style={
                     'textAlign': 'center'
                 })
    ),
])


# ========== UPLOADING FILES ========== #


@app.callback(
    Output("all-data", "children"),
    Input("upload-data", "contents"))
@all_args_none(default_val=None)
def update_streaming_history(file_contents):
    streaming_history_df = get_streaming_history(parse_zip(file_contents))
    streaming_history_dict = streaming_history_df.to_dict()
    return [
        dcc.Store(id="streaming-history-storage",
                  data=streaming_history_dict),
        dcc.Store(id="streaming-history-dates-filtered"),
        dcc.Store(id="streaming-history-last-x-songs"),
    ]


@app.callback(
    Output("separator1", "children"),
    Output("separator2", "children"),
    Output("separator3", "children"),
    Output("separator4", "children"),
    Output("separator5", "children"),
    Input("streaming-history-storage", "data"))
def place_separators(streaming_history):
    return [html.Hr()] * 5


@app.callback(
    Output("date-picker-div", "children"),
    Input("streaming-history-storage", "data"))
@all_args_none(default_val=None)
def update_date_picker(streaming_history):
    df = pd.DataFrame(streaming_history)
    dates = pd.to_datetime(df["endTime"], format="%Y-%m-%d %H:%M")
    min_date = dates.min()
    max_date = dates.max()
    return [
        dcc.DatePickerRange(
            id="when-listening-date-range",
            min_date_allowed=min_date,
            max_date_allowed=max_date,
            initial_visible_month=max_date,
            start_date=min_date,
            end_date=max_date,
            display_format="D MMM YYYY",
        ),
    ]


@app.callback(
    Output("slider-div", "children"),
    Input("streaming-history-storage", "data"))
@all_args_none(default_val=None)
def update_slider(streaming_history):
    return [
        html.H3("Pick a number of songs to create the word cloud from:",
                style={
                    'textAlign': 'center'
                }),
        html.Div(
            dcc.Slider(
                id="last-x-songs-slider",
                min=5,
                max=20,
                marks={i: '{}'.format(i) for i in range(3, 21)},
                value=10,
            ),
            style={
                'padding-left': '30%',
                'padding-right': '30%',
            },
        ),
    ]


# ====== FILTERING DATA (sliders, date pickers, etc.) ====== #


@app.callback(
    Output("streaming-history-dates-filtered", "data"),
    Input("when-listening-date-range", "start_date"),
    Input("when-listening-date-range", "end_date"),
    State("streaming-history-storage", "data"))
def on_date_range_changed(start_date, end_date, streaming_history):
    df = pd.DataFrame(streaming_history)
    df["endTime"] = pd.to_datetime(df["endTime"], format="%Y-%m-%d %H:%M")
    df = df[(df["endTime"] > start_date) & (df["endTime"] < end_date)]
    return df.to_dict()


@app.callback(
    Output("streaming-history-last-x-songs", "data"),
    Input("last-x-songs-slider", "value"),
    State("streaming-history-storage", "data"))
def on_slider_changed(new_value, streaming_history):
    df = pd.DataFrame(streaming_history)
    df = df.tail(new_value)
    return df.to_dict()


# ========== FAVOURITE ARTIST ========== #


@app.callback(
    Output("favourite-artist", "children"),
    Input("streaming-history-dates-filtered", "data"))
@all_args_none(default_val=None)
def update_favourite_artist(streaming_history):
    streaming_history = pd.DataFrame(streaming_history)
    artist_name, img_url = favourite_artist(streaming_history)
    import time
    time.sleep(2.0)
    return [
        html.H3("The artist you\'ve listened to the most:"),
        html.H3(artist_name),
        html.Img(
            alt=f"Image of {artist_name}",
            src=img_url
        ),
    ]


# ========== MOST SKIPPED TRACK ========== #


@app.callback(
    Output("most-skipped", "children"),
    Input("streaming-history-dates-filtered", "data"))
@all_args_none(default_val=None)
def update_most_skipped(streaming_history):
    streaming_history = pd.DataFrame(streaming_history)
    cover_img_url, number_of_skips, track_name, artist_name = \
        most_skipped(streaming_history)
    return [
        html.H3('The song you\'ve skipped the most'),
        html.H3('during its first two seconds.'),
        html.H3(f'You\'ve skipped it {number_of_skips} times.'),
        html.Img(
            src=cover_img_url,
            alt="most skipped track"
        ) if cover_img_url != "" else html.Br(),
        html.H4(f'\"{track_name}\" by {artist_name}')
    ]


# ========== WHEN LISTENING DISTRIBUTION ========== #


@app.callback(
    Output("when-listening", "children"),
    Input("streaming-history-dates-filtered", "data"))
@all_args_none(default_val=None)
def update_when_listening(streaming_history):
    df = pd.DataFrame(streaming_history)
    return [
        html.H3('When do you listen to Spotify?'),
        dcc.Graph(
            id="when-listening-plot",
            figure=when_listening_dist(df),
        ),
    ]


# ========== POPULARITY DISTRIBUTION ==========#


@app.callback(
    Output("popularity-dist", "children"),
    Input("streaming-history-dates-filtered", "data"))
@all_args_none(default_val=None)
def update_popularity_distribution(streaming_history):
    df = pd.DataFrame(streaming_history)
    return [
        html.H3("How mainstream are you?"),
        dcc.Graph(
            id="popularity-distribution-plot",
            figure=popularity(df)
        )
    ]


# ========== WORD CLOUD ========== #


@app.callback(
    Output("wordcloud", "children"),
    [Input("streaming-history-last-x-songs", "data"),
     Input("last-x-songs-slider", "value")])
@all_args_none(default_val=None)
def update_wordcloud(streaming_history, slider_value):
    streaming_history = pd.DataFrame(streaming_history)
    wcloud: wordcloud.WordCloud = create_wordcloud(
        get_lyrics(streaming_history)
    )
    img = io.BytesIO()
    wcloud.to_image().save(img, format="png")
    data = base64.b64encode(img.getvalue()).decode("utf8")
    return [
        html.H4(f'The most common words in last {slider_value} songs you\'ve listened to:'),
        html.Img(
            src=f"data:image/png;base64,{data}",
            alt="Wordcloud of words in lyrics"
        )
    ]


if __name__ == '__main__':
    app.run_server(debug=True)
