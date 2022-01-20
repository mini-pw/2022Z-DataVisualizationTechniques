import plotly.express as px
import plotly.graph_objs as go
import pandas as pd
import numpy as np

PATH = 'Data'
GREEN = '#1ed760'
BG_COLOR = '#161616'
FUCHSIA = '#ff55cc'
DARK_FUCHSIA = '#421434'

def get_track_count_plot(initials = None, df = None):
    if df is None:
        try:
            df = pd.read_csv(f"{PATH}/Analysed/Score/{initials}_track_weekly.csv", sep=';', index_col=0)
        except:
            print("Could not find data file")
            return None

    df = df.groupby("fullStreams")["fullStreams"].agg(count = len).reset_index()

    fig = px.bar(df, 
        x = "fullStreams", 
        y = "count", 
        template="plotly_dark",
        # labels={
        #     'fullStreams': 'Number of full streams',
        #     'count': 'Number of songs',
        # },
    )

    fig.update_layout(
        font_family='spotiFont',
        plot_bgcolor=BG_COLOR,
        paper_bgcolor =BG_COLOR,
        title={
            'text': "Number of full streams<br><sup>For songs streamed at least 5 times</sup>",
            'y':0.9,
            'x':0.5,
            'xanchor': 'center',
            'yanchor': 'top'},
        xaxis_title = 'Number of songs',
        yaxis_title = 'Number of full streams'
    )
    fig.update_traces(
        marker_color=GREEN,
        hovertemplate = "Songs: %{y}<br>Full Streams: %{x}"
    )
    return fig


def get_retention_rate_vs_fullstreams_plot(initials = None, df = None):
    if df is None:
        try:
            df = pd.read_csv(f"{PATH}/Analysed/Score/{initials}_track_weekly.csv", sep=';', index_col=0)
        except:
            print("Could not find data file")
            return None


    Q1 = df['retentionRate'].quantile(0.25)
    Q3 = df['retentionRate'].quantile(0.75)
    IQR = Q3 - Q1
    df = df.loc[df['retentionRate'].between(Q1 - 1.5 * IQR, Q3 + 1.5 * IQR)]
    fig = px.scatter(df, 
        x = "retentionRate", 
        y = "fullStreams", 
        template="plotly_dark",
        custom_data=["name"]
        )

    fig.update_layout(
        font_family = 'spotiFont',
        plot_bgcolor = BG_COLOR,
        paper_bgcolor = BG_COLOR,
        title={
            'text': "Full Streams vs Retention Rate<br><sup>with removed outliers</sup>",
            'y':0.9,
            'x':0.5,
            'xanchor': 'center',
            'yanchor': 'top'},
        xaxis_title = 'Retention Rate',
        yaxis_title = 'Full Streams'
    )
    fig.update_traces(
        marker = {
            'color': GREEN,
            'opacity': 0.5
        },
        hovertemplate = "Full Streams: %{y}<br>Retention Rate: %{x}<br>Song Name: %{customdata}"
    )
    fig.add_trace(go.Scatter(
        x=[1, 1],
        y=[0, df["fullStreams"].max()*1.1],
        mode="lines",
        hovertemplate = "RR=1 Line <extra></extra>",
        line=go.scatter.Line(color=FUCHSIA),
        showlegend=False))

    return fig


def get_streams_per_month_plot(initials = None, df = None):
    if df is None:
        try:
            df = pd.read_csv(f"{PATH}/Parsed/{initials}_parsed_data.csv", sep=';')
            df["date"] = pd.to_datetime(df["date"]).dt.tz_localize(None)
            df = df.sort_values(by = "date").reset_index(drop = True)
        except:
            print("Could not find data file")
            return None

    df = df[df["ms"] > 30000]
    df["month"] = df["date"].apply(lambda d : pd.Timestamp(year = d.year, month = d.month, day = 1))
    df = df.groupby("month")["month"].agg(count = len).reset_index()

    fig = px.bar(df,
        x = "month",
        y = "count",
        template="plotly_dark",
    )
    fig.update_layout(
        font_family='spotiFont',
        plot_bgcolor=BG_COLOR,
        paper_bgcolor =BG_COLOR,
        title={
            'text': "Number of streams per month<br><sup>Excluding skipped (under 30s)</sup>",
            'y':0.9,
            'x':0.5,
            'xanchor': 'center',
            'yanchor': 'top'},
        xaxis_title = 'Month',
        yaxis_title = 'Number of streams'
    )
    fig.update_traces(
        marker_color=GREEN,
        hovertemplate = "Streams: %{y}<br>Month: %{x}"
    )

    return fig


def get_mood_heatmap_plot(initials = None, pdf = None, mdf = None, choice = None):
    if pdf is None or mdf is None:
        try:
            pdf = pd.read_csv(f"{PATH}/Parsed/{initials}_parsed_data.csv", sep=';')
            pdf["date"] = pd.to_datetime(pdf["date"]).dt.tz_localize(None)
            pdf = pdf.sort_values(by = "date").reset_index(drop = True)
            mdf = pd.read_csv(f"{PATH}/Parsed/{initials}_meta_data.csv", sep=';')
        except Exception as e:
            print(e)
            print("Could not find data file")
            return None
    
    pdf = pdf[['date', 'track', 'artist']]
    mdf = mdf[['trackName', 'primaryArtist', choice]]

    df = pd.merge(pdf, mdf, left_on=['track', 'artist'], right_on=['trackName', 'primaryArtist'], how='left')
    df['hour'] = df['date'].dt.hour
    df['dayofweek'] = df['date'].dt.dayofweek

    df = df.groupby(['hour', 'dayofweek']).agg(
        summed = pd.NamedAgg(choice, sum),
        count = pd.NamedAgg(choice, len)
    ).reset_index()
    df['avg'] = (df['summed'] / df['count'])
    df = df.pivot(index="dayofweek", columns = "hour", values="avg")#.fillna(0)
    

    fig = px.imshow(df, 
        color_continuous_scale=[BG_COLOR, GREEN], 
        template="plotly_dark")
    fig.update_layout(
        font_family='spotiFont',
        plot_bgcolor=DARK_FUCHSIA,
        paper_bgcolor =BG_COLOR,
        title={
            'text': f"Average {choice}<br><sup>per hour and day of week</sup>",
            'y':0.9,
            'x':0.5,
            'xanchor': 'center',
            'yanchor': 'top'},
        xaxis_title = 'Hour',
        yaxis_title = 'Day of Week',
        yaxis = dict(
            tickmode = 'array',
            tickvals = np.arange(7),
            ticktext = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
        ),
        xaxis_showgrid=False, 
        yaxis_showgrid=False,
        xaxis_zeroline=False, 
        yaxis_zeroline=False
    )
    fig.update_traces(
        hovertemplate = "Day of Week: %{y}<br>Hour: %{x}" + f"<br>{choice}: " + "%{z}<extra></extra>"
    )

    return fig


def get_streams_heatmap_plot(initials = None, df = None):
    if df is None:
        try:
            df = pd.read_csv(f"{PATH}/Parsed/{initials}_parsed_data.csv", sep=';')
            df["date"] = pd.to_datetime(df["date"]).dt.tz_localize(None)
            df = df.sort_values(by = "date").reset_index(drop = True)
        except Exception as e:
            print(e)
            print("Could not find data file")
            return None
    
    df['hour'] = df['date'].dt.hour
    df['dayofweek'] = df['date'].dt.dayofweek

    df = df.groupby(['hour', 'dayofweek']).agg(
        count = pd.NamedAgg("hour", len)
    ).reset_index()
    df = df.pivot(index="dayofweek", columns = "hour", values="count").fillna(0)

    fig = px.imshow(df, color_continuous_scale=[BG_COLOR, GREEN], template="plotly_dark")
    fig.update_layout(
        font_family='spotiFont',
        plot_bgcolor=DARK_FUCHSIA,
        paper_bgcolor =BG_COLOR,
        title={
            'text': f"Total streams<br><sup>per hour and day of week</sup>",
            'y':0.9,
            'x':0.5,
            'xanchor': 'center',
            'yanchor': 'top'},
        xaxis_title = 'Hour',
        yaxis_title = 'Day of Week',
        yaxis = dict(
            tickmode = 'array',
            tickvals = np.arange(7),
            ticktext = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
        ),
        xaxis_showgrid=False, 
        yaxis_showgrid=False,
        xaxis_zeroline=False, 
        yaxis_zeroline=False
    )
    fig.update_traces(
        hovertemplate = "Day of Week: %{y}<br>Hour: %{x}<br>Streams: %{z}<extra></extra>"
    )

    return fig


def get_top_songs_plot(initials = None, df = None):
    if df is None:
        try:
            df = pd.read_csv(f"{PATH}/Analysed/Score/{initials}_track_weekly.csv", sep=';', index_col=0)
        except:
            print("Could not find data file")
            return None
    
    df = df.nlargest(150, 'score')
    df['score'] = df['score'].round(2)
    df['amortisedConsistency'] = df['amortisedConsistency'].round(2)
    # df['artist'] = df['name'].apply(lambda x : re.search('\(.*\)$', x).group(0).strip('()'))

    fig = px.scatter(df,
        x = 'fullStreams',
        y = 'amortisedConsistency',
        size = 'score',
        custom_data=["name", "score"],
        template="plotly_dark")

    fig.update_layout(
        font_family = 'spotiFont',
        plot_bgcolor = BG_COLOR,
        paper_bgcolor = BG_COLOR,
        title={
            'text': "Full Streams vs Retention Rate<br><sup>with removed outliers</sup>",
            'y':0.9,
            'x':0.5,
            'xanchor': 'center',
            'yanchor': 'top'},
        xaxis_title = 'Full Streams',
        yaxis_title = 'Consistency'
    )
    fig.update_traces(
        marker = {
            'color': GREEN,
            'opacity': 0.5
        },
        hovertemplate = "Song Name: %{customdata[0]}<br>Score: %{customdata[1]}<br>Consistency: %{y}<br>Full Streams: %{x}<br><extra></extra>"
    )

    return fig