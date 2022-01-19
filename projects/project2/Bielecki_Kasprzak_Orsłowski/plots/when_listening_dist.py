import pandas as pd
import plotly.express as px


def when_listening_dist(df: pd.DataFrame):
    """
    Returns a barplot showing distribution of when during a day music
    was played

    :param df: DataFrame with spotify streaming history
    :return: Plotly barplot
    """
    df = df.copy()
    # df["hour"] = df["endTime"].apply(
    #     lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M").hour)
    df["endTime"] = pd.to_datetime(df["endTime"], format="%Y-%m-%d %H:%M")
    df["hour"] = df["endTime"].apply(
        lambda date: date.hour)
    df = df[["hour", "artistName"]] \
        .groupby("hour") \
        .agg("count") \
        .reset_index() \
        .rename(columns={"artistName": "count"})
    # add missing hours
    for i in range(24):
        if i not in df["hour"].values:
            df = df.append({"hour": i, "count": 0}, ignore_index=True)
    df = df.sort_values("hour", ignore_index=True)
    # format the interval
    # df = df.assign(hourInterval=lambda x: x["hour"].astype(str) + " - " +
    #                                       ((x["hour"] + 1) % 24).astype(str))[
    #     ["hourInterval", "count"]]
    df["hour"] = df["hour"].astype(str)
    fig = px.bar(
        data_frame=df,
        x="hour",
        y="count",
        labels={
            "hour": "Hour interval",
            "count": "Number of tracks listened to",
        },
        color_discrete_sequence=['#CB772F'] * len(df),
        title="Number of songs you\'ve listened to in given time intervals"
    )
    fig.update_layout(paper_bgcolor='rgba(43, 43, 43, 1)',
                      plot_bgcolor='rgba(43, 43, 43, 1)',
                      font=dict(
                          family="Courier New, monospace",
                          size=14,
                          color="#CB772F")
                      )
    return fig
