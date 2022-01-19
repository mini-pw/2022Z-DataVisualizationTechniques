import pandas as pd
import plotly.express as px


def prepare_data(df_friends_received, df_friends_waiting, df_friends_rejected, df_friends_sent):
    df_friends_received["time"] = pd.to_datetime(df_friends_received['time']).dt.strftime('%Y')
    df_friends_waiting["time"] = pd.to_datetime(df_friends_waiting['time']).dt.strftime('%Y')
    df_friends_rejected["time"] = pd.to_datetime(df_friends_rejected['time']).dt.strftime('%Y')
    df_friends_sent["time"] = pd.to_datetime(df_friends_sent['time']).dt.strftime('%Y')
    df_friends_received = df_friends_received.groupby("time").size().reset_index(name='counts')
    df_friends_waiting = df_friends_waiting.groupby("time").size().reset_index(name='counts')
    df_friends_rejected = df_friends_rejected.groupby("time").size().reset_index(name='counts')
    df_friends_sent = df_friends_sent.groupby("time").size().reset_index(name='counts')
    df2 = pd.merge(df_friends_received, df_friends_waiting, on='time', how='left')
    df2.rename(columns={"counts_x": "received", "counts_y": "waiting"}, inplace=True)
    df2 = pd.merge(df2, df_friends_rejected, on='time', how='left')
    df2 = pd.merge(df2, df_friends_sent, on='time', how='left')
    df2.rename(columns={"counts_x": "rejected", "counts_y": "sent"}, inplace=True)
    df2 = df2.fillna(0)
    # df2 = df2.set_index("time")
    return df2


# def df_to_plotly(df):
#     return {'z': df.values.tolist(),
#             'x': df.columns.tolist(),
#             'y': df.index.tolist()}
#

def create_chart(df_friends_received, df_friends_waiting, df_friends_rejected, df_friends_sent):
    # data = df_to_plotly(prepare_data())
    # z = np.array(data.get("z"))
    # z = z.T
    # fig = px.imshow(z, labels=dict(x="Year", y="Type of friends invitation", color="Amount"), x=data.get("y"),
    #                 y=data.get("x"))
    data = prepare_data(df_friends_received, df_friends_waiting, df_friends_rejected, df_friends_sent)
    fig1 = px.bar(data, x="time", y="received", )
    fig2 = px.bar(data, x="time", y="waiting")
    fig3 = px.bar(data, x="time", y="rejected")
    fig4 = px.bar(data, x="time", y="sent")
    fig1.update_layout(
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        title="Friend requests received",
        margin_b=90,
        annotations=[dict(xref='paper',
                          yref='paper',
                          x=0.5, y=-0.25,
                          showarrow=False,
                          text='On bar chart you can notice how many friend requests you have received each year')]
    )
    fig2.update_layout(
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        title="Friend requests waiting",
        margin_b=90,
        annotations=[dict(xref='paper',
                          yref='paper',
                          x=0.5, y=-0.25,
                          showarrow=False,
                          text='On bar chart you can notice how many friend requests are waiting for your approval '
                               'each year')]
    )
    fig3.update_layout(
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        title="Friend requests rejected",
        margin_b=90,
        annotations=[dict(xref='paper',
                          yref='paper',
                          x=0.5, y=-0.25,
                          showarrow=False,
                          text='On bar chart you can notice how many friend requests have been rejected each year')]
    )
    fig4.update_layout(
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        title="Friend requests sent",
        margin_b=90,
        annotations=[dict(xref='paper',
                          yref='paper',
                          x=0.5, y=-0.25,
                          showarrow=False,
                          text='On bar chart you can notice how many friend requests you have sent each year')]
    )
    return fig1, fig2, fig3, fig4
