from pages.plots.utils import load_data_line_chart
import plotly.graph_objects as go


def create_plot(me):
    df = load_data_line_chart(me)
    start_date = df[["time"]].head(1).iloc[0]["time"]
    end_date = df[["time"]].tail(1).iloc[0]["time"]

    trace1 = go.Scatter(x=df['time'][:2],
                        y=df['cumsum'][:2],
                        fill='tozeroy'
                        )
    frames = [dict(data=[dict(type='scatter',
                              x=df['time'][:k + 1],
                              y=df['cumsum'][:k + 1]),

                         ],
                   traces=[0],
                   ) for k in range(1, len(df) - 1)]
    layout = go.Layout(width=700,
                       height=600,
                       showlegend=False,
                       hovermode='x unified',
                       updatemenus=[
                           dict(
                               type='buttons', showactive=False,
                               y=1.05,
                               x=1.15,
                               xanchor='right',
                               yanchor='top',
                               pad=dict(t=0, r=10),
                               buttons=[dict(label='Play',
                                             method='animate',
                                             args=[None,
                                                   dict(frame=dict(duration=1.5,
                                                                   redraw=False),
                                                        transition=dict(duration=0),
                                                        fromcurrent=True,
                                                        mode='immediate')]
                                             )]
                           ),
                           dict(
                               type="buttons",
                               direction="left",
                               buttons=list([
                                   dict(
                                       args=[{"yaxis.type": "linear"}, {"visible": False}],
                                       label="LINEAR",
                                       method="relayout"
                                   ),
                                   dict(
                                       args=[{"yaxis.type": "log"}],
                                       label="LOG",
                                       method="relayout"
                                   )
                               ]),
                           ),
                       ]
                       )
    layout.update(title="How total number of messages with your friend changed over time?<br><sup>"
                        "Click Play and check...</sup>",
                  xaxis=dict(range=[start_date, end_date], autorange=False),
                  yaxis=dict(range=[0, max(df["cumsum"])], autorange=False)
                  )
    fig = go.Figure(data=[trace1], frames=frames, layout=layout)

    return fig
