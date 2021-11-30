import plotly.express as px
import pandas as pd


df2 = pd.read_excel("C:/Users/domin/Desktop/TWD-PROJEKT1/drzewa4.xlsx")
df2 = df2[["Country Name", "2000","2020"]]
df2 = df2.rename(columns = {'Country Name': 'country'}, inplace = False)
df2["Afforestation"] = df2["2000"]-df2["2020"]
df2 = df2.loc[df2["Afforestation"] > 0]
df2 = df2[["country","Afforestation"]].sort_values(by=["Afforestation"])
df2 = df2.iloc[[-16,-23,-25,-26,-27,-28,-30, -31], :]

fig = px.bar(df2, x="country", y="Afforestation")


fig.update_traces(marker_color='red', marker_line_color='rgb(8,48,107)',
                  marker_line_width=1.5, opacity=0.95 )
fig.update_layout(
    font=dict(
        family="Courier New, monospace",
        size=25,
        color="rgba(1,127,56,255)",

    ),
    title={
        'text': "<b>Afforestation km^2  2020 - 2000<b>",
        'y': 0.98,
        'x': 0.5,
        'xanchor': 'center',
        'yanchor': 'top'},
    legend=dict(
        font=dict(
            size=20,
            color="rgba(1,127,56,255)",)
        ),
    geo=dict(bgcolor= 'rgba(255,246,219,255)'),
    margin = {"r": 0, "t": 40, "l": 0, "b": 0},
    paper_bgcolor = 'rgba(255,246,219,255)',
    plot_bgcolor = 'rgba(255,246,219,255)'
)


print(df2)
fig.show()





