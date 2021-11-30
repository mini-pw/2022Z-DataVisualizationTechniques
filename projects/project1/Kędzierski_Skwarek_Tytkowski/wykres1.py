import plotly.express as px
import pandas as pd

df = px.data.gapminder().query("year==2007")
df2 = pd.read_excel("C:/Users/domin/Desktop/TWD-PROJEKT1/drzewa2.xlsx")

df2 = df2[["Country Name", "2000","2020"]]
df2 = df2.rename(columns = {'Country Name': 'country'}, inplace = False)
df2["Difference"] = ((df2["2020"]/df2["2000"])-1)*100

data = pd.merge(df, df2, on='country')



fig = px.choropleth(data, locations="iso_alpha",
                    color="Difference",
                    hover_name="country",
                    range_color=(-30,30),
                    color_continuous_scale=px.colors.diverging.RdYlGn)

fig.update_layout(
    font=dict(
        family="Courier New, monospace",
        size=25,
        color="rgba(1,127,56,255)",

    ),
    title={
        'text': "<b>Afforestation percentage difference 2020 - 2000<b>",
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
    plot_bgcolor = 'rgba(255,246,219,255)',
    annotations=[dict(
        x=0.55,
        y=0.0,
        xref='paper',
        yref='paper',
        text='<b>Source: https://data.worldbank.org/indicator/AG.LND.FRST.ZS<b>',
        font = dict(
            color="rgba(1,127,56,255)")
    )]
)



print(df)
print(df2)
print(data)
fig.show()





