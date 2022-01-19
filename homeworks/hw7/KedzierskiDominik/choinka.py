import plotly.express as px
import random
import pandas as pd
from PIL import Image

df = pd.DataFrame(columns=['X', 'Y', 'Kategoria'])
for i in range(200):
    x = random.uniform(-3,3)
    y = random.uniform(-2,2)
    df.loc[i] = [x,y,'pien']

for i in range(200,3200):
    x = random.uniform(3,35)
    y = random.uniform(-20,20)
    if abs(y) < (35-x)/1.5 and x < 25:
        df.loc[i] = [x,y,'drzewko']
for i in range(3200,6000):
    x = random.uniform(25,50)
    y = random.uniform(-20,20)
    if abs(y) < (50-x)/2 and x < 48:
        df.loc[i] = [x,y,'drzewko']

for i in range(6000,6400):
    x = random.uniform(48,55)
    y = random.uniform(-10,10)
    if abs(y) < (55-x)/2:
        df.loc[i] = [x,y,'drzewko']

bombki = ["bombka1","bombka2","bombka3","bombka4"]
#bombki
for i in range(6400,6480):
    x = random.uniform(3,35)
    y = random.uniform(-20,20)
    if abs(y) < (35-x)/1.5 and x < 25:
        df.loc[i] = [x,y,bombki[random.randint(0,3)]]
for i in range(6480,6580):
    x = random.uniform(25,50)
    y = random.uniform(-20,20)
    if abs(y) < (50-x)/2 and x < 48:
        df.loc[i] = [x,y,bombki[random.randint(0,3)]]
for i in range(6580,6630):
    x = random.uniform(48,55)
    y = random.uniform(-10,10)
    if abs(y) < (55-x)/2:
        df.loc[i] = [x,y,bombki[random.randint(0,3)]]


df2 = px.data.iris()
fig = px.scatter(df, x="Y", y="X", color="Kategoria",
                 symbol_sequence=['hexagon', 'star-diamond', 'circle', 'circle', 'circle', 'circle'],
                 color_discrete_sequence=['brown', 'green', 'blue', 'red', 'yellow', 'purple'],

                )
fig.update_traces(marker=dict(size=12),
                  selector=dict(mode='markers'))
img = Image.open('gwiazdka.png')
fig.add_layout_image(
    dict(
        source=img,
        xref='paper', yref='paper',
        x=0.5, y=0.90,
        sizex=0.1, sizey=0.1,
        xanchor='center', yanchor='bottom'
    )
)
img2 = Image.open('tlo.png')
fig.add_layout_image(
        dict(
            source=img2,
            xref="x",
            yref="y",
            x=-80,
            y=60,
            sizex=190,
            sizey=70,
            sizing="stretch",
            layer="below"
        )
)
fig.update_layout(showlegend=False)
fig.update_xaxes(showgrid=False)
fig.update_yaxes(showgrid=False)
fig.update_yaxes(
    scaleanchor = "x",
    scaleratio = 1,
  )
fig.show()