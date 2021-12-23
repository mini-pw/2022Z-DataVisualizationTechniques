import collections
import dash
import pandas as pd

from dash.dependencies import Output, Input
from dash.exceptions import PreventUpdate

import dash_html_components as html
import dash_core_components as dcc
import DF

app = dash.Dash(__name__)

df = DF.get_df()

category = set(df['category'])


app.layout = html.Div([
    dcc.Store(id='memory-output'),
    dcc.Dropdown(id='memory-category', options=[
        {'value': x, 'label': x} for x in category
    ], multi=True, value=['Chemistry', 'Physiology or Medicine']),
    dcc.Dropdown(id='memory-field', options=[
        {'value': 'age', 'label': 'Age'},
        {'value': 'prizeAmount', 'label': 'Prize'},
    ], value='age'),
    html.Div([
        dcc.Graph(id='memory-graph')
    ])
])


@app.callback(Output('memory-output', 'data'),
              Input('memory-category', 'value'))
def filter_category(category_selected):
    if not category_selected:
        return df.to_dict('records')

    filtered = df.query('category in @category_selected')
    return filtered.to_dict('records')


@app.callback(Output('memory-graph', 'figure'),
              Input('memory-output', 'data'),
              Input('memory-field', 'value'))
def on_data_set_graph(data, field):
    if data is None:
        raise PreventUpdate

    aggregation = collections.defaultdict(
        lambda: collections.defaultdict(list)
    )

    for row in data:

        a = aggregation[row['category']]

        a['name'] = row['category']
        a['mode'] = 'lines+markers'

        a['y'].append(row[field])
        a['x'].append(row['awardYear'])

    return {
        'data': [x for x in aggregation.values()]
    }


if __name__ == '__main__':
    app.run_server(debug=True, threaded=True, port=10450)
