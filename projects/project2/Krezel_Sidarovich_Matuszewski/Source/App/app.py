from dash import dcc
from dash import html
from dash.dependencies import Input, Output
from dash.exceptions import PreventUpdate
from dash_extensions.enrich import MultiplexerTransform, DashProxy
import plotly.express as px
import pandas as pd
from src.custom_components import CCDropdown
import src.plots as pl
from src.page_content import PageContent as PC

pc = PC()

# APP LAYOUT
app = DashProxy(__name__, prevent_initial_callbacks=True, transforms=[MultiplexerTransform()],suppress_callback_exceptions=True, title='SpotiMy')

app.layout = html.Div(children=[
    html.Div(id='debug_div', style={'display':'none'}),
    pc.page_header,
    html.Div(id='page_div', 
        children = [
            pc.visualisations_page,
            pc.data_page,
            pc.about_page
        ]),
    ])

# APP CALLBACKS

@app.callback(
    Output('page_div', 'children'),
    Input(pc.visualisations_button.id, 'n_clicks'))
def on_visualisations_button_click(n_clicks):
    pc.set_current_page(pc.visualisations_page)
    return [
        pc.visualisations_page,
        pc.data_page,
        pc.about_page
    ]


@app.callback(
    Output('page_div', 'children'),
    Input(pc.data_button.id, 'n_clicks'))
def on_data_button_click(n_clicks):
    pc.set_current_page(pc.data_page)
    return [
        pc.visualisations_page,
        pc.data_page,
        pc.about_page
    ]


@app.callback(
    Output('page_div', 'children'),
    Input(pc.about_button.id, 'n_clicks'))
def on_about_button_click(n_clicks):
    pc.set_current_page(pc.about_page)
    return [
        pc.visualisations_page,
        pc.data_page,
        pc.about_page
    ]


@app.callback([Output(pc.user_dropdown.id+'_button', 'children'),
    Output(pc.id_visualisations_content, 'children'),
    Output(pc.id_visualisations_date_slider_container, 'children')],
    Input(pc.user_dropdown.options[pc.JK].id, 'n_clicks'),
    Input(pc.user_dropdown.options[pc.SS].id, 'n_clicks'),
    Input(pc.user_dropdown.options[pc.SM].id, 'n_clicks'))
def on_user_dropdown_click(*args):
    new_value, label = pc.user_dropdown.on_click(args[0:])
    if new_value == None:
        raise PreventUpdate       
    pc.user_dropdown.reload_label()
    output = tuple([
        pc.user_dropdown.full_label,
        # pc.current_visualisations_tab[new_value],
        [pc.overview_tabs[pc.user_dropdown.value],
        pc.stats_tabs[pc.user_dropdown.value],
        pc.recommend_tabs[pc.user_dropdown.value],
        pc.mood_tabs[pc.user_dropdown.value]],
        pc.date_sliders[new_value],
    ])
    return output


@app.callback(Output(pc.af_dropdown.id+'_button', 'children'),
    Output(f"chart_3", "figure"),
    [Input(pc.af_dropdown.options['danceability'].id, 'n_clicks'),
    Input(pc.af_dropdown.options['energy'].id, 'n_clicks'),
    Input(pc.af_dropdown.options['valence'].id, 'n_clicks'),
    Input(pc.af_dropdown.options['tempo'].id, 'n_clicks'),
    Input(pc.af_dropdown.options['instrumentalness'].id, 'n_clicks'),
    Input(pc.af_dropdown.options['speechiness'].id, 'n_clicks'),
    Input(pc.af_dropdown.options['acousticness'].id, 'n_clicks')])
def on_af_dropdown_click(*args):
    new_value, label = pc.af_dropdown.on_click(args[0:])
    if new_value == None:
        raise PreventUpdate       
    pc.af_dropdown.reload_label()
    output = tuple([
        pc.af_dropdown.full_label,
        pl.get_mood_heatmap_plot(pdf = pc.data[pc.user_dropdown.value], mdf = pc.meta_data[pc.user_dropdown.value], choice = new_value)
    ])
    return output


@app.callback(
    Output(pc.id_visualisations_content, 'children'),
    Input(pc.id_overview_button, 'n_clicks'))
def on_overview_button_click(n_clicks):
    if n_clicks == None or n_clicks == 0:
        raise PreventUpdate
    pc.set_current_visualisations_tab(pc.overview_tabs)
    output = [
        pc.overview_tabs[pc.user_dropdown.value],
        pc.stats_tabs[pc.user_dropdown.value],
        pc.recommend_tabs[pc.user_dropdown.value],
        pc.mood_tabs[pc.user_dropdown.value]
    ]
    return output


@app.callback(
    Output(pc.id_visualisations_content, 'children'), 
    Input(pc.id_stats_button, 'n_clicks'))
def on_stats_button_click(n_clicks):
    if n_clicks == None or n_clicks == 0:
        raise PreventUpdate
    pc.set_current_visualisations_tab(pc.stats_tabs)
    output = [
        pc.overview_tabs[pc.user_dropdown.value],
        pc.stats_tabs[pc.user_dropdown.value],
        pc.recommend_tabs[pc.user_dropdown.value],
        pc.mood_tabs[pc.user_dropdown.value]
    ]
    return output


@app.callback(
    Output(pc.id_visualisations_content, 'children'),
    Input(pc.id_recommend_button, 'n_clicks'))
def on_top_button_click(n_clicks):
    if n_clicks == None or n_clicks == 0:
        raise PreventUpdate
    pc.set_current_visualisations_tab(pc.recommend_tabs)
    output = [
        pc.overview_tabs[pc.user_dropdown.value],
        pc.stats_tabs[pc.user_dropdown.value],
        pc.recommend_tabs[pc.user_dropdown.value],
        pc.mood_tabs[pc.user_dropdown.value]
    ]
    return output

@app.callback(
    Output(pc.id_visualisations_content, 'children'),
    Input(pc.id_mood_button, 'n_clicks'))
def on_mood_button_click(n_clicks):
    if n_clicks == None or n_clicks == 0:
        raise PreventUpdate
    pc.set_current_visualisations_tab(pc.mood_tabs)
    output = [
        pc.overview_tabs[pc.user_dropdown.value],
        pc.stats_tabs[pc.user_dropdown.value],
        pc.recommend_tabs[pc.user_dropdown.value],
        pc.mood_tabs[pc.user_dropdown.value]
    ]
    return output



@app.callback(
    Output(pc.id_overview_tab[pc.JK] + '_dynamic_text', 'children'),
    Output(f"{pc.JK}_chart_2", 'figure'),
    Output(f"{pc.JK}_chart_4", 'figure'),
    #Output('chart_3', 'figure'),
    Input(pc.date_sliders[pc.JK].id, 'value'))
def on_jk_date_slider_value_changed(value):
    ts = pc.data[pc.JK].iloc[0]["date"]
    range_start = pd.Timestamp(year = ts.year, month = ts.month, day = 1) + pd.DateOffset(months = value[0])
    range_end = pd.Timestamp(year = ts.year, month = ts.month, day = 1) + pd.DateOffset(months = value[1])
    range_df = pc.data[pc.JK][pc.data[pc.JK]["date"].between(range_start, range_end, inclusive=True)]

    output = tuple ([
        dcc.Markdown(pc.get_overview_dynamic(pc.JK, range_start, range_end)),
        pl.get_streams_per_month_plot(df = range_df),
        pl.get_streams_heatmap_plot(df = range_df),
        # pl.get_mood_heatmap_plot(pdf = range_df, mdf = pc.meta_data[pc.user_dropdown.value], choice = pc.af_dropdown.value)
    ])
    return output

@app.callback(
    Output(pc.id_overview_tab[pc.SS] + '_dynamic_text', 'children'),
    Output(f"{pc.SS}_chart_2", 'figure'),
    Output(f"{pc.SS}_chart_4", 'figure'),
    #Output(f"chart_3", "figure"),
    [Input(pc.date_sliders[pc.SS].id, 'value')])
def on_ss_date_slider_value_changed(value):
    ts = pc.data[pc.SS].iloc[0]["date"]
    range_start = pd.Timestamp(year = ts.year, month = ts.month, day = 1) + pd.DateOffset(months = value[0])
    range_end = pd.Timestamp(year = ts.year, month = ts.month, day = 1) + pd.DateOffset(months = value[1])
    range_df = pc.data[pc.SS][pc.data[pc.SS]["date"].between(range_start, range_end, inclusive=True)]

    output = tuple ([
        dcc.Markdown(pc.get_overview_dynamic(pc.SS, range_start, range_end)),
        pl.get_streams_per_month_plot(df = range_df),
        pl.get_streams_heatmap_plot(df = range_df),
        #pl.get_mood_heatmap_plot(pdf = range_df, mdf = pc.meta_data[pc.user_dropdown.value], choice = pc.af_dropdown.value)
    ])
    return output

@app.callback(
    Output(pc.id_overview_tab[pc.SM] + '_dynamic_text', 'children'),
    Output(f"{pc.SM}_chart_2", 'figure'),
    Output(f"{pc.SM}_chart_4", 'figure'),
    #Output(f"{pc.}")
    #Output(f"chart_3", "figure"),
    [Input(pc.date_sliders[pc.SM].id, 'value')])
def on_sm_date_slider_value_changed(value):
    ts = pc.data[pc.SM].iloc[0]["date"]
    range_start = pd.Timestamp(year = ts.year, month = ts.month, day = 1) + pd.DateOffset(months = value[0])
    range_end = pd.Timestamp(year = ts.year, month = ts.month, day = 1) + pd.DateOffset(months = value[1])
    range_df = pc.data[pc.SM][pc.data[pc.SM]["date"].between(range_start, range_end, inclusive=True)]

    output = tuple ([
        dcc.Markdown(pc.get_overview_dynamic(pc.SM, range_start, range_end)),
        pl.get_streams_per_month_plot(df = range_df),
        pl.get_streams_heatmap_plot(df = range_df),
        #pl.get_mood_heatmap_plot(pdf = range_df, mdf = pc.meta_data[pc.user_dropdown.value], choice = pc.af_dropdown.value)
    ])
    return output
            
    


@app.callback(
    [Output(f"{key}_slider_label", 'children') for key in list(pc.af_sliders.keys())],
    [Input(pc.af_sliders[key].id, 'drag_value') for key in list(pc.af_sliders.keys())]
)
def on_af_slider_value_changing(*args):
    keys = list(pc.af_sliders.keys())
    output = tuple([
        f"{keys[i]}: {pc.af_sliders[keys[i]].value if args[i] is None else args[i]}" for i in range(len(keys))
    ])
    return output

@app.callback(
    Output('recommend_text', 'children'),
    [Input(pc.af_sliders[key].id, 'value') for key in list(pc.af_sliders.keys())]
)
def on_af_sliders_value_changed(*args):
    return dcc.Markdown(pc.get_recommendation_text(pc.user_dropdown.value, *args))


    

if __name__ == '__main__':
    app.run_server(debug=False)