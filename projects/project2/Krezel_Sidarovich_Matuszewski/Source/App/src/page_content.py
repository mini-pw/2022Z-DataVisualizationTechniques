from dash import dcc
from dash import html
from matplotlib.pyplot import figure
from src.custom_components import CCDropdown
import src.plots as pl
import pandas as pd
import numpy as np
import random

import sys
sys.path.append("Source")
import Analysers


class PageContent():

    def __init__(self) -> None:
        self.JK = 'JKX'
        self.SS = 'SSX'
        self.SM = 'SMX'
        self.initials = [self.JK, self.SS, self.SM]
        self.__init_data()
        self.__init_ids()
        self.__init_dropdowns()
        self.__init_sliders()
        self.__init_buttons()
        self.__init_text_buttons()
        self.__init_charts()
        self.__init_text()
        self.__init_content()

    def __init_data(self):
        def get_data(initials):
            data = pd.read_csv(f"Data/Parsed/{initials}_parsed_data.csv", sep=';')
            data["date"] = pd.to_datetime(data["date"]).dt.tz_localize(None)
            data = data.sort_values(by="date")
            return data
        def get_meta_data(initials):
            meta_data = pd.read_csv(f"Data/Parsed/{initials}_meta_data.csv", ';')
            return meta_data
        def get_score_data(initials):
            data = pd.read_csv(f"Data/Analysed/Score/{initials}_track_weekly.csv", sep=';')
            data = data.sort_values(by="score")
            return data
        def get_track_r_data(initials):
            r_data = pd.read_csv(f"Data/Analysed/RelevanceData/{initials}_track_r_data.csv", sep = ';')
            r_data["date"] = pd.to_datetime(r_data["date"]).dt.tz_localize(None)
            track_names = []
            with open(f"Data/Analysed/RelevanceData/{initials}_unique_track_names.txt", 'r', encoding='utf-8') as f:
                for line in f.read().splitlines():
                    track_names.append(line)
            return r_data, pd.Series(track_names)
        
        self.data = {initial : get_data(initial) for initial in self.initials}
        self.meta_data = {initial : get_meta_data(initial) for initial in self.initials}



    def recalculate_score_data(self, initials, range_start, range_end):
        def get_new_score_data():
            r_data, track_names = self.track_r_data[initials]
            r_data = r_data[r_data["date"].between(range_start, range_end, inclusive=True)]
            return Analysers.calculate_scores(r_data, track_names)
        self.score_data[initials] = get_new_score_data()


    def __init_ids(self):
        self.id_overview_tab = {initial : f"overview_tab_{initial}" for initial in self.initials}
        self.id_stats_tab = {initial : f"stats_tab_{initial}" for initial in self.initials}
        self.id_recommend_tab = {initial : f"recommend_tab_{initial}" for initial in self.initials}
        self.id_mood_tab = {initial : f"mood_tab_{initial}" for initial in self.initials}

        

    def __init_dropdowns(self):
        self.id_user_dropdown = 'user_dropdown'
        self.user_dropdown = CCDropdown(
            self.id_user_dropdown,'USER', [
            (self.JK, 'JAN KREZEL'),
            (self.SS, 'SABINA SIDAROVICH'),
            (self.SM, 'SZYMON MATUSZEWSKI')])

        self.id_af_dropdown = 'af_dropdown'
        self.af_dropdown = CCDropdown(
            self.id_af_dropdown,'AF', [
                ('danceability', 'DANCEABILITY'),
                ('energy', 'ENERGY'),
                ('valence', 'VALENCE'),
                ('tempo', 'TEMPO'),
                ('instrumentalness', 'INSTRUMENTALNESS'),
                ('speechiness', 'SPEECHINESS'),
                ('acousticness', 'ACOUSTICNESS')])


    def __init_sliders(self):
        # self.id_date_slider = 'date_slider'
        self.date_sliders = {initial : dcc.RangeSlider(**self.get_date_slider_values(initial)) for initial in self.initials}
        self.af_sliders = {
                'popularity':
                    dcc.Slider(
                        id = f"popularity_slider", 
                        min = 0, max = 100,
                        value = 50,
                        step = 1,
                        marks={0: "0", 50: "50", 100: "100"}, included=False),
                'danceability':
                    dcc.Slider(
                        id = f"danceability_slider", 
                        min = 0, max = 1,
                        value = 0.5,
                        step = 0.01,
                        marks={0: "0", 0.5: "0.5", 1: "1"}, included=False),
                'energy':
                    dcc.Slider(
                        id = f"energy_slider", 
                        min = 0, max = 1,
                        value = 0.5,
                        step = 0.01,
                        marks={0: "0", 0.5: "0.5", 1: "1"}, included=False),
                'valence':
                    dcc.Slider(
                        id = f"valence_slider", 
                        min = 0, max = 1,
                        value = 0.5,
                        step = 0.01,
                        marks={0: "0", 0.5: "0.5", 1: "1"}, included=False),
                'tempo':
                    dcc.Slider(
                        id = f"tempo_slider", 
                        min = 0, max = 250,
                        value = 125,
                        step = 1,
                        marks={0: "0", 125: "125", 250: "250"}, included=False),
                'instrumentalness':
                    dcc.Slider(
                        id = f"instrumentalness_slider", 
                        min = 0, max = 1,
                        value = 0.5,
                        step = 0.01,
                        marks={0: "0", 0.5: "0.5", 1: "1"}, included=False),
                'speechiness':
                    dcc.Slider(
                        id = f"speechiness_slider", 
                        min = 0, max = 1,
                        value = 0.5,
                        step = 0.01,
                        marks={0: "0", 0.5: "0.5", 1: "1"}, included=False),
                'acousticness':
                    dcc.Slider(
                        id = f"acousticness_slider", 
                        min = 0, max = 1,
                        value = 0.5,
                        step = 0.01,
                        marks={0: "0", 0.5: "0.5", 1: "1"}, included=False),
            }


    def __init_buttons(self):
        self.id_visualisations_button = 'visualisations_button'
        self.visualisations_button = html.Button(
            "Visualisations", 
            id = self.id_visualisations_button,
            className='button', 
            style={'width': '50%'})
        
        self.id_data_button = 'data_button'
        self.data_button = html.Button(
            "Data", 
            id = self.id_data_button,
            className='button', 
            style={'width': '25%'})

        self.id_about_button = 'about_button'
        self.about_button = html.Button(
            "About", 
            id = self.id_about_button,
            className='button', 
            style={'width': '25%'})


    def __init_text_buttons(self):
        self.id_overview_button = 'overview_button'
        self.overview_button = html.Button(
            "Overview",
            id=self.id_overview_button,
            className='text-button')

        self.id_recommend_button = 'recommend_button'
        self.recommend_button = html.Button(
            "Search",
            id = self.id_recommend_button,
            className='text-button')

        self.id_mood_button = 'mood_button'
        self.mood_button = html.Button(
            "Mood",
            id = self.id_mood_button,
            className='text-button')

        self.id_stats_button = 'stats_button'
        self.stats_button = html.Button(
            "Stats",
            id = self.id_stats_button,
            className='text-button')


    def __init_text(self):
        self.texts = {initial :
            [
                dcc.Markdown(self.get_overview_static(initial)),
                dcc.Markdown(self.get_overview_dynamic(initial)),
                dcc.Markdown(self.get_recommendation_text(initial, 50, 0.5, 0.5, 0.5, 125, 0.5, 0.5, 0.5))
            ]
            for initial in self.initials
        }        


    def redraw_dynamic_charts(self, initials):
        score_data = self.score_data[initials]
        self.charts[initials][0] = dcc.Graph(figure=pl.get_track_count_plot(df = score_data))
        self.charts[initials][1] = dcc.Graph(figure=pl.get_retention_rate_vs_fullstreams_plot(df = score_data))


    def __init_charts(self):
        self.charts = { initial :
            [
                dcc.Graph(id = f"{initial}_chart_0", figure=pl.get_track_count_plot(initials = initial)),
                dcc.Graph(id = f"{initial}_chart_1", figure=pl.get_retention_rate_vs_fullstreams_plot(initials = initial)),
                dcc.Graph(id = f"{initial}_chart_2", figure=pl.get_streams_per_month_plot(initials = initial)),
                dcc.Graph(id = f"chart_3", figure=pl.get_mood_heatmap_plot(
                    pdf = self.data[initial], mdf = self.meta_data[initial], choice = 'danceability')),
                dcc.Graph(id = f"{initial}_chart_4", figure=pl.get_streams_heatmap_plot(df = self.data[initial])),
                dcc.Graph(id = f"{initial}_chart_5", figure=pl.get_top_songs_plot(initial)),
            ]
            for initial in self.initials
        }



    def __init_tabs(self):
        def get_overview_tab(initials):
            return html.Div(id=self.id_overview_tab[initials], 
                children=[
                html.Div(
                    children = [
                    html.Div(children = self.texts[initials][0],
                        className = 'visualisations-text'),
                    dcc.Loading(html.Div(
                        id = self.id_overview_tab[initials] + '_dynamic_text',
                        children = self.texts[initials][1],
                        ),
                        parent_className = 'visualisations-text',
                        style = {'justify-content': 'center', 'display': 'flex'},
                        type = 'graph')
                ],
                    className='visualisations-row'),
                html.Div(
                    children = [
                        dcc.Loading(
                            self.charts[initials][2],
                            parent_style = {'width': '100%'},
                            style = {'width': '100%'},
                            type = 'graph'
                        )
                    ],
                    className='visualisations-row')
                ], 
                className='visualisations-tab',)
        def get_stats_tab(initials):
            return html.Div(id=self.id_stats_tab[initials], 
                children=[
                html.Div(
                    children=[
                    html.Div(children = dcc.Markdown(self.get_md_text("top_songs.md")),
                        className = 'visualisations-chart-desc'),
                    html.Div(
                        id = self.id_stats_tab[initials] + '_chart_5',
                        children = self.charts[initials][5],
                        className = 'visualisations-chart'),
                ],
                    className = 'visualisations-row'),
                html.Div(
                    children=[
                    html.Div(children = dcc.Markdown(self.get_md_text("full_streams.md")),
                        className = 'visualisations-chart-desc'),
                    html.Div(
                        id = self.id_stats_tab[initials] + '_chart_0',
                        children = self.charts[initials][0],
                        className = 'visualisations-chart'),
                ],
                    className = 'visualisations-row'),
                html.Div(
                    children=[
                    html.Div(children = dcc.Markdown(self.get_md_text("retention.md")),
                        className = 'visualisations-chart-desc'),
                    html.Div(
                        id = self.id_stats_tab[initials] + '_chart_1',
                        children = self.charts[initials][1],
                        className = 'visualisations-chart'),
                ],
                    className = 'visualisations-row'),
                ],
                className='visualisations-tab',)
        def get_recommend_tab(initials):
            return html.Div(id=self.id_recommend_tab[initials], 
                children = [
                    html.Div(
                    children=[
                        html.Div(id = 'af_sliders',
                            children = [
                                html.Div(
                                    children = [
                                        html.Div(
                                            html.P(
                                                id = f"{key}_slider_label",
                                                children = [
                                                    f"{key}: {self.af_sliders[key].value}", 
                                                ],
                                                style={'padding-left': '25px', 'margin-bottom': '4px'}
                                                ),
                                        ),
                                        self.af_sliders[key],
                                    ],
                                    className = 'af-slider-container'
                                )
                                for key in list(self.af_sliders.keys())
                            ],
                            style = {'display': 'block', 'width': '50%', 'margin-top': '30px'}
                        ),
                        html.Div(id = 'recommend_text',
                            children = [
                                self.texts[initials][2]
                            ],
                            style = {'width': '50%', 'margin-top': '30px'}
                        )
                    ],
                    className='visualisations-row'
                )],
                className='visualisations-tab',)
        
        def get_mood_tab(initials):
            return html.Div(id=self.id_mood_tab[initials],
                children=[
                    html.Div(
                        children = dcc.Markdown("## Mood section\n Here you can find your streaming habits."),
                        className = 'visualisations-rows'
                    ),
                    html.Div(
                        children = [
                        html.Div(
                            children = [dcc.Markdown(self.get_md_text("streams_heatmap.md"))],
                            className='visualisations-chart-desc'
                        ),
                        html.Div(
                            dcc.Loading(
                                html.Div(
                                    children = [self.charts[initials][4]],
                                ),
                                type = 'graph'
                            ),
                            className = 'visualisations-chart'
                        )],
                        className = 'visualisations-row'
                    ),
                    html.Div(
                        children = [
                            html.Div(
                                children = [
                                    dcc.Markdown(self.get_md_text("af_heatmap.md")),
                                    html.Div(
                                        self.af_dropdown.component,
                                        style = {'text-align': 'center'}
                                    )
                                ],
                                className='visualisations-chart-desc',
                            ),
                            html.Div(
                                dcc.Loading(
                                    html.Div(
                                        id = self.id_mood_tab[initials] + '_chart_3',
                                        children = self.charts[initials][3],
                                    ),
                                    type = 'graph'
                                ),
                                className = 'visualisations-chart'
                            )
                        ],
                        className = 'visualisations-row'
                    ),
                    html.Div(
                        className = 'visualisations-row',
                        style = {'height': '100px'}
                    )
                ],
                className='visualisations-tab',)


        self.overview_tabs = {initial : get_overview_tab(initial) for initial in self.initials}
        self.stats_tabs = {initial : get_stats_tab(initial) for initial in self.initials}
        self.recommend_tabs = {initial : get_recommend_tab(initial) for initial in self.initials}
        self.mood_tabs = {initial : get_mood_tab(initial) for initial in self.initials}
        
        
        
    def __init_content(self):
        # page header
        self.id_page_header = 'page_header'
        self.page_header = html.Div(id=self.id_page_header, children=[
            html.Div(id='created_by_div', 
                children=[
                    html.B("Created By", style={'color': '#1ed760'}), 
                    html.P([
                        dcc.Link(children="Jan Krezel", href="https://github.com/krezelj", 
                            id = 'JK_link', 
                            style={'margin': '0px', 'color': 'white', 'text-decoration': 'none'}),
                        html.Br(),
                        dcc.Link(children="Sabina Sidarovich", href="https://github.com/notsabina", 
                            id = 'SS_link', 
                            style={'margin': '0px', 'color': 'white', 'text-decoration': 'none'}),
                        html.Br(),
                        dcc.Link(children="Szymon Matuszewski", href="https://github.com/szymonsm", 
                            id = 'SM_link', 
                            style={'margin': '0px', 'color': 'white', 'text-decoration': 'none'})], 
                        style={'margin-left': '15px'})],
                style={'display':'flex', 'align-items': 'center','width':'33%'}),
            html.Div(id='title_div', 
                children=[
                    html.P(children = [
                        "Spoti", html.Span("My", style={'color': '#1ed760'}), " Analysis"
                    ])
                ], 
                className = 'title-container'),
            html.Div(id='navigation_div', 
                children=[
                    self.visualisations_button,
                    self.data_button,
                    self.about_button],
                className = 'buttons-container',
                style={'width':'33%'})
            ],
            className = "page-header"
        )

        # visualisations page
        self.__init_tabs()
        self.current_visualisations_tab = None
        self.set_current_visualisations_tab(self.overview_tabs)

        self.id_visualisations_controls = 'visualisations_controls'
        self.id_visualisations_navigation = 'visualisations_navigation'
        self.id_visualisations_user_dropdown_container = 'visualisations_user_dropdown_container'
        self.id_visualisations_content = 'visualisations_content'
        self.id_visualisations_page = 'visualisations_page'
        self.id_visualisations_date_slider_container = 'visualisations_date_slider_container'

        self.id_visualisations_page = 'visualisations_page'
        self.visualisations_controls = html.Div(id=self.id_visualisations_controls, 
            children = [
                html.Div(
                    children=[
                    html.Div(id=self.id_visualisations_navigation, 
                        children=[
                            self.overview_button,
                            self.stats_button,
                            self.mood_button,
                            self.recommend_button,
                        ],
                        className = 'text-buttons-container', 
                        style = {'width':'70%', 'background-color': '#161616'}),
                    html.Div(id=self.id_visualisations_user_dropdown_container, 
                        children=[
                            self.user_dropdown.component
                        ],
                        className = 'buttons-container',
                        style = {'width': '30%'})
                ],
                    style = {'display': 'flex'}),
                html.Div(
                    html.Div(id = self.id_visualisations_date_slider_container,
                        children = [
                            self.date_sliders[self.user_dropdown.value]
                        ],
                        style = {'width': '70%','margin-top': '30px'}
                    )
                )
            ],
            style = {'display': 'block'}
        )
        self.visualisations_content =  html.Div(id=self.id_visualisations_content, 
            children=[
                # self.current_visualisations_tab[self.user_dropdown.value]
                self.overview_tabs[self.user_dropdown.value],
                self.stats_tabs[self.user_dropdown.value],
                self.mood_tabs[self.user_dropdown.value],
                self.recommend_tabs[self.user_dropdown.value],
            ],
            className='visualisations-tab-container')
        self.visualisations_page = html.Div(id=self.id_visualisations_page, 
            children=[
                self.visualisations_controls, 
                self.visualisations_content
            ],
            className = 'page')

        # data page

        self.data_page = html.Div(dcc.Markdown(self.get_md_text("data_page.md")), 
            className = 'half-page',
            style = {'width': '50%'})

        # about page

        self.about_page = html.Div(dcc.Markdown(self.get_md_text("about_page.md")), 
            className = 'half-page',
            style = {'width': '50%'})

        #
        self.current_page = None
        self.set_current_page(self.visualisations_page)


    def set_current_page(self, page):
        self.visualisations_page.style = {'display': 'none'}
        self.data_page.style = {'display': 'none'}
        self.about_page.style = {'display': 'none'}
        self.current_page = page
        self.current_page.style = {'display': 'block'}


    def set_current_visualisations_tab(self, tabs):
        if self.current_visualisations_tab is not None:
            for k, v in self.current_visualisations_tab.items():
                # v.style = {'display': 'none'}
                v.style = {'display': 'none'}
        self.current_visualisations_tab = tabs
        for k, v in self.current_visualisations_tab.items():
            # v.style = {'display': 'block'}
            v.style = {'display': 'block'}


    def get_md_text(self, file_name):
        file_path = "Source/App/assets/mdfiles/" + file_name
        with open(file_path, 'r', encoding='utf-8') as text_file:
            return text_file.read()


    



    def get_date_slider_values(self, initials):
        data = self.data[initials]
        min_date = data.iloc[0]["date"]#data.iloc[data["date"].idxmin()]["date"]
        max_date = data.iloc[-1]["date"]#data.iloc[data["date"].idxmax()]["date"]
        months = (max_date.year - min_date.year) * 12 + (max_date.month - min_date.month) + 1
        marks = {13 - min_date.month + i*12: f"{min_date.year + i + 1}" for i in range(max_date.year - min_date.year)}
        marks[0] = f"{min_date.year}-{str(min_date.month).zfill(2)}"
        marks[months] = f"{max_date.year}-{str(max_date.month).zfill(2)}"
        marks[months - 6] = "6 months"# {"label": "6 months", "style": {"transform": "translateY(-20px)"}}
        return {
            'id': f'date_slider_{initials}',
            'min': 0,
            'max': months,
            'marks': marks,
            'step': 1,
            'value': [0, months],
            'persistence': False
            # 'updatemode': 'drag'
            # 'tooltip': {"placement": "bottom", "always_visible": True}
        }


    def date_to_text(self, date):
        def get_ordinal_suffix(v):
            v = str(v)
            if v[-1] == "1": return "st"
            if v[-1] == "2": return "nd"
            if v[-1] == "3": return "rd"
            else: return "th"
        return f"{date.day}{get_ordinal_suffix(date.day)} of {date.month_name()} {date.year}"


    def ms_to_hour_and_minutes(self, ms):
        total_minutes = ms / 60000
        hours = int(total_minutes / 60)
        minutes = int(total_minutes - hours * 60)
        hours_text = "" if hours == 0 and minutes != 0 else f"{hours} hour{'' if hours == 1 else 's'}"
        minutes_text = "" if minutes == 0 else f" and {minutes} minute{'' if minutes == 1 else 's'}"
        return f"{hours_text}{minutes_text}"



    def get_recommendation_text(self, initials, *args):
        random_comments = [
            "Ugh... is *that* what you were hoping to find?",
            "You really do have bad taste, don't you?",
            "My *grandpa* listens to that!",
            "Not bad... just... bad.",
            "Pretty basic but it could be worse. I guess.",
            "Pretentious, aren't we?",
            "That's actually a bad song. No joke here.",
            "I used to like this song. Now I don't.",
            "Well, at least it's better than the other ones.",
            "Do you even *know* any good songs?",
            "Hey, that's actually nice! Finally.",
            "Out of all of these, this one's the worst."
        ]
        
        md = self.meta_data[initials]
        keys = list(self.af_sliders.keys())

        args = [args[i] / self.af_sliders[keys[i]].max for i in range(len(keys))]

        distance = pd.Series(index=np.arange(len(md)), dtype = int)
        for i in range(len(keys)):
            norm_af = md[keys[i]].apply(lambda x : x / self.af_sliders[keys[i]].max)
            distance += (norm_af - args[i]) ** 2

        min_distances = distance.nsmallest(5).reset_index()
        matches = md.iloc[min_distances['index']].reset_index(drop=True)
        
        info = {}
        comments = random.sample(random_comments, 5)
        random.shuffle(comments)
        for i in range(1, 6):
            info[f"top_{i}_track"] = matches['trackName'][i-1]
            info[f"top_{i}_track_artist"] = matches['primaryArtist'][i-1]
            info[f"top_{i}_score"] = round(100 - 100 * np.sqrt(min_distances.iloc[i-1, 1]), 2)
            info[f"random_comment_{i}"] = comments[i-1]

        return self.get_md_text('recommendations.md').format(**info)

        
    def get_overview_static(self, initials):
        
        info = {}
        
        data = self.data[initials]
        meta_data = self.meta_data[initials]

        first_stream = data.iloc[0]# data.iloc[data["date"].idxmin()]
        ts = first_stream["date"]
        info["first_stream_date"] = self.date_to_text(ts)
        info["first_stream_track"] = first_stream["track"]
        info["first_stream_artist"] = first_stream["artist"]
        info["total_minutes"] = int(data["ms"].sum() / 60000)
        info["total_streams"] = len(data[data["ms"] > 30000])

        year_streams = data[data["date"] >= pd.Timestamp(year= 2021, month = 1, day = 1)]
        info["year_total_streams"] = len(year_streams[year_streams["ms"] > 30000])
        info["year_total_minutes"] = int(year_streams["ms"].sum() / 60000)

        tmp = data.groupby(["track", "artist"])["track"].agg(count = len).reset_index()
        most_played = tmp.iloc[tmp["count"].idxmax()]

        info["most_played_track"] = most_played["track"]
        info["most_played_track_artist"] = most_played["artist"]
        info["most_played_track_count"] = most_played["count"]

        df = pd.merge(data, meta_data, left_on=['track', 'artist'],
                      right_on=['trackName', 'primaryArtist'], how='left')
        info["most_played_genre"] = pd.Series(df['artistGenres'].str.strip('[[]]')
            .str.split(',', expand=True) \
            .values.ravel('F'), name='genre') \
            .replace('', np.nan).dropna().mode().to_string(header=False, index=False) \
            .strip(" ").strip("'")

        return self.get_md_text('overview_static.md').format(**info)


    def get_overview_dynamic(self, initials, range_start = None, range_end = None):
        data = self.data[initials]
        if range_start is None:
            range_start = data.iloc[0]["date"]
        if range_end is None:
            range_end = data.iloc[-1]["date"] + pd.DateOffset(months = 1)

        data = data[data["date"].between(range_start, range_end, inclusive=True)].reset_index(drop=True)
        meta_data = self.meta_data[initials]

        info = {}
        info["range_start"] = f"{range_start.month_name()} {range_start.year}"# self.date_to_text(range_start)
        info["range_end"] = f"{range_end.month_name()} {range_end.year}"# self.date_to_text(range_end)
        info["total_stream_count"] = len(data)
        info["total_minutes"] = self.ms_to_hour_and_minutes(data["ms"].sum())
        info["iss_orbits"] = int((data["ms"].sum() / 60000) / 90)
        info["not_skipped_stream_count"] = len(data[data["ms"] > 30000])

        tmp = data.groupby(["track", "artist"])["track"].agg(count = len).reset_index()
        most_played_track = tmp.iloc[tmp["count"].idxmax()]
        info["unique_count"] = len(tmp)
        info["most_played_track"] = most_played_track["track"]
        info["most_played_track_artist"] = most_played_track["artist"]
        info["most_played_track_count"] = most_played_track["count"]

        tmp = data.groupby(["artist"])["artist"].agg(count = len).reset_index()
        most_played_artist = tmp.iloc[tmp["count"].idxmax()]
        info["most_played_artist"] = most_played_artist["artist"]
        info["most_played_artist_count"] = most_played_artist["count"]

        tmp = data.groupby(["album", "artist"])["album"].agg(count = len).reset_index()
        most_played_album = tmp.iloc[tmp["count"].idxmax()]
        info["most_played_album"] = most_played_album["album"]
        info["most_played_album_count"] = most_played_album["count"]

        data["day"] = data["date"].apply(lambda d : pd.Timestamp(year = d.year, month = d.month, day = d.day))
        tmp = data.groupby(["day"])["ms"].agg(playtime = sum).reset_index()
        most_busy_day = tmp.iloc[tmp["playtime"].idxmax()]
        info["most_busy_day"] = self.date_to_text(most_busy_day["day"])
        most_busy_day_streams = data[data["date"].between(most_busy_day["day"], most_busy_day["day"] + pd.Timedelta(days = 1))]
        info["most_busy_day_count"] = len(most_busy_day_streams)
        info["most_busy_day_duration"] = self.ms_to_hour_and_minutes(most_busy_day_streams["ms"].sum())
        
        tmp = pd.merge(data, meta_data, left_on=['track', 'artist'],
                       right_on=['trackName', 'primaryArtist'], how='left')
        info["most_played_genre"] = pd.Series(tmp['artistGenres'].str.strip('[[]]')
                                              .str.split(',', expand=True)
                                              .values.ravel('F'), name='genre') \
            .replace('', np.nan).dropna().mode().to_string(header=False, index=False) \
            .strip(" ").strip("'")


        return self.get_md_text('overview_dynamic.md').format(**info)
