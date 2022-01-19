import random

import matplotlib as mpl
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import requests
import seaborn as sns
import streamlit as st
from streamlit_lottie import st_lottie
from wordcloud import WordCloud

from skrypty.skryptActivity import przygotowanieDanych
from skrypty.skryptEmojiCloud import przygotowanieDanychEmojiCloud
from skrypty.skryptHeatMap import przygotowanieDanychHeatmap
from skrypty.skryptWordCloud import przygotujDaneWordCloud
from wykresyKod.emojiCloud import EmojiCloud

st.set_page_config(layout="wide", page_title="Messenger Analysis", page_icon="💬")


@st.experimental_memo(show_spinner=False)
def load_lottieurl(url: str):
    r = requests.get(url)
    if r.status_code != 200:
        return None
    return r.json()


downloaded_url = load_lottieurl('https://assets3.lottiefiles.com/private_files/lf30_d9lonffd.json')

sZ, rowZ, sZ1, rowZ1, sZ2 = st.columns(
    (.1, .6, .1, 1.2, .2))

with sZ:
    st.markdown('')
with sZ1:
    st.markdown('')
with sZ2:
    st.markdown('')

with rowZ:
    st_lottie(downloaded_url, height=200)

with rowZ1:
    st.markdown('# Messenger Analysis')
    st.markdown('### Web app created by:  [Laura Hoang](https://github.com/hoanganhlinh), [Wiktor Jakubowski]('
                'https://github.com/WJakubowsk), [Mikołaj Gałkowski](https://github.com/galkowskim)')

for i in range(2):
    st.markdown("")

st.markdown("#### Our project concerns analysis of data from Messenger, that is messages sent to friends "
            " throughout entire being on platform.")

st.markdown("##### Tutorial how to create the csv file is available in [repository](https://github.com/galkowskim/messenger_analysis_app).")
file = st.file_uploader("Upload csv file with your data.", key='file')

data = None
if st.session_state.file is not None:
    data = pd.read_csv(st.session_state.file)

# NA POTRZEBY NAGRANIA
# add_selectbox = st.sidebar.selectbox(
#     'Who?',
#     ('Wiktor', 'Laura', 'Mikołaj'),
#     key="osoba"
# )

################################################ ACTIVITY CHART ########################################################
if data is not None:

    # NA POTRZEBY NAGRANIA
    # st.markdown(
    #     "##### Firstly, we wanted to explore our activity on Messenger. Based on the number of messages sent between"
    #     " us and other users we created line plots presenting number of messages sent to us from our friends. We "
    #     " categorized the data by gender in order to compare our activity with men and women respectively.")

    st.markdown(
        "##### Firstly, let's explore your activity on Messenger. Below there is a line plot presenting number of "
        "messages sent to you by your friends. The data is categorized by gender in order to compare your activity with"
        " men and women respectively.")

    row1_1, row1_2 = st.columns(
        (1, 3))

    with row1_1:
        st.markdown("")
        st.markdown("")
        st.markdown("")

        # NA POTRZEBY NAGRANIA
        # if str(st.session_state.osoba) == "Mikołaj":
        #     osoba = "Mikołaj Gałkowski"
        #     data = pd.read_csv('wiadomosci/wiadomosciMikolaj.csv')
        # elif str(st.session_state.osoba) == "Laura":
        #     osoba = "Laura Hoang"
        #     data = pd.read_csv('wiadomosci/wiadomosciLaura.csv')
        # elif str(st.session_state.osoba) == "Wiktor":
        #     osoba = "Wiktor Jakubowski"
        #     data = pd.read_csv('wiadomosci/wiadomosciWiktor.csv')

        data['author'] = data['author'].fillna("")

        femalebox = st.checkbox("Female", key="femalebox", value=True)
        malebox = st.checkbox("Male", key="malebox", value=True)

        dfGrouped, dfAll = przygotowanieDanych(data)

        starting = st.date_input("Starting date", key="starting",
                                 min_value=dfAll['date'].min(),
                                 value=dfAll['date'].min(),
                                 max_value=dfAll['date'].max())
        ending = st.date_input("Ending date", key="ending",
                               min_value=dfAll['date'].min(),
                               value=dfAll['date'].max(),
                               max_value=dfAll['date'].max())

    with row1_2:
        plt.figure(figsize=(12, 4), dpi=1000, facecolor="#3A5094")

        ax = plt.gca()
        ax.set_facecolor("#3A5094")
        plt.grid()

        if femalebox and malebox:
            palette = ["#FE5A75", "#148BFF"]
            sns.lineplot(x='date',
                         y='number',
                         hue="sex",
                         palette=palette,
                         data=dfGrouped,
                         ax=ax)
        if femalebox and not malebox:
            sns.lineplot(x='date',
                         y='number',
                         color="#FE5A75",
                         data=dfGrouped.loc[dfGrouped['sex'] == 'female'],
                         ax=ax)
        if malebox and not femalebox:
            sns.lineplot(x='date',
                         y='number',
                         color="#148BFF",
                         data=dfGrouped.loc[dfGrouped['sex'] == 'male'],
                         ax=ax)

        legend = plt.legend()
        frame = legend.get_frame()
        frame.set_facecolor('white')
        plt.locator_params(axis='x', nbins=50)
        ax.xaxis.set_major_locator(mdates.DayLocator(1))
        plt.setp(plt.gca().get_xticklabels(), rotation=70, ha="right")
        plt.tight_layout()
        plt.xlim((str(st.session_state.starting), str(st.session_state.ending)))
        plt.ylim(0)
        plt.margins(0, 0)
        st.set_option('deprecation.showPyplotGlobalUse', False)
        ax.tick_params(colors='white')
        ax.set_xlabel("", color="white")
        ax.set_ylabel("No. messages", color="white")
        ax.set_title("Number of my messages sent to me from other users by their gender", color="white")
        sns.set(rc={'axes.facecolor': '#3A5094', 'axes.edgecolor': 'white'})
        plt.grid()
        st.pyplot()

    for i in range(5):
        st.markdown("")

    ################################################# HEAT MAP #############################################################

    st.markdown("##### After research of numbers of messages sent to you, it would be an interesing idea to "
                " explore your own numbers. Below there is a heatmap displaying mean of messages sent throughout each "
                "hour by you. Data is categorized by weekday.")

    with st.container():
        spaceer2, col11, spaceeer2, col22, spaceer4 = st.columns([.1, .93, .2, 2.3, .20])

        with spaceeer2:
            st.markdown("")
        with spaceer4:
            st.markdown("")

        with col11:
            for i in range(3):
                st.markdown("")
            st.markdown("Select time period no shorter than 7 days.")
            starting2 = st.date_input("Starting date", key="starting2",
                                      min_value=dfAll['date'].min(),
                                      value=dfAll['date'].min(),
                                      max_value=dfAll['date'].max())
            ending2 = st.date_input("Ending date", key="ending2",
                                    min_value=dfAll['date'].min(),
                                    value=dfAll['date'].max(),
                                    max_value=dfAll['date'].max())

        if (ending2 != starting2) and (int(str(ending2 - starting2).split(" ")[0]) >= 7):

            dfHeatmap = przygotowanieDanychHeatmap(data, st.session_state.starting2, st.session_state.ending2)

            with col22:
                st.markdown("")
                st.set_option('deprecation.showPyplotGlobalUse', False)

                DAYS = ['Mon.', 'Tues.', 'Wed.', 'Thurs.', 'Fri.', 'Sat.', 'Sun.']

                heatmap = np.zeros((7, 24))

                for hour in range(24):
                    for week_day in range(7):
                        heatmap[week_day, hour] = \
                            dfHeatmap.loc[(dfHeatmap["week_day"] == week_day) & (dfHeatmap["hour"] == hour)][
                                "sum_mean_id"]

                x = np.arange(24 + 1) - 0.5
                y = np.arange(7 + 1) - 0.5

                ax = plt.gca()
                mesh = ax.pcolormesh(x, y, heatmap, edgecolor="white")
                ax.invert_yaxis()

                ax.set_yticks(np.arange(7))
                ax.set_yticklabels(DAYS, color="white")

                ax.set_xticks(np.arange(24))
                ax.set_xticklabels(np.arange(24), color='white')
                ax.set_title("Heatmap presenting my hourly acitivity throughout the day", color='white')

                plt.sca(ax)
                plt.sci(mesh)

                cb = plt.colorbar()
                cmap = mpl.cm.get_cmap('Blues')

                cb.ax.set_yticklabels(np.arange(-.5, max(dfHeatmap.sum_mean_id) + .5), color="white")
                cb.ax.set_title('Mean no. \nmessages', color='white')

                plt.set_cmap(cmap)

                cb.ax.tick_params(size=0)
                st.pyplot()
        else:
            st.markdown("<font color='red'> Selected time period is too short. Must be at least 7 days long. </font>",
                        unsafe_allow_html=True)
    for i in range(5):
        st.markdown("")

    ################################################ EMOJI WORD CLOUD ######################################################

    st.markdown(
        "##### Finally, let's take a closer look at the content of the messages. They split into two categories:"
        " words and emojis. Below there is a WordCloud consiting of the most frequently sent words by you."
        " On the other hand, after switching the button, your most popular emojis are shown on an EmojiCloud.")

    with st.container():
        st.set_option('deprecation.showPyplotGlobalUse', False)
        spaceer1, col1, spaceer, col2, spaceer3 = st.columns([.3, 1.5, .3, 1.5, .3])
        with spaceer:
            st.markdown("")
        with spaceer1:
            st.markdown("")
        with spaceer3:
            st.markdown("")
        with col1:
            for el in range(10):
                st.markdown("")

            cloudType = st.radio(
                "Select cloud type:",
                ('Emoji', 'Word'),
                key="cloudType",
                index=0)

            if cloudType == 'Emoji':
                emojis = st.slider(label="Select maximum number of emojies on EmojiCloud", min_value=10, max_value=100,
                                   key='emojis', value=50)
            else:
                znaki = st.slider(label="Select minimal word length", key='znaki', min_value=3, value=4, max_value=10)
        with col2:

            def grey_color_func(word, font_size, position, orientation, random_state=None,
                                **kwargs):
                return "hsl(0, 0%%, %d%%)" % random.randint(60, 100)


            if cloudType == 'Emoji':
                emoji = przygotowanieDanychEmojiCloud(data)
                emoji_cloud2 = EmojiCloud(font_path='./fonts/Symbola.otf', contour_width=50,
                                          contour_color='#6a80c4', background_color='#3A5094',
                                          maxwords=int(st.session_state.emojis))
                emoji_cloud2.generate(emoji)
                plt.imshow(emoji_cloud2.word_cloud.recolor(color_func=grey_color_func, random_state=42))
                plt.tight_layout(pad=0)
                st.pyplot()
            else:

                wiadomosci = przygotujDaneWordCloud(data, int(st.session_state.znaki))
                wordcloud = WordCloud(font_path='./fonts/Symbola.otf', width=500, height=400,
                                      max_font_size=100, max_words=100, background_color="#6a80c4"). \
                    generate(wiadomosci)
                plt.tight_layout(pad=0)
                plt.figure(figsize=(6, 3))
                plt.imshow(wordcloud.recolor(color_func=grey_color_func, random_state=3),
                           interpolation="bilinear")
                plt.axis("off")
                st.pyplot()
