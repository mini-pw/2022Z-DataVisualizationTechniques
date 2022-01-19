import streamlit as st
import matplotlib.pyplot as plt
import random
from pages.plots import generate_wordcloud, generate_emoji_wordcloud, utils


def plot_cloud(wordcloud):
    plt.figure(figsize=(40, 30))
    plt.imshow(wordcloud)
    plt.axis("off")
    return plt


def app():
    df = utils.load_data()

    if df is None:
        st.markdown("Please upload data through `Upload Data` page!")
    else:
        st.markdown("## Wordcloud")
        st.markdown("Graphical representations of most commonly used words")

        count = 10
        if st.button('Reload Wordcloud'):
            count = random.randint(15, 25)

        letters = st.slider("Minimum length of each word:", min_value=1, max_value=10, value=3)
        wc = generate_wordcloud.generate_wordcloud(df, count, letters)
        st.pyplot(plot_cloud(wc))

        st.markdown("## Emoji Wordcloud")
        st.markdown("Most used emojis")
        wc_emoji = generate_emoji_wordcloud.generate_emoji_wordcloud(df)
        st.pyplot(plot_cloud(wc_emoji))
