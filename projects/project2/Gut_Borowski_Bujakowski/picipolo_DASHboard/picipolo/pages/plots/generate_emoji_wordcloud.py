import os
import pandas as pd
import matplotlib.pyplot as plt
import re
from wordcloud import WordCloud
from pages.plots import utils


def plot_cloud(wordcloud):
    plt.figure(figsize=(40, 30))
    plt.imshow(wordcloud)
    plt.axis("off")
    return plt


def generate_emoji_wordcloud(df: pd.DataFrame) -> WordCloud:
    df = df[df['type'] == 'T']
    text = ""
    for el in list(df['text']):
        text += str(el) + " "

    emoji = re.compile("["
                       u"\U0001F600-\U0001F64F"  # emoticons
                       u"\U0001F300-\U0001F5FF"  # symbols & pictographs
                       u"\U0001F680-\U0001F6FF"  # transport & map symbols
                       u"\U0001F1E0-\U0001F1FF"  # flags (iOS)
                       "]", flags=re.UNICODE)

    font_path = utils.load_path_fonts()

    wc = WordCloud(font_path=font_path, regexp=emoji,
                   width=3000, height=2000, random_state=1, background_color='black', colormap='plasma',
                   collocations=False).generate(text)
    return wc


if __name__ == '__main__':
    path = os.getcwd() + '/messengerData.csv'
    df = pd.read_csv(path, sep=';')

    wc = generate_emoji_wordcloud(df)
    plot_cloud(wc)
