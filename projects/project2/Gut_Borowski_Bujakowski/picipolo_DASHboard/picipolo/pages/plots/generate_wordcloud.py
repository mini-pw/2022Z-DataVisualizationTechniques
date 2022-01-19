from collections import Counter
from wordcloud import WordCloud
import os
import pandas as pd
import matplotlib.pyplot as plt


def plot_cloud(wordcloud):
    plt.figure(figsize=(40, 30))
    plt.imshow(wordcloud)
    plt.axis("off")
    return plt


def generate_wordcloud(df: pd.DataFrame, count: int, letter: int) -> WordCloud:
    count += 3

    df = df[df['type'] == 'T']
    text = df['text'].tolist()

    mapka = Counter()

    words = []
    for el in text:
        el = str(el).split()
        for el2 in el:
            if len(el2) >= letter:
                words.append(str(el2))

    mapka = Counter(words)
    words_and_freq = mapka.most_common(count)
    words = ""
    for el in words_and_freq:
        words += (el[0] + " ")

    wc = WordCloud(width=3000, height=2000, background_color='black', colormap='plasma',
                   collocations=False).generate(words)
    return wc


if __name__ == '__main__':
    path = os.getcwd() + '/messengerData.csv'
    df = pd.read_csv(path, sep=';')

    wc = generate_wordcloud(df, 7)
    plot_cloud(wc)
