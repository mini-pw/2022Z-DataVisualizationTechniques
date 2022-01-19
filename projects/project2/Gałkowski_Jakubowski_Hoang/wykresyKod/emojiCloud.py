from collections import Counter

import emojis
import matplotlib.pyplot as plt
import streamlit as st
from wordcloud import WordCloud


@st.experimental_singleton
class EmojiCloud:
    def __init__(self, font_path='Symbola.otf', mask=None, contour_width=None, contour_color=None,
                 background_color=None, maxwords=200):
        self.font_path = font_path
        self.background_color = background_color
        self.mask = mask
        self.contour_width = contour_width
        self.contour_color = contour_color
        self.maxwords = maxwords
        self.word_cloud = self.initialize_wordcloud()
        self.emoji_probability = None

    def initialize_wordcloud(self):
        return WordCloud(font_path=self.font_path,
                         width=500,
                         height=500,
                         max_words=self.maxwords,
                         background_color=self.background_color,
                         random_state=42,
                         collocations=False,
                         mask=self.mask,
                         contour_width=self.contour_width,
                         contour_color=self.contour_color)

    def color_func(self, word, font_size, position, orientation, random_state=None,
                   **kwargs):
        hue_saturation = '60, 60%'

        current_emoji_probability = self.emoji_probability[word]
        if current_emoji_probability >= 0.10:
            opacity = 50
        else:
            opacity = 75 - current_emoji_probability / 0.2 * 5
        return f"hsl({hue_saturation},{opacity}%)"

    def generate(self, text):
        emoji_frequencies = Counter(emojis.iter(text))
        total_count = sum(emoji_frequencies.values())

        self.emoji_probability = {emoji: count / total_count for emoji, count in emoji_frequencies.items()}
        wc = self.word_cloud.generate_from_frequencies(emoji_frequencies)

        plt.figure(figsize=(6, 3))
        plt.imshow(wc.recolor(color_func=self.color_func, random_state=42))
        plt.axis("off")

    def recolor(self, color):
        self.word_cloud.recolor(color)
