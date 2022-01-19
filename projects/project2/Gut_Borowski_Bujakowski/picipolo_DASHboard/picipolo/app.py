import streamlit as st

from multipage import MultiPage
from pages import data_upload, heatmap, wordcloudpage, friends, messageranking

# Create an instance of the app
app = MultiPage()

# Title of the main page
st.title("Messenger and Facebook analysis")

app.add_page("Upload Data", data_upload.app)
app.add_page("Messages", heatmap.app)
app.add_page("Friends", friends.app)
app.add_page("Wordcloud", wordcloudpage.app)
app.add_page("Ranking", messageranking.app)

if __name__ == '__main__':
    app.run()
