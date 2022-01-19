import streamlit as st
from pages.plots import utils
import plotly.graph_objects as go


def app():
    st.markdown('## Total Number of Messages in Conversations')

    df = utils.load_data()
    if df is None:
        st.markdown("Please upload data through `Upload Data` page!")
    else:
        include_groups = st.checkbox('Include groups')

        ranking = df.groupby('name')['text'].count()
        if not include_groups:
            mask = df.groupby('name')['who'].nunique() <= 2
            ranking = ranking[mask]

        ranking = ranking.sort_values(ascending=False)

        fig = go.Figure(data=[go.Table(
            columnwidth=[50, 200, 150],
            header=dict(
                values=['Rank', 'Name', 'Total number of messages'],
                fill_color='#262730'),
            cells=dict(
                values=[[i + 1 for i in range(len(ranking))], ranking.index, ranking.values],
                line_color='#262730',
                fill_color='#0e1117')
        )])

        st.plotly_chart(fig, use_container_width=True)
