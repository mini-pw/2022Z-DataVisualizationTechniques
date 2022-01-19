import streamlit as st
from pages.plots import bar_chart
from pages.plots.utils import load_data_bar_chart


def app():
    data_bar_chart = load_data_bar_chart()
    if data_bar_chart is None:
        st.markdown("Please upload data through `Upload Data` page!")
    else:
        df_friends_received, df_friends_waiting, df_friends_rejected, df_friends_sent = data_bar_chart
        config = {'displayModeBar': False, 'scrollZoom': False}
        st.markdown("## Friends analysis")
        fig1, fig2, fig3, fig4 = bar_chart.create_chart(df_friends_received,
                                                        df_friends_waiting,
                                                        df_friends_rejected,
                                                        df_friends_sent)
        st.plotly_chart(fig1, config=config, use_container_width=True)
        st.plotly_chart(fig2, config=config, use_container_width=True)
        st.plotly_chart(fig3, config=config, use_container_width=True)
        st.plotly_chart(fig4, config=config, use_container_width=True)
