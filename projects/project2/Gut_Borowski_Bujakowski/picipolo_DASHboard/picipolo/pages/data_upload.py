import streamlit as st
import pandas as pd
from pathlib import Path
from PIL import Image
from pages.plots import utils


def app():
    st.markdown("## Data Upload")

    st.markdown(
        r'''
        Steps you need to take in order to use this app:
        - download Messenger and Facebook data
        '''
    )
    with st.expander("Click here to see detailed explanation"):
        st.write('Firstly go to Settings & privacy')
        image1 = Image.open(utils.get_img_path(1))
        st.image(image1)

        st.write('Then chose Settings')
        image2 = Image.open(utils.get_img_path(2))
        st.image(image2)

        st.write('Then go to Your Facebook information')
        image3 = Image.open(utils.get_img_path(3))
        st.image(image3)

        st.write('View section Download Your Information')
        image4 = Image.open(utils.get_img_path(4))
        st.image(image4)

        st.write('Select JSON as file format, and choose date range of your interest')
        image5 = Image.open(utils.get_img_path(5))
        st.image(image5)

        st.write('Make sure to check both Messages and Friends and followers')
        image6 = Image.open(utils.get_img_path(6))
        st.image(image6)
        image7 = Image.open(utils.get_img_path(7))
        st.image(image7)

        st.write('Finally press \'Request your download\'')
        image8 = Image.open(utils.get_img_path(8))
        st.image(image8)

    st.markdown(
        r'''
        - download both python files from this 
        [link](https://github.com/boro128/picipolo_DASHboard/tree/main/data/data_parser)
        - run parser.py and select directory \messages\inbox
        - run fb_parser.py and select directory \friends_and_followers
        - upload csv files created by both parsers using selectbox below
        '''
    )

    category = st.selectbox('Messenger/Facebook', ['Messenger', 'Facebook'])

    if category == 'Messenger':
        st.markdown("### Upload a csv file `messengerData.csv`.")
        st.write("\n")

        uploaded_file = st.file_uploader("Choose a file", type=['csv', 'xlsx'])

        global data
        if uploaded_file is not None:
            try:
                data = pd.read_csv(uploaded_file, delimiter=';')
            except Exception as e:
                print(e)
                data = pd.read_excel(uploaded_file)

        if st.button("Load Data"):
            # Display raw data
            st.dataframe(data.head())

            file_path = Path(__file__).resolve()
            data_path = file_path.parents[2].joinpath('data', 'user_data', 'parsed', 'messengerData.csv')
            data.to_csv(data_path, index=False, sep=';')
    else:
        st.markdown("## Data FB Upload")

        # Upload the dataset and save as csv
        st.markdown("### Upload a csv file `friend_requests_received.csv`.")
        st.write("\n")

        # Code to read a single file
        uploaded_file = st.file_uploader("Choose a file", type=['csv', 'xlsx'], key=1)
        global data1
        if uploaded_file is not None:
            try:
                data1 = pd.read_csv(uploaded_file)
            except Exception as e:
                print(e)
                data1 = pd.read_excel(uploaded_file)

        if st.button("Load Data", key=1):
            # Raw data
            st.dataframe(data1)
            data1.to_csv('data/friend_requests_received.csv', index=False)

        # Upload the dataset and save as csv
        st.markdown("### Upload a csv file `friend_requests_sent.csv`.")
        st.write("\n")

        # Code to read a single file
        uploaded_file = st.file_uploader("Choose a file", type=['csv', 'xlsx'], key=2)
        global data2
        if uploaded_file is not None:
            try:
                data2 = pd.read_csv(uploaded_file)
            except Exception as e:
                print(e)
                data2 = pd.read_excel(uploaded_file)

        if st.button("Load Data", key=2):
            # Raw data
            st.dataframe(data2)
            data2.to_csv('data/friend_requests_sent.csv', index=False)

        st.markdown("### Upload a csv file `friend_requests_rejected.csv`.")
        st.write("\n")

        # Code to read a single file
        uploaded_file = st.file_uploader("Choose a file", type=['csv', 'xlsx'], key=3)
        global data3
        if uploaded_file is not None:
            try:
                data3 = pd.read_csv(uploaded_file)
            except Exception as e:
                print(e)
                data3 = pd.read_excel(uploaded_file)

        if st.button("Load Data", key=3):
            # Raw data
            st.dataframe(data3)
            data3.to_csv('data/friend_requests_rejected.csv', index=False)

        st.markdown("### Upload a csv file `friends.csv`.")
        st.write("\n")

        # Code to read a single file
        uploaded_file = st.file_uploader("Choose a file", type=['csv', 'xlsx'], key=4)
        global data4
        if uploaded_file is not None:
            try:
                data4 = pd.read_csv(uploaded_file)
            except Exception as e:
                print(e)
                data4 = pd.read_excel(uploaded_file)

        if st.button("Load Data", key=4):
            # Raw data
            st.dataframe(data4)
            data4.to_csv('data/friends.csv', index=False)
