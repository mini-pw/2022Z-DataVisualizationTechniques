import pandas as pd
import streamlit as st


@st.experimental_memo(show_spinner=False)
def przygotowanieDanych(df):
    dfGrouped = df  # ramka danych pogrupowanych po płci
    dfGrouped["date"] = dfGrouped["date"].str.split("T").str.get(0)
    dfGrouped = dfGrouped.loc[dfGrouped.author == ""].groupby(
        [dfGrouped.date, dfGrouped.sex]).size().to_frame()
    dfGrouped = dfGrouped.reset_index()
    dfGrouped['date'] = pd.to_datetime(dfGrouped["date"])
    dfGrouped = dfGrouped.rename(columns={'date': 'date', 'sex': 'sex', 0: 'number'})

    dfAll = df.groupby(df.date).size().reset_index()  # ramka danych skumulowanych
    dfAll["date"] = dfAll["date"].str.split("T").str.get(0)
    dfAll['date'] = pd.to_datetime(dfAll["date"])
    dfAll = dfAll.reset_index()
    dfAll = dfAll.rename(columns={'date': 'date', 0: 'number'})

    days = pd.date_range(start=dfAll['date'].min(), end=dfAll['date'].max(), freq='1D')
    days = days.strftime('%Y-%m-%d')
    days = pd.DataFrame(days)[0].tolist()

    result = pd.DataFrame({'date': days})
    result['date'] = pd.to_datetime(result['date'])

    dfGrouped = pd.merge(result, dfGrouped, how='outer', on=['date'])

    dfGrouped['number'] = dfGrouped['number'].fillna(0)

    return dfGrouped, dfAll
