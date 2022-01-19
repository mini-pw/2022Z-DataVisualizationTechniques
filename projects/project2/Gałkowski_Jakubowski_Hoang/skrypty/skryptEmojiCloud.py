import streamlit as st


@st.experimental_memo(show_spinner=False)
def przygotowanieDanychEmojiCloud(df):
    data = df
    flattened = []
    for el in data.loc[data["author"] != ""].content.str.split(" ").to_list():
        if isinstance(el, list):
            for ell in el:
                flattened.append(ell)
        else:
            flattened.append(el)

    alf = "qwertyuiopasdfghjklzxcvbnmżłąęćźó"
    alfUpper = alf.upper()
    cyferki = "1234567890"
    dziwadlo = ",./;'[]-=)(*&^%$#@!:\"?><\{\}|+–'"

    flattened = [x for x in flattened if not isinstance(x, float)]

    filtered = filter(lambda mess: (not isinstance(mess, float)) & (len(mess) == 1) & (mess not in alf) & (mess not in alfUpper) & (mess != "\n") & (
            mess not in cyferki) & (mess not in dziwadlo), flattened)

    emoji = " ".join(filtered)

    return emoji
