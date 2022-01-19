import pandas as pd

from Spotipy import get_sp


def most_skipped(df: pd.DataFrame):
    """returns data about a song that has been skipped the most

    :param df: DataFrame with spotify streaming history
    :return: a tuple: (cover_img_link, number_of_skips, track_name, artist_name)
    """
    sp = get_sp()
    results = df.loc[(df.msPlayed <= 2000)].groupby(["trackName", "artistName"]).trackName.agg('count').sort_values(
        axis=0, ascending=False)
    track_name = results.index[0][0]
    artist_name = results.index[0][1]
    artist_id = ''
    cover = ""
    tracks = sp.search(q=track_name, type='track', limit=50)
    for track in tracks['tracks']['items']:
        artist = track['artists']
        if artist_name == artist[0]['name']:
            artist_id = artist[0]['id']
            cover = track['album']['images'][0]['url']
            break
    # fig = plt.figure()
    # ax = fig.add_subplot(1,1,1)
    # x.a

    # fig = plt.figure()
    # ax = fig.add_subplot(1, 1, 1)
    # print(cover)
    # image = io.imread(cover)
    # ax = plt.imshow(image)
    # plt.axis('off')
    # text = 'najczęściej przewijana piosenka w ciągu pierwszych dwóch sekund.\nPrzewinięta została ' + str(
    #     results[0]) + ' razy.'
    # fig.suptitle(text)
    # size = fig.get_size_inches() * fig.dpi
    # plt.text(0, size[0] + 60, track_name + ' wykonawcy ' + artist_name, size=12)
    return cover, results[0], track_name, artist_name
