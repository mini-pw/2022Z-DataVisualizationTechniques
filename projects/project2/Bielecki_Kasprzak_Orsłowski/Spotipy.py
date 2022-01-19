import spotipy, pandas as pd, matplotlib.pyplot as plt
from spotipy.oauth2 import SpotifyOAuth, SpotifyClientCredentials


def get_sp_auth():  # obiekt sp z autoryzacją
    sp = spotipy.Spotify(auth_manager=SpotifyOAuth(client_id="a8b8c48cee0545c488338c72fff5779c",
                                                   client_secret="d94ae17308ef4048aa131847ce052309",
                                                   redirect_uri="https://pyrki-dashboard.herokuapp.com/",
                                                   scope="user-read-recently-played"))
    return sp


def get_sp():  # obiekt sp bez autoryzacji
    sp = spotipy.Spotify(auth_manager=SpotifyClientCredentials(client_id="a8b8c48cee0545c488338c72fff5779c",
                                                               client_secret="d94ae17308ef4048aa131847ce052309"))
    return sp


if __name__ == '__main__':

    df = pd.read_json('MyData/StreamingHistory0.json')
    df2 = pd.read_json('MyData/StreamingHistory1.json')
    df = pd.concat([df, df2])
    #print(sp.artist(artist_id))
    #results = sp.current_user_recently_played(limit=50)  # limit = 50 to maksymalna wartość, parametr offset nie ma granicy
    #for idx, item in enumerate(results['items']):
    #    track = item['track']
    #    print(sp.audio_features(track['id']))
    #    print(idx, track['artists'][0]['name'], " – ", track['name'])
