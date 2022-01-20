import spotipy
import json
import os
from spotipy.oauth2 import SpotifyClientCredentials
import pandas as pd

SPOTIPY_CLIENT_ID = "7416ca3313944d309a5a55234923b298"
SPOTIPY_CLIENT_SECRET = "d7e3de7766d8492bb056406e7fc2610b"

class ExtendedStreamHistoryParser:

    __slots__ = ['directory_path', 'output_path']

    def __init__(self, directory_path, output_path="parsed_data.csv"):
        self.directory_path = directory_path
        self.output_path = output_path

    @classmethod
    def __parse_json(cls, f):
        return json.load(f)

    @classmethod
    def __parse_stream(cls, stream):
        fields = [
            "ts",
            "ms_played",
            "master_metadata_track_name",
            "spotify_track_uri",
            "master_metadata_album_artist_name",
            "master_metadata_album_album_name",
            "reason_start",
            "reason_end",
            "shuffle",
            "skipped",
            "platform",
        ]

        output = ""
        for key in fields:
            field = str(stream[key])
            if field is not None:
                field = field.replace(';', '')
            output += field + ';'
        
        return output[:-1]+'\n'

    def __get_streaming_data(self):
        i = 0
        while True:
            file_path = f"{self.directory_path}/endsong_{i}.json"
            if not os.path.isfile(file_path):
                break
            with open(file_path, encoding="utf-8") as json_file:
                streaming_data = ExtendedStreamHistoryParser.__parse_json(json_file)
            yield streaming_data
            i += 1

    def parse(self):
        with open(self.output_path, 'w', encoding="utf-8") as parsed_data:
            parsed_data.write("date;ms;track;track_uri;artist;album;reason_start;reason_end;shuffle;skipped;platform\n")
            for streaming_data in self.__get_streaming_data():
                for stream in streaming_data:
                    parsed_data.write(ExtendedStreamHistoryParser.__parse_stream(stream))

class StreamHistoryParser:
    
    __slots__ = ['directory_path', 'output_path']

    def __init__(self, directory_path, output_path="parsed_data.csv"):
        self.directory_path = directory_path
        self.output_path = output_path

    @classmethod
    def __parse_json(cls, f):
        return json.load(f)

    @classmethod
    def __parse_stream(cls, stream):
        date = stream["endTime"]
        artist = stream["artistName"]
        track = stream["trackName"]
        time = stream["msPlayed"]
        return f"{date};{artist};{track};{time}\n"

    def __get_streaming_data(self):
        i = 0
        while True:
            file_path = f"{self.directory_path}/StreamingHistory{i}.json"
            if not os.path.isfile(file_path):
                break
            with open(file_path, encoding="utf-8") as json_file:
                streaming_data = StreamHistoryParser.__parse_json(json_file)
            yield streaming_data
            i += 1

    def parse(self):
        """
        Parses all StreamingHistory.json files in a given directory
        into one .csv file. The resulting file has the following structure

        date;artist;track;ms

        date    -- the date when the stream has ended
        artist  -- the name of the artist 
        track   -- the name of the track streamed
        ms      -- the amount of time that the stream has played for (in miliseconds)
        """
        with open(self.output_path, 'w', encoding="utf-8") as parsed_data:
            parsed_data.write("date;artist;track;ms\n")
            for streaming_data in self.__get_streaming_data():
                for stream in streaming_data:
                    parsed_data.write(StreamHistoryParser.__parse_stream(stream))

class StreamHistoryMetaGenerator:

    auth_manager = SpotifyClientCredentials(client_id=SPOTIPY_CLIENT_ID, client_secret=SPOTIPY_CLIENT_SECRET)
    sp = spotipy.Spotify(auth_manager=auth_manager)

    def __init__(self):
        pass

    @classmethod
    def __get_track_object(cls, artist_name, track_name):
        result = cls.sp.search(q=f"artist:{artist_name} track:{track_name}",type="track")
        if len(result["tracks"]["items"]) != 0: return result["tracks"]["items"][0]
        return None


    @classmethod
    def generate_meta(cls, data_path, output_path = "stream_history_meta_data.csv", artist_column = "artist", track_column = "track", use_uri = False):
        data = pd.read_csv(data_path, sep=";")
        all_tracks_count = len(data)
        unique_tracks_count = len(data[track_column].unique())
        track_stream_count = dict()
        with open(output_path, 'w', encoding='utf-8') as meta_data:
            meta_data.write("trackName;artists;primaryArtist;album;durationMs;explicit;artistGenres;popularity;danceability;energy;key;loudness;mode;speechiness;acousticness;instrumentalness;liveness;valence;tempo;timeSignature\n")
            for i, row in data.iterrows():
                track_id = row[track_column]

                # add track to dictionary with default to 0
                if track_id not in track_stream_count:
                    track_stream_count[track_id] = 0
                
                # skip repeated tracks
                if track_stream_count[track_id] > 0:
                    track_stream_count[track_id] += 1
                    continue

                # try to get the track. If query failed skip the track
                # for standard streaming history
                if (use_uri == False):
                    track = cls.__get_track_object(row[artist_column], track_id.replace("'", ""))
                    if track == None:
                        continue
                else:
                    try:
                        track = cls.sp.track(row["track_uri"])
                    except Exception as e:
                        print(e)
                        continue
                

                os.system('cls')
                print(f"""Analysing track...\n
                            Total: {i+1}/{all_tracks_count}\n
                            Unique: {len(track_stream_count.keys())}/{unique_tracks_count}\n
                            Name: {track_id}""")

                track_stream_count[track_id] += 1
                track_meta = get_track_meta_csv(cls.sp, track)
                meta_data.write(f"{track_meta}\n")

class PlaylistReader:

    __slots__ = ['playlist_link', 'playlist_name', 'sp', 'entries', 'data', 'output_path']

    def __init__(self, playlist_link, output_path = None):
        self.playlist_link = playlist_link

        SPOTIPY_CLIENT_ID = "7416ca3313944d309a5a55234923b298"
        SPOTIPY_CLIENT_SECRET = "d7e3de7766d8492bb056406e7fc2610b"
        auth_manager = SpotifyClientCredentials(client_id=SPOTIPY_CLIENT_ID, client_secret=SPOTIPY_CLIENT_SECRET)
        self.sp = spotipy.Spotify(auth_manager=auth_manager)
        
        self.playlist_name = self.sp.playlist(playlist_link)["name"]
        if (output_path is None): self.output_path = self.playlist_name + "_data.csv"

        self.entries = []
        i = 0
        limit = 100
        while True:
            entry_batch = self.sp.playlist_tracks(playlist_link, limit=limit, offset=i*limit)["items"]
            if len(entry_batch) == 0: break
            self.entries.extend(entry_batch)
            i += 1
        self.__create_data()

    def save_data(self, output_path = None):
        if output_path is None: output_path = self.output_path
        self.data.to_csv(output_path, sep=';')


    def __create_data(self):
        columns = ['dateAdded', 'addedBy', 'primaryColor', 'trackUri', \
            'trackName', 'artists', 'primaryArtist', 'album', 'durationMs', 'explicit', 'artistGenres', 'popularity', \
            'danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', \
            'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'timeSignature']
        self.data = pd.DataFrame(columns=columns)
        for entry in self.entries:
            values = {  'dateAdded': entry['added_at'],
                        'addedBy': entry['added_by']['id'],
                        'primaryColor': entry['primary_color'],
                        'trackUri': entry["track"]['uri']}
            track = entry["track"]
            track_meta = get_track_meta_csv(self.sp, track).split(';')
            for i in range(4, len(columns)):
                values[columns[i]] = track_meta[i-4]
            self.data = self.data.append(pd.DataFrame(values,index=[1]), ignore_index=True)
        return self.data


def get_track_details_csv(sp : spotipy.Spotify, track):
    """
    returns general information about the track
    """
    track_name = track["name"].replace(';', '')
    artists = [artist["name"] for artist in track["artists"]]
    primary_artist = artists[0]
    album = track["album"]["name"].replace(';', '')
    duration = track["duration_ms"]
    explicit = track["explicit"]
    genres = get_artist_genres_list(sp, track["artists"][0]["uri"])
    popularity = track["popularity"]
    return f"{track_name};{artists};{primary_artist};{album};{duration};{explicit};{genres};{popularity}"


def get_track_audio_features_csv(sp : spotipy.Spotify, track):
    """
    returns audio features of the track
    """
    audio_features = sp.audio_features(track["uri"])[0]
    if audio_features is None:
        return ""
    danceability = audio_features["danceability"]
    energy = audio_features["energy"]
    key = audio_features["key"]
    loudness = audio_features["loudness"]
    mode = audio_features["mode"]
    speechiness = audio_features["speechiness"]
    acousticness = audio_features["acousticness"]
    instrumentalness = audio_features["instrumentalness"]
    liveness = audio_features["liveness"]
    valence = audio_features["valence"]
    tempo = audio_features["tempo"]
    time_signature = audio_features["time_signature"]
    return f"{danceability};{energy};{key};{loudness};{mode};{speechiness};{acousticness};{instrumentalness};{liveness};{valence};{tempo};{time_signature}"


def get_artist_genres_list(sp : spotipy.Spotify, artist=None, track=None):
    """
    Returns genres of the first listed artist.

    If you want to get genres of all involved artists use
    __get_all_artists_genres instead
    """
    if (artist is None and track is None): return None
    if (artist is None):
        artist = sp.track(track)["artists"][0]["external_urls"]["spotify"]
    return sp.artist(artist)["genres"]


def get_track_meta_csv(sp : spotipy.Spotify, track):
    details = get_track_details_csv(sp, track)
    audio_features = get_track_audio_features_csv(sp, track)
    return f"{details};{audio_features}"


def main():
    directory_path = "Data/Raw/SMX"
    output_path = "Data/Parsed/SMX_parsed_data.csv"
    eshp = ExtendedStreamHistoryParser(directory_path, output_path)
    eshp.parse()
    StreamHistoryMetaGenerator.generate_meta(data_path = "Data/Parsed/SMX_parsed_data.csv", output_path="Data/Parsed/SMX_meta_data.csv")

if __name__ == '__main__':
    main()