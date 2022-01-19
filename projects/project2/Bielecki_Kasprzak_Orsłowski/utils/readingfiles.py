import base64
import io
import shutil
import re
import os
from zipfile import ZipFile

import pandas as pd


def parse_zip(contents: str) -> ZipFile:
    """
    Creates a ZipFile object from a .zip file in the base64 string format
    (which is the way Dash provides uploaded files).
    :param contents: contents property of dcc.Upload (base64 encoded string
    containing uploaded file contents)
    :return: ZipFile object
    """
    content_type, content_string = contents.split(',')
    content_decoded = base64.b64decode(content_string)
    zip_str = io.BytesIO(content_decoded)
    return ZipFile(zip_str, 'r')


def get_streaming_history(zipped_data: ZipFile) -> pd.DataFrame:
    """
    Reads "StreamingHistoryX.json" files from zipped data from Spotify and
    dumps their content into a DataFrame.

    :param zipped_data: ZipFile class object representing .zip file
    containing Spotify data (or at least "StreamingHistoryX.json" files)
    :return: Data frame containing streaming history
    """
    output_dir_name = "tmp"
    try:
        os.makedirs(output_dir_name)  # create temp folder for imported files
    except FileExistsError:
        for file_to_remove in os.listdir(output_dir_name):  # if it already exists, clear it
            shutil.rmtree(os.path.join(output_dir_name, file_to_remove))
    zipped_data.extractall(output_dir_name)  # extract imported files
    result = pd.DataFrame()
    streaming_hist_regex = re.compile("StreamingHistory\\d+.json")
    # searching for "StreamingHistoryX.json" files
    curr_path = os.path.abspath(output_dir_name)
    dir_contents = os.listdir(curr_path)
    if len(dir_contents) == 1 and os.path.isdir(
            os.path.join(output_dir_name, dir_contents[0])):  # True if zip has a folder in it
        curr_path = os.path.join(curr_path, dir_contents[0])
        dir_contents = os.listdir(curr_path)
    for filename in filter(streaming_hist_regex.match, dir_contents):
        file_df = pd.read_json(os.path.join(curr_path, filename))
        result = pd.concat([result, file_df],
                           axis=0, ignore_index=True)
    shutil.rmtree(output_dir_name)
    return result.sort_values("endTime", ignore_index=True)
