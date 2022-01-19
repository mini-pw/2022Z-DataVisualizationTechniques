import os
import json

import pandas as pd

from typing import Union
from itertools import chain
from datetime import datetime
from tkinter import Tk
from tkinter import filedialog


class Parser:
    __slots__ = ['total_messages', 'path', 'messages_data', 'single_person']

    def __init__(self, path: str, single_person: bool = True):
        self.total_messages = 0
        self.path = path
        self.messages_data = []
        self.single_person = single_person

    @staticmethod
    def __get_date(timestamp_ms: int) -> str:
        """
        transform date from ms to year-month-day-hour format
        """
        date = datetime.fromtimestamp(timestamp_ms / 1000)
        return date.strftime("%Y.%m.%d %X")

    @staticmethod
    def __sanitize_string(s: str) -> str:
        s = s.replace('"', '""')
        s = s.replace(';', ' ')
        s = s.replace('\n', '')
        s = s.replace('\r', '')
        return s

    @staticmethod
    def parse_json(f: Union[str, bytes]) -> dict:
        def fix_unicode(obj: Union[str, list, dict]) -> object:
            try:
                if isinstance(obj, str):
                    return obj.encode('latin_1').decode('utf-8')
                if isinstance(obj, list):
                    return [fix_unicode(o) for o in obj]
                if isinstance(obj, dict):
                    return {key: fix_unicode(item) for key, item in obj.items()}
                return obj
            except:
                return obj

        return json.load(f, object_hook=fix_unicode)

    def export_to_csv(self, name: str) -> None:
        """
        Exports message_data to .csv file
        :param name - name of the file output
        """
        with open(self.path + '/' + name + '.csv', 'w', encoding='utf-8') as parsed_data:
            parsed_data.write('name;who;time;type;text;emoji\n')

            for mess in self.messages_data:
                name = mess['name']
                name = Parser.__sanitize_string(name)

                who = mess['sender_name']
                when = Parser.__get_date(mess['timestamp_ms'])
                emojis = Parser.__get_reactions(mess)
                type, content = Parser.__get_type_and_content(mess)
                content = Parser.__sanitize_string(content)

                row = f'{name};{who};{when};{type};{content};{emojis}'
                parsed_data.write(f'{row}\n')

    def __get_messages_data(self) -> None:
        """
        get: message_data, total amount of messages, conversation name
        by reading files in {self.path}
        """
        i = 0
        while True:
            i += 1
            file_path = f'{self.path}/message_{i}.json'
            if not os.path.isfile(file_path):
                break
            with open(file_path) as json_file:
                tmp = Parser.parse_json(json_file)
                self.messages_data.append(tmp['messages'])

        self.messages_data = list(chain.from_iterable(self.messages_data))
        self.total_messages = len(self.messages_data)

    def __get_all_messages_data(self) -> None:
        """
        get: message_data, total amount of messages, conversation name
        by reading files in from folders in {self.path}
        """
        for relative_dir in os.listdir(self.path):

            if not os.path.isdir(f'{self.path}/{relative_dir}'):
                continue

            print(f'parsing {relative_dir}')

            i = 0
            curr_messages = []
            while True:
                i += 1
                file_path = f'{self.path}/{relative_dir}/message_{i}.json'
                if not os.path.isfile(file_path):
                    break
                with open(file_path) as json_file:
                    tmp = Parser.parse_json(json_file)
                    for mess in tmp['messages']:
                        mess['name'] = tmp['title']
                    curr_messages.append(tmp['messages'])

            self.messages_data.extend(list(chain.from_iterable(curr_messages)))

        self.total_messages = len(self.messages_data)

    def parse_data(self) -> None:
        if self.single_person:
            self.__get_messages_data()
        else:
            self.__get_all_messages_data()

        self.export_to_csv('messengerData')

    @classmethod
    def __get_reactions(cls, message: dict) -> str:
        """
        :return: None or string that is combined with emojis left under specific message
        /w user that gave it
        """
        if "reactions" not in message:
            return ""
        reactions_list = list()
        for reaction_data in message["reactions"]:
            reactions_list.append(f"{reaction_data['reaction']} ({reaction_data['actor']})")
        return ",".join(reactions_list)

    @classmethod
    def __get_type_and_content(cls, message: dict) -> tuple:
        """
        :returns type of the message, information about specific message type
        """
        if "call_duration" in message:
            return "C", f"{message['call_duration']}"
        if "content" in message:
            return "T", message["content"]
        if "photos" in message:
            return "P", ";".join([message["photos"][i]["uri"] for i in range(len(message["photos"]))])
        if "videos" in message:
            return "V", ";".join([message["videos"][i]["uri"] for i in range(len(message["videos"]))])
        if "audio_files" in message:
            return "A", ";".join([message["audio_files"][i]["uri"] for i in range(len(message["audio_files"]))])
        if "gifs" in message:
            return "G", ";".join([message["gifs"][i]["uri"] for i in range(len(message["gifs"]))])
        if "sticker" in message:
            return "S", message["sticker"]["uri"]
        if message["is_unsent"]:
            return "U", ""
        return "-", ""


def main():
    root = Tk()
    root.withdraw()
    path = filedialog.askdirectory()
    print(f'path: {path}')
    parser = Parser(path, single_person=False)

    parser.parse_data()

    df = pd.read_csv(path + '/messengerData.csv', sep=';')
    print(df.head())


if __name__ == '__main__':
    main()
