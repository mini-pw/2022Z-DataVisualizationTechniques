import os
import json

import pandas as pd

from typing import Union
from itertools import chain
from datetime import datetime
from tkinter import Tk
from tkinter import filedialog


class Fb_parser:
    __slots__ = ['path', 'friends_data', 'friend_requests_sent_data', 'friend_requests_received_data',
                 'friend_requests_rejected_data']

    def __init__(self, path: str):
        self.path = path
        self.friends_data = []
        self.friend_requests_sent_data = []
        self.friend_requests_received_data = []
        self.friend_requests_rejected_data = []

    @staticmethod
    def __get_date(timestamp: int) -> str:
        """
        transform date from ms to year-month-day-hour format
        """
        date = datetime.fromtimestamp(timestamp)
        return date.strftime("%Y.%m.%d %X")

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

    def export_to_csv(self, name1: str, name2: str, name3: str, name4: str) -> None:
        '''
        Exports friends_data to .csv file
        :param name - name of the file output
        '''

        with open(self.path + '/' + name1 + '.csv', 'w', encoding='utf-8') as parsed_data1:
            parsed_data1.write('who;time\n')

            for people in self.friends_data:
                who = people['name']
                when = Fb_parser.__get_date(people['timestamp'])

                row = f'{who};{when}'
                parsed_data1.write(f'{row}\n')

        with open(self.path + '/' + name2 + '.csv', 'w', encoding='utf-8') as parsed_data2:
            parsed_data2.write('who;time\n')

            for people in self.friend_requests_sent_data:
                who = people['name']
                when = Fb_parser.__get_date(people['timestamp'])

                row = f'{who};{when}'
                parsed_data2.write(f'{row}\n')

        with open(self.path + '/' + name3 + '.csv', 'w', encoding='utf-8') as parsed_data3:
            parsed_data3.write('who;time\n')

            for people in self.friend_requests_received_data:
                who = people['name']
                when = Fb_parser.__get_date(people['timestamp'])

                row = f'{who};{when}'
                parsed_data3.write(f'{row}\n')

        with open(self.path + '/' + name4 + '.csv', 'w', encoding='utf-8') as parsed_data4:
            parsed_data4.write('who;time\n')

            for people in self.friend_requests_rejected_data:
                who = people['name']
                when = Fb_parser.__get_date(people['timestamp'])

                row = f'{who};{when}'
                parsed_data4.write(f'{row}\n')

    def __get_friends_data(self) -> None:
        '''
        get: message_data, total amount of messages, conversation name
        by reading files in {self.path}
        '''

        file_path = f'{self.path}'

        for filename in os.listdir(file_path):
            if filename == "friends.json":
                with open(file_path + "/" + filename) as json_file:
                    tmp = Fb_parser.parse_json(json_file)
                    self.friends_data.append(tmp['friends_v2'])
                self.friends_data = list(chain.from_iterable(self.friends_data))

            if filename == "friend_requests_sent.json":
                with open(file_path + "/" + filename) as json_file:
                    tmp = Fb_parser.parse_json(json_file)
                    self.friend_requests_sent_data.append(tmp['sent_requests_v2'])
                self.friend_requests_sent_data = list(chain.from_iterable(self.friend_requests_sent_data))

            if filename == "friend_requests_received.json":
                with open(file_path + "/" + filename) as json_file:
                    tmp = Fb_parser.parse_json(json_file)
                    self.friend_requests_received_data.append(tmp['received_requests_v2'])
                self.friend_requests_received_data = list(chain.from_iterable(self.friend_requests_received_data))

            if filename == "rejected_friend_requests.json":
                with open(file_path + "/" + filename) as json_file:
                    tmp = Fb_parser.parse_json(json_file)
                    self.friend_requests_rejected_data.append(tmp['rejected_requests_v2'])
                self.friend_requests_rejected_data = list(chain.from_iterable(self.friend_requests_rejected_data))

    def parse_data(self) -> None:
        self.__get_friends_data()
        print(self.friend_requests_received_data, "\n", self.friend_requests_rejected_data, "\n",
              self.friend_requests_sent_data, "\n", self.friends_data)
        self.export_to_csv('friends', 'friend_requests_sent', 'friend_requests_received', 'friend_requests_rejected')


def main():
    root = Tk()
    root.withdraw()
    path = filedialog.askdirectory()
    print(f'path: {path}')
    parser = Fb_parser(path)
    parser.parse_data()


if __name__ == '__main__':
    main()
