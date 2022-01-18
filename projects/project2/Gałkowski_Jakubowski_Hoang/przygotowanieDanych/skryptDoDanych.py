import os
import json
import datetime
import pandas as pd


def createDataframe(osoba):
    root = os.path.join(os.getcwd(), "wiadomosci")

    print(root)

    message_id = 1
    output = []

    for mess_folder in os.listdir(root):  # dodane
        if mess_folder == "tutajDodajemyFolder_Foledry_messages.txt":
            continue
        root = os.path.join(os.getcwd(), "wiadomosci")  # dodane
        root = os.path.join(root, mess_folder, "inbox")  # dodane
        for thread in os.listdir(root):
            for messagefile in os.listdir(os.path.join(root, thread)):
                if messagefile.endswith("json"):
                    filepath = os.path.join(root, thread, messagefile)
                    with open(filepath) as jsonfile:
                        data = json.load(jsonfile)
                        messages = data["messages"]
                        for message in messages:
                            if message["type"] == "Generic":
                                author = message["sender_name"].encode('iso-8859-1').decode('utf-8')
                                sex = "female" if author != "" and author != "Kuba" and \
                                                  author.split(" ")[0][-1] == "a" else "male"
                                if author != osoba:
                                    author = ""
                                dt = datetime.datetime.fromtimestamp(message["timestamp_ms"] // 1000)
                                whole_date = dt.isoformat()
                                year = dt.year
                                month = dt.month
                                day = dt.day
                                hour = dt.hour
                                minute = dt.minute
                                second = dt.second
                                content = message.get("content")
                                enc = ""
                                if content:
                                    enc = content.encode('iso-8859-1').decode('utf-8')
                                output.append([message_id, author, sex, whole_date, year, month, day, hour, minute,
                                               second, enc])
                                message_id += 1

    df = pd.DataFrame(output, columns=["id", "author", "sex", "date", "year", "month", "day", "hour", "minute",
                                       "second", "content"])

    df.to_csv(("wiadomosci" + osoba.split(" ")[0] + ".csv"), index=False)


createDataframe("Mikołaj Gałkowski")
