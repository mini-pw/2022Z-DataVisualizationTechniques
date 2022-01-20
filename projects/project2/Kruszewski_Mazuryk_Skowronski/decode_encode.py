import json
import os


def write_json(target_path, target_file, data):
    if not os.path.exists(target_path):
        try:
            os.makedirs(target_path)
        except Exception as e:
            print(e)
            raise
    with open(os.path.join(target_path, target_file), 'w') as f:
        json.dump(data, f, indent=4)

def parse_obj(obj):
    for key in obj:
        if isinstance(obj[key], str):
            obj[key] = obj[key].encode('latin_1').decode('utf-8')
        elif isinstance(obj[key], list):
            obj[key] = list(map(lambda x: x if type(x) != str else x.encode('latin_1').decode('utf-8'), obj[key]))
        pass
    return obj


for dirname in os.listdir("messages\inbox"):
    for filename in os.listdir(os.path.join("messages\inbox", dirname)):
        if filename.endswith("json"):
            with open(os.path.join("messages\inbox",dirname,filename)) as f:
                df = json.load(f, object_hook=parse_obj)
                pass
            write_json(os.path.join('new\messages\inbox',dirname), filename, df)











