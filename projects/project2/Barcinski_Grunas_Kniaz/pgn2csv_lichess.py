import chess.pgn
import pandas as pd

def convert_lichess(filepath):
    f = open(filepath)

    games_df = pd.DataFrame(columns=['id', 'date', 'time', 'white', 'black', 'white elo', 'black elo', 'time control', 'opening code', 'opening name', 'length', 'termination', 'result', 'hero points', 'has eval', 'has increment', 'link'])
    games_moves_df = pd.DataFrame(columns=['game id', 'move number', 'white', 'white time', 'white eval', 'black', 'black time', 'black eval'])

    i = 1
    while True:
        game = chess.pgn.read_game(f)
        if game is None:
            break

        if str(game.mainline_moves()) == "": continue

        game_details = str(game)
        game_date = game_details.split("Date \"")[1].split("\"")[0]
        game_time = game_details.split("UTCTime \"")[1].split("\"")[0]
        game_white = game_details.split("White \"")[1].split("\"]")[0]
        game_black = game_details.split("Black \"")[1].split("\"]")[0]
        game_white_elo = game_details.split("WhiteElo \"")[1].split("\"]")[0]
        game_black_elo = game_details.split("BlackElo \"")[1].split("\"]")[0]
        game_time_control = game_details.split("TimeControl \"")[1].split("\"]")[0]
        game_opening_code = game_details.split("ECO \"")[1].split("\"]")[0]
        game_opening_name = game_details.split("Opening \"")[1].split("\"]")[0]
        game_length = game_details.rsplit(". ", 1)[0].rsplit(" ", 1)[1]
        if game_length[-1] == '.':
            game_length = game_length[:-2]
        game_termination = game_details.split("Termination \"")[1].split("\"]")[0]
        if (game_termination == "Normal" and game_details.find('#') != -1):
            game_termination = "Checkmate"
        if game_termination == "Normal": game_termination = "Resign"
        game_result = game_details.split("Result \"")[1].split("\"")[0]
        if filepath.find('black') != -1:
            hero_points = 1 if game_result == '0-1' else 0.5 if game_result == '1/2-1/2' else 0
        else:
            hero_points = 0 if game_result == '0-1' else 0.5 if game_result == '1/2-1/2' else 1
        game_has_eval = True if str(game.mainline_moves()).find("eval") != -1 else False
        game_has_increment = True if game_time_control[-1] != '0' else False
        game_link = game_details.split("Site \"")[1].split("\"]")[0]
        games_df = games_df.append({'id' : i, 'date': game_date, 'time': game_time, 'white': game_white, 'black': game_black, 'white elo': game_white_elo, 'black elo': game_black_elo, 'time control': game_time_control, 'opening code': game_opening_code, 'opening name': game_opening_name, 'length': game_length, 'termination': game_termination,'result': game_result, 'hero points': hero_points, 'has eval': game_has_eval, 'has increment': game_has_increment, 'link': game_link}, ignore_index=True)

        game_moves_df = pd.DataFrame(
            columns=['game id', 'move number', 'white', 'white time', 'white eval', 'black', 'black time',
                     'black eval'])
        game_moves = str(game.mainline_moves()) + "eval ] clk ] .  eval ] clk ] .  eval ] clk ] . eval ] clk ] . "
        print(i, game_white, game_black, game_date)
        for j in range(int(game_length)):

            game_moves = game_moves.split(". ", 1)[1]
            game_white_move = game_moves.split(" ", 1)[0]
            if game_has_eval:
                if game_moves.find('eval') == -1:
                    game_has_eval = False
                    game_white_eval = None
                else:
                    game_moves = game_moves.split("eval ", 1)[1]
                    game_white_eval = game_moves.split("]")[0]
            else:
                game_white_eval = None
            game_moves = game_moves.split("clk ", 1)[1]
            game_white_time = game_moves.split("]")[0]
            game_moves = game_moves.split(". ", 1)[1]
            game_black_move = game_moves.split(" ", 1)[0]
            if game_has_eval:
                if game_moves.find('eval') == -1:
                    game_has_eval = False
                    game_black_eval = None
                else:
                    game_moves = game_moves.split("eval ", 1)[1]
                    game_black_eval = game_moves.split("]")[0]
            else:
                game_black_eval = None
            game_moves = game_moves.split("clk ", 1)[1]
            game_black_time = game_moves.split("]")[0]
            game_moves_df = game_moves_df.append(
                {'game id': i, 'move number': j + 1, 'white': game_white_move, 'white time': game_white_time,
                 'white eval': game_white_eval, 'black': game_black_move, 'black time': game_black_time,
                 'black eval': game_black_eval}, ignore_index=True)

        games_moves_df = games_moves_df.append(game_moves_df)
        i += 1

    f1 = "dane_pierwotne" + filepath[3:-4] + '_games.csv'
    f2 = "dane_pierwotne" + filepath[3:-4] + '_moves.csv'

    games_df.to_csv(f1, index=False)
    games_moves_df.to_csv(f2, index=False)

# convert_lichess()