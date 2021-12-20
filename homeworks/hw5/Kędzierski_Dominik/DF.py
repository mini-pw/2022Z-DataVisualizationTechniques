import pandas as pd

def get_df():
    df = pd.read_csv("complete.csv")
    df = df.loc[:,['awardYear', 'category', 'prizeAmount', 'birth_date']]
    df['age'] = df['birth_date'].map(lambda x: str(x)[0:4])
    df = df.dropna()
    df['age'] = df['awardYear'] - df['age'].astype('int32')
    df = df.groupby(['category', 'awardYear']).mean().reset_index()
    return df

if __name__ == '__main__':
    print(get_df())
    df = pd.read_csv('https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv')
    print(df)