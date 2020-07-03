import pandas as pd

def main():
    url1 = 'https://raw.githubusercontent.com/WeitzGroup/MAGEmodel_covid19_GA/master/SimulationOutput/28thApr_Run_int_1stMay/Social_1/GA_Summary.csv';
    df1 = pd.read_csv(url1,index_col=0);
    df1.reset_index(drop=False, inplace=False)
    df1 = df1.T;
    df1.to_csv('Baseline_data.csv');

    url2 = 'https://raw.githubusercontent.com/WeitzGroup/MAGEmodel_covid19_GA/master/SimulationOutput/28thApr_Run_int_1stMay/Social_3/GA_Summary.csv'
    df2 = pd.read_csv(url2,index_col=0);
    df2.reset_index(drop=False, inplace=False)
    df2 = df2.T;
    df2.to_csv('transmission50_data.csv');

    url3 = 'https://raw.githubusercontent.com/WeitzGroup/MAGEmodel_covid19_GA/master/SimulationOutput/28thApr_Run_int_1stMay/Social_4/GA_Summary.csv'
    df3 = pd.read_csv(url3,index_col=0);
    df3.reset_index(drop=False, inplace=False)
    df3 = df3.T;
    df3.to_csv('transmission75_data.csv');

    
    url4 = 'https://raw.githubusercontent.com/WeitzGroup/MAGEmodel_covid19_GA/master/data/GA_county_decade_age.csv'
    df4 = pd.read_csv(url4,index_col=0);
    df4.to_csv('POPsize.csv');
main()
