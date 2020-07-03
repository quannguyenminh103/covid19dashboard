import requests, zipfile, io
def main():
    def download_file(zip_file_url):
        r = requests.get(zip_file_url)
        z = zipfile.ZipFile(io.BytesIO(r.content))
        z.extractall()
    download_file('https://ga-covid19.ondemand.sas.com/docs/ga_covid_data.zip')

    csv_url = 'https://covid19-lake.s3.us-east-2.amazonaws.com/tableau-covid-datahub/csv/COVID-19-Activity.csv'
    req = requests.get(csv_url)
    url_content = req.content
    csv_file = open('COVID-19-Activity.csv', 'wb')
    csv_file.write(url_content)
    csv_file.close()

    csv_url = 'https://covidtracking.com/api/v1/states/ga/daily.csv'
    req = requests.get(csv_url)
    url_content = req.content
    csv_file = open('daily.csv', 'wb')
    csv_file.write(url_content)
    csv_file.close()
main()
