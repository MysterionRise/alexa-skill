import glob
import time
from datetime import datetime

import pandas as pd
import requests
from lxml import html
from requests.adapters import HTTPAdapter
from urllib3 import Retry


def make_request_with_retries(url):
    session = requests.Session()

    session.headers.update(
        {
            "User-Agent": "Mozilla/5.0 (iPad; CPU OS 12_2 like Mac OS X) "
            "AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148"
        }
    )

    retry_strategy = Retry(
        total=5,
        backoff_factor=1,
        status_forcelist=[429, 500, 502, 503, 504],
        allowed_methods=["HEAD", "GET", "OPTIONS"],
    )

    adapter = HTTPAdapter(max_retries=retry_strategy)
    session.mount("https://", adapter)
    session.mount("http://", adapter)
    time.sleep(1)
    try:
        response = session.get(url)
        response.raise_for_status()
        return response
    except requests.exceptions.HTTPError as errh:
        print(f"Http Error: {errh}")
    except requests.exceptions.ConnectionError as errc:
        print(f"Error Connecting: {errc}")
    except requests.exceptions.Timeout as errt:
        print(f"Timeout Error: {errt}")
    except requests.exceptions.RequestException as err:
        print(f"Something went wrong: {err}")


def read_links_from_file():
    file_name = "data/match_links.csv"
    with open(file_name, "r") as file:
        match_links = [link.strip() for link in file.readlines()]
    return match_links


def get_match_links(user_id, from_page, to_page):
    match_links = []

    for page in range(from_page, to_page + 1):
        url = f"https://www.dotabuff.com/players/{user_id}/matches?page={page}&lobby_type=ranked_matchmaking"

        # Send a GET request
        response = make_request_with_retries(url)
        time.sleep(1)
        # If the GET request is successful, the status code will be 200
        if response is not None:
            # Create an html element from the response content
            tree = html.fromstring(response.content)

            # Use XPath to find the matches
            rows = tree.xpath(
                "/html/body/div[2]/div[2]/div[3]/div[4]/section/section/article/table/tbody/tr"
            )

            for row in rows:
                match_link_element = row.xpath("./td[4]/a")

                if match_link_element:  # If there is a link
                    match_link = match_link_element[0].get("href")
                    match_links.append(match_link)
                    # Write to file
                    with open("data/match_links.csv", "a") as file:
                        file.write(match_link + "\n")
        else:
            print(f"Unable to retrieve page {page}")

    return match_links


def get_picks_bans(match_id):
    url = f"https://www.dotabuff.com/{match_id}"

    # Send a GET request
    response = make_request_with_retries(url)
    time.sleep(1)
    # If the GET request is successful, the status code will be 200
    if response is not None:
        # Create an html element from the response content
        tree = html.fromstring(response.content)

        # Use XPath to find the ban divs
        # Use XPath to find the divs containing the pick/ban data
        divs_path1 = tree.xpath(
            "/html/body/div[2]/div[2]/div[3]/div[4]/div[1]/div[3]/section[2]/footer/div/div"
        )
        divs_path2 = tree.xpath(
            "/html/body/div[2]/div[2]/div[3]/div[4]/div[1]/div[3]/section[1]/footer/div/div"
        )

        # Combine the lists
        pick_ban_divs = divs_path1 + divs_path2

        picks_bans = []

        for div in pick_ban_divs:
            class_name = div.get("class")  # Get class name of root div
            hero_name = div.xpath("./div/div/a/img/@alt")[0]
            picks_bans.append((class_name, hero_name))

        return picks_bans

    else:
        print(f"Unable to retrieve picks/bans for match with id {match_id}")


def get_match_result(match_id):
    url = f"https://www.dotabuff.com/{match_id}"

    # Send a GET request
    response = make_request_with_retries(url)
    time.sleep(1)
    # If the GET request is successful, the status code will be 200
    if response is not None:
        # Create an html element from the response content
        tree = html.fromstring(response.content)

        # Use XPath to find the match result
        match_result = tree.xpath(
            "/html/body/div[2]/div[2]/div[3]/div[4]/div[1]/div[1]/text()"
        )

        if match_result:  # If there is a match result
            return match_result[0].strip().replace("Victory", "").lower().strip()
        else:
            return "No Result Found"

    else:
        print(f"Unable to retrieve match result for match with id {match_id}")


def get_user_side(match_id, user_id):
    url = f"https://www.dotabuff.com/{match_id}"

    response = make_request_with_retries(url)
    time.sleep(0.1)

    if response is not None:
        tree = html.fromstring(response.content)
        player_link = f"/players/{user_id}"
        player_element = tree.xpath(f'//a[@href="{player_link}"]')
        if player_element:
            classes = player_element[0].attrib["class"].split()
            if "player-radiant" in classes:
                return "radiant"
            elif "player-dire" in classes:
                return "dire"
    return None


def get_ability_order(match_id, user_id):
    url = f"https://www.dotabuff.com/{match_id}"

    # Send a GET request
    response = make_request_with_retries(url)
    time.sleep(1)
    # If the GET request is successful, the status code will be 200
    if response is not None:
        # Create an html element from the response content
        tree = html.fromstring(response.content)

        # Use XPath to find the tr for this player
        player_trs = tree.xpath(
            f'//tr[contains(@class, "col-hints") and contains(@class, "player-{user_id}")]'
        )

        abilities = []
        for tr in player_trs:
            # Use XPath to find the abilities
            ability_imgs = tr.xpath('.//td[@class="ability"]/div/a/img')

            for img in ability_imgs:
                ability_name = img.get("alt")  # Get ability name
                if ability_name is not None:
                    abilities.append(ability_name)
                else:
                    abilities.append("No Ability Found")

        return abilities

    else:
        print(f"Unable to retrieve abilities for match with id {match_id}")


def merge_csv_files(directory):
    # Use glob to match all csv files in the directory
    csv_files = glob.glob(directory + "/*_chunk_*.csv")

    # Create a list to hold dataframes
    df_list = []

    # Read each csv file and append to the list
    for csv_file in csv_files:
        df = pd.read_csv(csv_file)
        df_list.append(df)

    # Concatenate all dataframes in the list
    merged_df = pd.concat(df_list, ignore_index=True)

    # Save the merged dataframe to a new csv file
    merged_df.to_csv(directory + "/merged.csv", index=False)


def get_match_date(match_id):
    url = f"https://www.dotabuff.com/{match_id}"

    # Send a GET request
    response = make_request_with_retries(url)
    time.sleep(1)
    # If the GET request is successful, the status code will be 200
    if response is not None:
        tree = html.fromstring(response.content)
        dl_elements = tree.xpath(
            "/html/body/div[2]/div[2]/div[3]/div[3]/div[1]/div[2]/dl"
        )
        for dl in dl_elements:
            time_element = dl.xpath("./dd/time")
            if time_element:
                date_string = time_element[0].attrib.get("datetime")
                return datetime.strptime(date_string, "%Y-%m-%dT%H:%M:%S%z")
    else:
        print(f"Unable to retrieve match_date for match with id {match_id}")


def process_matches_in_chunks(user_id, match_links, start_chunk=0, chunk_size=10):
    match_links_count = len(match_links)

    for i in range(start_chunk * chunk_size, match_links_count, chunk_size):
        rows = []
        chunk_links = match_links[i : i + chunk_size]

        for link in chunk_links:
            match_date = get_match_date(link)
            match_result = get_match_result(link)
            picks_bans = get_picks_bans(link)
            abilities = get_ability_order(link, user_id)
            user_side = get_user_side(link, user_id)
            rows.append(
                [
                    user_id,
                    match_date,
                    link.split("/")[-1],
                    "|".join([hero for action, hero in picks_bans if action == "pick"]),
                    "|".join([hero for action, hero in picks_bans if action == "ban"]),
                    user_side,
                    match_result,
                    "|".join(abilities),
                ]
            )

        time.sleep(1)

        df = pd.DataFrame(
            rows,
            columns=[
                "user_id",
                "date",
                "match_id",
                "picks",
                "bans",
                "user_side",
                "result",
                "abilities",
            ],
        )
        df.to_csv(f"matches_{user_id}_chunk_{i // chunk_size + 1}.csv", index=False)


if __name__ == "__main__":
    user_id = "118794347"
    # process_matches_in_chunks(user_id, read_links_from_file(), start_chunk=126)
    merge_csv_files("data")
