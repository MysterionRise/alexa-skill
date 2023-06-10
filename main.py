import time

import pandas as pd
import requests
from lxml import html


def get_match_links(user_id, pages):
    match_links = []

    for page in range(1, pages + 1):
        url = f"https://www.dotabuff.com/players/{user_id}/matches?page={page}"

        # Send a GET request
        response = requests.get(url, headers={"User-agent": "your bot 0.1"})
        time.sleep(0.1)
        # If the GET request is successful, the status code will be 200
        if response.status_code == 200:
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
        else:
            print(
                f"Unable to retrieve page {page}, server responded with: {response.status_code}"
            )

    return match_links


def get_picks_bans(match_id):
    url = f"https://www.dotabuff.com/{match_id}"

    # Send a GET request
    response = requests.get(url, headers={"User-agent": "your bot 0.1"})
    time.sleep(0.1)
    # If the GET request is successful, the status code will be 200
    if response.status_code == 200:
        # Create an html element from the response content
        tree = html.fromstring(response.content)

        # Use XPath to find the ban divs
        pick_ban_divs = tree.xpath(
            "/html/body/div[2]/div[2]/div[3]/div[4]/div[1]/div[3]/section[1]/footer/div/div"
        )

        picks_bans = []

        for div in pick_ban_divs:
            class_name = div.get("class")  # Get class name of root div
            hero_name = div.xpath("./div/div/a/img/@alt")[0]
            picks_bans.append((class_name, hero_name))

        return picks_bans

    else:
        print(f"Unable to retrieve page, server responded with: {response.status_code}")


def get_match_result(match_id):
    url = f"https://www.dotabuff.com/{match_id}"

    # Send a GET request
    response = requests.get(url, headers={"User-agent": "your bot 0.1"})
    time.sleep(0.1)
    # If the GET request is successful, the status code will be 200
    if response.status_code == 200:
        # Create an html element from the response content
        tree = html.fromstring(response.content)

        # Use XPath to find the match result
        match_result = tree.xpath(
            "/html/body/div[2]/div[2]/div[3]/div[4]/div[1]/div[1]/text()"
        )

        if match_result:  # If there is a match result
            return match_result[0].strip().replace("Victory", "").lower()
        else:
            return "No Result Found"

    else:
        print(
            f"Unable to retrieve match result, server responded with: {response.status_code}"
        )


def get_ability_order(match_id, user_id):
    url = f"https://www.dotabuff.com/{match_id}"

    # Send a GET request
    response = requests.get(url, headers={"User-agent": "your bot 0.1"})
    time.sleep(0.1)
    # If the GET request is successful, the status code will be 200
    if response.status_code == 200:
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
        print(
            f"Unable to retrieve match page, server responded with: {response.status_code}"
        )


if __name__ == "__main__":
    user_id = "118794347"
    # match_links = get_match_links(user_id, pages=50)
    match_links = get_match_links(user_id, pages=1)

    rows = []

    for link in match_links:
        match_result = get_match_result(link)
        picks_bans = get_picks_bans(link)
        abilities = get_ability_order(link, user_id)
        for ban in picks_bans:
            rows.append(
                [
                    user_id,
                    link.split("/")[-1],
                    ban[0],
                    ban[1],
                    match_result,
                    "|".join(abilities),
                ]
            )

    df = pd.DataFrame(
        rows,
        columns=[
            "user_id",
            "match_id",
            "pick_or_ban",
            "hero_name",
            "result",
            "abilities",
        ],
    )
    df.to_csv(f"{user_id}_stats.csv", index=False)
