#!/usr/bin/env python3
'''
This is a simple script to tell me when I need to leave, based on when I
arrive and about half an hour of break.
'''

import datetime
from typing import List
import re

TIME_AT_WORK = datetime.timedelta(hours=9)


def main():
    today = find_time_in_lines(todays_log())
    s = today.split(":")
    hours = int(s[0])
    minutes = int(s[1])
    arrived_time = datetime.time(hour=hours, minute=minutes)
    arrived = datetime.datetime.combine(datetime.date.today(), arrived_time)
    leaving = arrived + TIME_AT_WORK
    now = datetime.datetime.now()
    remaining = leaving - now
    print(f"Arrived today: {str(arrived_time)[:5]}")
    print(f"Leaving today: {str(leaving.time())[:5]}")
    print("-" * 20)
    print(f"Time to leave: {str(remaining)[:6]}")


def get_todays_file() -> str:
    today = datetime.datetime.now()
    filename = today.isoformat()[:10]
    filename = "/home/az/logs/" + filename
    return filename


def todays_log() -> List[str]:
    lines = []
    with open(get_todays_file(), 'r') as f:
        for line in f:
            lines.append(line)

    return lines


def find_time_in_lines(lines: List[str]) -> str:
    regex = r"^ (\d\d:\d\d) - Arrived at work$"
    for line in lines:
        match = re.match(regex, line)
        if match is None:
            continue

        return match.group(1)

    print(lines)
    raise ValueError("No match found in today's log.")


if __name__ == '__main__':
    main()
