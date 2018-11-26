#!/usr/bin/bash

# Opens all the sites that I need to authenticate to, so I have a valid cookie
# for the rest of the day.

sites=(
    https://app.datadoghq.com/logs
    https://console.aws.amazon.com
)

for site in "${sites[@]}"; do
    firefox --new-tab $site
done
