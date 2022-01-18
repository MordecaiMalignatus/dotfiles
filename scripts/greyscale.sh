#!/bin/bash

setGrayscale () {

    defaults write com.apple.universalaccess grayscale -bool $1
    defaults write com.apple.CoreGraphics DisplayUseForcedGray -bool $1
    launchctl unload /System/Library/LaunchAgents/com.apple.universalaccessd.plist
    launchctl load /System/Library/LaunchAgents/com.apple.universalaccessd.plist

    case "$1" in
        "NO")
            echo "  Changing Display to use color. This will take a moment..."
        ;;
        "YES")
            echo "  Changing Display to use grayscale. This will take a moment..."
        ;;
    esac

}

_bool="$(defaults read com.apple.universalaccess grayscale 2>/dev/null)"

case "$_bool" in
    "0")
        setGrayscale "YES"
    ;;
    "1")
        setGrayscale "NO"
    ;;
    *)
        setGrayscale "YES"
    ;;
esac

