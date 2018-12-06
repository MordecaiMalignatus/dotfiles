#!/usr/bin/bash
# Set up standard tmux sessions that I tend to need.

set -e

SESSION_NAME="work"
MAIN_WINDOW_NAME="command-central"

eval `ssh-agent`
ssh-add

if ! (tmux has-session -t $SESSION_NAME 2>/dev/null); then
  tmux new -d -s $SESSION_NAME -c "$HOME"
  tmux new-window -t $SESSION_NAME:2 -n $MAIN_WINDOW_NAME -c $HOME 'fish -iC "arrive"'
  tmux split-window -t $SESSION_NAME:2 -h
  tmux new-window -t $SESSION_NAME:3 -n wat-infrastructure -c "$HOME/work/wat-infrastructure"
  tmux new-window -t $SESSION_NAME:4 -n crawler-stream -c "$HOME/work/crawler-stream"
  tmux new-window -t $SESSION_NAME:5 -n zoom-importer -c "$HOME/work/zoom-importer"
  tmux kill-window -t $SESSION_NAME:1

  tmux attach -t $SESSION_NAME:1.0
fi
