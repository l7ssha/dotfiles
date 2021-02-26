#!/bin/bash

sudo ln -s $(pwd)/systemd/downloadupdate.service /etc/systemd/system/downloadupdate.service
sudo ln -s $(pwd)/systemd/downloadupdate.timer /etc/systemd/system/downloadupdate.timer

sudo systemctl daemon-reload
sudo systemctl enable --now downloadupdate.timer
