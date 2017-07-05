#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

cp /srv/shiny-server/.httr-oauth /.httr-oauth
exec shiny-server 2>&1

