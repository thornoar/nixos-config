#!/usr/bin/env bash

curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=AIzaSyAyEyMX6k5GoYy3KytyUYprPpbMSoVZAbc" -H "Content-Type: application/json" -X POST -d "{ \"contents\": [{ \"parts\":[{\"text\": \"$1\"}] }] }" | jq ".candidates.[].content.parts.[].text"
