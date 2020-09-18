# haskell-twitch-overlay

[![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)

Next Steps
---

We now can serve static content from `./static`, now we need a `index.js` which
contains a Purescript app.  We need to build this Purescript app and add
features. Need to build a `make serve` function to build the purescript app
and then run `stack run`.

Feature Requests
---

- `curl -X POST <frontend>/overlay?brb={true,false,toggle}` which will set, unset, or
  toggle some "Be Right Back" text.
- Respond to subscription, follow, and bit Twitch requests
  - Twitch sends `GET <backend>/...`, we send `POST <frontend>/new{Notification}?name={}&months={}...`
- Display snail emojis when chat enters snail emojis, i.e. get chat involved
