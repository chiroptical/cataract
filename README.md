# Pluto

[![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)

Pluto is a recursive acronym for "PLUto Twitch Overlay". The goal is to create
an open-source overlay with features similar to Streamlabs or Player.me. The
overlay should be responsive via an API and handle new follows, subscriptions,
and bit donations.

I believe we shouldn't have to focus on the various Twitch APIs and their
authentication and instead work on interesting custom components with our
communities. Ideally, getting viewers involved when possible.

This application is coded using Haskell, Yesod, and Persistent. The project is
early work in progress.

Next steps
---

1. Store the twitch username along with the identity
2. Bring in animatorium code into overlay.julius and ensure follow, raid, cheer, subscribe display as text to overlay
3. Subscribe to raid, cheer, follow, and subscribe events on overlay start up
4. Deploy the container to heroku and start testing live events

[twitch-cli]: https://dev.twitch.tv/docs/eventsub/handling-webhook-events#using-the-cli-to-test-your-handler
[crypto-hmac]: https://hackage.haskell.org/package/cryptonite-0.30/docs/Crypto-MAC-HMAC.html
