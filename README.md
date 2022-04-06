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

- Subscribe to streamer's follows, subscriptions, and bits webhooks
  - Start here: https://dev.twitch.tv/docs/eventsub/handling-webhook-events
  - Test locally using the twitch CLI: [twitch-cli][twitch-cli]
  - Validating HMAC signature: [Crypto.Mac.HMAC][crypto-hmac]
    - https://dev.twitch.tv/docs/eventsub/handling-webhook-events#verifying-the-event-message
- Save follows, subscriptions, and bits as events in the database
- Event source handler should read events and push them
- Store the twitch username along with the identity

[twitch-cli]: https://dev.twitch.tv/docs/eventsub/handling-webhook-events#using-the-cli-to-test-your-handler
[crypto-hmac]: https://hackage.haskell.org/package/cryptonite-0.30/docs/Crypto-MAC-HMAC.html
