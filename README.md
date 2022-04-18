# Cataract

[![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)

Cataract is an Overlay for OBS. The goal is to create
an open-source overlay with features similar to Streamlabs or Player.me. The
overlay should be responsive via an API and handle new follows, subscriptions,
and bit donations.

I believe we shouldn't have to focus on the various Twitch APIs and their
authentication and instead work on interesting custom components with our
communities. Ideally, getting viewers involved when possible.

This application is coded using Haskell, Yesod, and Persistent. The project is
early work in progress.

Next Steps
---

- Tighten security up
  - [x] More secure AES key
  - [ ] Encrypt Twitch tokens in the database
- Could we use [reshape][reshape] for migrations?
- Build a homepage that doesn't suck

[twitch-cli]: https://dev.twitch.tv/docs/eventsub/handling-webhook-events#using-the-cli-to-test-your-handler
[crypto-hmac]: https://hackage.haskell.org/package/cryptonite-0.30/docs/Crypto-MAC-HMAC.html
[reshape]: https://github.com/fabianlindfors/reshape
