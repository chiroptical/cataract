Pushing the Docker Image to Heroku Container Registry
---

```
heroku container:login # if not already logged in
docker tag 58896fa7abcb registry.heroku.com/chiroptical-pluto-staging/web
docker push registry.heroku.com/chiroptical-pluto-staging/web
heroku container:release web
```
