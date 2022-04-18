Pushing the Docker Image to Heroku Container Registry
---

```
heroku container:login # if not already logged in
docker tag 58896fa7abcb registry.heroku.com/chiroptical-cataract/web
docker push registry.heroku.com/chiroptical-cataract/web
heroku container:release -a chiroptical-cataract web
```
