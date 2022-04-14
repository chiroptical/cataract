#!/usr/bin/env bash

imageId=$(docker images --format "{{.Repository}} {{.ID}} {{.CreatedAt}}" | grep "^pluto" | head -1 | awk '{print $2}')
echo "Pushing image with id: $imageId"
docker tag $imageId registry.heroku.com/chiroptical-pluto-staging/web
docker push registry.heroku.com/chiroptical-pluto-staging/web
heroku container:release -a chiroptical-pluto-staging web
