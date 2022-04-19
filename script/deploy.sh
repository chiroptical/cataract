#!/usr/bin/env bash

imageId=$(docker images --format "{{.Repository}} {{.ID}} {{.CreatedAt}}" | grep "^cataract" | head -1 | awk '{print $2}')
echo "Pushing image with id: $imageId"
docker tag $imageId registry.heroku.com/cataract-chiroptical/web
docker push registry.heroku.com/cataract-chiroptical/web
heroku container:release -a cataract-chiroptical web
