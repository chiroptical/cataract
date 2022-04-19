#!/usr/bin/env bash

imageId=$(docker images --format "{{.Repository}} {{.ID}} {{.CreatedAt}}" | grep "^cataract" | head -1 | awk '{print $2}')
echo "Pushing image with id: $imageId"
docker tag $imageId registry.digitalocean.com/chiroptical/cataract
docker push registry.digitalocean.com/chiroptical/cataract
