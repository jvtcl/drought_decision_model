# drought_decision

## Dockerizing this shiny model

To create a Docker image for this app, execute the following command from a terminal (with Docker installed):

```
docker build -t ddm .
```

This builds the image and tags it (or names it) "ddm" for drought decision model. 

## Running a Docker container

To run a Docker container and serve the model, you can execute:

```
docker run -d -p 3838:3838 ddm
```

Then navigate to localhost:3838 or 0.0.0.0:3838 (when running locally) to access the shiny app. 
The `-d` in the call to `docker run` indicates to run the container in the background, printing the container ID, and `-p 3838:3838` links port 3838 on your host machine and the Docker container. 

To open a shell inside of your running container (for instance, if you need to look at logfiles in `/var/log/shiny-server/`), you can use the name or ID of the container:

```
docker exec -i -t 665b4a1e17b6 /bin/bash #by ID

docker exec -i -t loving_heisenberg /bin/bash #by Name
```

If you don't have the container ID or name, you can find it via `docker ps`.

