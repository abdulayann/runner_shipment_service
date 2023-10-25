#!/bin/sh

export COMPOSE_PROJECT_NAME=runner-reports

export COMPOSE_FILE=./tests/docker-compose.yml

if [ "$#" -eq 0 ]; then
     docker-compose up -d sql-server-db
     echo 'Sleeping for 10 seconds' # wait for database to accept connections
     sleep 10s
     docker-compose up -d mock cmsServiceApp
elif [ "$1" = 'init' ]; then
    echo 'Building Images'
    docker-compose build
    # echo 'Starting Containers'
    # docker-compose up -d sql-server-db db
    
   # docker-compose up -d db
    echo 'Sleeping for 10 seconds' # wait for database to accept connections
    sleep 10s
    docker-compose up -d

    # docker-compose up -d cmsServiceApp
    #  docker-compose up cmsServiceApp
elif [ "$1" = 'seed-data' ]; then
     echo 'Running Seeds'
     npm run sequelize:seed
elif [ "$1" = 'start' ]; then
    echo 'Starting Services'
    docker-compose start
elif [ "$1" = 'stop' ]; then
    echo 'Stopping Services'
    docker-compose stop
elif [ "$1" = 'down' ]; then
    echo 'Stopping & Removing Services'
    docker-compose down
elif [ "$1" = 'logs' ]; then
    echo 'Logs App'
    docker-compose logs app
elif [ "$1" = 'generate-coverage' ]; then
    echo "dump coverage"
    docker-compose exec -T cmsServiceApp java -jar jacococli.jar dump --address localhost --port 33933 --destfile coverage/jacoco.exec
    echo "parse coverage"
    docker-compose exec -T cmsServiceApp java -jar jacococli.jar report coverage/jacoco.exec --classfiles ./usr/local/lib/target/classes/com --sourcefiles src/main/java --html coverage/html --xml coverage/coverage.xml
else
    echo "Command not found - ${1}"
fi