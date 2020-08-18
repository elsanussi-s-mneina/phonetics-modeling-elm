# Troubleshooting Note: if it does not run due to permission denied error:
# Set the file permission to allow execution.
# You can use the command in the commented line:
# chmod +x run_with_docker.sh

# This runs the server.
# It does this by making a docker container,
# and connecting port 8000 in the container
# to port 8000 on the host.
# Within the container it runs the command
# "elm reactor"
docker-compose run -p 8000:8000 elm reactor
