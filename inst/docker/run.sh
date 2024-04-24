sudo docker build \
  -t paletteapp:latest \
  -f inst/docker/Dockerfile \
  .

docker tag \
  paletteapp:latest \
  bfatemi/paletteapp:latest

docker push \
  bfatemi/paletteapp:latest

# sudo docker pull \
#   bfatemi/apptemplate1:latest
#
sudo docker run \
  --name paletteapp \
  -p 3939:3939 \
  --rm \
  -dt bfatemi/paletteapp:latest
