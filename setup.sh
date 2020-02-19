# Based on digital ocean One click app: Docker-ce

docker build ./nginx/. -t nginx  #nginx image should be build very quick

docker build . -t webapp #webapp image need hours to build

echo 'Docker images have been built. Start downloading data.'

# The following does not work properly, needs to be run manually
sudo mkdir data 
cd data 


sudo wget https://sdsu.box.com/shared/static/65b64ehub3p1hyi7t79whicvvnctjag4.zip -O map.zip 
sudo apt install unzip 
sudo unzip map.zip 

# to support Chinese font
sudo wget https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc 
sudo cp wqy-zenhei.ttc  ../shinyapps/v/ 

sudo cp wqy-zenhei.ttc  ../shinyapps/e/

echo 'Data has been downloaded and unziped'

echo 'All image are ready to run'

