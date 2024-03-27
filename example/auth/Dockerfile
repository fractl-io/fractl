FROM openjdk:21-jdk

# add files to image 
ADD target/fractl-0.4.8-standalone.jar .
ADD config.edn .
ADD example/auth/order.fractl .
ADD example/auth/start.sh .

EXPOSE 3000
EXPOSE 8000

# start on run
CMD ["bash", "start.sh"]