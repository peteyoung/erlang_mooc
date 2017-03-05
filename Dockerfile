FROM peteyoung/devbase
MAINTAINER Pete Young "gnuoy.etep@gmail.com"
ENV REFRESHED_AT 2017-03-05

RUN apk -U add erlang

RUN mkdir /root/src
WORKDIR /root/src
