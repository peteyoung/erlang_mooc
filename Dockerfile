FROM alpine:3.4
MAINTAINER Pete Young "gnuoy.etep@gmail.com"
ENV REFRESHED_AT 2017-02-21

RUN apk -U add tmux vim git
RUN apk -U add erlang

RUN mkdir /root/src
WORKDIR /root/src
