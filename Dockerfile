FROM peteyoung/devbase
MAINTAINER Pete Young "gnuoy.etep@gmail.com"
ENV REFRESHED_AT 2017-03-05

# erlang - language and runtime
# erlang-dev - Erlang/OTP development libraries and headers
# erlang-eunit - Erlang/OTP module for unit testing (depends on erlang-dev)
RUN apk -U add erlang erlang-dev erlang-eunit

RUN mkdir /root/src
WORKDIR /root/src
