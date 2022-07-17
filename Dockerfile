# Build stage 0
FROM erlang:23 as base

RUN apt-get update
RUN apt-get install libcap-dev

#Set working directory
RUN mkdir /data
WORKDIR /data

COPY src src/
COPY priv priv/
COPY config config/
COPY config/sys.config.local_docker config/sys.config
COPY include include/
COPY scripts scripts/
COPY rebar.config .
COPY rebar.lock .
COPY rebar3 .

RUN ./rebar3 --version

#Build the release
RUN ./rebar3 release

#FROM alpine

#RUN apk add iptables ip6tables ipset ulogd

FROM erlang:23 as deploy

RUN apt-get update
RUN apt-get install -y iptables ipset

RUN mkdir -p /opt/dog
RUN mkdir -p /etc/dog
COPY config/config.json.local_docker /etc/dog/config.json
RUN mkdir -p /var/log/dog
# Install the released application
COPY --from=base /data/_build/default/rel/dog /opt/dog
RUN ls -latr /opt/dog

# Expose relevant ports
EXPOSE 22

CMD ["/opt/dog/bin/dog", "foreground"]
