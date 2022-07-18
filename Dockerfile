# Build stage 0
FROM erlang:23 as base

RUN apt-get update
RUN apt-get install libcap-dev

#Set working directory
RUN mkdir /data
WORKDIR /data
COPY rebar.config .
COPY rebar.lock .
COPY rebar3 .
RUN ./rebar3 compile

FROM base as compile

WORKDIR /data
COPY . .
COPY --from=base /data/_build .
#COPY src src/
#COPY priv priv/
#COPY config config/
COPY config/sys.config.local_docker config/sys.config
#COPY include include/
#COPY scripts scripts/
#COPY rebar.config .
#COPY rebar.lock .
#COPY rebar3 .

RUN ./rebar3 --version

#Build the release
RUN ./rebar3 release

#FROM alpine

#RUN apk add iptables ip6tables ipset ulogd

FROM base as deploy

RUN apt-get update
RUN apt-get install -y iptables ipset

RUN mkdir -p /opt/dog
RUN mkdir -p /etc/dog
COPY config/config.json.local_docker /etc/dog/config.json
RUN mkdir -p /var/log/dog
# Install the released application
COPY --from=compile /data/_build/default/rel/dog /opt/dog
RUN chmod 4555 /opt/dog/lib/erlexec-1.20.1/priv/x86_64-pc-linux-gnu/exec-port
RUN mkdir -p /home/dog/bin
RUN cp /sbin/ipset /home/dog/bin/ipset
RUN cp /usr/sbin/iptables-save /home/dog/bin/
RUN cp /usr/sbin/ip6tables-save /home/dog/bin/
RUN cp /usr/sbin/iptables-restore /home/dog/bin/
RUN cp /usr/sbin/ip6tables-restore /home/dog/bin/

RUN ls -latr /opt/dog

# Expose relevant ports
EXPOSE 22

CMD ["/opt/dog/bin/dog", "foreground"]
