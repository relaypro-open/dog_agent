%define name dog
%define version 20190702.1543
%define release 1

# Macro that print mesages to syslog at package (un)install time
%define nnmmsg logger -t %{name}/rpm

Summary: Dog agent
URL: http://republicwireless.com
Name: %{name}
Version: %{version}
Release: %{release}
License: GPL
Group: Application/System
BuildRoot: %{_tmppath}/%{name}-buildroot
Requires: daemonize
Requires: iptables >= 1.4.7-19
Requires: iptables-ipv6 >= 1.4.7-19
Requires: ipset >= 6.11-4

%description
dog agent to manage iptables/ipsets firewall.

%install
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/opt/dog
cp /opt/dog/dog.init ${RPM_BUILD_ROOT}/
rm -rf archive
mkdir archive
cd archive
tar -xzmf /opt/dog.scp/_build/qa/rel/dog/%{name}-%{version}.tar.gz
cp -r * ${RPM_BUILD_ROOT}/opt/dog/
cd ..

%post
/sbin/iptables -F
/sbin/ip6tables -F
/sbin/ipset destroy
/sbin/modprobe -r xt_set
/sbin/modprobe -r ip_set_hash_net
/sbin/modprobe -r ip_set_hash_ip
/sbin/modprobe -r ip_set
echo 'options ip_set max_sets=1024' > /etc/modprobe.d/ip_set.conf
/sbin/modprobe ip_set
/sbin/modprobe ip_set_hash_net
/sbin/modprobe xt_set
ln -sf /usr/sbin/ipset /sbin/ipset
mkdir /etc/iptables
ln -sf /etc/iptables/rules.ipset /etc/sysconfig/ipset
id -g dog &>/dev/null || groupadd -g 10611 dog
id -u dog &>/dev/null || adduser --system --shell /bin/bash --home /home/dog -m --uid 10611 --gid 10611 dog
usermod -a -G consul dog
chown -R dog: /opt/dog/
mkdir -p /etc/dog/
chown -R dog: /etc/dog/
mkdir -p /var/log/dog/
chown -R dog: /var/log/dog/
mkdir -p /var/run/dog/
chown -R dog: /var/run/dog/
cp dog.init /etc/init.d/dog
chkconfig dog on

%postun
chkconfig --del dog
rm -rf /opt/dog

%clean
rm -rf $RPM_BUILD_ROOT

%files
/
