#!/bin/bash

iptables -F
iptables -X
iptables -Z

cat << EOF | iptables-restore
*mangle
:PREROUTING ACCEPT [0:0]
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
:POSTROUTING ACCEPT [0:0]
COMMIT

*nat
:PREROUTING ACCEPT [0:0]
:INPUT ACCEPT [0:0]
:OUTPUT ACCEPT [0:0]
:POSTROUTING ACCEPT [0:0]
COMMIT

# Set a default DROP policy.
*filter
:INPUT DROP [0:0]
:FORWARD DROP [0:0]
:OUTPUT DROP [0:0]

# Allow basic INPUT traffic.
-A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
-A INPUT -i lo -j ACCEPT
-A INPUT -p icmp --icmp-type 8 -m conntrack --ctstate NEW -j ACCEPT

# Allow basic OUTPUT traffic.
-A OUTPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
-A OUTPUT -o lo -j ACCEPT
-A OUTPUT -p icmp -j ACCEPT

# Allow traffic to the OpenVPN server and via the tunnel.
-A OUTPUT -o tun+ -j ACCEPT

# protonvpn nl1
-A OUTPUT -p udp -m udp -d 89.39.107.191 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.204 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 217.23.3.92 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.198 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 109.201.133.22 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 109.201.133.24 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.190 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 46.166.142.220 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.192 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.200 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.194 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.199 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 217.23.3.96 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 46.166.142.215 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 109.201.133.26 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 217.23.3.91 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 46.166.142.214 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.195 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.202 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.203 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 89.39.107.196 --dport 1194 -j ACCEPT
-A OUTPUT -p udp -m udp -d 46.166.142.21 --dport 1194 -j ACCEPT

#mullvad_se
-A OUTPUT -p udp -m udp -d 185.213.154.131 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.132 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.133 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.134 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.135 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.136 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.137 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.138 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.139 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.140 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.141 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.154.142 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.152.131 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.152.132 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.152.133 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.152.134 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.152.137 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.213.152.138 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 193.138.218.131 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 193.138.218.132 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 193.138.218.133 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 193.138.218.134 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 193.138.218.135 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 141.98.255.83 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 141.98.255.85 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 141.98.255.87 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 141.98.255.88 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 141.98.255.90 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 141.98.255.91 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 141.98.255.92 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 141.98.255.93 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.131 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.132 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.133 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.134 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.135 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.136 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.137 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.138 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.139 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.140 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.141 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.142 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.143 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.144 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.145 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.146 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.147 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.148 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.149 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.150 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.151 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.152 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.153 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.154 --dport 1195 -j ACCEPT
-A OUTPUT -p udp -m udp -d 185.65.135.155 --dport 1195 -j ACCEPT

# Reject everything else.
-A INPUT -m conntrack --ctstate INVALID -j DROP
-A INPUT -j REJECT --reject-with icmp-port-unreachable
-A FORWARD -j REJECT --reject-with icmp-port-unreachable
-A OUTPUT -j REJECT --reject-with icmp-port-unreachable
COMMIT
EOF

# systemctl restart libvirtd
# sleep 2
# mod0="$(iptables --line-numbers -nL LIBVIRT_FWO | awk '$5=="192.168.122.0/24" {print $1; exit}')"
# iptables -R LIBVIRT_FWO "$mod0" -s 192.168.122.0/24 -i virbr0 -o tun+ -j ACCEPT
# mod1="$(iptables --line-numbers -nL LIBVIRT_FWO | awk '$5=="10.0.2.0/24" {print $1; exit}')"
# iptables -R LIBVIRT_FWO "$mod1" -s 10.0.2.0/24 -i virbr1 -o tun+ -j ACCEPT

ip6tables -F
ip6tables -X
ip6tables -Z
ip6tables -P INPUT DROP
ip6tables -P FORWARD DROP
ip6tables -P OUTPUT DROP
