#!/bin/sh

# Younix OpenVPN hardening system. See {{ openvpn_hardened_readme_path }}

#
# Copyright (C) 2013-2015 Andrew Ayer
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# Except as contained in this notice, the name(s) of the above copyright
# holders shall not be used in advertising or otherwise to promote the
# sale, use or other dealings in this Software without prior written
# authorization.
#

#
# Environment variables that can be passed in via OpenVPN's --setenv:
#  OPENVPN_ROUTE_TABLE		- the routing table to which all routes are added
#  OPENVPN_ROUTE_SOURCE		- the source IP address for all routes
#

TABLE=$OPENVPN_ROUTE_TABLE
SOURCE=$OPENVPN_ROUTE_SOURCE

if [ -n "$TABLE" -o -n "$SOURCE" ]
then
	# Replace existing routes for our interface with new routes with different table/src
	if [ -n "$SOURCE" ]
	then
		src_arg="src $SOURCE"
	else
		src_arg=
	fi
	/sbin/ip route show dev $dev table main | while read route
	do
		/sbin/ip route delete $route dev $dev table main
		/sbin/ip route add $route dev $dev $src_arg table ${TABLE:-main}
	done
fi

if [ -n "$TABLE" ]
then
	# Replace existing IPv6 routes for our interface with new routes with different table
	/sbin/ip -6 route show dev $dev table main | while read route
	do
		/sbin/ip -6 route delete $route dev $dev table main
		/sbin/ip -6 route add $route dev $dev table $TABLE
	done
fi

# Add IPv4 routes
i=1
while true
do
	network=`eval 'echo $route_network_'$i`
	netmask=`eval 'echo $route_netmask_'$i`
	gateway=`eval 'echo $route_gateway_'$i`
	metric=`eval 'echo $route_metric_'$i`

	if [ "$network" = "" ]
	then
		break
	fi

	command="/sbin/ip route add $network"
	if [ "$netmask" != "" ]
	then
		command="$command/$netmask"
	fi
	if [ "$TABLE" != "" ]
	then
		command="$command table $TABLE"
	fi
	if [ "$metric" != "" ]
	then
		command="$command metric $metric"
	fi
	command="$command dev $dev via $gateway"
	if [ "$SOURCE" != "" ]
	then
		command="$command src $SOURCE"
	fi

	$command

	i=`expr $i + 1`
done

# Add IPv6 routes
i=1
while true
do
	network=`eval 'echo $route_ipv6_network_'$i`
	gateway=`eval 'echo $route_ipv6_gateway_'$i`

	if [ "$network" = "" ]
	then
		break
	fi

	command="/sbin/ip -6 route add $network"
	if [ "$TABLE" != "" ]
	then
		command="$command table $TABLE"
	fi
	command="$command dev $dev via $gateway"

	$command

	i=`expr $i + 1`
done

exit 0
