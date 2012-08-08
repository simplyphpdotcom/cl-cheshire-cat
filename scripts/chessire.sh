#!/bin/sh

## Copyright © 2012, Mathieu Lemoine <mlemoine@mentel.com>, Mentel Inc.
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer.
##     * Redistributions in binary form must reproduce the above copyright
##       notice, this list of conditions and the following disclaimer in the
##       documentation and/or other materials provided with the distribution.
##     * Neither the name of "Mentel Inc." nor the names of its contributors may be
##       used to endorse or promote products derived from this software without
##       specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
## DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
## ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

### BEGIN INIT INFO
# Provides:          chessire
# Required-Start:    $local_fs $remote_fs $network $syslog $named
# Required-Stop:     $local_fs $remote_fs $network $syslog $named
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start chessire redirection server
### END INIT INFO

op="$1"
config_file="${2:-/etc/chessire.conf}"

lisp="sbcl"

if [ -r "${config_file}" ]
then
    pid_file="$(grep -E '^pid_file=' "${config_file}" | cut -d '=' -f 2)"
    system_file="$(grep -E '^system=' "${config_file}" | cut -d '=' -f 2)"
fi

[ -z "${pid_file}" ] && pid_file="/var/run/chessire.pid"
[ -z "${system_file}" ] && system_file="/usr/share/chessire/scripts/chessire.lisp"

case "${op}" in
    start)
        if [ -r "${pid_file}" ]
        then
            pid="$(cat "${pid_file}")"
            if [ -n "${pid}" ] && ps -p "${pid}" > /dev/null
            then
                echo "Chessire ${pid} is already grinning." >&2
                echo "If this is not true, please remove or truncate the file ${pid_file}." >&2
                exit 1
            fi
        fi
        echo "Tickling Chessire using ${system_file}..."
        env "${lisp}" --script "${system_file}" "${config_file}"
        ;;
    stop)
        if [ ! -r "${pid_file}" ]
        then
            echo "Chessire already disapeared: no pid file." >&2
            exit 0
        fi
        pid="$(cat "${pid_file}")"
        if [ -z "${pid}" ]
        then
            echo "Chessire already disapeared: no pid." >&2
            exit 0
        fi
        echo "Shushing Chessire ${pid}..."
        kill "${pid}"
        ;;
    force-stop)
        if [ ! -r "${pid_file}" ]
        then
            echo "Chessire already disapeared: no pid file." >&2
            exit 0
        fi
        pid="$(cat "${pid_file}")"
        if [ -z "${pid}" ]
        then
            echo "Chessire already disapeared: no pid." >&2
            exit 0
        fi
        echo "Off with its head! (${pid})"
        kill -9 "${pid}"
        ;;
    restart)
        if [ -r "${pid_file}" ]
        then
            pid="$(cat "${pid_file}")"
            if [ -n "${pid}" ] && ps -p "${pid}" > /dev/null
            then
                echo "Shushing Chessire ${pid}..."
                kill "${pid}"
            fi
        fi
        sleep 2
        if ! ps -p "${pid}" > /dev/null
        then
            echo "Chessire ${pid} is still grinning (but may be disapearing)." >&2
            exit 1
        fi
        echo "Tickling Chessire using ${system_file}"
        env "${lisp}" --script "${system_file}" "${config_file}"
        ;;
    force-restart)
        if [ -r "${pid_file}" ]
        then
            pid="$(cat "${pid_file}")"
            if [ -n "${pid}" ] && ps -p "${pid}" > /dev/null
            then
                echo "Off with its head!"
                kill -9 "${pid}"
            fi
        fi
        if ! ps -p "${pid}" > /dev/null
        then
            echo "Chessire ${pid} is still grinning (but may be disapearing)." >&2
            exit 1
        fi
        echo "Tickling Chessire using ${system_file}"
        env "${lisp}" --script "${system_file}" "${config_file}"
        ;;
    status)
        if [ ! -r "${pid_file}" ]
        then
            echo "Chessire already disapeared: no pid file."
            exit 0
        fi
        pid="$(cat "${pid_file}")"
        if [ -z "${pid}" ]
        then
            echo "Chessire already disapeared: no pid."
            exit 0
        fi
        if ps -p "${pid}" > /dev/null
        then
            echo "Chessire ${pid} still grinning."
        else
            echo "Chessire ${pid} already disapeared."
        fi
        ;;
    *)
        cat <<EOF >&2
Usage: $(basename "$0") start|stop|status|restart|force-stop|force-restart [ config_file ]
EOF
        [ "${op}" != "help" ] && exit 1
esac
