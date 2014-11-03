#!/bin/sh

rsync -e ssh --progress -lzuogthvr --compress-level=9 --delete-after root@85.175.153.180:/var/MCHS/ /root/mchs.bkp/
