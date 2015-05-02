---
title: Automated, encrypted backups for mobile computers with box-backup
author: Sharif Olorin
tags: linux, system administration
---

Backups are hard. The most sophisticated backup system in the world is
practically useless if it requires a user to push a button to trigger
a backup. That's what cron is for, of course, but what if the machine
in question is a portable workstation and not connected to the
Internet when the cron job triggers? What if it's using a packet radio
interface with a tiny data transfer cap?

# What I needed in a backup system

 - Client-side encryption; apart from other considerations, the system
   would have multiple people using it and I have no desire to be able
   to read my partner's backups just because I have root on the backup
   server.
 - diff-based backups; while transferring (for example) my entire
   music collection over the local network might be acceptable (though
   not ideal), I would like to be able to run hourly backups from
   whatever network my laptop happens to be connected to without a
   horde of network administrators pursuing me with cluebats.
 - Open source (because I have no particular desire to chase after
   vendors whenever anything breaks when I could fix the problem
   myself, and because relying on proprietary software for encryption
   is just silly).
 - Support for multiple UNIXes, at least on the client-side; some of
   my machines run OpenBSD.
   
I didn't think my requirements were that atypical, but finding a
pre-existing solution which met them took longer than expected. I was
almost at the point of giving up and writing my own when I found
[Box Backup](https://www.boxbackup.org/).
   
Box Backup has two daemons, a client and a server. The client daemon
maintains a data structure in memory which keeps track of modified
timestamps; it can trigger backups of changed files either in real
time or batched and triggered via cron job.

Box Backup is available for
[various UNIXes including OS X, plus Microsoft Windows](https://www.boxbackup.org/wiki/SystemRequirements),
so no issues there. It doesn't support bandwidth management itself,
but this
[proved not to be a problem](#dealing-with-unreliable-network-connectivity).

# Server configuration

## Certificate authority

The necessary x509 keypairs could be generated on the backup server
itself, but best practice is to keep the CA on a fully trusted
airgapped machine; I took the opportunity to move my home network CA
to a spare Raspberry Pi with a clean OpenBSD installation.

## Server

# Client configuration

# Dealing with unreliable network connectivity
