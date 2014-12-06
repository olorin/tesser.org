---
title: Notes on setting up a Hackage mirror
author: Sharif Olorin
---

## Preparation

You will need:

 - around 100GB diskspace (as of December 2014, the full mirror occupies
   45GB plus operating system and applications; I rounded up an order
   magnitude for future growth/error margin/disks aren't the expensive
   bit here);
 - at least 4GB memory (you might be able to get away with less, but I
   wouldn't bet on it);
 - CPU time to burn (the initial sync from `hackage.haskell.org` took me
   north of seven days on four physical cores, and the associated
   documentation build was around the same amount of compute time); and
 - GHC 7.6.x (no, GHC 7.8.x [will not suffice](https://github.com/haskell/hackage-server/issues/260)).

For reference, the machine I used was a 4-vCPU machine with 8GB RAM
running Ubuntu 14.04. It would probably work unmodified on a recent
Debian release, and as long as you have a semirecent Linux distribution
you should only need to modify the dependency-installation bit. You can
separate the mirror from the machine that does the builds, but I haven't 
found that to be necessary yet (assuming one is willing to wait a little 
longer for the initial sync). I used a single-purpose machine; it could be 
done on a multi-purpose server if it had CPU to burn.

## Process

### Environment

Install some dependencies:

```bash
sudo apt-get install zlib1g-dev libicu-dev happy alex git ghc cabal-install nginx haddock
```

Create a user to run the server:

```bash
sudo useradd -m -s /bin/bash hackage
sudo su - hackage
```

Get the source and build (I used the latest `master`, at the time
`489df240266f2b3066cf6e99304e704c53c8e04f`):

```bash
git clone https://github.com/haskell/hackage-server
cd hackage-server
cabal update
cabal install
```

Add `cabal`'s bindir to your `$PATH` and update:

```bash
echo 'export PATH=$HOME/.cabal/bin:$PATH' >> ~/.bashrc
. ~/.bashrc
set +h
```

Set up some data directories:

```bash
export HACKAGE_STATE=$HOME/state
export HACKAGE_TMP="${HACKAGE_STATE}/tmp"
mkdir -p $HACKAGE_TMP
```

### Running the server

Work out the URL you want to host your mirror at (the base URI - we'll
set up nginx to listen here).

```bash
export HACKAGE_URL="http://hackage.yourserver.net"
```

Initialize and run the server:

```bash
hackage-server init --state-dir=$HACKAGE_STATE --admin=$USERNAME:$PASSWORD
hackage-server run --ip=127.0.0.1 --base-uri=$HACKAGE_URL --state-dir=$HACKAGE_STATE --tmp-dir=$HACKAGE_TMP
```

You should now be able to access the server at `http://localhost:8080`.
Of course, for production you'll want to set this up to be supervised by
a process manager (I used [daemontools](http://cr.yp.to/daemontools.html)).

Now, this will only use a single core; if you have more than one, you
probably want to use them:

```bash
hackage-server +RTS -N$NUM_CORES -RTS run --ip=127.0.0.1 --base-uri=$HACKAGE_URL --state-dir=$HACKAGE_STATE --tmp-dir=$HACKAGE_TMP
```

For public access, I put `hackage-server` behind [nginx](http://nginx.org/); this isn't
strictly necessary, but it's nice for logging and caching purposes. A sample
nginx configuration file might look like this:

```bash
server {
    listen 80 default_server;
    server_name hackage.yourserver.net;
    location / {
        proxy_pass http://127.0.0.1:8080$uri;
    }
}
```

### Running the mirror

Using the admin interface (http://localhost:8080/admin), create a
`mirror` user and add them to the `mirrors` group.

We need to specify where we want to mirror from; assuming the official
Hackage at `hackage.haskell.org`, it looks like this:

```bash
echo -e "http://hackage.haskell.org\nhttp://mirror:$MIRROR_PASSWORD@localhost:8080/" > servers.cfg
```

That's all we need there (`hackage-mirror` is an HTTP client which talks
to the upstream and downstream Hackage servers; it doesn't need to
access state directly). Now run it:

```bash
hackage-mirror $HOME/servers.cfg --continuous --keep-going
```

This will take a while (read: days), so leave it running headless (as
with the server, you probably want to use a process manager for this).

### Running the build client

You now have packages, but no documentation; we'll build that now.

The 'admin' user type doesn't have upload privileges, so use the
server's admin interface to create a `build` user in the `trustees`
group. Now we can initialize the build client configuration:

```bash
hackage-build init http://localhost:8080/
```

Enter the username and password of the `build` user when prompted. Now
you can run the build client itself:

```bash
hackage-build build --continuous --keep-going
```

You should now have a working Hackage mirror. (Actually, that part
happens in around a week when the sync is done.)
