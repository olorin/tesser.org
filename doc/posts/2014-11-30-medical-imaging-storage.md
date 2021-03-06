---
title: Store and index your own medical imaging data 
author: Sharif Olorin
tags: medical imaging
---

I broke my wrist (right distal radius) a couple of weeks ago. My doctor initially
missed it on the X-ray and said it looked like a scaphoid fracture, and
the registrar at the emergency department concurred.
The orthopaedics department disagreed and pointed out the (quite subtle)
radial fracture, but agreed that my scaphoid looked "weird" (their
words), so they got me in for an MRI scan and asked if I'd had any previous
imaging done of that wrist. I happened to have data from a year-old MRI scan of 
that wrist on my backup server, and this got me thinking about storage and
retrieval of these records in general. I finally got smart and started
taking backups of the things, but during a less-organized period of my
life the chances of tracking down a copy of that MRI data in time for it to
be clinically relevant would have been negligible.

Maintaining an accurate and complete history of medical imaging is
often critical for a doctor to make good treatment decisions with
historical context. Generally (at least in Australia, other countries
may have it better), these records are kept distributed on the
fileservers of individual hospitals and private radiology clinics. These
entities are normally pretty good at not losing these records, but
unless you are much more organized than I it's probably a challenge to
keep track of what imaging you've had, where you
had it, and how to access the data. The same issue pops up if you move countries, your
radiology clinic goes out of business, et cetera. Bytes can last a lot longer than
governments if you treat them right.

## Imaging formats

From the inception of any new technology until a critical mass of
entities realize it's a good idea, there will be as many competing file
formats as there are equipment manufacturers or software
companies. Fortunately, medical imaging is pretty important and has been
around for a while. The stone age lasted from whenever someone first
thought of hooking an X-ray machine up to a computer up until the early
1990s, when [DICOM](http://en.wikipedia.org/wiki/DICOM) became an [ISO
standard](http://www.iso.org/iso/catalogue_detail?csnumber=43218) and attained almost
universal adoption for storage and transmission of most types of medical
imaging (with the exception of certain time series datatypes such as
[electroencephalograms](http://en.wikipedia.org/wiki/Electroencephalography) for which the equivalent open format is
[EDF](http://en.wikipedia.org/wiki/European_Data_Format) and its
successors). So when you go to a clinic and leave with a CD, the data on it
will almost certainly be in DICOM format. As with every other useful
open format, a solid ecosystem of free and open-source tools has sprung
up to work with DICOM data.

## Storage options

### Physical films

This is the simplest option, but it has numerous disadvantages; films
degrade, get lost or misfiled, you have another pile of physical stuff
you need to take with you every time you relocate, and films can never
store the same imaging resolution as the original DICOM files can.

### Disk images

If you store an image of each CD, you can
always write one out to whatever storage medium is in common use at the
time and take it along to the hospital. This might be the best option
for people who don't need to get a lot of imaging done (that is,
healthy, not accident-prone, and not involved in risky recreational activities).

### DICOM server

This is the same kind of application your radiology clinic will be
using to store your images. If you're running your own, you
have all your data in one place, can see it all at a glance, and easily
export whatever subset of it might be relevant. If you have an
internet-accessible server, you can even do this without leaving the
hospital.

It does take a little bit more work to set up, but there are a few
reasons you might decide it's worthwhile:

 - You have a family and you want easy access to your children's medical
   records;
 - You have a chronic health condition which involves seeing a large
   number of specialists, or a family member who does;
 - You travel a lot and want a way to 
   access your medical imaging data without the delays and red tape involved in requesting
   it from another country's healthcare system (in the cases where this
   is even possible);
 - You're a medical imaging nerd (hi!).

## Implementation

I decided to go with [Orthanc](http://www.orthanc-server.com/index.php)
as it seems like the most mature of the available open-source options.
The documentation is excellent and it took me less than an hour
to get it running on a Linux machine (it also supports Mac OS and
Windows).
It provides both a DICOM and an HTTP server, including a JSON REST API (this to my
enormous relief; I was worried I'd be stuck with SOAP or something, but
it seems that the medical imaging industry moves faster than, say,
payment processing).
