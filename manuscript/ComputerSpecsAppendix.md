---
output:
  pdf_document: default
  html_document: default
---
# Appendix A: System Information for Benchmarking

The following system details are provided to support reproducibility of the performance measurements reported in this work.
All benchmarks were conducted on a local machine with the specifications shown below. 
Information was collected via standard system commands at time of receiving reviewer comments. 


## Operating System

```
$ lsb_release -a

Distributor ID:	Ubuntu  
Description:	Ubuntu 22.04.5 LTS  
Release:	22.04  
Codename:	jammy  
```

## CPU

```
$ lscpu

Architecture:             x86_64  
  CPU op-mode(s):         32-bit, 64-bit  
  Address sizes:          39 bits physical, 48 bits virtual  
  Byte Order:             Little Endian  
CPU(s):                   16  
  On-line CPU(s) list:    0-15  
Vendor ID:                GenuineIntel  
  Model name:             11th Gen Intel(R) Core(TM) i7-11700K @ 3.60GHz  
    CPU family:           6  
    Model:                167  
    Thread(s) per core:   2  
    Core(s) per socket:   8  
    Socket(s):            1  
    Stepping:             1  
    CPU max MHz:          5100.0000  
    CPU min MHz:          800.0000  
    BogoMIPS:             7200.00  
```

## Memory

```
$ free -h
               total        used        free      shared  buff/cache   available  
Mem:           125Gi       3.4Gi       112Gi       585Mi       9.7Gi       120Gi  
Swap:          2.0Gi          0B       2.0Gi  
```

## Internet Speed

```
$ speedtest-cli

Retrieving speedtest.net configuration...  
Testing from Astound Broadband (168.91.196.6)...  
Retrieving speedtest.net server list...  
Selecting best server based on ping...  
Hosted by Sangoma (Chicago, IL) [17.63 km]: 18.388 ms  
Testing download speed..........................................................  
Download: 21.58 Mbit/s  
Testing upload speed............................................................  
Upload: 27.43 Mbit/s  
```

## Filesystem Usage

```
$ df -h

Filesystem      Size  Used Avail Use% Mounted on  
tmpfs            13G  2.6M   13G   1% /run  
/dev/nvme0n1p2  468G  242G  203G  55% /  
tmpfs            63G   14M   63G   1% /dev/shm  
tmpfs           5.0M  4.0K  5.0M   1% /run/lock  
efivarfs        192K   88K  100K  47% /sys/firmware/efi/efivars  
/dev/nvme0n1p1  511M  6.1M  505M   2% /boot/efi  
tmpfs            13G  132K   13G   1% /run/user/1000  
/dev/sda1       3.6T  3.2T  295G  92% /media/steppe/hdd  
```

## I/O Performance


```
sudo hdparm -Tt /dev/nvme0n1p2

Timing cached reads:   44096 MB in  2.00 seconds = 22095.91 MB/sec  
Timing buffered disk reads: 4470 MB in  3.00 seconds = 1489.83 MB/sec  
```

All files were stored on the following drive.  

```
sudo hdparm -Tt /dev/sda1

Timing cached reads:   45870 MB in  2.00 seconds = 22986.94 MB/sec  
Timing buffered disk reads: 514 MB in  3.01 seconds = 170.57 MB/sec  
```
