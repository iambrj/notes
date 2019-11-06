# Two aspects
* Data Structures - on-disk structures, organization
* Access Methods - read, write, open
# Overall org
* Disk into blocks
* Track file info using inodes
* Free space - bitmaps, linked list
 * Super Block
# Mounting
* Read superblock & initialize parameters
# File organization
* inode, inumber = low level name
* Translate inodes to disk position
* Block = Collection of sectors B.S.
# Big Files
* Multi level index
* Extents = pointer + length
* Link based approaches
# Directory Organization
* Directory = list<entry name, i-node>
* Directories = special type
# Access Paths
* Reading & writing require several I/Os - use caching & buffering
