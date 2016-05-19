This script monitors a ftp directory for changes, and downloads files
that are new or whose timestamp or size has changed.

The main purpose of this tool right now is to download the daily-updated
HTTP logs from a shared webhost,  and is fairly specific to this use case.

This utility also implements a simple content addressable store, inspired
by git but also more efficient (in some ways) and better suited to my
specific needs.

###Features I hope to add soon:

  * Detect when the updated files are simply extensions of previous versions,
    note the fact in `metadata.sqlite`,  and delete the previous version.

  * Splitting log files into smaller segments

  * Recompression using xz

  * Upload logs to dropbox

###External Dependencies:

  * sqlite
  * curl
  * tee
  * sha256sum
  * gunzip
  * linux kernel 3.11 or later
  * filesystem that supports O_TMPFILE

###Current issues:

  * Currently assumes all files are gz compressed

  * Use of curl means that we initiate a new ftp connection for each directory
    and file;  potentially inefficient if using to download a large-ish number
    of small-ish files.

  * There's a race condition if the file changes between the time we 
    obtain the directory listing and the time we download the file.  This 
    race condition is unavoidable with FTP, but we could do a better job
    dealing with it.
