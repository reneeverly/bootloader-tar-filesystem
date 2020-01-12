# bootloader-tar-filesystem
This is a bootloader for disks using the Ustar filesystem.  It scans for a specified file, loads it into memory, and jumps to it.  It takes up exactly two sectors, masquerading as a file name "$".  The technical reasoning for the filename is spelled out in `bootloader.asm`.  The bootloader can be prepended to an uncompressed ustar formatted tar file and distributed as a raw image file (.img, .flp, etc.).

An uncompressed tar file is one that hasn't been GZ'd or otherwise compressed.  A safe way to create an uncompressed ustar formatted tar file would be:
```bash
tar -H ustar -cf newarchive.tar ../../files/to/put/in/archive/*
```

## Usage
The filename scanned for, `FILE_TO_LOCATE`, is defined directly beneath the copyright notice in `bootloader.asm`.  The length of the filename is restricted by the Ustar standard (155 bytes), but also by the free space remaining in the first sector of the bootloader (at least 31 bytes available, possible more).


### Packaging
This bootloader is designed to be prepended to uncompressed tar files for distribution as a binary disk image (.img, .flp, etc.).  Make sure to pad out the file to the required size

This can be done in many ways, such as:
```bash
cat bootloader.bin source.tar > destination.tar  # prepend with bootloader
truncate -s 1440k distfloppy.img                 # create 1.44Mb file
dd status=noxfer conv=notrunc if=destination.img # insert prepended tar directly
```
or
```bash
make package
```
(Which assumes `source.tar` and creates `destination.tar` & `distfloppy.img`.)
