# bootloader-tar-filesystem
# (c) 2019-2020 Renee Waverly Sonntag
# This Source Code Form is subject to the terms of the Mozilla Public
# License, V. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

ASM=nasm
ASMFLAGS=-O0 -f bin

bootloader.bin: bootloader.asm
	$(ASM) $(ASMFLAGS) -o bootloader.bin bootloader.asm

package: bootloader.bin source.tar
	cat bootloader.bin source.tar > destination.tar
	truncate -s 1440k distfloppy.img
	dd status=noxfer conv=notrunc if=destination.tar of=distfloppy.img

clean:
	rm bootloader.bin destination.tar distfloppy.img

