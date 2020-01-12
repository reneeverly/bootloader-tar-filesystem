;******************************************************************************
; bootloader-tar-filesystem
; (c) 2019-2020 Renee Waverly Sonntag
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;******************************************************************************
; This is a 2 sector bootloader, for disks using the tar filesystem.
; The headers are spec'd for a standard 1.44 floppy, but shouldn't be too
; difficult to adapt to a different medium.
; The path to the executable file (from the root of the TAR disk):
%define FILE_TO_LOCATE "reneeverlyOS/KernelLoader.16bit" ; (no preceding slash)
;******************************************************************************
bits 16
   ; Figuring out a filename is a bit of a challenge, due to the following:
   ; 1) The filename must be a valid 8086 instruction, or set of instructions.
   ; 2) The filename must end with a byte with value 0.
   ; 3) The filename must consist of valid ASCII characters.
   ; Given these restrictions, I settled upon "$\0", which corresponds to the
   ; 8086 instruction `and al, 0`.
filename: db "$", 0

;******************************************************************************
; UStar header block offsets & definition
; reference: (Last Accessed Nov 2019) https://www.ibm.com/support/knowledgecenter/en/ssw_aix_71/filesreference/tar.h.html
;******************************************************************************
; All of the "numeric fields" are stored as "ascii octal strings".
; For example, encoding the number 6789 to a numeric field is as follows:
;       Base-10: 6789 -> Base-8: 15205 -> "0", "0", "1", "5", "2", "0", "5", 0
; And the opposite holds for decoding from a field.
;******************************************************************************
OFFSET_NAME equ 0       ; file path name, prefixed by the prefix field.
OFFSET_MODE equ 100     ; 9 bits for file perms, 3 bits each for SUID SGID SVTX
OFFSET_UID equ 108      ; userid of owner (this is not the username)
OFFSET_GID equ 116      ; groupid of owner (this is not the groupname)
OFFSET_SIZE equ 124     ; file size, 0 if a link or directory
OFFSET_MTIME equ 136    ; modification time
OFFSET_CKSUM equ 148    ; checksum of header: sum all bytes in header
OFFSET_TYPEFLAG equ 156 ; Type of file:
                        ;       '0'     Regular File
                        ;       '1'     Link
                        ;       '2'     Reserved
                        ;       '3'     Character Special
                        ;       '4'     Block Special
                        ;       '5'     Directory (size has no meaning)
                        ;       '6'     FIFO Special
                        ;       '7'     Reserved
OFFSET_LINKNAME equ 157 ; Path for the link (if a link type)
OFFSET_MAGIC equ 257    ; Declare the UStar format
OFFSET_VERSION equ 263  ; Using version 00
OFFSET_UNAME equ 265    ; username of owner, 32 bytes long
OFFSET_GNAME equ 297    ; groupname of owner, 32 bytes long
OFFSET_DEVMAJOR equ 329 ; I'm not sure what this is
OFFSET_DEVMINOR equ 337 ; Again, not sure.
OFFSET_PREFIX equ 345   ; This is the prefix to the filename, used for longpath
;******************************************************************************


;******************************************************************************
; Values specific to the medium being booted from
;******************************************************************************
   ; Standard 1.44 Floppy Disk
FLOPPY_SECTOR_MAX equ 18
FLOPPY_HEAD_MAX equ 2
   ; Generic Hard Disk (partially tested via qemu)
HDD_SECTOR_MAX equ 63
HDD_HEAD_MAX equ 2
;******************************************************************************

; Subroutine: start
; Purpose: initializes the stack, data segments, string direction, device num.
; Parameters: dl (device number)
; Affects: ax, ss, sp, ds, es
start:
   ; Define the stack.
   cli                  ; clear interrupts (enables stack movement)
   mov ax, 0x9000       ; put the stack segment at 0x9000
   mov ss, ax           ; (cannot move literal to `ss`, use `ax` as helper)
   mov sp, 4096         ; put the pointer at 4096 (stack is 4kb in size now)
   sti                  ; set interrupts (prevents stack movement)

   ; Setup the data segments.
   mov ax, 0x07c0       ; The BIOS places the bootsector at segment 0x07c0,
   mov ds, ax           ;       but only sets the code segment/pointer. We have
   mov es, ax           ;       to adjust the data segments manually.

   ; Setup string direction.
   cld                  ; clear direction flag (strings go forwards)

   ; Save the boot disk number (provided by the BIOS).
   mov byte [bootDeviceNumber], dl ; save device

; Subroutine: loadSector2
; Purpose: Loads the second sector of the bootloader
; Parameters: (none)
; Affects: ax, bx, cx, dx
loadSector2:
   ; When the BIOS loads the bootloader, it only grabs the first sector.  As
   ; this bootloader is 2 sectors in length, we have to load the second sector
   ; ourselves.  We'll be using a BIOS interrupt function to load it.
   mov cl, 2            ; sector number
   mov bx, sector2      ; memory address to load to
   mov ah, 0x02         ; BIOS function number: Read Sectors from Disk
   mov al, 1            ; number of sectors to load
   mov ch, 0            ; cylinder number
   mov dh, 0            ; head number
   mov dl, byte [bootDeviceNumber] ; boot disk number
   stc                  ; set the carry flag (bugfix to ensure BIOS sets flag)
   int 0x13             ; Using Interrupt 0x13, run the BIOS function 0x02
   jc bootstrapFailure  ; If the carry flag is still set, the disk read failed.
   jmp sector2          ; Else, jump to the memory address of the second sector.

; Subroutine: bootstrapFailure
; Purpose: prints the failure message to the screen and halts.
; Parameters: (none)
; Affects: si, (calls teletypeString: ax, bh)
bootstrapFailure:
   ; To indicate the failure, we'll display a message on the screen.
   mov si, .failMsg     ; Load the error message
   call teletypeString  ; call the teletype function
   jmp halt             ; Halt execution (via failsafe function)
.failMsg db "Could Not bootstrap bootloader.", 0

;******************************************************************************
; Headers following the filename
;******************************************************************************
times OFFSET_MODE-($-$$) db 0 ; pad out to file mode offset 
filemode: dq 0
ownerid: dq 0
groupid: dq 0
filesize: db "00000000531", 0 ; TODO: Not correct size
modtime: times 12 db 0
checksum: dq 0
typeflag: db "0"
linkdfilename: db 0

times OFFSET_MAGIC-($-$$) db 0 ; pad out to UStar indicator
indicator: db "ustar", 0
version: db "00", 0
uname: db "reneeverly", 0 ; 32 byte max length (incl \0)

times OFFSET_GNAME-($-$$) db 0 ; offset to gname
gname: db "reneeverly", 0

times OFFSET_DEVMAJOR-($-$$) db 0 ; offset to devmajor
devicemajor: dq "0000000", 0
deviceminor: dq "0000000", 0
fileprefix: db 0
;******************************************************************************

; Function: teletypeString
; Purpose: Writes the string at si to the screen.
; Parameters: si
; Returns: (nothing)
; Affects: ax, bh
teletypeString:
   ; We'll be using a BIOS interrupt function to write each character to the
   ; screen, using a while-do loop structure.
   mov ah, 0x0e                         ; BIOS function for teletype output
   xor bh, bh                           ; page number 0
.loop:
   lodsb                                ; get character from string (a byte)
   cmp al, 0x00                         ; check for EOL (ASCII)
   je .done                             ; if yes, we're done
   int 0x10                             ; call the specified BIOS function
   jmp .loop
.done:
   ret

newlineString db 13, 10, 0

; Function: octalStringToEDX
; Purpose: Converts a tar value (stored as an octal string) to an unsigned
;          integer in the register EDX
; Parameters: si
; Returns: edx
; Affects: eax
octalStringToEDX:
   ; TODO: this function isn't optimized yet
   ; size can be at most 8^7, or 2^21 -> 21 bits needed to store.
   ; so, we'll need a 32bit counter: `edx`.  `al` will be managed by `lodsb`
   xor edx, edx         ; Set edx to 0, the "space saving" way.
.loop:
   ; The loop works by multiplying the current value by 8, and adding the next
   ; character.  This is a fairly easy to understand way to convert from octal
   ; to decimal.  The current character's value will be stored in eax, while
   ; the total value is stored in edx.
   xor eax, eax         ; clear eax
   lodsb                ; get character from string ( a byte)
   cmp al, 0            ; check for EOL
   je .done             ; If this character is '\0', then the string is done
   sub al, '0'          ; convert from character to integer (If it's a '9',
                        ; or 57 in CP437, then subtracting '0', or 48 in CP437,
                        ; from it will yield a value of (57 - 48) = 9.)
   shl edx, 3           ; multiply `ecx` by 8 (the normal multiplication
                        ; instruction is somewhat complex, while bit shifting
                        ; is much easier.  Luckily, 8 happens to be 2^3, so
                        ; bit shifting to the left by 3 functionally multiplies
                        ; by 3.
   add edx, eax         ; add the number to the total (edx += eax)
   jmp .loop
.done:
   ret

; Function: halt
; Purpose: Issues the `hlt` command to the processor, which prevents it from
;          performing any other operations until an interrupt.  Wrapping the
;          instruction in a loop ensures there is no way for the hlt state to
;          be escaped (via interrupt).
; Parameters: (none)
; Returns: (nothing)
; Affects: (nothing)
halt:
   hlt                  ; halt the processor
   jmp halt

; Function: printDiskParameters
; Purpose: Displays the CHS state of the searchDisk function.
; Parameters: (none)
; Returns: (nothing)
; Affects: al, (calls teletypeString: ax, bh)
printDiskParameters:
   mov al, byte [searchDisk.cylinder]
   add al, '0'
   mov byte [.i1], al

   mov al, byte [searchDisk.head]
   add al, '0'
   mov byte [.i2], al

   mov al, byte [searchDisk.sector]
   add al, '0'
   mov byte [.i3], al

   mov si, .diskParametersString
   call teletypeString
   ret
.diskParametersString: db "Disk Parameters: '"
.i1: db "_' '"
.i2: db "_' '"
.i3: db "_'", 13, 10, 0

   ; This is the filename the bootloader is to locate and load into memory.
targetFileName db FILE_TO_LOCATE, 0
targetFileName_end:

   ; This is the boot device number, used by the BIOS to identify the boot disk
bootDeviceNumber db 0

;******************************************************************************
; End of sector 1
;******************************************************************************
   ; This is the signature the BIOS looks for to determine if a disk is bootable
times 510-($-$$) db 0 ; offset to end of sector1
dw 0xaa55
;******************************************************************************
; Start of sector 2
;******************************************************************************

sector2:
   ; For debugging purposes, we'll display a message to indicate that the
   ; bootstrapping was successfull.
   mov si, .success
   call teletypeString

   jmp searchDisk ; go to the searchDisk function
.success: db "Successfully bootstrapped bootloader.", 13, 10, 0

; Function: fixCHS
; Purpose: Takes the Cylinder-Head-Sector combination and makes sure that it
;          maps to a valid combination.  Example: [C: 0, H: 255, S: 255] isn't
;          a valid CHS combo, so it would be adjusted to [C: 134, H: 1, S: 3].
; Parameters: (none)
; Returns: (nothing)
; Affects: ax, cl
fixCHS:
   ; Is the sector number greater than the maximum allowed sector?
   cmp byte [searchDisk.sector], FLOPPY_SECTOR_MAX
   jle .noadjust ; if not, then we can proceed.
   ; (less than or equal because the first sector is 1.)

   ; Otherwise, we need to reset the sector and adjust the head.
   xor ah, ah                   ; prepping for division
   mov al, byte [searchDisk.sector]       ; let's divide the sector number
   mov cl, FLOPPY_SECTOR_MAX    ; by the sector max
   div cl                       ; (ah = remainder, al = dividend)
   add byte [searchDisk.head], al         ; and add that to the head number
   mov byte [searchDisk.sector], ah       ; reset the sector to the remainder

   ; Is the head number greater than the maximum allowed head?
   cmp byte [searchDisk.head], FLOPPY_HEAD_MAX
   jl .noadjust ; if not, then we can proceed.
   ; (less than, which differs from above, because the first head is 0.)

   ; Otherwise, we need to reset the head and adjust the cylinder.
   xor ah, ah                   ; prepping for division
   mov al, byte [searchDisk.head]         ; let's divide the head number
   mov cl, FLOPPY_HEAD_MAX      ; by the head max
   div cl                       ; (ah = remainder, al = dividend)
   add byte [searchDisk.cylinder], al     ; and add that to the cylinder number
   mov byte [searchDisk.head], ah         ; reset the head to the remainder
.noadjust:
   ret

; Subroutine: searchDisk
; Purpose: Scans through the disk, searching for a file with matching filename
; Parameters: (none)
; Returns: (nothing)
; Affects: (si, di, ax, bx, cx, edx)
searchDisk:
   call fixCHS
; load sector
   mov bx, workspace
   mov ah, 0x02
   mov al, 1
   mov ch, byte [.cylinder]
   mov cl, byte [.sector]
   mov dh, byte [.head]
   mov dl, byte [bootDeviceNumber]
   stc
   int 0x13
   jnc .magic
.failLoop:
   jmp invalidDiskParameters
; check for magic
.magic:
   ; check this file's magic value with the expected magic value
   mov si, workspace+OFFSET_MAGIC       ; compare this magic
   mov di, .magicString                 ; against the known magic
   mov cx, 5                            ; length limit of 5 chars
   repe cmpsb                           ; repeat until length or \0
   jnz invalidMagic                     ; if unequal at exit condition, fail

   ; print file name
   mov si, workspace+OFFSET_NAME        ; go to the file name
   call teletypeString                  ; print it
   mov si, newlineString                ; go to the newline sequence
   call teletypeString                  ; print it
   call printDiskParameters             ; print out the C-H-S combo

   ; get file size
   mov si, workspace+OFFSET_SIZE        ; go to the size offset
   call octalStringToEDX                ; convert from octal and place in EDX

   ; convert the file size to a sector count
   add edx, 512-1 ; dividing, say, 1 by 512 yields 0 sectors.  By boosting
                  ; that 1 to 512, we guarantee that the correct number of
                  ; sectors is calculated.
   shr edx, 9 ; divide by 512
   inc edx ; account for the definition sector in the offset

   ; Before we increment the sector, we need to see if this is our file.
   mov si, workspace+OFFSET_NAME                ; compare this file name
   mov di, targetFileName                       ; against the target file name
   mov cx, targetFileName_end-targetFileName    ; set a length limit
   repe cmpsb                                   ; repeat until length or \0
   jz loadFile                                  ; if equal at exit condition,
                                                ; this is the target file.
   add byte [.sector], dl       ; otherwise, add the length to the sector

   ; Let's delay for a bit, just in case things are running haywire.
   ; Debugging is easier if you can read the text being printed.
   mov     cx, 0x0001                           ; delay for 0x00010000
   mov     dx, 0x0000
   mov     ah, 0x86                             ; BIOS function number: 0x86
   int     0x15                                 ; Use BIOS Interrupt 0x15 (0x86)

   jmp searchDisk               ; and go again

   ; variables
.cylinder: db 0
.head: db 0
.sector: db 1
.magicString: db "ustar", 0
.errCount: dw 1024

; Subroutine: loadFile
; Purpose: From the current CHS, loads the entire file into memory.
; Parameters: dl (file length in sectors)
; Affects: ax, es, bx, cx, dx
loadFile:
   ; Save the file length
   mov byte [.length], dl

   ; Display the debugging message indicating that the file was found.
   mov si, .alert               ; go to the alert string
   call teletypeString          ; print it

   ; load the file, starting with memory address 0x0e00:0x0000 (linear 0xe000)
   mov ax, 0x0e00               ; set up the extended segment to point at 0x0e00
   mov es, ax                   ; (`es` can't be literally mov'd to, using `ax`)
   mov bx, 0                    ; The BIOS function uses es:bx, not es:di.
.loadSector:
   inc byte [searchDisk.sector] ; select the next sector
   call fixCHS                  ; fix the CHS values

   ; populate arguments other than bx
   mov ah, 0x02                 ; BIOS function number: 0x02
   mov al, 1                    ; number of sectors to load
   mov ch, byte [searchDisk.cylinder]   ; cylinder number
   mov cl, byte [searchDisk.sector]     ; sector number
   mov dh, byte [searchDisk.head]       ; head number
   mov dl, byte [bootDeviceNumber]      ; device number
   stc                  ; set carry flag (bugfix for function failure)
   int 0x13             ; Use BIOS Interrupt 0x13 to run function 0x02
   jc .fail             ; If the carry flag is still set, read failure
   add bx, 512          ; Move the memory pointer ahead 512 bytes (sector len.)
   dec byte [.length]   ; remove 1 from the file length sector count
   jnz .loadSector      ; If file length isn't 0, there's still sectors to load
.done:
   jmp 0x0e00:0x0000    ; Else, we can transfer execution to the memory address
.fail:
   jmp invalidDiskParameters ; Read failure, we can't recover
.alert: db "File Found!", 13, 10, 0
.length:

; Subroutine: invalidDiskParameters
; Purpose: Displays the CHS parameters under the error condition.
; Parameters: ah (disk error number from BIOS)
; Affects: ax
invalidDiskParameters:
   ; Conver the error number to an ascii string
   mov al, ah           ; duplicate ah to al
   and ah, 0xF0         ; strip out the lower half of ah
   shr ah, 4            ; move the top half of ah into the lower half
   and al, 0x0F         ; strip out the upper half of al
   add ah, '0'          ; convert the one-digit number to ascii
   add al, '0'          ; convert the one-digit number to ascii

   ; store the ascii string
   mov byte [.eh], ah
   mov byte [.el], al

   ; print the string
   mov si, .invalidDiskParametersString
   call teletypeString

   ; and display the disk parameters
   call printDiskParameters

   ; and halt
   jmp halt
.invalidDiskParametersString: db "Disk Error: '"
.eh: db "_"
.el: db "_'", 13, 10, 0

; Subroutine: invalidMagic
; Purpose: Display end of disk / damaged sector / invalid magic error message.
; Parameters: (none)
; Affects si, (calls teletypeString: ax, bh)
invalidMagic:
   mov si, .invalidMagicString
   call teletypeString
   call printDiskParameters
   jmp halt
.invalidMagicString: db "Reached End of Disk, or Damaged Sector. (Invalid Magic)", 13, 10, "Target File Not Found.", 13, 10, 0

;******************************************************************************
; End of sector 2
;******************************************************************************
times 1024-($-$$) db 0 ; offset to end of sector2

workspace:
