;TODO:
;add mouse features (mouse scrolling, mouse graphical cursor, mouse clicking)
;save all lines in the file in order to load them when they go offscreen
;add the ability to save and load from a file (maybe .txt)
;add horizontal scrolling
;implement clean display without stuttering (paging or double buffering)

IDEAL
MODEL small
STACK 100h
DATASEG
	;constants
	TextModeSeg equ 0B800h
	BackgroundIndex equ 0111b
	ForegroundIndex equ 0b
	
	BackgroundText equ 01111b
	ForegroundText equ 0b
	
	;since resolution is 80x25 (VGA mode 3h)
	MaxColumn equ 79
	MaxRow equ 24
	MinColumn equ 4
	MinRow equ 0
	AbsoluteMinRow equ 0

	;variables
	absoluteX dw MinColumn
	absoluteY dw MinRow
	cursorX db ?
	cursorY db ?
	textColor db ForegroundText
	backgroundColor db BackgroundText
	
	FileStr db "output.txt", 0
	FileHandle dw ?
	
	;represents which function to activate when key is pressed based on BIOS scan code
	scanCodeSwitch  dw WriteSTDOUT, Exit,        NewLine,     WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, backspace,   WriteSTDOUT
					dw Dummy,       WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, NewLine,     WriteSTDOUT, WriteSTDOUT, SaveToFile
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw CursorUp,    WriteSTDOUT, WriteSTDOUT, CursorLeft,  WriteSTDOUT, CursorRight, WriteSTDOUT, WriteSTDOUT
					dw CursorDown,  WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
					dw WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT, WriteSTDOUT
	
CODESEG

;input - dh: row, dl: column | output - None
;set cursor position
proc UpdateCursor

	push dx
	mov ah, 02h
	xor bx, bx
	mov dl, [byte ptr cursorX]
	mov dh, [byte ptr cursorY]
	int 10h
	pop dx
	ret

endp UpdateCursor


;input - None | output - None
;go to the start of the next line
proc NewLine

	inc [byte ptr cursorY]
	mov [byte ptr cursorX], MinColumn
	call UpdateCursor
	ret

endp NewLine


;input - None | output - None
;functionality check
proc Dummy

	call SaveToFile
	ret

endp Dummy


;input - al: character to write | output - None
;write character to screen
proc Write

	push ax
	;hide cursor
	mov ax, 02h
	int 33h
	
	pop ax
	mov ah, 09h
	xor bh, bh
	mov bl, [byte ptr backgroundColor]
	shl bl, 4d
	or bl, [byte ptr textColor]
	mov cx, 1d
	int 10h
	
	;show cursor
	mov ax, 01h
	int 33h
	ret
	
endp Write


;input - al: character to write | output - None
;write character to screen via the stdout
proc WriteSTDOUT

	push dx
	push cx
	call Write
	call CursorRight
	pop cx
	pop dx
	ret

endp WriteSTDOUT


;input - None | output - None
;move the cursor down by 1 cell
proc CursorDown

	inc [word ptr absoluteY]
	mov al, [byte ptr cursorY]
	cmp al, MaxRow
	jz down_boundary
	inc al
	jmp down_final
	
	down_block:
		ret
	
	down_boundary:
		mov al, 1d
		call ScrollDown
		ret
	
	down_final:
		mov [byte ptr cursorY], al
		call UpdateCursor
		ret
	
endp CursorDown


;input - None | output - None
;move the cursor up by 1 cell
proc CursorUp

	dec [word ptr absoluteY]
	mov al, [byte ptr cursorY]
	cmp al, MinRow
	jz up_boundary
	dec al
	jmp up_final
	
	up_block:
		ret
	
	up_boundary:
		mov al, 1d
		call scrollUp
		ret
	
	up_final:
		mov [byte ptr cursorY], al
		call UpdateCursor
		ret
	
endp CursorUp


;input - None | output - None
;move the cursor left by 1 cell
proc CursorLeft

	dec [word ptr absoluteX]
	mov al, [byte ptr cursorX]
	cmp al, MinColumn
	jz left_boundary
	dec al
	jmp left_final
	
	left_boundary:
		dec [byte ptr cursorY]
		mov al, MaxColumn
	
	left_final:
		mov [byte ptr cursorX], al
		call UpdateCursor
		ret
	
endp CursorLeft


;input - None | output - None
;move the cursor right by 1 cell
proc CursorRight

	inc [word ptr absoluteX]
	mov al, [byte ptr cursorX]
	cmp al, MaxColumn
	jz right_boundary
	inc al
	jmp right_final
	
	right_boundary:
		inc [byte ptr cursorY]
		mov al, MinColumn
	
	right_final:
		mov [byte ptr cursorX], al
		call UpdateCursor
		ret
	
endp CursorRight


;input - al: number of lines to scroll | output - None
;scroll the screen up
proc ScrollUp

	mov ah, 07h
	mov bh, [byte ptr backgroundColor]
	shl bh, 4d
	or bh, [byte ptr textColor]
	xor cx, cx
	mov dh, MaxRow
	mov dl, MaxColumn
	int 10h
	ret
		
endp ScrollUp


;input - al: number of lines to scroll | output - None
;scroll the screen down
proc ScrollDown

	mov ah, 06h
	mov bh, [byte ptr backgroundColor]
	shl bh, 4d
	or bh, [byte ptr textColor]
	xor cx, cx
	mov dh, MaxRow
	mov dl, MaxColumn
	int 10h

	;mov cl, [byte ptr cursorX]
	;mov [byte ptr cursorX], 0
	;call UpdateCursor
	;call LoadIndexPalette
	
	;xor di, di
	;mov al, [byte ptr absoluteY]
	;xor ah, ah
	;call WriteIndex
	
	;call LoadTextPalette
	
	;mov [byte ptr cursorX], cl
	;call UpdateCursor
	ret
		
endp ScrollDown


proc SaveToFile

	cmp al, 13h
	jz ctrl_held
	call WriteSTDOUT
	ret
	
ctrl_held:
    mov ah, 3Ch
    mov cx, 0
    mov	dx, OFFSET FileStr
    int 21h
	jc file_failed
	
	;mov ah, 3Dh
	;mov al, 1d
	;int 21h
	;jc file_falied
	
	;mov [word ptr FileHandle], ax
	;mov bx, [word ptr FileHandle]
	mov bx, ax
	mov ah, 40h
	
file_failed:
	ret

endp SaveToFile


;input - al: line number to be printed | output - None
;write the index of a line
proc WriteIndex

	push_num:
		cmp ax, 0
		jz print_num
		mov bx, 10d
		xor dx, dx
		div bx
		xchg ax, dx
		or al, 30h			
		push ax
		mov ax, dx
		inc di
		jmp push_num

	print_num:
		mov dx, MinColumn
		sub dx, di
		dec dx
			
	print_padding_before:
		mov al, ' '
		call WriteSTDOUT
		dec dx
		cmp dx, 0
		jnz print_padding_before

	print_char:
		pop ax
		call WriteSTDOUT
		dec di
		cmp di, 0
		jnz print_char

	print_padding_after:
		mov al, ' '
		call WriteSTDOUT
		ret

endp WriteIndex


proc LoadTextPalette
	
	mov [byte ptr backgroundColor], BackgroundText
	mov [byte ptr textColor], ForegroundText
	ret

endp LoadTextPalette


proc LoadIndexPalette
	
	mov [byte ptr backgroundColor], BackgroundIndex
	mov [byte ptr textColor], ForegroundIndex
	ret

endp LoadIndexPalette


;input - None | output - None
;remove the character on the cursor and move the cursor left by 1 cell
proc Backspace

	call CursorLeft
	mov al, ' '
	call Write
	ret
	
endp Backspace


start:
	mov ax, @data
	mov ds, ax

setup:
	;init screen
	mov ax, 03h
	int 10h
	
	;disable blinking (causes 1 bit to be used as a blinking on/off bit in function ah = 6h / 7h)
	mov ax, 1003h
    xor bl, bl
    int 10h

	mov al, MaxRow + 1
	call ScrollDown
	
	;enable blinking
	;mov ax, 1003h
    ;mov bl, 01h
    ;int 10h
	
	;reset mouse driver
	xor ax, ax
    int 33h
	
	;set text mouse cursor
    mov cx, 00FFh                ; screen mask (clear out attrib)
    mov dx, 8F00h                ; cursor mask (set attrib color)
    mov ax, 000Ah                
    xor bx, bx                   ; 0 = software cursor (1 = hardware)
    int 33h
	
	;restrict mouse horizontal position
	mov ax, 07h
	mov dx, 32d
	mov cx, 639d
	int 33h
	
	;restrict mouse vertical position
	mov ax, 08h
	mov cx, 0h
	mov dx, 199d
	int 33h
	
	;set mouse sensitivity threshold
	mov ax, 13h
	mov dx, 1d
	int 33h
	
	;show mouse cursor
    mov ax, 01h
    int 33h 
	
	mov [byte ptr backgroundColor], BackgroundIndex
	mov [byte ptr textColor], ForegroundIndex
	
	;print line numbers
	xor bx, bx
	mov cx, MaxRow + 1
	mov si, 1d
	line_index:
		dec si
		mov dx, si
		mov [byte ptr cursorX], 0
		mov [byte ptr cursorY], dl
		inc si
		call UpdateCursor
		xor di, di
		mov ax, si
		call WriteIndex
		
		inc si
		loop line_index

	mov [byte ptr cursorX], MinColumn
	mov [byte ptr cursorY], MinRow
	call UpdateCursor
	
	call LoadTextPalette
	
	;set cursor shape
	mov ah, 01h
	mov cx, 0607h
	int 10h
	
input_loop:
	mov ah, 01h
	int 16h
	jz check_mouse_input

key_detected:
	;empty buffer
	xor ah, ah
	int 16h
	
	;cmp al, 0
	;jz input_loop
	
	lea bx, [scanCodeSwitch]
	mov si, ax
	shr si, 7d
	call [word ptr bx+si]
		
check_mouse_input:
	mov ax, 03h
	int 33h
	test bx, 01b
	jz input_loop
	
	;update text cursor to mouse cursor on left click
	shr cx, 3d
	shr dx, 3d
	mov [byte ptr absoluteX], cl
	mov [byte ptr absoluteY], dl
	mov [byte ptr cursorX], cl
	mov [byte ptr cursorY], dl
	call UpdateCursor
	jmp input_loop


exit:
	mov ax, 4c00h
	int 21h
END start