#| -*-Scheme-*-

$Id: os2winp.scm,v 1.22 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; OS/2 PM Interface -- Primitives
;;; package: (runtime os2-window-primitives)

(declare (usual-integrations))

(define-primitives
  (os2-clipboard-read-text 0)
  (os2-clipboard-write-text 1)
  (os2-map-window-point 3)
  (os2-window-handle-from-id 2)
  (os2menu-create 3)
  (os2menu-destroy 1)
  (os2menu-get-item 3)
  (os2menu-get-item-attributes 4)
  (os2menu-insert-item 7)
  (os2menu-n-items 1)
  (os2menu-nth-item 2)
  (os2menu-remove-item 4)
  (os2menu-set-item-attributes 5)
  (os2pm-synchronize 0)
  (os2ps-bitblt 6)
  (os2ps-clear 5)
  (os2ps-create-bitmap 3)
  (os2ps-create-memory-ps 0)
  (os2ps-destroy-bitmap 1)
  (os2ps-destroy-memory-ps 1)
  (os2ps-draw-point 3)
  (os2ps-get-bitmap 1)
  (os2ps-get-bitmap-bits 5)
  (os2ps-get-bitmap-parameters 1)
  (os2ps-get-font-metrics 1)
  (os2ps-line 3)
  (os2ps-move-graphics-cursor 3)
  (os2ps-poly-line 3)
  (os2ps-poly-line-disjoint 3)
  (os2ps-query-capabilities 3)
  (os2ps-query-capability 2)
  (os2ps-reset-clip-rectangle 1)
  (os2ps-set-bitmap 2)
  (os2ps-set-bitmap-bits 5)
  (os2ps-set-clip-rectangle 5)
  (os2ps-set-colors 3)
  (os2ps-set-font 3)
  (os2ps-set-line-type 2)
  (os2ps-set-mix 2)
  (os2ps-text-width 4)
  (os2ps-write 6)
  (os2win-activate 1)
  (os2win-alarm 1)
  (os2win-beep 2)
  (os2win-client-handle 1)
  (os2win-close 1)
  (os2win-close-event-qid 1)
  (os2win-console-wid 0)
  (os2win-desktop-height 0)
  (os2win-desktop-width 0)
  (os2win-destroy-pointer 1)
  (os2win-event-ready? 2)
  (os2win-focus? 1)
  (os2win-font-dialog 2)
  (os2win-frame-handle 1)
  (os2win-get-event 2)
  (os2win-get-frame-size 1)
  (os2win-get-pos 1)
  (os2win-get-size 1)
  (os2win-invalidate 5)
  (os2win-load-pointer 3)
  (os2win-load-menu 3)
  (os2win-move-cursor 3)
  (os2win-open 2)
  (os2win-open-event-qid 0)
  (os2win-popup-menu 7)
  (os2win-ps 1)
  (os2win-query-sys-value 2)
  (os2win-scroll 7)
  (os2win-set-capture 2)
  (os2win-set-grid 3)
  (os2win-set-icon 2)
  (os2win-set-pos 3)
  (os2win-set-size 3)
  (os2win-set-state 2)
  (os2win-set-title 2)
  (os2win-shape-cursor 4)
  (os2win-show 2)
  (os2win-show-cursor 2)
  (os2win-track-mouse 2)
  (os2win-update-frame 2))

(define-integrable (event-type event) (vector-ref event 0))
(define-integrable (event-wid event) (vector-ref event 1))
(define-integrable (set-event-wid! event wid) (vector-set! event 1 wid))

(define-syntax define-event
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form))
	   (type (close-syntax (caddr form) environment))
	   (slots (cdddr form)))
       `(BEGIN
	  (DEFINE-INTEGRABLE ,(symbol-append 'EVENT-TYPE: name) ,type)
	  ,@(let loop ((slots slots) (index 2))
	      (if (pair? slots)
		  (cons `(DEFINE-INTEGRABLE
			   (,(symbol-append name '-EVENT/ (car slots)) EVENT)
			   (VECTOR-REF EVENT ,index))
			(loop (cdr slots) (+ index 1)))
		  '())))))))

;; These must match "microcode/pros2pm.c"
(define-event button     0 number type x y flags)
(define-event close      1)
(define-event focus      2 gained?)
(define-event key        3 code flags repeat)
(define-event paint      4 xl xh yl yh)
(define-event resize     5 width height)
(define-event visibility 6 shown?)
(define-event command    7 code source mouse?)
(define-event help       8 code source mouse?)
(define-event mousemove  9 x y hit-test flags)

(define-integrable number-of-event-types 10)

(define-integrable button-event-type:down 0)
(define-integrable button-event-type:up 1)
(define-integrable button-event-type:click 2)
(define-integrable button-event-type:double-click 3)

(define-structure (font-metrics (type vector) (conc-name font-metrics/))
  (width #f read-only #t)
  (height #f read-only #t)
  (descender #f read-only #t))

;;; Constants from OS/2 header file "pmwin.h":

(define-integrable CURSOR_SOLID		#x0000)
(define-integrable CURSOR_HALFTONE	#x0001)
(define-integrable CURSOR_FRAME		#x0002)
(define-integrable CURSOR_FLASH		#x0004)

(define-integrable VK_BUTTON1		#x01)
(define-integrable VK_BUTTON2		#x02)
(define-integrable VK_BUTTON3		#x03)
(define-integrable VK_BREAK		#x04)
(define-integrable VK_BACKSPACE		#x05)
(define-integrable VK_TAB		#x06)
(define-integrable VK_BACKTAB		#x07)
(define-integrable VK_NEWLINE		#x08)
(define-integrable VK_SHIFT		#x09)
(define-integrable VK_CTRL		#x0A)
(define-integrable VK_ALT		#x0B)
(define-integrable VK_ALTGRAF		#x0C)
(define-integrable VK_PAUSE		#x0D)
(define-integrable VK_CAPSLOCK		#x0E)
(define-integrable VK_ESC		#x0F)
(define-integrable VK_SPACE		#x10)
(define-integrable VK_PAGEUP		#x11)
(define-integrable VK_PAGEDOWN		#x12)
(define-integrable VK_END		#x13)
(define-integrable VK_HOME		#x14)
(define-integrable VK_LEFT		#x15)
(define-integrable VK_UP		#x16)
(define-integrable VK_RIGHT		#x17)
(define-integrable VK_DOWN		#x18)
(define-integrable VK_PRINTSCRN		#x19)
(define-integrable VK_INSERT		#x1A)
(define-integrable VK_DELETE		#x1B)
(define-integrable VK_SCRLLOCK		#x1C)
(define-integrable VK_NUMLOCK		#x1D)
(define-integrable VK_ENTER		#x1E)
(define-integrable VK_SYSRQ		#x1F)
(define-integrable VK_F1		#x20)
(define-integrable VK_F2		#x21)
(define-integrable VK_F3		#x22)
(define-integrable VK_F4		#x23)
(define-integrable VK_F5		#x24)
(define-integrable VK_F6		#x25)
(define-integrable VK_F7		#x26)
(define-integrable VK_F8		#x27)
(define-integrable VK_F9		#x28)
(define-integrable VK_F10		#x29)
(define-integrable VK_F11		#x2A)
(define-integrable VK_F12		#x2B)
(define-integrable VK_F13		#x2C)
(define-integrable VK_F14		#x2D)
(define-integrable VK_F15		#x2E)
(define-integrable VK_F16		#x2F)
(define-integrable VK_F17		#x30)
(define-integrable VK_F18		#x31)
(define-integrable VK_F19		#x32)
(define-integrable VK_F20		#x33)
(define-integrable VK_F21		#x34)
(define-integrable VK_F22		#x35)
(define-integrable VK_F23		#x36)
(define-integrable VK_F24		#x37)
(define-integrable VK_ENDDRAG		#x38)
(define-integrable VK_CLEAR		#x39)
(define-integrable VK_EREOF		#x3A)
(define-integrable VK_PA1		#x3B)
(define-integrable virtual-key-supremum #x3C)

(define-integrable KC_NONE		#x0000)
(define-integrable KC_CHAR		#x0001)
(define-integrable KC_VIRTUALKEY	#x0002)
(define-integrable KC_SCANCODE		#x0004)
(define-integrable KC_SHIFT		#x0008)
(define-integrable KC_CTRL		#x0010)
(define-integrable KC_ALT		#x0020)
(define-integrable KC_KEYUP		#x0040)
(define-integrable KC_PREVDOWN		#x0080)
(define-integrable KC_LONEKEY		#x0100)
(define-integrable KC_DEADKEY		#x0200)
(define-integrable KC_COMPOSITE		#x0400)
(define-integrable KC_INVALIDCOMP	#x0800)
(define-integrable KC_TOGGLE		#x1000)
(define-integrable KC_INVALIDCHAR	#x2000)

(define-integrable LINETYPE_DEFAULT       0)
(define-integrable LINETYPE_DOT           1)
(define-integrable LINETYPE_SHORTDASH     2)
(define-integrable LINETYPE_DASHDOT       3)
(define-integrable LINETYPE_DOUBLEDOT     4)
(define-integrable LINETYPE_LONGDASH      5)
(define-integrable LINETYPE_DASHDOUBLEDOT 6)
(define-integrable LINETYPE_SOLID         7)
(define-integrable LINETYPE_INVISIBLE     8)
(define-integrable LINETYPE_ALTERNATE     9)

(define-integrable FM_DEFAULT     0)
(define-integrable FM_OR          1)
(define-integrable FM_OVERPAINT   2)
(define-integrable FM_XOR         4)
(define-integrable FM_LEAVEALONE  5)
(define-integrable FM_AND         6)
(define-integrable FM_SUBTRACT    7)
(define-integrable FM_MASKSRCNOT  8)
(define-integrable FM_ZERO        9)
(define-integrable FM_NOTMERGESRC 10)
(define-integrable FM_NOTXORSRC   11)
(define-integrable FM_INVERT      12)
(define-integrable FM_MERGESRCNOT 13)
(define-integrable FM_NOTCOPYSRC  14)
(define-integrable FM_MERGENOTSRC 15)
(define-integrable FM_NOTMASKSRC  16)
(define-integrable FM_ONE         17)

(define-integrable window-state:top        0)
(define-integrable window-state:bottom     1)
(define-integrable window-state:show       2)
(define-integrable window-state:hide       3)
(define-integrable window-state:activate   4)
(define-integrable window-state:deactivate 5)
(define-integrable window-state:minimize   6)
(define-integrable window-state:maximize   7)
(define-integrable window-state:restore    8)

(define-integrable WS_VISIBLE      #x80000000)
(define-integrable WS_DISABLED     #x40000000)
(define-integrable WS_CLIPCHILDREN #x20000000)
(define-integrable WS_CLIPSIBLINGS #x10000000)
(define-integrable WS_PARENTCLIP   #x08000000)
(define-integrable WS_SAVEBITS     #x04000000)
(define-integrable WS_SYNCPAINT    #x02000000)
(define-integrable WS_MINIMIZED    #x01000000)
(define-integrable WS_MAXIMIZED    #x00800000)
(define-integrable WS_ANIMATE      #x00400000)

;; codes for OS2PS-QUERY-CAPABILITIES and OS2PS-QUERY-CAPABILITY
(define-integrable CAPS_FAMILY                     0)
(define-integrable CAPS_IO_CAPS                    1)
(define-integrable CAPS_TECHNOLOGY                 2)
(define-integrable CAPS_DRIVER_VERSION             3)
(define-integrable CAPS_WIDTH                      4) ;pels
(define-integrable CAPS_HEIGHT                     5) ;pels
(define-integrable CAPS_WIDTH_IN_CHARS             6)
(define-integrable CAPS_HEIGHT_IN_CHARS            7)
(define-integrable CAPS_HORIZONTAL_RESOLUTION      8) ;pels per meter
(define-integrable CAPS_VERTICAL_RESOLUTION        9) ;pels per meter
(define-integrable CAPS_CHAR_WIDTH                10) ;pels
(define-integrable CAPS_CHAR_HEIGHT               11) ;pels
(define-integrable CAPS_SMALL_CHAR_WIDTH          12) ;pels
(define-integrable CAPS_SMALL_CHAR_HEIGHT         13) ;pels
(define-integrable CAPS_COLORS                    14)
(define-integrable CAPS_COLOR_PLANES              15)
(define-integrable CAPS_COLOR_BITCOUNT            16)
(define-integrable CAPS_COLOR_TABLE_SUPPORT       17)
(define-integrable CAPS_MOUSE_BUTTONS             18)
(define-integrable CAPS_FOREGROUND_MIX_SUPPORT    19)
(define-integrable CAPS_BACKGROUND_MIX_SUPPORT    20)
(define-integrable CAPS_VIO_LOADABLE_FONTS        21)
(define-integrable CAPS_WINDOW_BYTE_ALIGNMENT     22)
(define-integrable CAPS_BITMAP_FORMATS            23)
(define-integrable CAPS_RASTER_CAPS               24)
(define-integrable CAPS_MARKER_HEIGHT             25) ;pels
(define-integrable CAPS_MARKER_WIDTH              26) ;pels
(define-integrable CAPS_DEVICE_FONTS              27)
(define-integrable CAPS_GRAPHICS_SUBSET           28)
(define-integrable CAPS_GRAPHICS_VERSION          29)
(define-integrable CAPS_GRAPHICS_VECTOR_SUBSET    30)
(define-integrable CAPS_DEVICE_WINDOWING          31)
(define-integrable CAPS_ADDITIONAL_GRAPHICS       32)
(define-integrable CAPS_PHYS_COLORS               33)
(define-integrable CAPS_COLOR_INDEX               34)
(define-integrable CAPS_GRAPHICS_CHAR_WIDTH       35)
(define-integrable CAPS_GRAPHICS_CHAR_HEIGHT      36)
(define-integrable CAPS_HORIZONTAL_FONT_RES       37)
(define-integrable CAPS_VERTICAL_FONT_RES         38)
(define-integrable CAPS_DEVICE_FONT_SIM           39)
(define-integrable CAPS_LINEWIDTH_THICK           40)
(define-integrable CAPS_DEVICE_POLYSET_POINTS     41)

;; Constants for CAPS_IO_CAPS
(define-integrable CAPS_IO_DUMMY       1)
(define-integrable CAPS_IO_SUPPORTS_OP 2)
(define-integrable CAPS_IO_SUPPORTS_IP 3)
(define-integrable CAPS_IO_SUPPORTS_IO 4)

;; Constants for CAPS_TECHNOLOGY
(define-integrable CAPS_TECH_UNKNOWN        0)
(define-integrable CAPS_TECH_VECTOR_PLOTTER 1)
(define-integrable CAPS_TECH_RASTER_DISPLAY 2)
(define-integrable CAPS_TECH_RASTER_PRINTER 3)
(define-integrable CAPS_TECH_RASTER_CAMERA  4)
(define-integrable CAPS_TECH_POSTSCRIPT     5)

;; Constants for CAPS_COLOR_TABLE_SUPPORT
(define-integrable CAPS_COLTABL_RGB_8      #x0001)
(define-integrable CAPS_COLTABL_RGB_8_PLUS #x0002)
(define-integrable CAPS_COLTABL_TRUE_MIX   #x0004)
(define-integrable CAPS_COLTABL_REALIZE    #x0008)

;; Constants for CAPS_FOREGROUND_MIX_SUPPORT
(define-integrable CAPS_FM_OR              #x0001)
(define-integrable CAPS_FM_OVERPAINT       #x0002)
(define-integrable CAPS_FM_XOR             #x0008)
(define-integrable CAPS_FM_LEAVEALONE      #x0010)
(define-integrable CAPS_FM_AND             #x0020)
(define-integrable CAPS_FM_GENERAL_BOOLEAN #x0040)

;; Constants for CAPS_BACKGROUND_MIX_SUPPORT
(define-integrable CAPS_BM_OR              #x0001)
(define-integrable CAPS_BM_OVERPAINT       #x0002)
(define-integrable CAPS_BM_XOR             #x0008)
(define-integrable CAPS_BM_LEAVEALONE      #x0010)
(define-integrable CAPS_BM_AND             #x0020)
(define-integrable CAPS_BM_GENERAL_BOOLEAN #x0040)
(define-integrable CAPS_BM_SRCTRANSPARENT  #x0080)
(define-integrable CAPS_BM_DESTTRANSPARENT #x0100)

;; Constants for CAPS_DEVICE_WINDOWING
(define-integrable CAPS_DEV_WINDOWING_SUPPORT 1)

;; Constants for CAPS_ADDITIONAL_GRAPHICS
(define-integrable CAPS_VDD_DDB_TRANSFER          #x0001)
(define-integrable CAPS_GRAPHICS_KERNING_SUPPORT  #x0002)
(define-integrable CAPS_FONT_OUTLINE_DEFAULT      #x0004)
(define-integrable CAPS_FONT_IMAGE_DEFAULT        #x0008)
;; bits represented by values #x0010 and #x0020 are reserved
(define-integrable CAPS_SCALED_DEFAULT_MARKERS    #x0040)
(define-integrable CAPS_COLOR_CURSOR_SUPPORT      #x0080)
(define-integrable CAPS_PALETTE_MANAGER           #x0100)
(define-integrable CAPS_COSMETIC_WIDELINE_SUPPORT #x0200)
(define-integrable CAPS_DIRECT_FILL               #x0400)
(define-integrable CAPS_REBUILD_FILLS             #x0800)
(define-integrable CAPS_CLIP_FILLS                #x1000)
(define-integrable CAPS_ENHANCED_FONTMETRICS      #x2000)
(define-integrable CAPS_TRANSFORM_SUPPORT         #x4000)

;; Constants for CAPS_WINDOW_BYTE_ALIGNMENT
(define-integrable CAPS_BYTE_ALIGN_REQUIRED     0)
(define-integrable CAPS_BYTE_ALIGN_RECOMMENDED  1)
(define-integrable CAPS_BYTE_ALIGN_NOT_REQUIRED 2)

;; Constants for CAPS_RASTER_CAPS
(define-integrable CAPS_RASTER_BITBLT         #x0001)
(define-integrable CAPS_RASTER_BANDING        #x0002)
(define-integrable CAPS_RASTER_BITBLT_SCALING #x0004)
(define-integrable CAPS_RASTER_SET_PEL        #x0010)
(define-integrable CAPS_RASTER_FONTS          #x0020)
(define-integrable CAPS_RASTER_FLOOD_FILL     #x0040)

;; Constants for OS2PS-BITBLT raster-op argument
(define-integrable ROP_SRCCOPY     #xCC)
(define-integrable ROP_SRCPAINT    #xEE)
(define-integrable ROP_SRCAND      #x88)
(define-integrable ROP_SRCINVERT   #x66)
(define-integrable ROP_SRCERASE    #x44)
(define-integrable ROP_NOTSRCCOPY  #x33)
(define-integrable ROP_NOTSRCERASE #x11)
(define-integrable ROP_MERGECOPY   #xC0)
(define-integrable ROP_MERGEPAINT  #xBB)
(define-integrable ROP_PATCOPY     #xF0)
(define-integrable ROP_PATPAINT    #xFB)
(define-integrable ROP_PATINVERT   #x5A)
(define-integrable ROP_DSTINVERT   #x55)
(define-integrable ROP_ZERO        #x00)
(define-integrable ROP_ONE         #xFF)
   
;; Constants for OS2PS-BITBLT options argument
(define-integrable BBO_OR            0)
(define-integrable BBO_AND           1)
(define-integrable BBO_IGNORE        2)
(define-integrable BBO_PAL_COLORS    4)
(define-integrable BBO_NO_COLOR_INFO 8)

;; Menu item positions:
(define-integrable MIT_END                    #xFFFF)
(define-integrable MIT_NONE                   #xFFFF)
(define-integrable MIT_MEMERROR               #xFFFF)
(define-integrable MIT_ERROR                  #xFFFF)
(define-integrable MIT_FIRST                  #xFFFE)
(define-integrable MIT_LAST                   #xFFFD)

;; Menu item styles:
(define-integrable MIS_TEXT                   #x0001)
(define-integrable MIS_BITMAP                 #x0002)
(define-integrable MIS_SEPARATOR              #x0004)
(define-integrable MIS_OWNERDRAW              #x0008)
(define-integrable MIS_SUBMENU                #x0010)
(define-integrable MIS_MULTMENU               #x0020) ;multiple choice submenu
(define-integrable MIS_SYSCOMMAND             #x0040)
(define-integrable MIS_HELP                   #x0080)
(define-integrable MIS_STATIC                 #x0100)
(define-integrable MIS_BUTTONSEPARATOR        #x0200)
(define-integrable MIS_BREAK                  #x0400)
(define-integrable MIS_BREAKSEPARATOR         #x0800)
(define-integrable MIS_GROUP                  #x1000) ;multiple choice group
;; In multiple choice submenus a style of 'single' denotes the item is
;; a radiobutton.  Absence of this style defaults the item to a
;; checkbox.
(define-integrable MIS_SINGLE                 #x2000)

;; Menu item attributes:
(define-integrable MIA_NODISMISS              #x0020)
(define-integrable MIA_FRAMED                 #x1000)
(define-integrable MIA_CHECKED                #x2000)
(define-integrable MIA_DISABLED               #x4000)
(define-integrable MIA_HILITED                #x8000)

(define-integrable FID_SYSMENU                #x8002)
(define-integrable FID_TITLEBAR               #x8003)
(define-integrable FID_MINMAX                 #x8004)
(define-integrable FID_MENU                   #x8005)
(define-integrable FID_VERTSCROLL             #x8006)
(define-integrable FID_HORZSCROLL             #x8007)
(define-integrable FID_CLIENT                 #x8008)

;; Menu control styles */
(define-integrable MS_ACTIONBAR               #x0001)
(define-integrable MS_TITLEBUTTON             #x0002)
(define-integrable MS_VERTICALFLIP            #x0004)
(define-integrable MS_CONDITIONALCASCADE      #x0040)

;; Frame window styles:
(define-integrable FCF_TITLEBAR               #x00000001)
(define-integrable FCF_SYSMENU                #x00000002)
(define-integrable FCF_MENU                   #x00000004)
(define-integrable FCF_SIZEBORDER             #x00000008)
(define-integrable FCF_MINBUTTON              #x00000010)
(define-integrable FCF_MAXBUTTON              #x00000020)
(define-integrable FCF_MINMAX                 #x00000030)
(define-integrable FCF_VERTSCROLL             #x00000040)
(define-integrable FCF_HORZSCROLL             #x00000080)
(define-integrable FCF_DLGBORDER              #x00000100)
(define-integrable FCF_BORDER                 #x00000200)
(define-integrable FCF_SHELLPOSITION          #x00000400)
(define-integrable FCF_TASKLIST               #x00000800)
(define-integrable FCF_NOBYTEALIGN            #x00001000)
(define-integrable FCF_NOMOVEWITHOWNER        #x00002000)
(define-integrable FCF_ICON                   #x00004000)
(define-integrable FCF_ACCELTABLE             #x00008000)
(define-integrable FCF_SYSMODAL               #x00010000)
(define-integrable FCF_SCREENALIGN            #x00020000)
(define-integrable FCF_MOUSEALIGN             #x00040000)
(define-integrable FCF_HIDEBUTTON             #x01000000)
(define-integrable FCF_HIDEMAX                #x01000020)
(define-integrable FCF_AUTOICON               #x40000000)
(define-integrable FCF_STANDARD               #x0000CC3F)

;;; Window handles.
(define-integrable NULLHANDLE 0)
(define-integrable HWND_DESKTOP 1)

;;; Hit-test values (event-type:mousemove).
(define-integrable HT_NORMAL 0)
(define-integrable HT_TRANSPARENT -1)
(define-integrable HT_DISCARD -2)
(define-integrable HT_ERROR -3)

;;; Pop-up menu option flags.
(define-integrable PU_POSITIONONITEM          #x0001)
(define-integrable PU_HCONSTRAIN              #x0002)
(define-integrable PU_VCONSTRAIN              #x0004)
(define-integrable PU_NONE                    #x0000)
(define-integrable PU_MOUSEBUTTON1DOWN        #x0008)
(define-integrable PU_MOUSEBUTTON2DOWN        #x0010)
(define-integrable PU_MOUSEBUTTON3DOWN        #x0018)
(define-integrable PU_SELECTITEM              #x0020)
(define-integrable PU_MOUSEBUTTON1            #x0040)
(define-integrable PU_MOUSEBUTTON2            #x0080)
(define-integrable PU_MOUSEBUTTON3            #x0100)
(define-integrable PU_KEYBOARD                #x0200)

;;; Alarm types (os2win-alarm).
(define-integrable WA_WARNING 0)
(define-integrable WA_NOTE 1)
(define-integrable WA_ERROR 2)

(define-integrable SPTR_ARROW 1)
(define-integrable SPTR_TEXT 2)
(define-integrable SPTR_WAIT 3)
(define-integrable SPTR_SIZE 4)
(define-integrable SPTR_MOVE 5)
(define-integrable SPTR_SIZENWSE 6)
(define-integrable SPTR_SIZENESW 7)
(define-integrable SPTR_SIZEWE 8)
(define-integrable SPTR_SIZENS 9)
(define-integrable SPTR_APPICON 10)
(define-integrable SPTR_ICONINFORMATION 11)
(define-integrable SPTR_ICONQUESTION 12)
(define-integrable SPTR_ICONERROR 13)
(define-integrable SPTR_ICONWARNING 14)
(define-integrable SPTR_ILLEGAL 18)
(define-integrable SPTR_FILE 19)
(define-integrable SPTR_FOLDER 20)
(define-integrable SPTR_MULTFILE 21)
(define-integrable SPTR_PROGRAM 22)

;;; Constants for use with os2win-load-pointer.
(define-integrable IDI_BCH	10)
(define-integrable IDI_COFFEE	11)
(define-integrable IDI_CONSES	12)
(define-integrable IDI_EDWIN	13)
(define-integrable IDI_ENVIR1	14)
(define-integrable IDI_GRAPHICS	15)
(define-integrable IDI_LAMBDA	16)
(define-integrable IDI_LAMBDA2	17)
(define-integrable IDI_LIAR1	18)
(define-integrable IDI_LIAR2	19)
(define-integrable IDI_LIAR3	20)
(define-integrable IDI_MINCER	21)
(define-integrable IDI_SHIELD1	22)
(define-integrable IDI_SHIELD2	23)
(define-integrable IDI_SHIELD3	24)
(define-integrable IDI_SHIELD4	25)

(define-integrable SV_SWAPBUTTON              0)
(define-integrable SV_DBLCLKTIME              1)
(define-integrable SV_CXDBLCLK                2)
(define-integrable SV_CYDBLCLK                3)
(define-integrable SV_CXSIZEBORDER            4)
(define-integrable SV_CYSIZEBORDER            5)
(define-integrable SV_ALARM                   6)
;;; 7-8
(define-integrable SV_CURSORRATE              9)
(define-integrable SV_FIRSTSCROLLRATE         10)
(define-integrable SV_SCROLLRATE              11)
(define-integrable SV_NUMBEREDLISTS           12)
(define-integrable SV_WARNINGFREQ             13)
(define-integrable SV_NOTEFREQ                14)
(define-integrable SV_ERRORFREQ               15)
(define-integrable SV_WARNINGDURATION         16)
(define-integrable SV_NOTEDURATION            17)
(define-integrable SV_ERRORDURATION           18)
;;; 19
(define-integrable SV_CXSCREEN                20)
(define-integrable SV_CYSCREEN                21)
(define-integrable SV_CXVSCROLL               22)
(define-integrable SV_CYHSCROLL               23)
(define-integrable SV_CYVSCROLLARROW          24)
(define-integrable SV_CXHSCROLLARROW          25)
(define-integrable SV_CXBORDER                26)
(define-integrable SV_CYBORDER                27)
(define-integrable SV_CXDLGFRAME              28)
(define-integrable SV_CYDLGFRAME              29)
(define-integrable SV_CYTITLEBAR              30)
(define-integrable SV_CYVSLIDER               31)
(define-integrable SV_CXHSLIDER               32)
(define-integrable SV_CXMINMAXBUTTON          33)
(define-integrable SV_CYMINMAXBUTTON          34)
(define-integrable SV_CYMENU                  35)
(define-integrable SV_CXFULLSCREEN            36)
(define-integrable SV_CYFULLSCREEN            37)
(define-integrable SV_CXICON                  38)
(define-integrable SV_CYICON                  39)
(define-integrable SV_CXPOINTER               40)
(define-integrable SV_CYPOINTER               41)
(define-integrable SV_DEBUG                   42)
(define-integrable SV_CMOUSEBUTTONS           43)
(define-integrable SV_CPOINTERBUTTONS         43)
(define-integrable SV_POINTERLEVEL            44)
(define-integrable SV_CURSORLEVEL             45)
(define-integrable SV_TRACKRECTLEVEL          46)
(define-integrable SV_CTIMERS                 47)
(define-integrable SV_MOUSEPRESENT            48)
(define-integrable SV_CXBYTEALIGN             49)
(define-integrable SV_CXALIGN                 49)
(define-integrable SV_CYBYTEALIGN             50)
(define-integrable SV_CYALIGN                 50)
;;; 51-55
(define-integrable SV_NOTRESERVED             56)
(define-integrable SV_EXTRAKEYBEEP            57)
(define-integrable SV_SETLIGHTS               58)
(define-integrable SV_INSERTMODE              59)
;;; 60-63
(define-integrable SV_MENUROLLDOWNDELAY       64)
(define-integrable SV_MENUROLLUPDELAY         65)
(define-integrable SV_ALTMNEMONIC             66)
(define-integrable SV_TASKLISTMOUSEACCESS     67)
(define-integrable SV_CXICONTEXTWIDTH         68)
(define-integrable SV_CICONTEXTLINES          69)
(define-integrable SV_CHORDTIME               70)
(define-integrable SV_CXCHORD                 71)
(define-integrable SV_CYCHORD                 72)
(define-integrable SV_CXMOTIONSTART           73)
(define-integrable SV_CYMOTIONSTART           74)
(define-integrable SV_BEGINDRAG               75)
(define-integrable SV_ENDDRAG                 76)
(define-integrable SV_SINGLESELECT            77)
(define-integrable SV_OPEN                    78)
(define-integrable SV_CONTEXTMENU             79)
(define-integrable SV_CONTEXTHELP             80)
(define-integrable SV_TEXTEDIT                81)
(define-integrable SV_BEGINSELECT             82)
(define-integrable SV_ENDSELECT               83)
(define-integrable SV_BEGINDRAGKB             84)
(define-integrable SV_ENDDRAGKB               85)
(define-integrable SV_SELECTKB                86)
(define-integrable SV_OPENKB                  87)
(define-integrable SV_CONTEXTMENUKB           88)
(define-integrable SV_CONTEXTHELPKB           89)
(define-integrable SV_TEXTEDITKB              90)
(define-integrable SV_BEGINSELECTKB           91)
(define-integrable SV_ENDSELECTKB             92)
(define-integrable SV_ANIMATION               93)
(define-integrable SV_ANIMATIONSPEED          94)
(define-integrable SV_MONOICONS               95)
(define-integrable SV_KBDALTERED              96)
(define-integrable SV_PRINTSCREEN             97)
(define-integrable SV_LOCKSTARTINPUT          98)
;;; 99-104
(define-integrable SV_CSYSVALUES              105)