/* -*-C-*-

$Id: ux.h,v 1.73 2000/01/18 05:09:49 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* Unix system include file */

#ifndef SCM_UX_H
#define SCM_UX_H

#define SYSTEM_NAME "unix"

#include "oscond.h"
#include "ansidecl.h"
#include "posixtyp.h"

#ifndef _POSIX			/* Prevent multiple inclusion */
# include <sys/times.h>
#endif /* _POSIX */
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#endif

extern int errno;

/* These seem to be missing from versions of unistd.h */

#if !(defined(_HPUX) || defined(sonyrisc) || defined(_SUNOS4) || defined(_IRIX6))
/* As specified by OSF/1 Programmer's reference: */
extern int EXFUN (ioctl, (int, unsigned long, ...));
#endif
#if !(defined(_SUNOS4) || defined(_AIX))
extern int EXFUN (open, (const char *, int, ...));
#endif
extern int EXFUN (kill, (pid_t, int));

#include "intext.h"
#include "dstack.h"
#include "osscheme.h"
#include "syscall.h"

/* Conditionalizations that are overridden by _POSIX. */

#ifdef _POSIX

#ifdef __osf__
#  include <sys/time.h>
#  include <sys/ioctl.h>
#  define NO_BAUD_CONVERSION
#  define SYSTEM_VARIANT "OSF"
#endif

#ifdef __386BSD__
#  include <sys/ioctl.h>
#  define EMULATE_FPATHCONF
#  define EMULATE_SYSCONF
#  define NO_BAUD_CONVERSION
#  define SYSTEM_VARIANT "386BSD"
#endif

#ifdef __FreeBSD__
#  include <sys/ioctl.h>
#  define EMULATE_FPATHCONF
#  define EMULATE_SYSCONF
#  define NO_BAUD_CONVERSION
#  define SYSTEM_VARIANT "FreeBSD"
#endif

#ifdef __bsdi__			/* works on bsdi 3.0 */
#  define SELECT_DECLARED
#  include <sys/ioctl.h>
#  define EMULATE_FPATHCONF
#  define EMULATE_SYSCONF
#  define NO_BAUD_CONVERSION
#  define SYSTEM_VARIANT "BSDI BSD/OS"
#endif

#ifdef _IRIX6
#define NO_BAUD_CONVERSION
#endif

/* no longer needed */
#if 0
#ifdef sonyrisc
/* <limits.h> will redefine these. */
#undef DBL_MAX
#undef DBL_MIN
#undef FLT_MAX
#undef FLT_MIN
#endif
#endif

#include <limits.h>
#include <unistd.h>
#include <time.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <dirent.h>
#include <utime.h>

#define DECL_GETLOGIN
#define HAVE_APPEND
#define HAVE_DIRENT
#define HAVE_DUP2
#define HAVE_FCNTL
#define HAVE_GETCWD
#define HAVE_MKDIR
/* MKTIME is really ANSI C, but POSIX has it too ? */
#define HAVE_MKTIME
#define HAVE_POSIX_SIGNALS
#define HAVE_RENAME
#define HAVE_RMDIR
#define HAVE_TERMIOS
#define HAVE_TIMES
#define HAVE_UTIME
#define HAVE_WAITPID
#define VOID_SIGNAL_HANDLERS

#define ERRNO_NONBLOCK EAGAIN
#define FCNTL_NONBLOCK O_NONBLOCK

#ifdef _IRIX

#define HAVE_DIR
#define HAVE_SELECT
#define HAVE_SIGCONTEXT
#define HAVE_SOCKETS
#define HAVE_SYMBOLIC_LINKS
#define HAVE_UNIX_SOCKETS

#endif /* _IRIX */

#ifdef _AIX
#define UNION_WAIT_STATUS
#define SYSTEM_VARIANT "AIX"
#endif /* _AIX */

#else /* not _POSIX */
#ifdef _BSD

#include <fcntl.h>
#include <sys/dir.h>
#include <sgtty.h>
#include <sys/time.h>
#include <sys/wait.h>

#define HAVE_APPEND
#define HAVE_BSD_SIGNALS
#define HAVE_BSD_TTY_DRIVER
#define HAVE_DIR
#define HAVE_DUP2
#define HAVE_FCNTL
#define HAVE_GETWD
#define HAVE_MKDIR
#define HAVE_RENAME
#define HAVE_RMDIR
#define HAVE_TIMES
#define HAVE_WAIT3
/* MORE/BSD has this -- do all 4.3 implementations? */
/* #define HAVE_WAIT4 */
#define UNION_WAIT_STATUS

#if defined(_SUNOS4) || defined(sun4) || defined(_NEXTOS)
#define VOID_SIGNAL_HANDLERS
#endif

#if defined(_SUNOS4) && defined(SIG_BLOCK)
#define HAVE_POSIX_SIGNALS
#endif

#define ERRNO_NONBLOCK EWOULDBLOCK
#define FCNTL_NONBLOCK FNDELAY

#else /* not _BSD */
#ifdef _SYSV

#include <time.h>
#include <termio.h>
#include <fcntl.h>

#define HAVE_APPEND
#define HAVE_FCNTL
#define HAVE_GETCWD
#define HAVE_TERMIO
#define HAVE_TIMES
#define HAVE_TIMEZONE

#define AMBIGUOUS_NONBLOCK
#define ERRNO_NONBLOCK EAGAIN
#define FCNTL_NONBLOCK O_NDELAY

#ifdef _SYSV3

#include <dirent.h>

#define HAVE_DIRENT
#define HAVE_DUP2
#define HAVE_MKDIR
#define HAVE_RMDIR
#define HAVE_SYSV3_SIGNALS
#define VOID_SIGNAL_HANDLERS

#else /* not _SYSV3 */
#ifdef _HPUX

#include <sys/wait.h>

#define HAVE_BSD_SIGNALS
#define HAVE_DUP2
#define HAVE_FTRUNCATE
#define HAVE_MKDIR
#define HAVE_RENAME
#define HAVE_RMDIR
#define HAVE_WAIT3

#if (_HPUX_VERSION < 65)

#include <ndir.h>
#define HAVE_DIR

#else /* (_HPUX_VERSION >= 65) */

#include <dirent.h>
#define HAVE_DIRENT
#define HAVE_POSIX_SIGNALS
#define HAVE_WAITPID
#define VOID_SIGNAL_HANDLERS
#define HAVE_STATFS

#endif /* _HPUX_VERSION */

#endif /* _HPUX */
#endif /* _SYSV3 */
#else /* not _SYSV */
#ifdef _PIXEL

#include <time.h>
#include <sgtty.h>

#define HAVE_BSD_TTY_DRIVER
#define HAVE_DUMB_OPEN
#define HAVE_DUP2
#define HAVE_TIMES

#endif /* _PIXEL */
#endif /* _SYSV */
#endif /* _BSD */
#endif /* _POSIX */

/* Conditionalizations that are independent of _POSIX. */

#ifdef _BSD

#define HAVE_BSD_JOB_CONTROL
#define HAVE_FIONREAD
#define HAVE_GETTIMEOFDAY
#define HAVE_ITIMER
#define HAVE_PTYS
#define FIRST_PTY_LETTER 'p'
#define HAVE_SELECT
#define HAVE_SIGCONTEXT
#define HAVE_SOCKETS
#define HAVE_SYMBOLIC_LINKS
#define HAVE_TRUNCATE
#define HAVE_UNIX_SOCKETS
#define HAVE_VFORK

#ifdef __linux
#define SYSTEM_VARIANT "GNU/Linux"
#include <sys/time.h>
#define HAVE_FTRUNCATE
#define HAVE_STATFS
#define HAVE_TIMEZONE
#endif

#ifdef _ULTRIX
#define SYSTEM_VARIANT "Ultrix"
#define HAVE_FTRUNCATE
/* For now, they don't work */
#undef HAVE_PTYS
#endif

#ifdef _NEXTOS
#define SYSTEM_VARIANT "NeXT"
#define HAVE_FTRUNCATE
#define TIOCSIGSEND TIOCSIG
#endif

#ifdef __osf__
#define HAVE_FTRUNCATE
#define TIOCSIGSEND TIOCSIG
#endif

#ifdef _SUNOS4
#define HAVE_FTRUNCATE
#ifdef sun4
#define TIOCSIGSEND TIOCSIGNAL
#endif
#endif

#ifdef apollo
#define SYSTEM_VARIANT "Domain"
#undef S_IFIFO
#endif

#ifdef _SUNOS

#define SYSTEM_VARIANT "SunOS"

#include <sys/vadvise.h>
#ifdef _SUNOS3
#define USE_HOSTENT_ADDR
#endif

#else /* not _SUNOS */

#ifdef _BSD4_2
#define USE_HOSTENT_ADDR
#endif

#endif /* _SUNOS */

#ifndef SYSTEM_VARIANT
#define SYSTEM_VARIANT "BSD"
#endif

#else /* not _BSD */
#ifdef _HPUX

#include <sys/ptyio.h>

#define SYSTEM_VARIANT "HP-UX"
#define HAVE_GETTIMEOFDAY
#define HAVE_ITIMER
#define HAVE_NICE
#define HAVE_PTYS
#define FIRST_PTY_LETTER 'p'
#define HAVE_SELECT
#define HAVE_SIGCONTEXT
#define HAVE_SOCKETS
#define HAVE_SYMBOLIC_LINKS
#define HAVE_TRUNCATE
#define HAVE_VFORK
 
#if (_HPUX_VERSION >= 90)
#define HAVE_POLL
#endif

#if (_HPUX_VERSION >= 65)
/* Is this right for 800-series machines? */
#define HAVE_UNIX_SOCKETS
#endif

#if (_HPUX_VERSION >= 65) || defined(hp9000s800)
#include <bsdtty.h>
#define HAVE_BSD_JOB_CONTROL
#endif

#if (_HPUX_VERSION >= 70) || defined(hp9000s800)
#define HAVE_FIONREAD
#endif

#if (_HPUX_VERSION <= 65)
#define USE_HOSTENT_ADDR
#endif

#else /* not _HPUX */
#ifdef _AIX

#define SYSTEM_VARIANT "AIX"
#define HAVE_SOCKETS
#define HAVE_VFORK

#else /* not _AIX */
#ifdef _SYSV4

#define SYSTEM_VARIANT "ATT (Vr4)"

#define HAVE_FIONREAD
#define HAVE_FTRUNCATE
#define HAVE_GETTIMEOFDAY
#define HAVE_ITIMER
#define HAVE_NICE
#define HAVE_PTYS
#define HAVE_SELECT
#define HAVE_SIGCONTEXT
#define HAVE_SOCKETS
#define HAVE_SYMBOLIC_LINKS
#define HAVE_TRUNCATE
#define HAVE_UNIX_SOCKETS
#define HAVE_VFORK

#include <stropts.h>

#undef PTY_ITERATION

#define PTY_MASTER_NAME_SPRINTF(master_name)				\
  sprintf ((master_name), "/dev/ptmx")

#ifdef sonyrisc

#define PTY_DECLARATIONS						\
  extern int EXFUN (grantpt, (int));					\
  extern int EXFUN (unlockpt, (int));					\
  extern char * EXFUN (ptsname, (int));					\
  extern void EXFUN (sony_block_sigchld, (void));			\
  extern void EXFUN (sony_unblock_sigchld, (void))

#define PTY_SLAVE_NAME_SPRINTF(slave_name, fd)				\
{									\
  sony_block_sigchld ();						\
  grantpt (fd);								\
  unlockpt (fd);							\
  sprintf ((slave_name), "%s", (ptsname (fd)));				\
  sony_unblock_sigchld ();						\
}

#else /* not sonyrisc */

#define PTY_DECLARATIONS						\
  extern int EXFUN (grantpt, (int));					\
  extern int EXFUN (unlockpt, (int));					\
  extern char * EXFUN (ptsname, (int))

#define PTY_SLAVE_NAME_SPRINTF(slave_name, fd)				\
{									\
  grantpt (fd);								\
  unlockpt (fd);							\
  sprintf ((slave_name), "%s", (ptsname (fd)));				\
}

#endif /* not sonyrisc */

/* Would be nice if HPUX and SYSV4 agreed on the name of this. */
#define TIOCSIGSEND TIOCSIGNAL

/* Must push various STREAMS modules onto the slave side of a PTY when
   it is opened. */

#define SLAVE_PTY_P(filename) ((strncmp ((filename), "/dev/pts/", 9)) == 0)

#define SETUP_SLAVE_PTY(fd)						\
  (((ioctl ((fd), I_PUSH, "ptem")) >= 0)				\
   && ((ioctl ((fd), I_PUSH, "ldterm")) >= 0)				\
   && ((ioctl ((fd), I_PUSH, "ttcompat")) >= 0))

#else /* not _SYSV4 */
#ifdef _SYSV3

#define SYSTEM_VARIANT "ATT (Vr3)"

#else /* not _SYSV3 */
#ifdef _SYSV

#define SYSTEM_VARIANT "ATT (V)"

#else /* not _SYSV */
#ifdef _PIXEL

#define SYSTEM_VARIANT "Pixel"

#define HAVE_FIONREAD
#define HAVE_NICE

#else /* not _PIXEL */

#define SYSTEM_VARIANT "unknown"

#endif /* not _PIXEL */
#endif /* not _SYSV */
#endif /* not _SYSV3 */
#endif /* not _SYSV4 */
#endif /* not _AIX */
#endif /* not _HPUX */
#endif /* not _BSD */

#ifdef VOID_SIGNAL_HANDLERS
typedef void Tsignal_handler_result;
#define SIGNAL_HANDLER_RETURN() return
#else
typedef int Tsignal_handler_result;
#define SIGNAL_HANDLER_RETURN() return (0)
#endif

typedef Tsignal_handler_result (*Tsignal_handler) ();

#ifndef SIG_ERR
#define SIG_ERR ((Tsignal_handler) (-1))
#endif

#if !defined(SIGCHLD) && defined(SIGCLD)
#define SIGCHLD SIGCLD
#endif
#if !defined(SIGABRT) && defined(SIGIOT)
#define SIGABRT SIGIOT
#endif

#ifndef HAVE_SIGCONTEXT
struct sigcontext { long sc_sp, sc_pc; };
#define HAVE_SIGCONTEXT
#endif

/* Crufty, but it will work here. */
#ifndef ENOSYS
#define ENOSYS 0
#endif

#ifndef HAVE_UTIME
/* It's really there, but there may not be an include file. */

struct utimbuf
{
  time_t actime;
  time_t modtime;
};

extern int EXFUN (utime, (CONST char *, struct utimbuf *)); 
#endif /* HAVE_UTIME */

#ifdef UNION_WAIT_STATUS

typedef union wait wait_status_t;

#ifndef WEXITSTATUS
#define WEXITSTATUS(_X) ((_X) . w_retcode)
#endif

#ifndef WTERMSIG
#define WTERMSIG(_X) ((_X) . w_termsig)
#endif

#ifndef WSTOPSIG
#define WSTOPSIG(_X) ((_X) . w_stopsig)
#endif

#else /* not UNION_WAIT_STATUS */

typedef int wait_status_t;

#ifndef WIFEXITED
#define WIFEXITED(_X) (((_X) & 0377) == 0)
#endif

#ifndef WIFSTOPPED
#define WIFSTOPPED(_X) (((_X) & 0377) == 0177)
#endif

#ifndef WIFSIGNALED
#define WIFSIGNALED(_X) ((((_X) & 0377) != 0) && (((_X) & 0377) != 0177))
#endif

#ifndef WEXITSTATUS
#define WEXITSTATUS(_X) (((_X) >> 8) & 0377)
#endif

#ifndef WTERMSIG
#define WTERMSIG(_X) ((_X) & 0177)
#endif

#ifndef WSTOPSIG
#define WSTOPSIG(_X) (((_X) >> 8) & 0377)
#endif

#endif /* UNION_WAIT_STATUS */

/* Provide null defaults for all the signals we're likely to use so we
   aren't continually testing to see if they're defined. */

#ifndef SIGLOST
#define SIGLOST 0
#endif
#ifndef SIGWINCH
#define SIGWINCH 0
#endif
#ifndef SIGURG
#define SIGURG 0
#endif
#ifndef SIGIO
#define SIGIO 0
#endif
#ifndef SIGUSR1
#define SIGUSR1 0
#endif
#ifndef SIGUSR2
#define SIGUSR2 0
#endif
#ifndef SIGVTALRM
#define SIGVTALRM 0
#endif
#ifndef SIGABRT
#define SIGABRT 0
#endif
#ifndef SIGPWR
#define SIGPWR 0
#endif
#ifndef SIGPROF
#define SIGPROF 0
#endif
#ifndef SIGSTOP
#define SIGSTOP 0
#endif
#ifndef SIGTSTP
#define SIGTSTP 0
#endif
#ifndef SIGCONT
#define SIGCONT 0
#endif
#ifndef SIGCHLD
#define SIGCHLD 0
#endif
#ifndef SIGTTIN
#define SIGTTIN 0
#endif
#ifndef SIGTTOU
#define SIGTTOU 0
#endif
#ifndef SIGBUS
#define SIGBUS 0
#endif
#ifndef SIGEMT
#define SIGEMT 0
#endif
#ifndef SIGSYS
#define SIGSYS 0
#endif

/* constants for access() */
#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#ifdef __STDC__
#define ALERT_CHAR '\a'
#define ALERT_STRING "\a"
#else
#define ALERT_CHAR '\007'
#define ALERT_STRING "\007"
#endif

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
#endif

/* constants for open() and fcntl() */
#ifndef O_RDONLY
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#endif

/* mode bit definitions for open(), creat(), and chmod() */
#ifndef S_IRWXU
#define S_IRWXU 0700
#define S_IRWXG 0070
#define S_IRWXO 0007
#endif

#ifndef S_IRUSR
#define S_IRUSR 0400
#define S_IWUSR 0200
#define S_IXUSR 0100
#define S_IRGRP 0040
#define S_IWGRP 0020
#define S_IXGRP 0010
#define S_IROTH 0004
#define S_IWOTH 0002
#define S_IXOTH 0001
#endif

#define MODE_REG (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#define MODE_DIR (MODE_REG | S_IXUSR | S_IXGRP | S_IXOTH)

/* constants for lseek() */
#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#ifndef DECL_GETLOGIN
extern char * EXFUN (getlogin, (void));
#endif

#define UX_abort abort
#define UX_access access
#define UX_alarm alarm
#define UX_chdir chdir
#define UX_chmod chmod
#define UX_close close
#define UX_ctime ctime
#define UX_dup dup
#define UX_free free
#define UX_fstat fstat
#define UX_fstatfs fstatfs
#define UX_getegid getegid
#define UX_getenv getenv
#define UX_geteuid geteuid
#define UX_getgid getgid
#define UX_getgrgid getgrgid
#define UX_gethostname gethostname
#define UX_getlogin getlogin
#define UX_getpid getpid
#define UX_getpwnam getpwnam
#define UX_getpwuid getpwuid
#define UX_getuid getuid
#define UX_gmtime gmtime
#define UX_ioctl ioctl
#define UX_link link
#define UX_localtime localtime
#define UX_lseek lseek
#define UX_malloc malloc
#define UX_mknod mknod
#define UX_mktime mktime
#define UX_pause pause
#define UX_pipe pipe
#define UX_read read
#define UX_realloc realloc
#define UX_signal signal
#define UX_sleep sleep
#define UX_stat stat
#define UX_statfs statfs
#define UX_system system
#define UX_time time
#define UX_unlink unlink
#define UX_utime utime
#define UX_write write
#define UX_wait wait

extern PTR EXFUN (malloc, (unsigned int size));
extern PTR EXFUN (realloc, (PTR ptr, unsigned int size));
extern char * EXFUN (getenv, (CONST char * name));

#ifndef __linux
/* <unistd.h> in linux libc 2.3.3 has
   extern int gethostname (char *__name, size_t __len); */
# ifndef _HPUX
/* <unistd.h> in HP-UX has mis-matching prototype 
   The following is as specified by OSF/1 Programmer's reference.
 */
extern int EXFUN (gethostname, (char * name, int size));
# endif /* _HPUX */
#endif /* linux */

#ifdef HAVE_FCNTL
#define UX_fcntl fcntl
#endif

#ifdef HAVE_TRUNCATE
#define UX_ftruncate ftruncate
#define UX_truncate truncate
#endif

#ifdef HAVE_VFORK
#define UX_vfork vfork
#else
#define UX_vfork fork
#endif

#ifdef HAVE_SYMBOLIC_LINKS
#define UX_lstat lstat
#define UX_readlink readlink
#define UX_symlink symlink
#else
#define UX_lstat stat
#endif

extern void EXFUN (UX_prim_check_errno, (enum syscall_names name));

#define STD_VOID_SYSTEM_CALL(name, expression)				\
{									\
  while ((expression) < 0)						\
    if (errno != EINTR)							\
      error_system_call (errno, (name));				\
}

#define STD_UINT_SYSTEM_CALL(name, result, expression)			\
{									\
  while (((result) = (expression)) < 0)					\
    if (errno != EINTR)							\
      error_system_call (errno, (name));				\
}

#define STD_PTR_SYSTEM_CALL(name, result, expression)			\
{									\
  while (((result) = (expression)) == 0)				\
    if (errno != EINTR)							\
      error_system_call (errno, (name));				\
}

#ifdef HAVE_TERMIOS

typedef struct
{
  struct termios tio;
#ifdef _HPUX
  struct ltchars ltc;
#endif
} Ttty_state;

#define UX_tcflush tcflush
#define UX_tcdrain tcdrain
#define UX_tcgetattr tcgetattr
#define UX_tcsetattr tcsetattr

#else /* not HAVE_TERMIOS */

extern int EXFUN (UX_tcdrain, (int fd));
extern int EXFUN (UX_tcflush, (int fd, int queue_selector));
/* These values chosen to match the ioctl TCFLSH argument for termio. */
#define TCIFLUSH 0
#define TCOFLUSH 1
#define TCIOFLUSH 2

#ifdef HAVE_TERMIO

typedef struct
{
  struct termio tio;
#ifdef HAVE_BSD_JOB_CONTROL
  struct ltchars ltc;
#endif
} Ttty_state;

#else /* not HAVE_TERMIO */
#ifdef HAVE_BSD_TTY_DRIVER

typedef struct
{
  struct sgttyb sg;
  struct tchars tc;
#ifdef HAVE_BSD_JOB_CONTROL
  struct ltchars ltc;
#endif
  int lmode;
} Ttty_state;

#endif /* HAVE_BSD_TTY_DRIVER */
#endif /* HAVE_TERMIO */
#endif /* HAVE_TERMIOS */

extern int EXFUN (UX_terminal_get_state, (int fd, Ttty_state * s));
extern int EXFUN (UX_terminal_set_state, (int fd, Ttty_state * s));

#ifdef _POSIX
#define UX_getpgrp getpgrp
#define UX_setsid setsid
#else
#ifdef _SYSV
#define UX_getpgrp getpgrp
#define UX_setsid setpgrp
#else /* not _SYSV */
extern pid_t EXFUN (UX_getpgrp, (void));
extern pid_t EXFUN (UX_setsid, (void));
#endif /* _SYSV */
#endif /* _POSIX */

#ifdef _POSIX

#define UX_setpgid setpgid
#define UX_tcgetpgrp tcgetpgrp
#define UX_tcsetpgrp tcsetpgrp

#else /* not _POSIX */

extern pid_t EXFUN (UX_tcgetpgrp, (int fd));
extern int EXFUN (UX_tcsetpgrp, (int fd, pid_t pgrp_id));

#ifdef HAVE_BSD_JOB_CONTROL

#ifdef _SYSV
#define UX_setpgid setpgrp2
#else
#define UX_setpgid setpgrp
#endif

#endif /* HAVE_BSD_JOB_CONTROL */
#endif /* _POSIX */

#ifdef HAVE_GETTIMEOFDAY
#define UX_gettimeofday gettimeofday
#endif
#ifdef HAVE_ITIMER
#define UX_setitimer setitimer
#endif
#ifdef HAVE_RMDIR
#define UX_rmdir rmdir
#endif
#ifdef HAVE_TIMES
#define UX_times times
#endif
#ifdef HAVE_SOCKETS
#define UX_connect connect
#define UX_gethostbyname gethostbyname
#define UX_getservbyname getservbyname
#define UX_socket socket
#define UX_bind bind
#define UX_listen listen
#define UX_accept accept
#endif

#ifdef HAVE_DUMB_OPEN
extern int EXFUN (UX_open, (CONST char * name, int oflag, mode_t mode));
#else
#define UX_open open
#endif

#ifdef HAVE_DUP2
#define UX_dup2 dup2
#else
#ifdef HAVE_FCNTL
#define EMULATE_DUP2
#define HAVE_DUP2
extern int EXFUN (UX_dup2, (int fd, int fd2));
#endif
#endif

#ifdef HAVE_GETCWD
#define UX_getcwd getcwd
#else
#define EMULATE_GETCWD
#define HAVE_GETCWD
extern char * EXFUN (UX_getcwd, (char * buffer, size_t length));
#endif

#ifdef HAVE_MKDIR
#define UX_mkdir mkdir
#else
#define EMULATE_MKDIR
#define HAVE_MKDIR
extern int EXFUN (UX_mkdir, (CONST char * name, mode_t mode));
#endif

#ifdef HAVE_RENAME
#define UX_rename rename
#else
#define EMULATE_RENAME
#define HAVE_RENAME
extern int EXFUN (UX_rename, (CONST char * from_name, CONST char * to_name));
#endif

#ifdef HAVE_WAITPID
#define UX_waitpid waitpid
#else /* not HAVE_WAITPID */
#ifdef HAVE_WAIT3
#define EMULATE_WAITPID
#define HAVE_WAITPID
extern int EXFUN
  (UX_waitpid, (pid_t pid, wait_status_t * stat_loc, int options));
#endif /* HAVE_WAIT3 */
#endif /* HAVE_WAITPID */

#ifndef WUNTRACED
#define WUNTRACED 0
#endif

#ifdef HAVE_SELECT
#define UX_select select
#endif /* HAVE_SELECT */

#ifdef _BSD
#define BSD_DEV_TTY "/dev/tty"
#endif

#if !defined(_POSIX) && defined(_BSD) && !defined(_SUNOS)
#define L_ctermid ((strlen (BSD_DEV_TTY)) + 1);
extern char * EXFUN (UX_ctermid, (char * s));
extern int EXFUN (UX_kill, (pid_t pid, int sig));
#else
#define UX_ctermid ctermid
#define UX_kill kill
#endif

#ifdef HAVE_POSIX_SIGNALS

#define UX_sigemptyset sigemptyset
#define UX_sigfillset sigfillset
#define UX_sigaddset sigaddset
#define UX_sigdelset sigdelset
#define UX_sigismember sigismember
#define UX_sigaction sigaction
#define UX_sigsuspend sigsuspend
#define UX_sigprocmask sigprocmask

#else /* not HAVE_POSIX_SIGNALS */

typedef long sigset_t;
extern int EXFUN (UX_sigemptyset, (sigset_t * set));
extern int EXFUN (UX_sigfillset, (sigset_t * set));
extern int EXFUN (UX_sigaddset, (sigset_t * set, int signo));
extern int EXFUN (UX_sigdelset, (sigset_t * set, int signo));
extern int EXFUN (UX_sigismember, (CONST sigset_t * set, int signo));

#ifdef HAVE_BSD_SIGNALS

struct sigaction
{
  Tsignal_handler sa_handler;
  sigset_t sa_mask;
  int sa_flags;
};

extern int EXFUN
  (UX_sigaction,
   (int signo, CONST struct sigaction * act, struct sigaction * oact));
extern int EXFUN
  (UX_sigprocmask, (int how, CONST sigset_t * set, sigset_t * oset));
extern int EXFUN (UX_sigsuspend, (CONST sigset_t * set));
#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2

#define HAVE_POSIX_SIGNALS

#else /* not HAVE_BSD_SIGNALS */
#ifdef HAVE_SYSV3_SIGNALS

#define UX_sigset sigset
#define UX_sighold sighold
#define UX_sigrelse sigrelse

#endif /* HAVE_SYSV3_SIGNALS */
#endif /* HAVE_BSD_SIGNALS */
#endif /* HAVE_POSIX_SIGNALS */

#ifdef _POSIX

#define HAVE_SIGSET_OPS

#ifdef EMULATE_FPATHCONF

/* These values match HP-UX, and the index in the table in the 
   OSF/1 Programmer's reference.
 */

extern long EXFUN (fpathconf, (int, int));

#ifndef _PC_VDISABLE
# define _PC_VDISABLE		8
#endif

#endif /* EMULATE_FPATHCONF */

#ifdef EMULATE_SYSCONF

extern long EXFUN (sysconf, (int));

/* These values match HP-UX, and the index in the table in the 
   OSF/1 Programmer's reference.

   Note: The code assumes that if one is present, the rest
   are too.  Otherwise there is no simple way to guarantee
   that non-conflicting values have been chosen.
 */

#ifndef _SC_CHILD_MAX
# define _SC_CHILD_MAX		1
# define _SC_CLK_TCK		2
# define _SC_OPEN_MAX		4
# define _SC_JOB_CONTROL	5
#endif

#endif /* EMULATE_SYSCONF */

extern cc_t EXFUN (UX_PC_VDISABLE, (int fildes));
extern clock_t EXFUN (UX_SC_CLK_TCK, (void));
#define UX_SC_OPEN_MAX() ((size_t) (sysconf (_SC_OPEN_MAX)))
#define UX_SC_CHILD_MAX() ((size_t) (sysconf (_SC_CHILD_MAX)))

#ifdef _POSIX_JOB_CONTROL
#define UX_SC_JOB_CONTROL() 1
#else
#define UX_SC_JOB_CONTROL() ((sysconf (_SC_JOB_CONTROL)) >= 0)
#endif

#else /* not _POSIX */

#ifdef _SUNOS4
#define HAVE_SIGSET_OPS
#endif

#define UX_PC_VDISABLE(fildes) '\377'

#ifdef OPEN_MAX
#define UX_SC_OPEN_MAX() OPEN_MAX
#else
#ifdef _NFILE
#define UX_SC_OPEN_MAX() _NFILE
#else
#define UX_SC_OPEN_MAX() 16
#endif
#endif

#ifdef CHILD_MAX
#define UX_SC_CHILD_MAX() CHILD_MAX
#else
#define UX_SC_CHILD_MAX() 6
#endif

#ifdef CLK_TCK
#define UX_SC_CLK_TCK() CLK_TCK
#else
#ifdef HZ
#define UX_SC_CLK_TCK() HZ
#else
#define UX_SC_CLK_TCK() 60
#endif
#endif

#ifdef HAVE_BSD_JOB_CONTROL
#define UX_SC_JOB_CONTROL() 1
#else
#define UX_SC_JOB_CONTROL() 0
#endif

#endif /* _POSIX */

#endif /* SCM_UX_H */
