/* -*-C-*-

$Id: uxsock.h,v 1.12.2.1 2005/08/22 18:06:01 cph Exp $

Copyright 1990,1992,1993,1997,1998,1999 Massachusetts Institute of Technology
Copyright 2001,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

*/

#ifndef SCM_UXSOCK_H
#define SCM_UXSOCK_H

#include "osio.h"

extern Tchannel OS_open_tcp_stream_socket (void *, unsigned int);
extern int OS_get_service_by_name (const char *, const char *);
extern unsigned long OS_get_service_by_number (const unsigned long);
extern unsigned int OS_host_address_length (void);
extern char ** OS_get_host_by_name (const char *);
extern const char * OS_get_host_name (void);
extern const char * OS_canonical_host_name (const char *);
extern const char * OS_get_host_by_address (const char *);
extern void OS_host_address_any (void *);
extern void OS_host_address_loopback (void *);

#ifdef HAVE_UNIX_SOCKETS
   extern Tchannel OS_open_unix_stream_socket (const char *);
#endif

extern Tchannel OS_create_tcp_server_socket (void);
extern void OS_bind_tcp_server_socket (Tchannel, void *, unsigned int);
extern void OS_listen_tcp_server_socket (Tchannel);
extern Tchannel OS_server_connection_accept (Tchannel, void *, unsigned int *);

#endif /* SCM_UXSOCK_H */
