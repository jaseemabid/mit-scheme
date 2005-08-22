/* -*-C-*-

$Id: transact.c,v 1.7.2.1 2005/08/22 18:06:00 cph Exp $

Copyright 1990,1993,2000,2005 Massachusetts Institute of Technology

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

#include "config.h"
#include "outf.h"
#include "dstack.h"

static void
error (const char * procedure_name, const char * message)
{
  outf_fatal ("%s: %s\n", procedure_name, message);
  outf_flush_fatal ();
  abort ();
}

enum transaction_state { active, aborting, committing };

struct transaction
{
  void * checkpoint;
  enum transaction_state state;
};

static struct transaction * current_transaction;

static void
guarantee_current_transaction (const char * proc)
{
  if (current_transaction == 0)
    error (proc, "no transaction");
  switch (current_transaction -> state)
    {
    case committing: error (proc, "commit in progress"); break;
    case aborting: error (proc, "abort in progress"); break;
    case active: break;
    }
}

void
transaction_initialize (void)
{
  current_transaction = 0;
}

void
transaction_begin (void)
{
  void * checkpoint = dstack_position;
  struct transaction * transaction =
    (dstack_alloc (sizeof (struct transaction)));
  (transaction -> checkpoint) = checkpoint;
  (transaction -> state) = active;
  dstack_bind ((&current_transaction), transaction);
}

void
transaction_abort (void)
{
  guarantee_current_transaction ("transaction_abort");
  (current_transaction -> state) = aborting;
  dstack_set_position (current_transaction -> checkpoint);
}

void
transaction_commit (void)
{
  guarantee_current_transaction ("transaction_commit");
  (current_transaction -> state) = committing;
  dstack_set_position (current_transaction -> checkpoint);
}

struct action
{
  enum transaction_action_type type;
  void (*procedure) (void * environment);
  void * environment;
};

static void
execute_action (void * action)
{
  if ((((struct action *) action) -> type) !=
      (((current_transaction -> state) == committing)
       ? tat_abort : tat_commit))
    (* (((struct action *) action) -> procedure))
      (((struct action *) action) -> environment);
}

void
transaction_record_action (enum transaction_action_type type,
			   void (*procedure) (void * environment),
			   void * environment)
{
  guarantee_current_transaction ("transaction_record_action");
  {
    struct action * action = (dstack_alloc (sizeof (struct action)));
    (action -> type) = type;
    (action -> procedure) = procedure;
    (action -> environment) = environment;
    dstack_protect (execute_action, action);
  }
}
