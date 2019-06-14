/* <file:emacsy.c>=                                                         */
/* \subsection{Legal Stuff}                                                 */
/*                                                                          */
/* <+ Copyright>=                                                           */
/*
  Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
*/
/* <+ License>=                                                             */
/*
  Emacsy is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Emacsy is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Emacsy.  If not, see <http://www.gnu.org/licenses/>.
*/
#include "emacsy.h"
#include <libguile.h>

/* The function [[scm_c_use_module]] throws an exception if it cannot       */
/* find the module, so we have to split that functionality into a body      */
/* function [[load_module_try]] and an error handler [[load_module_error]]. */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Utility Functions>=                                        */
SCM load_module_try(void *data)
{
  scm_c_use_module((const char *)data);
  return scm_list_1(SCM_BOOL_T);
}
/* <emacsy-c-api:Utility Functions>=                                        */
SCM load_module_error(void *data, SCM key, SCM args)
{
  //fprintf(stderr, "error: Unable to load module (%s).\n", (const char*) data);
  return scm_list_3(SCM_BOOL_F, key, args);
}
/* Attempt to load a module.  Returns 0 if no errors, and non-zero otherwise. */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Utility Functions>=                                        */
int emacsy_load_module(const char *module)
{
  SCM result = scm_internal_catch(SCM_BOOL_T,
                                  load_module_try,   (void *) module,
                                  load_module_error, (void *) module);
  if (scm_is_false(scm_car(result))) {
    fprintf(stderr, "error: Unable to load module (%s); got error to key %s with args %s. Try setting the "
                    "GUILE_LOAD_PATH environment variable.\n", module,
                    scm_to_locale_string(scm_car(scm_cdr(result))),
                    scm_to_locale_string(scm_car(scm_cdr(scm_cdr(result))))
                    );
    return 1; //EMACSY_ERR_NO_MODULE;
  }
  return 0;
}

/* <emacsy-c-api:Utility Functions>=                                        */
SCM scm_c_string_to_symbol(const char* str) {
  return scm_string_to_symbol(scm_from_locale_string(str));
}

SCM modifier_key_flags_to_list(int modifier_key_flags)
{
  const char* modifiers[] = { "alt", "control", "hyper", "meta", "super", "shift" };
  SCM list = SCM_EOL;
  for (int i = 0; i < EMACSY_MODKEY_COUNT; i++) {
    if (modifier_key_flags & (1 << i)) {
      list = scm_cons(scm_c_string_to_symbol(modifiers[i]), list);
    }
  }

  return list;
}

SCM_DEFINE(scm_modifier_key_flags_to_list, "modifier-key-flags->list",
           1, 0, 0,
           (SCM flags),
           "Convert an integer of modifier key flags to a list of symbols.")
{
  int modifier_key_flags = scm_to_int(flags);
  return modifier_key_flags_to_list(modifier_key_flags);
}

/* The implementation of the API calls similarly named Scheme procedures.   */
/*                                                                          */
/* \section{emacsy\_initialize}                                             */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Functions>=                                                */
int emacsy_initialize(int init_flags)
{
  /* Load the (emacsy emacsy) module. */
  const char *module = "emacsy emacsy";
  int err = emacsy_load_module(module);
  if (err)
    return err;

  (void) scm_call_1(scm_c_public_ref("emacsy emacsy", "emacsy-initialize"),
                    (init_flags & EMACSY_INTERACTIVE) ? SCM_BOOL_T : SCM_BOOL_F);

  return err;
}
/* \section{emacsy\_key\_event}                                             */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Functions>=                                                */
void emacsy_key_event(int char_code,
                      int modifier_key_flags)
{
  SCM i = scm_from_int(char_code);
  //fprintf(stderr, "i = %d\n", scm_to_int(i));
  SCM c = scm_integer_to_char(i);
  //fprintf(stderr, "c = %d\n", scm_to_int(scm_char_to_integer(c)));

  (void) scm_call_2(scm_c_public_ref("emacsy emacsy", "emacsy-key-event"),
                    c,
                    modifier_key_flags_to_list(modifier_key_flags));
}
/* \section{emacsy\_mouse\_event}                                           */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Functions>=                                                */
void emacsy_mouse_event(int x, int y,
                        int state,
                        int button,
                        int modifier_key_flags)
{

  SCM down_sym   = scm_c_string_to_symbol("down");
  SCM up_sym     = scm_c_string_to_symbol("up");
  SCM motion_sym = scm_c_string_to_symbol("motion");
  SCM state_sym;
  switch(state) {
  case EMACSY_MOUSE_BUTTON_UP:   state_sym = up_sym;     break;
  case EMACSY_MOUSE_BUTTON_DOWN: state_sym = down_sym;   break;
  case EMACSY_MOUSE_MOTION:      state_sym = motion_sym; break;
  default:
    fprintf(stderr, "warning: mouse event state received invalid input %d.\n",
            state);
    return;
  }

  (void) scm_call_3(scm_c_public_ref("emacsy emacsy", "emacsy-mouse-event"),
                    scm_vector(scm_list_2(scm_from_int(x),
                                          scm_from_int(y))),
                    scm_from_int(button),
                    state_sym);
}
/* \section{emacsy\_tick}                                                   */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Functions>=                                                */
int emacsy_tick()
{
  int flags = 0;
  (void) scm_call_0(scm_c_public_ref("emacsy emacsy",
                                     "emacsy-tick"));
  if (scm_is_true(scm_c_public_ref("emacsy emacsy",
                                   "emacsy-quit-application?")))
    flags |= EMACSY_QUIT_APPLICATION_P;
  if (scm_is_true(scm_c_public_ref("emacsy emacsy",
                                   "emacsy-ran-undefined-command?")))
    flags |= EMACSY_RAN_UNDEFINED_COMMAND_P;

  return flags;
}

/* \section{emacsy\_message\_or\_echo\_area}                                */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Functions>=                                                */
char *emacsy_message_or_echo_area()
{
  return scm_to_locale_string(
    scm_call_0(scm_c_public_ref("emacsy emacsy",
                                "emacsy-message-or-echo-area")));
}
/* \section{emacsy\_current\_buffer}                                        */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Functions>=                                                */
char *emacsy_current_buffer()
{
  return scm_to_locale_string(
    scm_call_1(scm_c_public_ref("emacsy emacsy", "buffer-name"),
      scm_call_0(scm_c_public_ref("emacsy emacsy",
                                  "current-buffer"))));
}
/* \section{emacsy\_mode\_line}                                             */
/*                                                                          */
/* \todo[inline]{Keep name as modeline.}                                    */
/*                                                                          */
/* <emacsy-c-api:Functions>=                                                */
char *emacsy_mode_line()
{
  return scm_to_locale_string(
    scm_call_0(scm_c_public_ref("emacsy emacsy",
                                "emacsy-mode-line")));
}
/* \section{emacsy\_terminate}                                              */
/*                                                                          */
/*                                                                          */
/* <emacsy-c-api:Functions>=                                                */
int emacsy_terminate()
{
  SCM result;
  result = scm_call_0(scm_c_public_ref("emacsy emacsy",
                                       "emacsy-terminate"));
  return 0;
}
/* <emacsy-c-api:Functions>=                                                */
int  emacsy_run_hook_0(const char *hook_name)
{
  /* This should be protected from all sorts of errors that the hooks
     could throw. */
  SCM result;
  result = scm_call_1(scm_c_public_ref("emacsy emacsy",
                                       "emacsy-run-hook"),
                      scm_c_private_ref("guile-user",
                                        hook_name));
  return 0;
}
/* <emacsy-c-api:Functions>=                                                */
int  emacsy_minibuffer_point()
{
  return scm_to_int(
    scm_call_0(scm_c_public_ref("emacsy emacsy",
                                "emacsy-minibuffer-point")));
}
