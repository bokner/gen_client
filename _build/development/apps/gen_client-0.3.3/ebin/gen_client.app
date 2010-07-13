%% This is the application resource file (.app file) for the gen_client,
%% application.
{application, gen_client, 
  [{description, "Generic XMPP client library"},
   {vsn, "0.3.3"},
   {modules, [gen_client_app,
              gen_client_sup,
              adhoc_handler,
              adhoc_processor,
              disco_handler,
              gen_client,
              gen_command,
              stanza,
              gen_client_utils,
              dummy_client,
              test_gen_client,
              tidy_bot
              ]},
   {registered,[gen_client_sup]},
   {applications, [kernel, stdlib]},
   {mod, {gen_client_app,[]}},
   {start_phases, []}]}.

