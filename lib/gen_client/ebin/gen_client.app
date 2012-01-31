%% This is the application resource file (.app file) for the gen_client
%% application.
{application, gen_client, 
  [{description, "Generic XMPP client library"},
   {vsn, "1.0.0"},
   {modules, [gen_client_app,
              gen_client_sup,
              adhoc_plugin,
              adhoc_processor,
              disco_plugin,
              disco_handler_impl,
              gen_client,
              gen_client_event_server,
              gen_client_plugin,
              gen_command,
              stanza,
              gen_client_utils,
              macaddr,
			  test_adhoc,
			  test_disco,	
              test_gen_client,
              tidy_bot
              ]},
   {registered,[gen_client_sup]},
   {applications, [kernel, stdlib, exmpp]},
   {mod, {gen_client_app,[]}},
   {start_phases, []}]}.

