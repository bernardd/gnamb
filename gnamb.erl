-module(gnamb).

-define(SERVER, "10.1.1.10").

-compile(export_all).

start() ->
	crypto:start(),
	application:start(asn1),
	mc_map:init(),
	mc_client:start_link(?SERVER).
