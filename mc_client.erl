-module(mc_client).
-include("mc_prot.hrl").

-export([start_link/1]).

-define(MC_PORT, 25565).

-record(state, {
		socket,
		server_key :: binary()

	}).

start_link(Server) ->
	crypto:start(),
	{ok, Socket} = gen_tcp:connect(Server, ?MC_PORT, [binary, {active, false}]),
	gen_tcp:send(Socket, mc_prot:handshake()),
	gen_tcp:send(Socket, mc_prot:login_start("mc_client")),
	loop(#state{socket = Socket}, <<>>).

loop(State = #state{socket = Socket}, DataSoFar) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
			io:fwrite("Got data: ~p\n", [Data]),
			FullData = <<DataSoFar/binary, Data/binary>>,
			case mc_prot:get_packet(FullData) of
				incomplete ->
					loop(State, FullData);
				{Packet, Rest} ->
					NewState = handle_packet(Packet, State)
					loop(State, Rest)
			end;
		{tcp_closed, Socket} ->
			io:fwrite("Socket closed.\n", []);
		{tcp_error, Socket, Reason} ->
			io:fwrite("Socket error: ~p\n", [Reason])
	end.

handle_packet(Packet, State = #state{socket = Socket}) ->
	R = mc_prot:decode(Packet),
	io:fwrite("Decoded packet: ~p\n", [R]),
	case R of
		#encryption_request{server_id = Server, public_key = ServerKey, verify_token = Token} ->
			SharedSecret = crypto:rand_bytes(16),
			
			gen_tcp:send(Socket, mc_prot:encryption_response(
			State#state{server_key = ServerKey};
		_ ->
			io:fwrite("Unhandled message\n"),
			State
	end.
