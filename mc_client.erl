-module(mc_client).
-include("mc_prot.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([start_link/1]).

-define(MC_PORT, 25565).
-define(CYPHER, aes_cfb8).

-record(state, {
		state = login :: login | connected,
		socket,
		key :: binary(),
		server_id,
		uuid,
		pos
	}).

start_link(Server) ->
	put(key, undefined),
	{ok, Socket} = gen_tcp:connect(Server, ?MC_PORT, [binary, {active, false}]),
	put(socket, Socket),
	send(mc_prot:handshake()),
	send(mc_prot:login_start("emc"++pid_to_list(self()))),
	loop(#state{socket = Socket}, <<>>).

loop(State = #state{socket = Socket}, DataSoFar) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
			DecryptedData = case get(key) of
				undefined ->
					Data;
				Key ->
					crypto:block_decrypt(?CYPHER, Key, Key, Data)
			end,
%			io:fwrite("Got data (unencrypted): ~p\n", [DecryptedData]),
			FullData = <<DataSoFar/binary, DecryptedData/binary>>,
%			io:fwrite("Got data (encrypted): ~p\n", [RawData]),
			handle_data(State, FullData);
		{tcp_closed, Socket} ->
			io:fwrite("Socket closed.\n", []);
		{tcp_error, Socket, Reason} ->
			io:fwrite("Socket error: ~p\n", [Reason])
	end.

handle_data(State, <<>>) -> loop(State, <<>>);
handle_data(State, Data) ->
	case mc_prot:get_packet(Data) of
		incomplete ->
			loop(State, Data);
		{Packet, Remainder} ->
			NewState = handle_packet(Packet, State),
			handle_data(NewState, Remainder)
	end.

handle_packet(Packet, State) ->
	R = case State#state.state of
		login -> mc_prot:decode_login(Packet);
		_ -> mc_prot:decode(Packet)
	end,
	io:fwrite("Decoded packet: ~p\n", [R]),
	case R of
		#encryption_request{server_id = Server, public_key = ServerKey, verify_token = Token} ->
			SharedSecret = crypto:rand_bytes(16),
			PEMKey = <<"-----BEGIN PUBLIC KEY-----\n",
				(base64:encode(ServerKey))/binary,
				"\n-----END PUBLIC KEY-----">>,
			[PEM] = public_key:pem_decode(PEMKey),
			RSAKey = public_key:pem_entry_decode(PEM),
			EncSecret = public_key:encrypt_public(SharedSecret, RSAKey),
			EncToken = public_key:encrypt_public(Token, RSAKey),
			send(mc_prot:encryption_response(EncSecret, EncToken)),
			put(key, SharedSecret),
			State#state{key = SharedSecret, server_id = Server};
		#login_success{uuid = UUID} ->
			% Hooray!
			io:fwrite("Successful login!\n", []),
			State#state{uuid = UUID, state = running};
		#disconnect{reason = Reason} ->
			io:fwrite("Disconnected: ~s\n", [Reason]),
			exit(normal);
		#keep_alive{id = ID} ->
			send(mc_prot:keep_alive(ID)),
			State;
		#join_game{} ->
			% TODO
			State;
		#chat_message{} ->
			% Nothing
			State;
		#time_update{} ->
			% Nothing
			State;
		#entity_equipment{} ->
			% Nothing
			State;
		#player_abilities{} ->
			% TODO
			State;
		#plugin_message{} ->
			% Nothing?
			State;
		#spawn_pos{x=X, y=Y, z=Z} ->
			State#state{pos = {X, Y, Z}};
		_ ->
			io:fwrite("Unhandled message\n"),
			State
	end.

send(Packet) ->
	Encrypted = case get(key) of
		undefined -> Packet;
		Key -> crypto:block_encrypt(?CYPHER, Key, Key, Packet)
	end,
	gen_tcp:send(get(socket), Encrypted).
