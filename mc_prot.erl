-module(mc_prot).

-include("mc_prot.hrl").

-compile(export_all).

-define(VI(X), (vi(X))/binary).
-define(S(X), (s(X))/binary).

packet(B) -> <<(vi(byte_size(B)))/binary, B/binary>>.

get_packet(B) ->
	{Size, Rest} = vi(B),
	io:fwrite("Size: ~p, byte_size(Rest) = ~p\n", [Size, byte_size(Rest)]),
	if
		byte_size(Rest) >= Size ->
			<<Packet:Size/binary, Remainder/binary>> = Rest,
			{Packet, Remainder};
		true ->
			incomplete
	end.

%%%%% Varint handling %%%%%
vi(X) when is_integer(X) ->
	encode_vi(X, <<>>);
vi(X) when is_binary(X) ->
	decode_vi(X, <<>>).

encode_vi(0, <<>>) -> <<0:8>>;
encode_vi(0, Acc) -> Acc;
encode_vi(-1, <<>>) -> <<-1:8>>;
encode_vi(-1, Acc) -> Acc;
encode_vi(X, Acc) ->
	Leader = if
		X > 127 orelse X < -128 -> 1;
		true -> 0
	end,
	encode_vi(X bsr 7, <<Acc/bitstring, Leader:1, (X band 127):7>>).

decode_vi(<<0:1, N:7/bitstring, Rest/binary>>, Acc) ->
	Final = <<N/bitstring, Acc/bitstring>>,
	Size = bit_size(Final),
	<<X:Size/signed>> = Final,
	{X, Rest};
decode_vi(<<1:1, N:7/bitstring, Rest/binary>>, Acc) ->
	decode_vi(Rest, <<N/bitstring, Acc/bitstring>>).

%%%%% String handling %%%%%
s(S) when is_list(S) ->
	L = vi(length(S)),
	<<L/bitstring, (list_to_binary(S))/binary>>;

s(B) when is_binary(B) ->
	{Length, Rest} = vi(B),
	<<S:Length/binary, Rest2/binary>> = Rest,
	{binary_to_list(S), Rest2}.

%%%%% Minecraft packets - to server %%%%%
handshake() ->
	packet(<<0:8, ?VI(4), ?S("localhost"), 25565:16, ?VI(2)>>).

login_start(Name) ->
	packet(<<0:8, (s(Name))/binary>>).

encryption_response(SharedSecret, VerifyToken) ->
	packet(<<1:8, (byte_size(SharedSecret)):16, SharedSecret/binary,
				(byte_size(VerifyToken)):16, VerifyToken/binary>>).

keep_alive(ID) ->
	packet(<<0:8, ID:32>>).

%%%%% Minecraft packets - from server %%%%%

%%% Login state:

% Disconnect
decode_login(<<0:8, Data/binary>>) ->
	{Reason, <<>>} = s(Data),
	#disconnect{reason = Reason};

% Encryption Request
decode_login(<<1:8, Data/binary>>) ->
	{Server, Rest} = s(Data),
	<<KeyLength:16, PublicKey:KeyLength/binary, TokenLength:16, Token:TokenLength/binary>> = Rest,
	#encryption_request{
		server_id = Server,
		public_key = PublicKey,
		verify_token = Token
	};

% Login Success
decode_login(<<2:8, Data/binary>>) ->
	{UUID, Rest} = s(Data),
	{Username, <<>>} = s(Rest),
	#login_success{
		uuid = UUID,
		username = Username
	};

decode_login(<<X:8, _Data/binary>>) ->
	io:fwrite("Unhandled packet type for login state: 0x~.16B\n", [X]),
	unhandled.

% Keep Alive
decode(<<0:8, ID:32>>) ->
	#keep_alive{id=ID};

% Join Game
decode(<<1:8, EntityID:32, GameMode:8/unsigned, Dimension:8, Difficulty:8/unsigned,
			MaxPlayers:8/unsigned, Rest/binary>>) ->
	{LevelType, <<>>} = s(Rest),
	#join_game{
		entity_id = EntityID,
		game_mode = GameMode,
		dimension = Dimension,
		difficult = Difficulty,
		max_players = MaxPlayers,
		level_type = LevelType
	};

% Chat Message
decode(<<2:8, Data/binary>>) ->
	{Message, <<>>} = s(Data),
	#chat_message{
		message = Message
	};

% Time Update
decode(<<3:8, WorldAge:64, TimeOfDay:64>>) ->
	#time_update{
		world_age = WorldAge,
		time_of_day = TimeOfDay
	};

% Entity Equipment
decode(<<4:8, EntityID:32, Slot:16, _Rest/binary>>) ->
	#entity_equipment{
		entity_id = EntityID,
		slot = Slot,
		item = undefined % TODO
	};

decode(<<5:8, X:32, Y:32, Z:32>>) ->
	#spawn_pos{x=X, y=Y, z=Z};

decode(<<X:8, _Data/binary>>) ->
	io:fwrite("Unhandled packet type: 0x~.16B\n", [X]),
	unhandled.
