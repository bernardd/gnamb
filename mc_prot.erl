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

encode_vi(0, <<>>) -> <<0>>;
encode_vi(0, Acc) -> Acc;
encode_vi(X, Acc) ->
	Leader = if
		X > 127 -> 1;
		true -> 0
	end,
	encode_vi(X bsr 7, <<Acc/bitstring, Leader:1, (X band 127):7>>).

decode_vi(<<0:1, N:7/bitstring, Rest/binary>>, Acc) ->
	Final = <<N/bitstring, Acc/bitstring>>,
	Size = bit_size(Final),
	<<X:Size/unsigned>> = Final,
	{X, Rest};
decode_vi(<<1:1, N:7/bitstring, Rest/binary>>, Acc) ->
	decode_vi(Rest, <<N/bitstring, Acc/bitstring>>).


% Well this was all very clever when I needed signed varints...which it turns out
% is never:
%
%encode_vi(0, <<>>) -> <<0>>;
%encode_vi(0, Acc) -> Acc;
%encode_vi(-1, <<>>) -> <<-1>>;
%encode_vi(-1, Acc) -> Acc;
%encode_vi(X, Acc) ->
%	Leader = if
%		X > 127 orelse X < -128 -> 1;
%		true -> 0
%	end,
%	encode_vi(X bsr 7, <<Acc/bitstring, Leader:1, (X band 127):7>>).
%
%decode_vi(<<0:1, N:7/bitstring, Rest/binary>>, Acc) ->
%	Final = <<N/bitstring, Acc/bitstring>>,
%	Size = bit_size(Final),
%	<<X:Size/signed>> = Final,
%	{X, Rest};
%decode_vi(<<1:1, N:7/bitstring, Rest/binary>>, Acc) ->
%	decode_vi(Rest, <<N/bitstring, Acc/bitstring>>).

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
	packet(<<16#0, ?VI(4), ?S("localhost"), 25565:16, ?VI(2)>>).

login_start(Name) ->
	packet(<<16#0, (s(Name))/binary>>).

encryption_response(SharedSecret, VerifyToken) ->
	packet(<<16#1, (byte_size(SharedSecret)):16, SharedSecret/binary,
				(byte_size(VerifyToken)):16, VerifyToken/binary>>).

keep_alive(ID) ->
	packet(<<16#0, ID:32>>).

%%%%% Minecraft packets - from server %%%%%

%%% Login state:

% Disconnect
decode_login(<<16#0, Data/binary>>) ->
	{Reason, <<>>} = s(Data),
	#disconnect{reason = Reason};

% Encryption Request
decode_login(<<16#1, Data/binary>>) ->
	{Server, Rest} = s(Data),
	<<KeyLength:16, PublicKey:KeyLength/binary, TokenLength:16, Token:TokenLength/binary>> = Rest,
	#encryption_request{
		server_id = Server,
		public_key = PublicKey,
		verify_token = Token
	};

% Login Success
decode_login(<<16#2, Data/binary>>) ->
	{UUID, Rest} = s(Data),
	{Username, <<>>} = s(Rest),
	#login_success{
		uuid = UUID,
		username = Username
	};

decode_login(<<X, _Data/binary>>) ->
	io:fwrite("Unhandled packet type for login state: 0x~.16B\n", [X]),
	unhandled.

% Keep Alive
decode(<<16#0, ID:32>>) ->
	#keep_alive{id=ID};

% Join Game
decode(<<16#1, EntityID:32, GameMode/unsigned, Dimension, Difficulty/unsigned,
			MaxPlayers/unsigned, Rest/binary>>) ->
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
decode(<<16#2, Data/binary>>) ->
	{Message, <<>>} = s(Data),
	#chat_message{
		message = Message
	};

% Time Update
decode(<<16#3, WorldAge:64, TimeOfDay:64>>) ->
	#time_update{
		world_age = WorldAge,
		time_of_day = TimeOfDay
	};

% Entity Equipment
decode(<<16#4, EntityID:32, Slot:16, _Rest/binary>>) ->
	#entity_equipment{
		entity_id = EntityID,
		slot = Slot,
		item = undefined % TODO
	};

% Player Position and Look
decode(<<16#8, X/float, Y/float, Z/float, Yaw:32/float, Pitch:32/float, OnGround>>) ->
	#player_position_and_look{
		x=X, y=Y, z=Z,
		yaw = Yaw,
		pitch = Pitch,
		on_ground = OnGround
	};

% Held Item Change
decode(<<16#9, Slot>>) ->
	#held_item_change{
		slot = Slot
	};

% Spawn Mob
decode(<<16#F, Data/binary>>) ->
	{EntityID, Rest} = vi(Data),
	<<Type, X:32, Y:32, Z:32, Pitch, HeadPitch, Yaw,
		VelocityX:16, VelocityY:16, VelocityZ:16, MetaData/binary>> = Rest,
	#spawn_mob{
		entity_id = EntityID,
		type = Type,
		x = X, y = Y, z = Z,
		pitch = Pitch,
		head_pitch = HeadPitch,
		yaw = Yaw,
		velocity_x = VelocityX,
		velocity_y = VelocityY,
		velocity_z = VelocityZ,
		metadata = MetaData % TODO
	};

% Entity Metadata
decode(<<16#1C, EntityID:32, MetaData/binary>>) ->
	#entity_metadata{
		entity_id = EntityID,
		metadata = MetaData % TODO
	};

% Entity Properties
decode(<<16#20, EntityID:32, _Count:32, Properties/binary>>) ->
	#entity_properties{
		entity_id = EntityID,
		properties = read_properties(Properties, [])
	};

% Map Chunk Bulk
decode(<<16#26, ChunkColumnCount:16, Length:32, LightSent, Data:Length/binary,
			ChunkX:32/signed, ChunkZ:32/signed, PrimaryBitmap:16, AddBitmap:16>>) ->
	#map_chunk_bulk{
		chunks = ChunkColumnCount,
		sky_light_sent = LightSent,
		data = Data,
		x = ChunkX,
		z = ChunkZ,
		primary_bitmap = PrimaryBitmap,
		add_bitmap = AddBitmap
	};

% Change Game State
decode(<<16#2B, Reason, Value:32/float>>) ->
	#change_game_state{
		reason = Reason,
		value = Value
	};

% Set Slot
decode(<<16#2F, WindowID, Slot:16, Data/binary>>) ->
	#set_slot{
		window_id = WindowID,
		slot = Slot,
		data = Data
	};

% Window Items
decode(<<16#30, WindowID, Count:16, Data/binary>>) ->
	#window_items{
		window_id = WindowID,
		count = Count,
		data = Data % TODO
	};

% Statistics
decode(<<16#37, Data/binary>>) ->
	{_Count, Rest1} = vi(Data),
	Stats = read_stats(Rest1, []),
	#statistics{statistics = Stats};

% Player List Item
decode(<<16#38, Data/binary>>) ->
	{Name, Rest} = s(Data),
	<<Online, Ping:16>> = Rest,
	#player_list_item{
		name = Name,
		online = Online,
		ping = Ping
	};

% Player Abilities
decode(<<16#39, Flags, FlyingSpeed:32/float, WalkingSpeed:32/float>>) ->
	#player_abilities{
		flags = Flags,
		flying_speed = FlyingSpeed,
		walking_speed = WalkingSpeed
	};

% Plugin Message
decode(<<16#3F, Rest/binary>>) ->
	{Channel, Rest1} = s(Rest),
	<<Length:16, Data:Length/binary>> = Rest1,
	#plugin_message{
		channel = Channel,
		data = Data
	};

decode(<<5, X:32, Y:32, Z:32>>) ->
	#spawn_pos{x=X, y=Y, z=Z};

decode(<<X, _Data/binary>>) ->
	io:fwrite("**** Unhandled packet type: 0x~.16B\n", [X]),
	unhandled.


% Helper functions
read_stats(<<>>, Acc) -> lists:reverse(Acc);
read_stats(<<Data/binary>>, Acc) ->
	{Name, Rest} = s(Data),
	{Value, Remainder} = vi(Rest),
	read_stats(Remainder, [{Name, Value} | Acc]).

read_properties(<<>>, Acc) -> lists:reverse(Acc);
read_properties(<<Data/binary>>, Acc) ->
	{Key, Rest1} = s(Data),
	<<Value/float, Length:16, Rest2/binary>> = Rest1,
	{Mods, Remainder} = read_modifiers(Length, Rest2, []),
	read_properties(Remainder, [#property{key = Key, value = Value, mods = Mods} | Acc]).

read_modifiers(0, Bin, Acc) -> {Acc, Bin};
read_modifiers(N, <<UUID:128, Amount/float, Operation, Rest/binary>>, Acc) ->
	read_modifiers(N-1, Rest, [#modifier{uuid = UUID, amount = Amount, operation = Operation} | Acc]).
