%% Login state

%0
-record(
	disconnect, {
		reason
	}).

%1
-record(
	encryption_request, {
		server_id,
		public_key,
		verify_token
	}).

%2
-record(
	login_success, {
		uuid,
		username
	}).

%% Running state

%0
-record(
	keep_alive, {
		id
	}).

%1
-record(
	join_game, {
		entity_id,
		game_mode,
		dimension,
		difficult,
		max_players,
		level_type
	}).

%2
-record(
	chat_message, {
		message
	}).

%3
-record(
	time_update, {
		world_age,
		time_of_day
	}).

%4
-record(
	entity_equipment, {
		entity_id,
		slot,
		item
	}).

%5
-record(
	spawn_pos, {
		x, y, z
	}).

%8
-record(
	player_position_and_look, {
		x, y, z,
		yaw, pitch, on_ground
	}).

%9
-record(
	held_item_change, {
		slot
	}).

%F
-record(
	spawn_mob, {
		entity_id,
		type,
		x, y, z,
		pitch,
		head_pitch,
		yaw,
		velocity_x,
		velocity_y,
		velocity_z,
		metadata
	}).

%17
-record(
	entity_look_and_relative_move, {
		entity_id,
		dx, dy, dz,
		yaw,
		pitch
	}).

%19
-record(
	entity_head_look, {
		entity_id,
		yaw
	}).

%1C
-record(
	entity_metadata, {
		entity_id,
		metadata
	}).

%20
-record(
	entity_properties, {
		entity_id,
		properties
	}).
-record(
	property, {
		key,
		value,
		mods
	}).
-record(
	modifier, {
		uuid,
		amount,
		operation
	}).

%26
-record(
	map_chunk_bulk, {
		chunks,
		sky_light_sent,
		data,
		chunk_metadata
	}).
-record(
	chunk_metadata, {
		x, z,
		primary_bitmap,
		add_bitmap
	}).

%2B
-record(
	change_game_state, {
		reason,
		value
	}).

%2F
-record(
	set_slot, {
		window_id,
		slot,
		data
	}).

%30
-record(
	window_items, {
		window_id,
		count,
		data
	}).

%37
-record(
	statistics, {
		statistics
	}).

%38
-record(
	player_list_item, {
		name,
		online,
		ping
	}).

%39
-record(
	player_abilities, {
		flags,
		flying_speed,
		walking_speed
	}).

%3F
-record(
	plugin_message, {
		channel,
		data
	}).



% Map chunk
-record(
	chunk, {
		pos % {x, z}
