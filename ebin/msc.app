{application, 'msc', [
	{description, "MySQL/MariaDB Client"},
	{vsn, ""},
	{id, "c7bbdfe-dirty"},
	{modules, ['msc','msc_app','msc_binlog_ets','msc_config','msc_connection_sup','msc_connections_sup','msc_mm','msc_mm_auth','msc_mm_binlog_dump','msc_mm_common','msc_mm_execute','msc_mm_prepare','msc_mm_query','msc_mm_register_replica','msc_mm_stmt_close','msc_mm_stmt_reset','msc_mm_sync','msc_socket','msc_statem','msc_sup','msc_telemetry','msc_uri','msc_util']},
	{registered, [msc_sup]},
	{applications, [kernel,stdlib,ssl,backoff,envy,msmp,telemetry]},
	{optional_applications, []},
	{mod, {msc_app, []}},
	{env, []}
]}.