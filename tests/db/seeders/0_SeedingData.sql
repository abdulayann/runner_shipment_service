-- app_config definition

-- Drop table

-- DROP TABLE app_config;

CREATE TABLE app_config (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	config_key varchar(255) NOT NULL,
	config_value varchar(255) NOT NULL,
	data_type varchar(255) NOT NULL,
	description varchar(255) NULL,
	display_name varchar(255) NULL,
	CONSTRAINT app_config_pkey PRIMARY KEY (id),
	CONSTRAINT uk_lb58j8yn1k5qn7efmtic7mc0r UNIQUE (config_key)
);


-- batch_job_instance definition

-- Drop table

-- DROP TABLE batch_job_instance;

CREATE TABLE batch_job_instance (
	job_instance_id int8 NOT NULL,
	"version" int8 NULL,
	job_name varchar(100) NOT NULL,
	job_key varchar(32) NOT NULL,
	CONSTRAINT batch_job_instance_pkey PRIMARY KEY (job_instance_id),
	CONSTRAINT job_inst_un UNIQUE (job_name, job_key)
);


-- column_config definition

-- Drop table

-- DROP TABLE column_config;

CREATE TABLE column_config (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	column_index int4 NOT NULL,
	data_type varchar(255) NOT NULL,
	description varchar(255) NULL,
	editable bool NOT NULL DEFAULT true,
	enabled_excel_download bool NOT NULL DEFAULT true,
	enabled_for_upload bool NOT NULL DEFAULT true,
	excel_column_name varchar(255) NOT NULL,
	influencing_columns_path jsonb NOT NULL,
	input_type varchar(255) NOT NULL DEFAULT 'SIMPLE_TEXT'::character varying,
	is_influencing_columns_path_enabled bool NOT NULL,
	is_mandatory bool NOT NULL DEFAULT false,
	mapped_column_name varchar(255) NOT NULL,
	section_code varchar(255) NULL,
	setter_enabled bool NOT NULL DEFAULT true,
	show_on_ui bool NOT NULL DEFAULT true,
	template_type varchar(255) NOT NULL,
	use_values bool NOT NULL DEFAULT false,
	"values" jsonb NULL,
	CONSTRAINT column_config_pkey PRIMARY KEY (id),
	CONSTRAINT tenant_column_index UNIQUE (template_type, excel_column_name)
);


-- container definition

-- Drop table

-- DROP TABLE container;

CREATE TABLE container (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	branch varchar(255) NULL,
	sub_organization_code varchar(255) NOT NULL,
	tenant_code varchar(255) NOT NULL,
	container_number varchar(255) NOT NULL,
	container_type varchar(255) NOT NULL,
	contract_number varchar(255) NOT NULL,
	max_cargo_weight float8 NULL,
	remark varchar(255) NULL,
	tare_weight float8 NULL,
	vintage varchar(255) NULL,
	CONSTRAINT container_container_index UNIQUE (tenant_code, sub_organization_code, container_number),
	CONSTRAINT container_pkey PRIMARY KEY (id)
);


-- container_update definition

-- Drop table

-- DROP TABLE container_update;

CREATE TABLE container_update (
	id bigserial NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	branch varchar(255) NULL,
	sub_organization_code varchar(255) NOT NULL,
	tenant_code varchar(255) NOT NULL,
	container_number varchar(255) NOT NULL,
	container_status varchar(255) NOT NULL,
	contract_number varchar(255) NOT NULL,
	country varchar(255) NULL,
	customer varchar(255) NULL,
	depot varchar(255) NULL,
	estimated_arrival timestamp NULL,
	full_empty_status varchar(255) NULL,
	hbl_number varchar(255) NULL,
	in_out_status varchar(255) NULL,
	port varchar(255) NULL,
	rail_or_vessel varchar(255) NULL,
	release_number varchar(255) NULL,
	remark varchar(255) NULL,
	status_time timestamp NOT NULL,
	vessel varchar(255) NULL,
	vessel_number varchar(255) NULL,
	voyage varchar(255) NULL,
	CONSTRAINT container_update_pkey PRIMARY KEY (id)
);
CREATE INDEX container_update_container_index ON container_update USING btree (tenant_code, sub_organization_code, container_number);
CREATE INDEX container_update_container_is_active_index ON container_update USING btree (tenant_code, sub_organization_code, container_number, is_active);
CREATE INDEX container_update_id_index ON container_update USING btree (tenant_code, sub_organization_code, id);


-- contract definition

-- Drop table

-- DROP TABLE contract;

CREATE TABLE contract (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	branch varchar(255) NULL,
	sub_organization_code varchar(255) NOT NULL,
	tenant_code varchar(255) NOT NULL,
	alt_standards varchar(255) NULL,
	build_down_scale int4 NULL,
	contract_date timestamp NOT NULL,
	contract_number varchar(255) NOT NULL,
	contract_type varchar(255) NOT NULL,
	depreciation_rate float8 NULL,
	di_fee numeric(19, 2) NULL,
	dispute_resolution varchar(255) NULL,
	effective_date timestamp NULL,
	empty_redelivery int4 NULL,
	equipment_quantity int4 NOT NULL,
	est_term_date timestamp NULL,
	estimated_resp_days int4 NULL,
	expire_date timestamp NULL,
	free_days int4 NULL,
	general_minimum_limit numeric(19, 2) NULL,
	interest_rate float8 NULL,
	lease_date timestamp NULL,
	lease_type varchar(255) NOT NULL,
	lessee_code varchar(255) NULL,
	lessor_code varchar(255) NULL,
	lift_off_charger varchar(255) NULL,
	lift_on_charger varchar(255) NULL,
	lump_sum numeric(19, 2) NULL,
	min_days int4 NULL,
	objection_notice int4 NULL,
	other_charges numeric(19, 2) NULL,
	payment_days int4 NULL,
	reference_number varchar(255) NULL,
	repairs_notice int4 NULL,
	replacement_value numeric(19, 2) NULL,
	repos_notice int4 NULL,
	term_notice int4 NULL,
	CONSTRAINT contract_contract_index UNIQUE (tenant_code, sub_organization_code, contract_number),
	CONSTRAINT contract_pkey PRIMARY KEY (id)
);
CREATE INDEX contract_all_contract_index ON contract USING btree (tenant_code, sub_organization_code);
CREATE INDEX contract_contract_date_index ON contract USING btree (contract_date);
CREATE INDEX contract_contract_no_index ON contract USING btree (contract_number);
CREATE INDEX contract_contract_type_index ON contract USING btree (contract_type);
CREATE INDEX contract_lease_type_index ON contract USING btree (lease_type);
CREATE INDEX contract_lessor_code_index ON contract USING btree (lessor_code);
CREATE INDEX contract_reference_number_index ON contract USING btree (reference_number);


-- contract_container definition

-- Drop table

-- DROP TABLE contract_container;

CREATE TABLE contract_container (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	branch varchar(255) NULL,
	sub_organization_code varchar(255) NOT NULL,
	tenant_code varchar(255) NOT NULL,
	container_number varchar(255) NOT NULL,
	container_status varchar(255) NOT NULL,
	container_type varchar(255) NULL,
	contract_number varchar(255) NOT NULL,
	country varchar(255) NULL,
	depot varchar(255) NULL,
	full_empty_status varchar(255) NULL,
	hbl_number varchar(255) NULL,
	in_out_status varchar(255) NULL,
	lessee_code varchar(255) NULL,
	port varchar(255) NULL,
	seller_amount numeric(19, 2) NULL,
	start_date timestamp NULL,
	status_date timestamp NOT NULL,
	stop_date timestamp NULL,
	vessel varchar(255) NULL,
	vessel_number varchar(255) NULL,
	CONSTRAINT contract_container_index UNIQUE (tenant_code, sub_organization_code, contract_number, container_number),
	CONSTRAINT contract_container_pkey PRIMARY KEY (id)
);
CREATE INDEX contract_container_container_number_index ON contract_container USING btree (container_number);
CREATE INDEX contract_container_container_status_index ON contract_container USING btree (container_status);
CREATE INDEX contract_container_container_type_count_index ON contract_container USING btree (tenant_code, sub_organization_code, contract_number, container_type);
CREATE INDEX contract_container_contract_number_index ON contract_container USING btree (contract_number);
CREATE INDEX contract_container_country_index ON contract_container USING btree (country);
CREATE INDEX contract_container_depot_index ON contract_container USING btree (depot);
CREATE INDEX contract_container_in_out_status_index ON contract_container USING btree (in_out_status);
CREATE INDEX contract_container_port_index ON contract_container USING btree (port);
CREATE INDEX contract_container_start_date_index ON contract_container USING btree (start_date);
CREATE INDEX contract_container_stop_date_index ON contract_container USING btree (stop_date);
CREATE INDEX contract_container_sub_organization_code_index ON contract_container USING btree (sub_organization_code);


-- contract_container_inventory definition

-- Drop table

-- DROP TABLE contract_container_inventory;

CREATE TABLE contract_container_inventory (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	branch varchar(255) NULL,
	sub_organization_code varchar(255) NOT NULL,
	tenant_code varchar(255) NOT NULL,
	container_type varchar(255) NULL,
	contract_container_count int4 NULL,
	contract_number varchar(255) NULL,
	CONSTRAINT contract_container_inventory_container_type_index UNIQUE (tenant_code, sub_organization_code, contract_number, container_type),
	CONSTRAINT contract_container_inventory_pkey PRIMARY KEY (id)
);
CREATE INDEX contract_container_inventory_contract_number_index ON contract_container_inventory USING btree (tenant_code, sub_organization_code, contract_number);
CREATE INDEX contract_container_inventory_sub_org_index ON contract_container_inventory USING btree (sub_organization_code);


-- "event" definition

-- Drop table

-- DROP TABLE "event";

CREATE TABLE "event" (
	id int8 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	branch varchar(255) NULL,
	sub_organization_code varchar(255) NOT NULL,
	tenant_code varchar(255) NOT NULL,
	event_type varchar(255) NOT NULL,
	retry_count int4 NOT NULL,
	entity_id varchar(255) NOT NULL,
	error_code varchar(255) NULL,
	error_description varchar(255) NULL,
	event_payload jsonb NULL,
	event_status varchar(255) NOT NULL,
	CONSTRAINT event_pkey PRIMARY KEY (id)
);
CREATE INDEX cr_dt_idx ON event USING btree (created);
CREATE INDEX fetch_events_idx ON event USING btree (created, event_type, event_status, retry_count);


-- event_retry_config definition

-- Drop table

-- DROP TABLE event_retry_config;

CREATE TABLE event_retry_config (
	id int4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	backlog_end int4 NOT NULL,
	backlog_start int4 NOT NULL,
	description varchar(255) NULL,
	event_type varchar(255) NOT NULL,
	retry_limit int4 NOT NULL,
	statuses _text NOT NULL,
	CONSTRAINT event_retry_config_pkey PRIMARY KEY (id),
	CONSTRAINT uk_kkwuhjk88mv2cmnkjr0yfgveq UNIQUE (event_type)
);


-- file definition

-- Drop table

-- DROP TABLE file;

CREATE TABLE file (
	id bigserial NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	branch varchar(255) NULL,
	sub_organization_code varchar(255) NOT NULL,
	tenant_code varchar(255) NOT NULL,
	description varchar(255) NULL,
	file_id varchar(255) NOT NULL,
	file_type varchar(255) NOT NULL,
	template_type varchar NOT NULL DEFAULT 'GLOBAL_TEMPLATE'::character varying,
	upload_path varchar(255) NULL,
	uploaded_file_name varchar(255) NOT NULL,
	CONSTRAINT file_file_id_idx UNIQUE (tenant_code, sub_organization_code, file_id),
	CONSTRAINT file_pkey PRIMARY KEY (id)
);
CREATE INDEX file_created_by_idx ON file USING btree (created_by);
CREATE INDEX file_created_idx ON file USING btree (created);
CREATE INDEX file_file_id_2_idx ON file USING btree (file_id);
CREATE INDEX file_sub_org_idx ON file USING btree (sub_organization_code);
CREATE INDEX file_template_type_idx ON file USING btree (template_type);


-- file_attribute_rule definition

-- Drop table

-- DROP TABLE file_attribute_rule;

CREATE TABLE file_attribute_rule (
	rule_id bigserial NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	attribute_check_enable bool NOT NULL DEFAULT true,
	attribute_checks jsonb NULL,
	attribute_name varchar(255) NOT NULL,
	dependent_column_validations jsonb NULL,
	expected_values jsonb NULL,
	influencing_columns_path varchar(255) NULL,
	is_enabled bool NOT NULL DEFAULT true,
	template_type varchar(255) NOT NULL,
	value_check_enable bool NOT NULL DEFAULT true,
	CONSTRAINT file_attribute_rule_attribute_name_index UNIQUE (template_type, attribute_name),
	CONSTRAINT file_attribute_rule_pkey PRIMARY KEY (rule_id)
);


-- file_audit definition

-- Drop table

-- DROP TABLE file_audit;

CREATE TABLE file_audit (
	id int8 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	branch varchar(255) NULL,
	sub_organization_code varchar(255) NOT NULL,
	tenant_code varchar(255) NOT NULL,
	event_type varchar(255) NOT NULL,
	retry_count int4 NOT NULL,
	column_name_to_column_value_map jsonb NULL,
	file_id varchar(255) NOT NULL,
	row_id varchar(255) NOT NULL,
	row_status varchar(255) NOT NULL,
	template_type varchar NOT NULL DEFAULT 'GLOBAL_TEMPLATE'::character varying,
	"type" varchar(255) NULL,
	validation_errors jsonb NULL,
	validation_status varchar(255) NOT NULL,
	CONSTRAINT file_audit_file_row_idx UNIQUE (tenant_code, sub_organization_code, file_id, row_id),
	CONSTRAINT file_audit_pkey PRIMARY KEY (id)
);
CREATE INDEX file_audit_file_id_idx ON file_audit USING btree (tenant_code, sub_organization_code, file_id);
CREATE INDEX file_audit_id_idx ON file_audit USING btree (tenant_code, sub_organization_code, id);


-- master_data_type definition

-- Drop table

-- DROP TABLE master_data_type;

CREATE TABLE master_data_type (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	description varchar(255) NULL,
	"type" varchar(255) NULL,
	CONSTRAINT master_data_type_pkey PRIMARY KEY (id),
	CONSTRAINT uk_ibm5tknd1l0gn6k69s0irfgbm UNIQUE (type)
);


-- master_data_value definition

-- Drop table

-- DROP TABLE master_data_value;

CREATE TABLE master_data_value (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	"cascade" varchar(255) NULL,
	description varchar(255) NULL,
	master_data_type varchar(255) NULL,
	value varchar(255) NULL,
	CONSTRAINT master_data_value_pkey PRIMARY KEY (id),
	CONSTRAINT master_data_value_value_idx UNIQUE (master_data_type, value)
);


-- qrtz_calendars definition

-- Drop table

-- DROP TABLE qrtz_calendars;

CREATE TABLE qrtz_calendars (
	calendar_name varchar(80) NOT NULL,
	calendar text NOT NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	CONSTRAINT qrtz_calendars_pkey PRIMARY KEY (sched_name, calendar_name)
);


-- qrtz_fired_triggers definition

-- Drop table

-- DROP TABLE qrtz_fired_triggers;

CREATE TABLE qrtz_fired_triggers (
	entry_id varchar(95) NOT NULL,
	trigger_name varchar(80) NOT NULL,
	trigger_group varchar(80) NOT NULL,
	instance_name varchar(80) NOT NULL,
	fired_time int8 NOT NULL,
	priority int4 NOT NULL,
	state varchar(16) NOT NULL,
	job_name varchar(80) NULL,
	job_group varchar(80) NULL,
	is_nonconcurrent bool NULL,
	is_update_data bool NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	sched_time int8 NOT NULL,
	requests_recovery bool NULL,
	CONSTRAINT qrtz_fired_triggers_pkey PRIMARY KEY (sched_name, entry_id)
);
CREATE INDEX idx_qrtz_ft_j_g ON qrtz_fired_triggers USING btree (sched_name, job_name, job_group);
CREATE INDEX idx_qrtz_ft_jg ON qrtz_fired_triggers USING btree (sched_name, job_group);
CREATE INDEX idx_qrtz_ft_t_g ON qrtz_fired_triggers USING btree (sched_name, trigger_name, trigger_group);
CREATE INDEX idx_qrtz_ft_tg ON qrtz_fired_triggers USING btree (sched_name, trigger_group);
CREATE INDEX idx_qrtz_ft_trig_inst_name ON qrtz_fired_triggers USING btree (sched_name, instance_name);


-- qrtz_job_details definition

-- Drop table

-- DROP TABLE qrtz_job_details;

CREATE TABLE qrtz_job_details (
	job_name varchar(128) NOT NULL,
	job_group varchar(80) NOT NULL,
	description varchar(120) NULL,
	job_class_name varchar(200) NOT NULL,
	is_durable bool NULL,
	is_nonconcurrent bool NULL,
	is_update_data bool NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	requests_recovery bool NULL,
	job_data bytea NULL,
	CONSTRAINT qrtz_job_details_pkey PRIMARY KEY (sched_name, job_name, job_group)
);
CREATE INDEX idx_qrtz_j_grp ON qrtz_job_details USING btree (sched_name, job_group);


-- qrtz_locks definition

-- Drop table

-- DROP TABLE qrtz_locks;

CREATE TABLE qrtz_locks (
	lock_name varchar(40) NOT NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	CONSTRAINT qrtz_locks_pkey PRIMARY KEY (sched_name, lock_name)
);


-- qrtz_paused_trigger_grps definition

-- Drop table

-- DROP TABLE qrtz_paused_trigger_grps;

CREATE TABLE qrtz_paused_trigger_grps (
	trigger_group varchar(80) NOT NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	CONSTRAINT qrtz_paused_trigger_grps_pkey PRIMARY KEY (sched_name, trigger_group)
);


-- qrtz_scheduler_state definition

-- Drop table

-- DROP TABLE qrtz_scheduler_state;

CREATE TABLE qrtz_scheduler_state (
	instance_name varchar(200) NOT NULL,
	last_checkin_time int8 NULL,
	checkin_interval int8 NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	CONSTRAINT qrtz_scheduler_state_pkey PRIMARY KEY (sched_name, instance_name)
);


-- resource_permission definition

-- Drop table

-- DROP TABLE resource_permission;

CREATE TABLE resource_permission (
	id int4 NOT NULL GENERATED BY DEFAULT AS IDENTITY,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	"permission" varchar(255) NULL,
	resource varchar(255) NULL,
	"type" varchar(255) NULL,
	CONSTRAINT resource_permission_pkey PRIMARY KEY (id)
);
CREATE INDEX is_active ON resource_permission USING btree (is_active);
CREATE INDEX resource_permission_type ON resource_permission USING btree (resource, permission, type);


-- retry_job_config definition

-- Drop table

-- DROP TABLE retry_job_config;

CREATE TABLE retry_job_config (
	id serial4 NOT NULL,
	created timestamp NULL,
	created_by varchar(255) NULL,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	job_config_wrapper jsonb NOT NULL,
	job_cron varchar(255) NOT NULL,
	job_name varchar(255) NOT NULL,
	CONSTRAINT retry_job_config_pkey PRIMARY KEY (id),
	CONSTRAINT uk_l36vfd0meao0fi8nwx2eyl2f5 UNIQUE (job_name)
);


-- templates definition

-- Drop table

-- DROP TABLE templates;

CREATE TABLE templates (
	id serial4 NOT NULL,
	created timestamp NOT NULL,
	created_by varchar(255) NOT NULL,
	extra_params jsonb NULL,
	is_active bool NOT NULL DEFAULT true,
	updated timestamp NULL,
	updated_by varchar(255) NULL,
	display_screens jsonb NOT NULL,
	header_row_index int4 NOT NULL,
	sheet_no int4 NOT NULL,
	template_name varchar(255) NOT NULL,
	template_path varchar(255) NOT NULL,
	template_type varchar(255) NOT NULL,
	CONSTRAINT templates_pkey PRIMARY KEY (id),
	CONSTRAINT uk_do120edp4pgrba6dc5t96gqg6 UNIQUE (template_type)
);


-- batch_job_execution definition

-- Drop table

-- DROP TABLE batch_job_execution;

CREATE TABLE batch_job_execution (
	job_execution_id int8 NOT NULL,
	"version" int8 NULL,
	job_instance_id int8 NOT NULL,
	create_time timestamp NOT NULL,
	start_time timestamp NULL,
	end_time timestamp NULL,
	status varchar(10) NULL,
	exit_code varchar(2500) NULL,
	exit_message varchar(2500) NULL,
	last_updated timestamp NULL,
	job_configuration_location varchar(2500) NULL,
	CONSTRAINT batch_job_execution_pkey PRIMARY KEY (job_execution_id),
	CONSTRAINT job_inst_exec_fk FOREIGN KEY (job_instance_id) REFERENCES batch_job_instance(job_instance_id)
);


-- batch_job_execution_context definition

-- Drop table

-- DROP TABLE batch_job_execution_context;

CREATE TABLE batch_job_execution_context (
	job_execution_id int8 NOT NULL,
	short_context varchar(2500) NOT NULL,
	serialized_context text NULL,
	CONSTRAINT batch_job_execution_context_pkey PRIMARY KEY (job_execution_id),
	CONSTRAINT job_exec_ctx_fk FOREIGN KEY (job_execution_id) REFERENCES batch_job_execution(job_execution_id)
);


-- batch_job_execution_params definition

-- Drop table

-- DROP TABLE batch_job_execution_params;

CREATE TABLE batch_job_execution_params (
	job_execution_id int8 NOT NULL,
	type_cd varchar(6) NOT NULL,
	key_name varchar(100) NOT NULL,
	string_val varchar(250) NULL,
	date_val timestamp NULL,
	long_val int8 NULL,
	double_val float8 NULL,
	identifying bpchar(1) NOT NULL,
	CONSTRAINT job_exec_params_fk FOREIGN KEY (job_execution_id) REFERENCES batch_job_execution(job_execution_id)
);


-- batch_step_execution definition

-- Drop table

-- DROP TABLE batch_step_execution;

CREATE TABLE batch_step_execution (
	step_execution_id int8 NOT NULL,
	"version" int8 NOT NULL,
	step_name varchar(100) NOT NULL,
	job_execution_id int8 NOT NULL,
	start_time timestamp NOT NULL,
	end_time timestamp NULL,
	status varchar(10) NULL,
	commit_count int8 NULL,
	read_count int8 NULL,
	filter_count int8 NULL,
	write_count int8 NULL,
	read_skip_count int8 NULL,
	write_skip_count int8 NULL,
	process_skip_count int8 NULL,
	rollback_count int8 NULL,
	exit_code varchar(2500) NULL,
	exit_message varchar(2500) NULL,
	last_updated timestamp NULL,
	CONSTRAINT batch_step_execution_pkey PRIMARY KEY (step_execution_id),
	CONSTRAINT job_exec_step_fk FOREIGN KEY (job_execution_id) REFERENCES batch_job_execution(job_execution_id)
);


-- batch_step_execution_context definition

-- Drop table

-- DROP TABLE batch_step_execution_context;

CREATE TABLE batch_step_execution_context (
	step_execution_id int8 NOT NULL,
	short_context varchar(2500) NOT NULL,
	serialized_context text NULL,
	CONSTRAINT batch_step_execution_context_pkey PRIMARY KEY (step_execution_id),
	CONSTRAINT step_exec_ctx_fk FOREIGN KEY (step_execution_id) REFERENCES batch_step_execution(step_execution_id)
);


-- qrtz_triggers definition

-- Drop table

-- DROP TABLE qrtz_triggers;

CREATE TABLE qrtz_triggers (
	trigger_name varchar(80) NOT NULL,
	trigger_group varchar(80) NOT NULL,
	job_name varchar(80) NOT NULL,
	job_group varchar(80) NOT NULL,
	description varchar(120) NULL,
	next_fire_time int8 NULL,
	prev_fire_time int8 NULL,
	priority int4 NULL,
	trigger_state varchar(16) NOT NULL,
	trigger_type varchar(8) NOT NULL,
	start_time int8 NOT NULL,
	end_time int8 NULL,
	calendar_name varchar(80) NULL,
	misfire_instr int2 NULL,
	job_data bytea NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	CONSTRAINT qrtz_triggers_pkey PRIMARY KEY (sched_name, trigger_name, trigger_group),
	CONSTRAINT qrtz_triggers_sched_name_fkey FOREIGN KEY (sched_name,job_name,job_group) REFERENCES qrtz_job_details(sched_name,job_name,job_group)
);
CREATE INDEX fki_qrtz_simple_triggers_job_details_name_group ON qrtz_triggers USING btree (job_name, job_group);
CREATE INDEX idx_qrtz_t_c ON qrtz_triggers USING btree (sched_name, calendar_name);
CREATE INDEX idx_qrtz_t_g ON qrtz_triggers USING btree (sched_name, trigger_group);
CREATE INDEX idx_qrtz_t_j ON qrtz_triggers USING btree (sched_name, job_name, job_group);
CREATE INDEX idx_qrtz_t_jg ON qrtz_triggers USING btree (sched_name, job_group);
CREATE INDEX idx_qrtz_t_n_g_state ON qrtz_triggers USING btree (sched_name, trigger_group, trigger_state);
CREATE INDEX idx_qrtz_t_n_state ON qrtz_triggers USING btree (sched_name, trigger_name, trigger_group, trigger_state);
CREATE INDEX idx_qrtz_t_next_fire_time ON qrtz_triggers USING btree (sched_name, next_fire_time);
CREATE INDEX idx_qrtz_t_nft_misfire ON qrtz_triggers USING btree (sched_name, misfire_instr, next_fire_time);
CREATE INDEX idx_qrtz_t_nft_st ON qrtz_triggers USING btree (sched_name, trigger_state, next_fire_time);
CREATE INDEX idx_qrtz_t_nft_st_misfire ON qrtz_triggers USING btree (sched_name, misfire_instr, next_fire_time, trigger_state);
CREATE INDEX idx_qrtz_t_nft_st_misfire_grp ON qrtz_triggers USING btree (sched_name, misfire_instr, next_fire_time, trigger_group, trigger_state);
CREATE INDEX idx_qrtz_t_state ON qrtz_triggers USING btree (sched_name, trigger_state);


-- qrtz_blob_triggers definition

-- Drop table

-- DROP TABLE qrtz_blob_triggers;

CREATE TABLE qrtz_blob_triggers (
	trigger_name varchar(80) NOT NULL,
	trigger_group varchar(80) NOT NULL,
	blob_data text NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	CONSTRAINT qrtz_blob_triggers_pkey PRIMARY KEY (sched_name, trigger_name, trigger_group),
	CONSTRAINT qrtz_blob_triggers_sched_name_fkey FOREIGN KEY (sched_name,trigger_name,trigger_group) REFERENCES qrtz_triggers(sched_name,trigger_name,trigger_group)
);


-- qrtz_cron_triggers definition

-- Drop table

-- DROP TABLE qrtz_cron_triggers;

CREATE TABLE qrtz_cron_triggers (
	trigger_name varchar(80) NOT NULL,
	trigger_group varchar(80) NOT NULL,
	cron_expression varchar(80) NOT NULL,
	time_zone_id varchar(80) NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	CONSTRAINT qrtz_cron_triggers_pkey PRIMARY KEY (sched_name, trigger_name, trigger_group),
	CONSTRAINT qrtz_cron_triggers_sched_name_fkey FOREIGN KEY (sched_name,trigger_name,trigger_group) REFERENCES qrtz_triggers(sched_name,trigger_name,trigger_group)
);


-- qrtz_simple_triggers definition

-- Drop table

-- DROP TABLE qrtz_simple_triggers;

CREATE TABLE qrtz_simple_triggers (
	trigger_name varchar(80) NOT NULL,
	trigger_group varchar(80) NOT NULL,
	repeat_count int8 NOT NULL,
	repeat_interval int8 NOT NULL,
	times_triggered int8 NOT NULL,
	sched_name varchar(120) NOT NULL DEFAULT 'TestScheduler'::character varying,
	CONSTRAINT qrtz_simple_triggers_pkey PRIMARY KEY (sched_name, trigger_name, trigger_group),
	CONSTRAINT qrtz_simple_triggers_sched_name_fkey FOREIGN KEY (sched_name,trigger_name,trigger_group) REFERENCES qrtz_triggers(sched_name,trigger_name,trigger_group)
);
CREATE INDEX fki_qrtz_simple_triggers_qrtz_triggers ON qrtz_simple_triggers USING btree (trigger_name, trigger_group);


-- qrtz_simprop_triggers definition

-- Drop table

-- DROP TABLE qrtz_simprop_triggers;

CREATE TABLE qrtz_simprop_triggers (
	sched_name varchar(120) NOT NULL,
	trigger_name varchar(200) NOT NULL,
	trigger_group varchar(200) NOT NULL,
	str_prop_1 varchar(512) NULL,
	str_prop_2 varchar(512) NULL,
	str_prop_3 varchar(512) NULL,
	int_prop_1 int4 NULL,
	int_prop_2 int4 NULL,
	long_prop_1 int8 NULL,
	long_prop_2 int8 NULL,
	dec_prop_1 numeric(13, 4) NULL,
	dec_prop_2 numeric(13, 4) NULL,
	bool_prop_1 bool NULL,
	bool_prop_2 bool NULL,
	CONSTRAINT qrtz_simprop_triggers_pkey PRIMARY KEY (sched_name, trigger_name, trigger_group),
	CONSTRAINT qrtz_simprop_triggers_sched_name_fkey FOREIGN KEY (sched_name,trigger_name,trigger_group) REFERENCES qrtz_triggers(sched_name,trigger_name,trigger_group)
);

INSERT INTO master_data_type (created,created_by,extra_params,is_active,updated,updated_by,description,"type") VALUES
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','FullEmptyStatus'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','InOutStatus'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','TransportMode'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','ContractType'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','LeaseType_LI'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','LeaseType_LO'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','LeaseType_OV'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','LeaseType'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','LiftOnOffCharger'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','ContainerStatus');
INSERT INTO master_data_type (created,created_by,extra_params,is_active,updated,updated_by,description,"type") VALUES
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','ContainerStatusStopped'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','ContainerStatusStarted'),
	 ('2022-10-26 15:19:59.123565','ADMIN','{}',true,'2022-10-26 15:19:59.123565','ADMIN','','ContainerType');

INSERT INTO master_data_value (created,created_by,extra_params,is_active,updated,updated_by,"cascade",description,master_data_type,value) VALUES
	 ('2022-10-26 15:19:59.217325','ADMIN','{}',true,'2022-10-26 15:19:59.217325','ADMIN','','Full Container','FullEmptyStatus','FULL'),
	 ('2022-10-26 15:19:59.217325','ADMIN','{}',true,'2022-10-26 15:19:59.217325','ADMIN','','Empty Container','FullEmptyStatus','EMPTY'),
	 ('2022-10-26 15:19:59.295484','ADMIN','{}',true,'2022-10-26 15:19:59.295484','ADMIN','','Container Moved In','InOutStatus','IN'),
	 ('2022-10-26 15:19:59.295484','ADMIN','{}',true,'2022-10-26 15:19:59.295484','ADMIN','','Container Moved Out','InOutStatus','OUT'),
	 ('2022-10-26 15:19:59.373604','ADMIN','{}',true,'2022-10-26 15:19:59.373604','ADMIN','','Rail Transport','TransportMode','RAIL'),
	 ('2022-10-26 15:19:59.373604','ADMIN','{}',true,'2022-10-26 15:19:59.373604','ADMIN','','Sea Transport','TransportMode','VESSEL'),
	 ('2022-10-26 15:19:59.451724','ADMIN','{}',true,'2022-10-26 15:19:59.451724','ADMIN','','Lease In','ContractType','LI'),
	 ('2022-10-26 15:19:59.451724','ADMIN','{}',true,'2022-10-26 15:19:59.451724','ADMIN','','Lease Out','ContractType','LO'),
	 ('2022-10-26 15:19:59.451724','ADMIN','{}',true,'2022-10-26 15:19:59.451724','ADMIN','','Own Van','ContractType','OV'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Long Term','LeaseType_LI','LT');
INSERT INTO master_data_value (created,created_by,extra_params,is_active,updated,updated_by,"cascade",description,master_data_type,value) VALUES
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Master Term','LeaseType_LI','MT'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','One Way','LeaseType_LI','OW'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Round Turn','LeaseType_LI','RT'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Sub Lease','LeaseType_LI','SL'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Short Term','LeaseType_LI','ST'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Long Term','LeaseType_LO','LT'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','One Way','LeaseType_LO','OW'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Round Turn','LeaseType_LO','RT'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Sub Lease','LeaseType_LO','SL'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Short Term','LeaseType_LO','ST');
INSERT INTO master_data_value (created,created_by,extra_params,is_active,updated,updated_by,"cascade",description,master_data_type,value) VALUES
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','2nd Hand','LeaseType_OV','2H'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','New Van','LeaseType_OV','NV'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','Long Term','LeaseType','LT'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','Master Term','LeaseType','MT'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','One Way','LeaseType','OW'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','Round Turn','LeaseType','RT'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','Sub Lease','LeaseType','SL'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','Short Term','LeaseType','ST'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','2nd Hand','LeaseType','2H'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','New Van','LeaseType','NV');
INSERT INTO master_data_value (created,created_by,extra_params,is_active,updated,updated_by,"cascade",description,master_data_type,value) VALUES
	 ('2022-10-26 15:19:59.701727','ADMIN','{}',true,'2022-10-26 15:19:59.701727','ADMIN','','Lessor','LiftOnOffCharger','Lessor'),
	 ('2022-10-26 15:19:59.701727','ADMIN','{}',true,'2022-10-26 15:19:59.701727','ADMIN','','Lessee','LiftOnOffCharger','Lessee'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','LEASE OUT','ContainerStatus','LIO'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','ON HIRE','ContainerStatus','ONH'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','LEASE RETURN','ContainerStatus','RTN'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','CONSIGNEE PROPERTY CONTAINER','ContainerStatus','CPC'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','DISPOSAL','ContainerStatus','DSP'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','OFF HIRE','ContainerStatus','OFH'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','TOTAL LOSS','ContainerStatus','TTL'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','GATE','ContainerStatus','GAT');
INSERT INTO master_data_value (created,created_by,extra_params,is_active,updated,updated_by,"cascade",description,master_data_type,value) VALUES
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','RAIL STATION','ContainerStatus','STN'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','VESSEL','ContainerStatus','VSL'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','MAINTENANCE & REPAIR','ContainerStatus','MAR'),
	 ('2022-10-26 15:19:59.779858','ADMIN','{}',true,'2022-10-26 15:19:59.779858','ADMIN','','RESERVED AGAINST BOOKING','ContainerStatus','RAB'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','LEASE OUT','ContainerStatusStarted','LIO'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','ON HIRE','ContainerStatusStarted','ONH'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','GATE','ContainerStatusStarted','GAT'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','RAIL STATION','ContainerStatusStarted','STN'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','VESSEL','ContainerStatusStarted','VSL'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','LEASE RETURN','ContainerStatusStopped','RTN');
INSERT INTO master_data_value (created,created_by,extra_params,is_active,updated,updated_by,"cascade",description,master_data_type,value) VALUES
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','CONSIGNEE PROPERTY CONTAINER','ContainerStatusStopped','CPC'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','DISPOSAL','ContainerStatusStopped','DSP'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','OFF HIRE','ContainerStatusStopped','OFH'),
	 ('2022-10-26 15:19:59.857976','ADMIN','{}',true,'2022-10-26 15:19:59.857976','ADMIN','','TOTAL LOSS','ContainerStatusStopped','TTL'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','20FT DV','ContainerType','20DV'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','20 FLAT RACK CONTAINER','ContainerType','20FR'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','20FT REEFER','ContainerType','20RF'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','20 OPEN TOP CONTAINER','ContainerType','20OT'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','20FT TANK','ContainerType','20TN'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','20 High CUBIC CONTAINER','ContainerType','20HQ');
INSERT INTO master_data_value (created,created_by,extra_params,is_active,updated,updated_by,"cascade",description,master_data_type,value) VALUES
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','40FT DV','ContainerType','40DV'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','40 OPEN TOP CONTAINER','ContainerType','40OT'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','40FT REEFER','ContainerType','40RE'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','40 FLAT RACK CONTAINER','ContainerType','40FR'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','40FT HQ REEFER','ContainerType','40HR'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','40HQ GENERAL PURPOSE CONTAINER','ContainerType','40HQ'),
	 ('2022-10-26 15:19:59.936104','ADMIN','{}',true,'2022-10-26 15:19:59.936104','ADMIN','','45FT DRY','ContainerType','45FT'),
	 ('2022-10-26 15:19:59.529852','ADMIN','{}',true,'2022-10-26 15:19:59.529852','ADMIN','','Lease & Purchase','LeaseType_OV','LP'),
	 ('2022-10-26 15:19:59.623588','ADMIN','{}',true,'2022-10-26 15:19:59.623588','ADMIN','','Lease & Purchase','LeaseType','LP');



INSERT INTO templates (created,created_by,extra_params,is_active,updated,updated_by,display_screens,header_row_index,sheet_no,template_name,template_path,template_type) VALUES
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'LUMP_CONTAINER_TEMPLATE','/','LUMP_CONTAINER_TEMPLATE'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD','/','LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'START_CONTAINER_TEMPLATE','/','START_CONTAINER_TEMPLATE'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD','/','START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'MOVEMENT_LOG_TEMPLATE','/','MOVEMENT_LOG_TEMPLATE'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD','/','MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'STOP_CONTAINER_TEMPLATE','/','STOP_CONTAINER_TEMPLATE'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD','/','STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE','/','IDLING_INQUIRY_DOWNLOAD_TEMPLATE'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE','/','CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE');
INSERT INTO templates (created,created_by,extra_params,is_active,updated,updated_by,display_screens,header_row_index,sheet_no,template_name,template_path,template_type) VALUES
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE','/','START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE','/','STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE','/','STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE'),
	 ('2022-10-26 15:20:00.029815','Admin','{"EXCEL_HEADER_VALIDATION_ENABLED": "true"}',true,'2022-10-26 15:20:00.029815','Admin','["NA"]',2,0,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE','/','CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE');



INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',0,'STRING',NULL,true,true,true,'Contract Number','["NA"]','SIMPLE_TEXT',false,true,'contractNumber','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',1,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',2,'STRING',NULL,true,true,true,'Container Type','["NA"]','SIMPLE_TEXT',false,true,'containerType','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',3,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',5,'DECIMAL',NULL,true,true,true,'Seller Amount','["NA"]','SIMPLE_TEXT',false,true,'sellerAmount','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',6,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',7,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',8,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'country','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',10,'STRING',NULL,true,true,true,'Vintage','["NA"]','SIMPLE_TEXT',false,false,'vintage','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',11,'DECIMAL',NULL,true,true,true,'Tare Weight','["NA"]','SIMPLE_TEXT',false,false,'tareWeight','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',12,'DECIMAL',NULL,true,true,true,'Max Cargo Weight','["NA"]','SIMPLE_TEXT',false,false,'maxCargoWeight','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',13,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,false,'remark','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',0,'STRING',NULL,true,true,true,'Contract Number','["NA"]','SIMPLE_TEXT',false,true,'contractNumber','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',1,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',2,'STRING',NULL,true,true,true,'Container Type','["NA"]','SIMPLE_TEXT',false,true,'containerType','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',3,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',5,'DECIMAL',NULL,true,true,true,'Seller Amount','["NA"]','SIMPLE_TEXT',false,true,'sellerAmount','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',6,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',7,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',8,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'country','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',10,'STRING',NULL,true,true,true,'Vintage','["NA"]','SIMPLE_TEXT',false,false,'vintage','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',11,'DECIMAL',NULL,true,true,true,'Tare Weight','["NA"]','SIMPLE_TEXT',false,false,'tareWeight','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',12,'DECIMAL',NULL,true,true,true,'Max Cargo Weight','["NA"]','SIMPLE_TEXT',false,false,'maxCargoWeight','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',13,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,false,'remark','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',14,'STRING',NULL,true,true,true,'Status','["NA"]','SIMPLE_TEXT',false,false,'status','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',15,'STRING',NULL,true,true,true,'Comments','["NA"]','SIMPLE_TEXT',false,false,'comments','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',0,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',1,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'country','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',2,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',3,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',4,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',5,'STRING',NULL,true,true,true,'Customer','["NA"]','SIMPLE_TEXT',false,false,'customer','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',6,'STRING',NULL,true,true,true,'In/Out','["NA"]','SIMPLE_TEXT',false,true,'inOutStatus','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',0,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',7,'STRING',NULL,true,true,true,'H.B/L No','["NA"]','SIMPLE_TEXT',false,false,'hblNumber','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',8,'STRING',NULL,true,true,true,'Vessel','["NA"]','SIMPLE_TEXT',false,false,'vessel','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',9,'STRING',NULL,true,true,true,'Vessel No','["NA"]','SIMPLE_TEXT',false,false,'vesselNumber','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',10,'STRING',NULL,true,true,true,'Voyage No','["NA"]','SIMPLE_TEXT',false,false,'voyage','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',1,'STRING',NULL,true,true,true,'Release Number','["NA"]','SIMPLE_TEXT',false,false,'releaseNumber','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',12,'DATE',NULL,true,true,true,'Estimated Arrival','["NA"]','SIMPLE_TEXT',false,false,'estimatedArrival','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',2,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',3,'DATE',NULL,true,true,true,'Stop Date','["NA"]','SIMPLE_TEXT',false,true,'stopDate','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',0,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',1,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'country','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',2,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',3,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',4,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',5,'STRING',NULL,true,true,true,'Customer','["NA"]','SIMPLE_TEXT',false,false,'customer','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',6,'STRING',NULL,true,true,true,'In/Out','["NA"]','SIMPLE_TEXT',false,true,'inOutStatus','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',0,'STRING',NULL,true,true,true,'Contract Number','["NA"]','SIMPLE_TEXT',false,false,'contractNumber','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',1,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',2,'STRING',NULL,true,true,true,'Release Number','["NA"]','SIMPLE_TEXT',false,false,'releaseNumber','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',3,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',4,'DATE',NULL,true,true,true,'Stop Date','["NA"]','SIMPLE_TEXT',false,true,'stopDate','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',5,'DECIMAL',NULL,true,true,true,'Buy Amount','["NA"]','SIMPLE_TEXT',false,false,'buyAmount','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',6,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,false,'depot','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',7,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,false,'port','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',8,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,false,'country','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',9,'DATE',NULL,true,true,true,'Receipt Date','["NA"]','SIMPLE_TEXT',false,false,'receiptDate','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',10,'STRING',NULL,true,true,true,'Buyer','["NA"]','SIMPLE_TEXT',false,false,'buyer','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',11,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,false,'remark','Container',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',12,'STRING',NULL,true,true,true,'Comments','["NA"]','SIMPLE_TEXT',false,false,'comments','Movement',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.169642','Admin',NULL,true,'2022-10-31 15:01:01.169642','Admin',13,'STRING',NULL,true,true,true,'Status','["NA"]','SIMPLE_TEXT',false,false,'rowStatus','Movement',true,true,'STOP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',4,'DECIMAL',NULL,true,true,true,'Buy Amount','["NA"]','SIMPLE_TEXT',false,false,'buyAmount','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',13,'STRING',NULL,true,true,true,'Full/Empty','["NA"]','SIMPLE_TEXT',false,true,'fullEmptyStatus','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',14,'STRING',NULL,true,true,true,'Rail/Vessel','["NA"]','SIMPLE_TEXT',false,true,'railOrVessel','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',13,'STRING',NULL,true,true,true,'Full/Empty','["NA"]','SIMPLE_TEXT',false,true,'fullEmptyStatus','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',15,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,false,'remark','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',7,'STRING',NULL,true,true,true,'H.B/L No','["NA"]','SIMPLE_TEXT',false,false,'hblNumber','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',8,'STRING',NULL,true,true,true,'Vessel','["NA"]','SIMPLE_TEXT',false,false,'vessel','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',9,'STRING',NULL,true,true,true,'Vessel No','["NA"]','SIMPLE_TEXT',false,false,'vesselNumber','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',10,'STRING',NULL,true,true,true,'Voyage No','["NA"]','SIMPLE_TEXT',false,false,'voyage','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',15,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,false,'remark','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',12,'DATE',NULL,true,true,true,'Estimated Arrival','["NA"]','SIMPLE_TEXT',false,false,'estimatedArrival','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',14,'STRING',NULL,true,true,true,'Rail/Vessel','["NA"]','SIMPLE_TEXT',false,true,'railOrVessel','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',16,'STRING',NULL,true,true,true,'Comments','["NA"]','SIMPLE_TEXT',false,false,'comments','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',17,'STRING',NULL,true,true,true,'Status','["NA"]','SIMPLE_TEXT',false,false,'rowStatus','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',5,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,false,'depot','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',6,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,false,'port','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',7,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,false,'country','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',8,'DATE',NULL,true,true,true,'Receipt Date','["NA"]','SIMPLE_TEXT',false,false,'receiptDate','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',9,'STRING',NULL,true,true,true,'Buyer','["NA"]','SIMPLE_TEXT',false,false,'buyer','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-31 15:01:01.0759','Admin',NULL,true,'2022-10-31 15:01:01.0759','Admin',10,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,false,'remark','Container',true,true,'STOP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',0,'STRING',NULL,true,true,true,'Container No.','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',1,'STRING',NULL,true,true,true,'Container Size/Type','["NA"]','SIMPLE_TEXT',false,true,'sizeType','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',2,'STRING',NULL,true,true,true,'Status','["NA"]','SIMPLE_TEXT',false,false,'status','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',3,'STRING',NULL,true,true,true,'In/Out','["NA"]','SIMPLE_TEXT',false,true,'inOutStatus','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',4,'STRING',NULL,true,true,true,'Full/Empty','["NA"]','SIMPLE_TEXT',false,true,'fullEmptyStatus','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',5,'DATE',NULL,true,true,true,'Status Date','["NA"]','SIMPLE_TEXT',false,true,'statusDate','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',6,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',7,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',8,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'country','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',9,'STRING',NULL,true,true,true,'Lessor Contract No.','["NA"]','SIMPLE_TEXT',false,true,'lessorContractNumber','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',10,'STRING',NULL,true,true,true,'Lessor','["NA"]','SIMPLE_TEXT',false,true,'lessorCode','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',0,'STRING',NULL,true,true,true,'Contract No.','["NA"]','SIMPLE_TEXT',false,true,'contractNumber','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',1,'DATE',NULL,true,true,true,'Contract Date','["NA"]','SIMPLE_TEXT',false,true,'contractDate','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',2,'STRING',NULL,true,true,true,'Lease Type','["NA"]','SIMPLE_TEXT',false,true,'leaseType','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',3,'STRING',NULL,true,true,true,'Size/Type','["NA"]','SIMPLE_TEXT',false,true,'containerType','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',4,'INTEGER',NULL,true,true,true,'Qty.','["NA"]','SIMPLE_TEXT',false,true,'contractContainerCount','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',5,'STRING',NULL,true,true,true,'Ref. No.','["NA"]','SIMPLE_TEXT',false,true,'referenceNumber','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',6,'STRING',NULL,true,true,true,'Input ID','["NA"]','SIMPLE_TEXT',false,true,'createdBy','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',7,'DATE',NULL,true,true,true,'Input time','["NA"]','SIMPLE_TEXT',false,true,'created','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',8,'STRING',NULL,true,true,true,'Update ID','["NA"]','SIMPLE_TEXT',false,true,'updatedBy','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.857379','Admin',NULL,true,'2022-10-28 11:28:21.857379','Admin',9,'DATE',NULL,true,true,true,'Update time','["NA"]','SIMPLE_TEXT',false,true,'updated','Contract',true,true,'CONTRACT_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',0,'STRING',NULL,true,true,true,'Contract No.','["NA"]','SIMPLE_TEXT',false,true,'contractNumber','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',1,'STRING',NULL,true,true,true,'Container No.','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',2,'STRING',NULL,true,true,true,'Size/Type','["NA"]','SIMPLE_TEXT',false,true,'sizeType','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',0,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Lump',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',1,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'country','Lump',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',2,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Lump',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',3,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Container',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',4,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Container',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',5,'STRING',NULL,true,true,true,'In/Out','["NA"]','SIMPLE_TEXT',false,true,'inOutStatus','Lump',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',8,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,false,'remark','Lump',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',0,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Lump',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',1,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'country','Lump',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',2,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Lump',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',3,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Container',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',4,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Container',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',5,'STRING',NULL,true,true,true,'In/Out','["NA"]','SIMPLE_TEXT',false,true,'inOutStatus','Lump',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',6,'STRING',NULL,true,true,true,'Full/Empty','["NA"]','SIMPLE_TEXT',false,true,'fullEmptyStatus','Lump',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',8,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,false,'remark','Lump',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',9,'STRING',NULL,true,true,true,'Comments','["NA"]','SIMPLE_TEXT',false,false,'comments','Movement',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',10,'STRING',NULL,true,true,true,'Status','["NA"]','SIMPLE_TEXT',false,false,'rowStatus','Movement',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',6,'STRING',NULL,true,true,true,'Full/Empty','["NA"]','SIMPLE_TEXT',false,true,'fullEmptyStatus','Lump',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',11,'STRING',NULL,true,true,true,'Lessee Contract No.','["NA"]','SIMPLE_TEXT',false,true,'lesseeContractNumber','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',4,'STRING',NULL,true,true,true,'Lease Type','["NA"]','SIMPLE_TEXT',false,true,'leaseType','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',5,'STRING',NULL,true,true,true,'Start Type','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.containerStatus','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',6,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.depot','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',7,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.port','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',8,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.country','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',9,'DECIMAL',NULL,true,true,true,'Start Price','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.price','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.388','Admin',NULL,true,'2022-10-28 11:28:22.388','Admin',12,'STRING',NULL,true,true,true,'Lessee','["NA"]','SIMPLE_TEXT',false,true,'lesseeCode','Stock',true,true,'STOCK_INQUIRY_CONTAINER_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',11,'STRING',NULL,true,true,true,'Input ID','["NA"]','SIMPLE_TEXT',false,true,'createdBy','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',12,'DATE',NULL,true,true,true,'Input Time','["NA"]','SIMPLE_TEXT',false,true,'createdDate','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',13,'STRING',NULL,true,true,true,'Update ID','["NA"]','SIMPLE_TEXT',false,true,'updatedBy','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',14,'DATE',NULL,true,true,true,'Update Time','["NA"]','SIMPLE_TEXT',false,true,'updateDate','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',0,'STRING',NULL,true,true,true,'Contract Number','["NA"]','SIMPLE_TEXT',false,true,'contractNumber','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',1,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',2,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'containerStatus','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',3,'STRING',NULL,true,true,true,'H.B/L No','["NA"]','SIMPLE_TEXT',false,true,'hblNumber','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',4,'STRING',NULL,true,true,true,'Vessel','["NA"]','SIMPLE_TEXT',false,true,'vessel','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',5,'STRING',NULL,true,true,true,'Voyage','["NA"]','SIMPLE_TEXT',false,true,'voyage','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',7,'DATE',NULL,true,true,true,'Estimated Arrival','["NA"]','SIMPLE_TEXT',false,true,'estimatedArrival','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',8,'STRING',NULL,true,true,true,'Full/Empty','["NA"]','SIMPLE_TEXT',false,true,'fullEmptyStatus','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',9,'STRING',NULL,true,true,true,'Rail/Vessel','["NA"]','SIMPLE_TEXT',false,true,'railOrVessel','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',10,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',11,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',12,'STRING',NULL,true,true,true,'Customer Name','["NA"]','SIMPLE_TEXT',false,true,'customer','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',13,'STRING',NULL,true,true,true,'Remark','["NA"]','SIMPLE_TEXT',false,true,'remark','Container',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',14,'STRING',NULL,true,true,true,'Input ID','["NA"]','SIMPLE_TEXT',false,true,'createdBy','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',15,'DATE',NULL,true,true,true,'Input time','["NA"]','SIMPLE_TEXT',false,true,'created','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',16,'STRING',NULL,true,true,true,'Update ID','["NA"]','SIMPLE_TEXT',false,true,'updatedBy','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',17,'DATE',NULL,true,true,true,'Update time','["NA"]','SIMPLE_TEXT',false,true,'updated','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',18,'STRING',NULL,true,true,true,'In/Out','["NA"]','SIMPLE_TEXT',false,true,'inOutStatus','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',0,'DATE',NULL,true,true,true,'Stop Date','["NA"]','SIMPLE_TEXT',false,true,'stopDate','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',2,'INTEGER',NULL,true,true,true,'Date Diff','["NA"]','SIMPLE_TEXT',false,true,'dateDiff','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',3,'STRING',NULL,true,true,true,'Contract No.','["NA"]','SIMPLE_TEXT',false,true,'contractNumber','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',4,'STRING',NULL,true,true,true,'Container No.','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',5,'STRING',NULL,true,true,true,'Start Type','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.containerStatus','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',6,'STRING',NULL,true,true,true,'Start Depot','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.depot','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',7,'STRING',NULL,true,true,true,'Start Port','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.port','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',8,'STRING',NULL,true,true,true,'Start Country','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.country','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',9,'DECIMAL',NULL,true,true,true,'Start Price','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.price','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',10,'STRING',NULL,true,true,true,'Release No','["NA"]','SIMPLE_TEXT',false,true,'stopContainerContractInfo.releaseNumber','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',11,'STRING',NULL,true,true,true,'Stop Depot','["NA"]','SIMPLE_TEXT',false,true,'stopContainerContractInfo.depot','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',12,'STRING',NULL,true,true,true,'Stop Port','["NA"]','SIMPLE_TEXT',false,true,'stopContainerContractInfo.port','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',13,'STRING',NULL,true,true,true,'Stop Country','["NA"]','SIMPLE_TEXT',false,true,'stopContainerContractInfo.country','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',14,'DECIMAL',NULL,true,true,true,'Stop Price','["NA"]','SIMPLE_TEXT',false,true,'stopContainerContractInfo.price','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',15,'STRING',NULL,true,true,true,'Customer','["NA"]','SIMPLE_TEXT',false,true,'stopContainerContractInfo.customer','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',16,'STRING',NULL,true,true,true,'Input ID','["NA"]','SIMPLE_TEXT',false,true,'createdBy','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',17,'DATE',NULL,true,true,true,'Input Time','["NA"]','SIMPLE_TEXT',false,true,'createdDate','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',18,'STRING',NULL,true,true,true,'Update ID','["NA"]','SIMPLE_TEXT',false,true,'updatedBy','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',19,'DATE',NULL,true,true,true,'Update time','["NA"]','SIMPLE_TEXT',false,true,'updateDate','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',4,'DATE',NULL,true,true,true,'Start Date','["NA"]','SIMPLE_TEXT',false,true,'startDate','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',4,'DATE',NULL,true,true,true,'Start Date','["NA"]','SIMPLE_TEXT',false,true,'startDate','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',11,'DATE',NULL,true,true,true,'Status Time','["NA"]','SIMPLE_TEXT',false,true,'statusTime','Movement',true,true,'MOVEMENT_LOG_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-28 11:28:21.247966','Admin',NULL,true,'2022-10-28 11:28:21.247966','Admin',11,'DATE',NULL,true,true,true,'Status Time','["NA"]','SIMPLE_TEXT',false,true,'statusTime','Movement',true,true,'MOVEMENT_LOG_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.669844','Admin',NULL,true,'2022-10-28 11:28:21.669844','Admin',7,'DATE',NULL,true,true,true,'Status Time','["NA"]','SIMPLE_TEXT',false,true,'statusTime','Lump',true,true,'LUMP_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.763642','Admin',NULL,true,'2022-10-28 11:28:21.763642','Admin',7,'DATE',NULL,true,true,true,'Status Time','["NA"]','SIMPLE_TEXT',false,true,'statusTime','Lump',true,true,'LUMP_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',3,'DATE',NULL,true,true,true,'Start Date','["NA"]','SIMPLE_TEXT',false,true,'startDate','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.951106','Admin',NULL,true,'2022-10-28 11:28:21.951106','Admin',10,'DATE',NULL,true,true,true,'Payment Date','["NA"]','SIMPLE_TEXT',false,true,'startContainerContractInfo.paymentDate','Container',true,true,'START_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',10,'DATE',NULL,true,true,true,'Updated Time','["NA"]','SIMPLE_TEXT',false,true,'update','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.279226','Admin',NULL,true,'2022-10-28 11:28:22.279226','Admin',6,'DATE',NULL,true,true,true,'Status Time','["NA"]','SIMPLE_TEXT',false,true,'statusTime','Movement',true,true,'CONTAINER_UPDATES_ENQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:22.48235','Admin',NULL,true,'2022-10-28 11:28:22.48235','Admin',1,'DATE',NULL,true,true,true,'Start Date','["NA"]','SIMPLE_TEXT',false,true,'startDate','Container',true,true,'STOP_CONTAINER_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:20.8261','Admin',NULL,true,'2022-10-28 11:28:20.8261','Admin',9,'DATE',NULL,true,true,true,'Payment Date','["NA"]','SIMPLE_TEXT',false,true,'paymentDate','Container',true,true,'START_CONTAINER_TEMPLATE',false,NULL),
	 ('2022-10-28 11:28:21.15427','Admin',NULL,true,'2022-10-28 11:28:21.15427','Admin',9,'DATE',NULL,true,true,true,'Payment Date','["NA"]','SIMPLE_TEXT',false,true,'paymentDate','Container',true,true,'START_CONTAINER_TEMPLATE_UPLOADED_DATA_DOWNLOAD',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',0,'STRING',NULL,true,true,true,'Container Number','["NA"]','SIMPLE_TEXT',false,true,'containerNumber','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',1,'INTEGER',NULL,true,true,true,'Idling Days','["NA"]','SIMPLE_TEXT',false,true,'idlingDays','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',3,'STRING',NULL,true,true,true,'Container Status','["NA"]','SIMPLE_TEXT',false,true,'status','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',4,'STRING',NULL,true,true,true,'In/Out','["NA"]','SIMPLE_TEXT',false,true,'inOutStatus','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',7,'STRING',NULL,true,true,true,'Depot','["NA"]','SIMPLE_TEXT',false,true,'depot','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',8,'STRING',NULL,true,true,true,'Port','["NA"]','SIMPLE_TEXT',false,true,'port','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',9,'STRING',NULL,true,true,true,'Country','["NA"]','SIMPLE_TEXT',false,true,'country','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',14,'STRING',NULL,true,true,true,'Lessor Contract No','["NA"]','SIMPLE_TEXT',false,true,'lessorContractNumber','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',15,'STRING',NULL,true,true,true,'Lessee Contract No','["NA"]','SIMPLE_TEXT',false,true,'lesseeContractNumber','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',16,'STRING',NULL,true,true,true,'Container Lessor','["NA"]','SIMPLE_TEXT',false,true,'lessorCode','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',12,'STRING',NULL,true,true,true,'Updated By','["NA"]','SIMPLE_TEXT',false,true,'updatedBy','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',13,'STRING',NULL,true,true,true,'Created By','["NA"]','SIMPLE_TEXT',false,true,'createdBy','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',11,'DATE',NULL,true,true,true,'Created Time','["NA"]','SIMPLE_TEXT',false,true,'created','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL);
INSERT INTO column_config (created,created_by,extra_params,is_active,updated,updated_by,column_index,data_type,description,editable,enabled_excel_download,enabled_for_upload,excel_column_name,influencing_columns_path,input_type,is_influencing_columns_path_enabled,is_mandatory,mapped_column_name,section_code,setter_enabled,show_on_ui,template_type,use_values,"values") VALUES
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',6,'DATE',NULL,true,true,true,'Status Time','["NA"]','SIMPLE_TEXT',false,true,'statusTime','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',5,'STRING',NULL,true,true,true,'F/E','["NA"]','SIMPLE_TEXT',false,true,'fullEmptyStatus','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL),
	 ('2022-10-31 14:32:15.96077','Admin',NULL,true,'2022-10-31 14:32:15.96077','Admin',2,'STRING',NULL,true,true,true,'Container Size','["NA"]','SIMPLE_TEXT',false,true,'size','Idling',true,true,'IDLING_INQUIRY_DOWNLOAD_TEMPLATE',false,NULL);


INSERT INTO resource_permission (created,created_by,extra_params,is_active,updated,updated_by,"permission",resource,"type") VALUES
	 ('2022-10-31 18:17:06.505834','Admin','{}',true,'2022-10-31 18:17:06.505834','Admin','CMS:CONTRACT:CONTRACT_ENTRY','/v1/contracts','POST'),
	 ('2022-10-31 18:17:06.505834','Admin','{}',true,'2022-10-31 18:17:06.505834','Admin','CMS:CONTRACT:CONTRACT_ENTRY','/v1/master-data/types','GET'),
	 ('2022-10-31 18:17:06.505834','Admin','{}',true,'2022-10-31 18:17:06.505834','Admin','CMS:CONTRACT:CONTRACT_ENTRY','/v1/master-data/types/{type}/values','GET'),
	 ('2022-10-31 18:17:06.56834','Admin','{}',true,'2022-10-31 18:17:06.56834','Admin','CMS:CONTRACT:CONTRACT_ENQUIRY','/v1/contracts/enquiry','GET'),
	 ('2022-10-31 18:17:06.56834','Admin','{}',true,'2022-10-31 18:17:06.56834','Admin','CMS:CONTRACT:CONTRACT_ENQUIRY','/v1/contracts/enquiry/download','GET'),
	 ('2022-10-31 18:17:06.56834','Admin','{}',true,'2022-10-31 18:17:06.56834','Admin','CMS:CONTRACT:CONTRACT_ENQUIRY','/v1/master-data/types','GET'),
	 ('2022-10-31 18:17:06.56834','Admin','{}',true,'2022-10-31 18:17:06.56834','Admin','CMS:CONTRACT:CONTRACT_ENQUIRY','/v1/master-data/types/{type}/values','GET'),
	 ('2022-10-31 18:17:06.630882','Admin','{}',true,'2022-10-31 18:17:06.630882','Admin','CMS:CONTRACT:STOP_CONTAINER_INQUIRY','/v1/contracts/containers/containerInquiry','POST'),
	 ('2022-10-31 18:17:06.630882','Admin','{}',true,'2022-10-31 18:17:06.630882','Admin','CMS:CONTRACT:STOP_CONTAINER_INQUIRY','/v1/contracts/containers/containerInquiry/download','POST'),
	 ('2022-10-31 18:17:06.630882','Admin','{}',true,'2022-10-31 18:17:06.630882','Admin','CMS:CONTRACT:STOP_CONTAINER_INQUIRY','/v1/master-data/types','GET');
INSERT INTO resource_permission (created,created_by,extra_params,is_active,updated,updated_by,"permission",resource,"type") VALUES
	 ('2022-10-31 18:17:06.630882','Admin','{}',true,'2022-10-31 18:17:06.630882','Admin','CMS:CONTRACT:STOP_CONTAINER_INQUIRY','/v1/master-data/types/{type}/values','GET'),
	 ('2022-10-31 18:17:06.693448','Admin','{}',true,'2022-10-31 18:17:06.693448','Admin','CMS:CONTRACT:START_CONTAINER_INQUIRY','/v1/contracts/containers/containerInquiry','POST'),
	 ('2022-10-31 18:17:06.693448','Admin','{}',true,'2022-10-31 18:17:06.693448','Admin','CMS:CONTRACT:START_CONTAINER_INQUIRY','/v1/contracts/containers/containerInquiry/download','POST'),
	 ('2022-10-31 18:17:06.693448','Admin','{}',true,'2022-10-31 18:17:06.693448','Admin','CMS:CONTRACT:START_CONTAINER_INQUIRY','/v1/master-data/types','GET'),
	 ('2022-10-31 18:17:06.693448','Admin','{}',true,'2022-10-31 18:17:06.693448','Admin','CMS:CONTRACT:START_CONTAINER_INQUIRY','/v1/master-data/types/{type}/values','GET'),
	 ('2022-10-31 18:17:06.755915','Admin','{}',true,'2022-10-31 18:17:06.755915','Admin','CMS:CONTRACT:START_CONTAINER','/v1/contracts','GET'),
	 ('2022-10-31 18:17:06.755915','Admin','{}',true,'2022-10-31 18:17:06.755915','Admin','CMS:CONTRACT:START_CONTAINER','/v1/contracts/containers/start','POST'),
	 ('2022-10-31 18:17:06.755915','Admin','{}',true,'2022-10-31 18:17:06.755915','Admin','CMS:CONTRACT:START_CONTAINER','/v1/master-data/types','GET'),
	 ('2022-10-31 18:17:06.755915','Admin','{}',true,'2022-10-31 18:17:06.755915','Admin','CMS:CONTRACT:START_CONTAINER','/v1/master-data/types/{type}/values','GET'),
	 ('2022-10-31 18:17:06.818345','Admin','{}',true,'2022-10-31 18:17:06.818345','Admin','CMS:CONTRACT:START_CONTAINER_BULK','/v1/templates/all','GET');
INSERT INTO resource_permission (created,created_by,extra_params,is_active,updated,updated_by,"permission",resource,"type") VALUES
	 ('2022-10-31 18:17:06.818345','Admin','{}',true,'2022-10-31 18:17:06.818345','Admin','CMS:CONTRACT:START_CONTAINER_BULK','/v1/files/upload','POST'),
	 ('2022-10-31 18:17:06.880855','Admin','{}',true,'2022-10-31 18:17:06.880855','Admin','CMS:CONTRACT:STOP_CONTAINER','/v1/contracts/containers/stop','POST'),
	 ('2022-10-31 18:17:06.880855','Admin','{}',true,'2022-10-31 18:17:06.880855','Admin','CMS:CONTRACT:STOP_CONTAINER','/v1/master-data/types','GET'),
	 ('2022-10-31 18:17:06.880855','Admin','{}',true,'2022-10-31 18:17:06.880855','Admin','CMS:CONTRACT:STOP_CONTAINER','/v1/master-data/types/{type}/values','GET'),
	 ('2022-10-31 18:17:06.943357','Admin','{}',true,'2022-10-31 18:17:06.943357','Admin','CMS:CONTRACT:STOP_CONTAINER_BULK','/v1/templates/all','GET'),
	 ('2022-10-31 18:17:06.943357','Admin','{}',true,'2022-10-31 18:17:06.943357','Admin','CMS:CONTRACT:STOP_CONTAINER_BULK','/v1/files/upload','POST'),
	 ('2022-10-31 18:17:07.005848','Admin','{}',true,'2022-10-31 18:17:07.005848','Admin','CMS:CONTAINER_MOVEMENT:MOVEMENT_LUMP_BULK_UPLOAD','/v1/templates/all','GET'),
	 ('2022-10-31 18:17:07.005848','Admin','{}',true,'2022-10-31 18:17:07.005848','Admin','CMS:CONTAINER_MOVEMENT:MOVEMENT_LUMP_BULK_UPLOAD','/v1/files/upload','POST'),
	 ('2022-10-31 18:17:07.068342','Admin','{}',true,'2022-10-31 18:17:07.068342','Admin','CMS:CONTAINER_MOVEMENT:MOVEMENT_LOG','/v1/containers/updates','GET'),
	 ('2022-10-31 18:17:07.068342','Admin','{}',true,'2022-10-31 18:17:07.068342','Admin','CMS:CONTAINER_MOVEMENT:MOVEMENT_LOG','/v1/containers/updates/download','GET');
INSERT INTO resource_permission (created,created_by,extra_params,is_active,updated,updated_by,"permission",resource,"type") VALUES
	 ('2022-10-31 18:17:07.130847','Admin','{}',true,'2022-10-31 18:17:07.130847','Admin','CMS:CONTAINER_MOVEMENT:LOG_DELETE_MOVEMENT','/v1/containers/updates','GET'),
	 ('2022-10-31 18:17:07.130847','Admin','{}',true,'2022-10-31 18:17:07.130847','Admin','CMS:CONTAINER_MOVEMENT:LOG_DELETE_MOVEMENT','/v1/containers/updates','DELETE'),
	 ('2022-10-31 18:17:07.193341','Admin','{}',true,'2022-10-31 18:17:07.193341','Admin','CMS:CONTAINER_MOVEMENT:STOCK_INQUIRY','/v1/containers/inventory','GET'),
	 ('2022-10-31 18:17:07.193341','Admin','{}',true,'2022-10-31 18:17:07.193341','Admin','CMS:CONTAINER_MOVEMENT:STOCK_INQUIRY','/v1/contracts/containers/containerInquiry/download','POST'),
	 ('2022-10-31 18:17:07.302717','Admin','{}',true,'2022-10-31 18:17:07.302717','Admin','CMS:UPLOAD_STATUS','/v1/files/stats','POST'),
	 ('2022-10-31 18:17:07.302717','Admin','{}',true,'2022-10-31 18:17:07.302717','Admin','CMS:UPLOAD_STATUS','/v1/templates/all','GET'),
	 ('2022-10-31 18:17:07.302717','Admin','{}',true,'2022-10-31 18:17:07.302717','Admin','CMS:UPLOAD_STATUS','/v1/files/records','GET'),
	 ('2022-10-31 18:17:07.302717','Admin','{}',true,'2022-10-31 18:17:07.302717','Admin','CMS:UPLOAD_STATUS','/v1/files/download','GET'),
	 ('2022-10-31 18:17:07.302717','Admin','{}',true,'2022-10-31 18:17:07.302717','Admin','CMS:UPLOAD_STATUS','/v1/files','GET'),
	 ('2022-10-31 18:17:07.302717','Admin','{}',true,'2022-10-31 18:17:07.302717','Admin','CMS:UPLOAD_STATUS','/v1/files/','GET');
INSERT INTO resource_permission (created,created_by,extra_params,is_active,updated,updated_by,"permission",resource,"type") VALUES
	 ('2022-10-31 18:17:07.255846','Admin','{}',true,'2022-10-31 18:17:07.255846','Admin','CMS:CONTAINER_MOVEMENT:IDLING_INQUIRY','/v1/idling/inquiry','POST'),
	 ('2022-10-31 18:17:07.255846','Admin','{}',true,'2022-10-31 18:17:07.255846','Admin','CMS:CONTAINER_MOVEMENT:IDLING_INQUIRY','/v1/idling/inquiry/download','POST'),
	 ('2022-10-31 18:17:06.818345','Admin','{}',true,'2022-10-31 18:17:06.818345','Admin','CMS:CONTRACT:START_CONTAINER_BULK','/v1/templates','GET'),
	 ('2022-10-31 18:17:06.943357','Admin','{}',true,'2022-10-31 18:17:06.943357','Admin','CMS:CONTRACT:STOP_CONTAINER_BULK','/v1/templates','GET'),
	 ('2022-10-31 18:17:07.005848','Admin','{}',true,'2022-10-31 18:17:07.005848','Admin','CMS:CONTAINER_MOVEMENT:MOVEMENT_LUMP_BULK_UPLOAD','/v1/templates','GET'),
	 ('2022-10-31 18:17:06.818','Admin','{}',true,'2022-10-31 18:17:06.818','Admin','CMS:CONTRACT:START_CONTAINER','/v1/templates/all','GET'),
	 ('2022-10-31 18:17:06.818','Admin','{}',true,'2022-10-31 18:17:06.818','Admin','CMS:CONTRACT:START_CONTAINER','/v1/templates','GET'),
	 ('2022-10-31 18:17:06.818','Admin','{}',true,'2022-10-31 18:17:06.818','Admin','CMS:CONTRACT:START_CONTAINER','/v1/files/upload','POST'),
	 ('2022-10-31 18:17:06.943','Admin','{}',true,'2022-10-31 18:17:06.943','Admin','CMS:CONTRACT:STOP_CONTAINER','/v1/templates/all','GET'),
	 ('2022-10-31 18:17:06.943','Admin','{}',true,'2022-10-31 18:17:06.943','Admin','CMS:CONTRACT:STOP_CONTAINER','/v1/templates','GET');

INSERT INTO resource_permission (created,created_by,extra_params,is_active,updated,updated_by,"permission",resource,"type") VALUES
	 ('2022-10-31 18:17:06.943','Admin','{}',true,'2022-10-31 18:17:06.943','Admin','CMS:CONTRACT:STOP_CONTAINER','/v1/files/upload','POST');


-- CONTAINER MOVEMENT UPDATE SEEDS

INSERT INTO contract (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, alt_standards, build_down_scale, contract_date, contract_number, contract_type, depreciation_rate, di_fee, dispute_resolution, effective_date, empty_redelivery, equipment_quantity, est_term_date, estimated_resp_days, expire_date, free_days, general_minimum_limit, interest_rate, lease_date, lease_type, lessee_code, lessor_code, lift_off_charger, lift_on_charger, lump_sum, min_days, objection_notice, other_charges, payment_days, reference_number, repairs_notice, replacement_value, repos_notice, term_notice) VALUES
	('2022-10-31 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-10-31 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-30 18:30:00.000', 'Ds_Contract_0099', 'LO', NULL, NULL, NULL, '2022-10-01 18:30:00.000', NULL, 3, NULL, NULL, '2022-11-29 18:30:00.000', NULL, NULL, NULL, '2022-10-02 18:30:00.000', 'ST', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-10-31 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-10-31 07:27:11.778', 'Admin', NULL, '666', '666', NULL, NULL, '2022-09-30 18:30:00.000', 'Ds_Contract_0099', 'LO', NULL, NULL, NULL, '2022-10-01 18:30:00.000', NULL, 2, NULL, NULL, '2022-11-29 18:30:00.000', NULL, NULL, NULL, '2022-10-02 18:30:00.000', 'ST', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL);


INSERT INTO contract_container_inventory (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_type, contract_container_count, contract_number)VALUES
	('2022-10-31 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-10-31 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 3, 'Ds_Contract_0099'),
	('2022-10-31 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-10-31 07:27:11.782', 'Admin', NULL, '666', '666', '20DV', 2, 'Ds_Contract_0099');


INSERT INTO container (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_number, container_type, contract_number, max_cargo_weight, remark, tare_weight, vintage) VALUES
	('2022-10-31 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-10-31 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_00100', '20DV', 'Ds_Contract_0099', NULL, NULL, NULL, NULL),
	('2022-10-31 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-10-31 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_00101', '20DV', 'Ds_Contract_0099', NULL, NULL, NULL, NULL),
	('2022-10-31 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-10-31 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_00102', '20DV', 'Ds_Contract_0099', NULL, NULL, NULL, NULL),
	('2022-10-31 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-10-31 07:32:02.850', 'Admin', NULL, '666', '666', 'Ds_Container_00110', '20DV', 'Ds_Contract_0099', NULL, NULL, NULL, NULL);

INSERT INTO contract_container (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_number, container_status, container_type, contract_number, country, depot, full_empty_status, hbl_number, in_out_status, lessee_code, port, seller_amount, start_date, status_date, stop_date, vessel, vessel_number) VALUES
	('2022-10-31 07:32:02.820', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1664735400, "containerStatus": "ONH"}}'::jsonb, true, '2022-10-31 07:34:46.135', '123', NULL, '777', '777', 'Ds_Container_00100', 'ONH', '20DV', 'Ds_Contract_0099', 'India', 'Depot1', 'FULL', NULL, 'IN', 'Lesse01', 'Port1', 110.00, '2022-10-03 18:30:00.000', '2022-10-29 00:00:00.000', NULL, NULL, NULL),
	('2022-10-31 07:32:02.820', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1664735400, "containerStatus": "LIO"}}'::jsonb, true, '2022-10-31 07:34:46.135', '123', NULL, '777', '777', 'Ds_Container_00101', 'LIO', '20DV', 'Ds_Contract_0099', 'India', 'Depot1', 'FULL', NULL, 'IN', 'Lesse01', 'Port1', 110.00, '2022-10-03 18:30:00.000', '2022-10-29 00:00:00.000', NULL, NULL, NULL),
	('2022-10-31 07:32:02.820', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1664735400, "containerStatus": "LIO"}}'::jsonb, true, '2022-10-31 07:34:46.135', '123', NULL, '777', '777', 'Ds_Container_00102', 'LIO', '20DV', 'Ds_Contract_0099', 'India', 'Depot1', 'FULL', NULL, 'IN', 'Lesse01', 'Port1', 110.00, '2022-10-03 18:30:00.000', '2022-10-29 00:00:00.000', NULL, NULL, NULL),
	('2022-10-31 07:32:02.820', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1664735400, "containerStatus": "ONH"}}'::jsonb, true, '2022-10-31 07:34:46.135', '123', NULL, '666', '666', 'Ds_Container_00110', 'ONH', '20DV', 'Ds_Contract_0099', 'India', 'Depot1', 'FULL', NULL, 'IN', 'Lesse01', 'Port1', 110.00, '2022-10-03 18:30:00.000', '2022-10-29 00:00:00.000', NULL, NULL, NULL);

INSERT INTO container_update ( created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_number, container_status, contract_number, country, customer, depot, estimated_arrival, full_empty_status, hbl_number, in_out_status, port, rail_or_vessel, release_number, remark, status_time, vessel, vessel_number, voyage) VALUES
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'Dubai', 'sample_cus', 'Depot_0001', NULL, 'EMPTY', 'HBL0001', 'IN',  'Port_0001', 'VESSEL', 'REL0001', 'Remark0001', '2022-10-03 18:30:00.000', 'VES_001', 'Ves_No_0001', 'Voy_0001'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0002', NULL, 'FULL',  'HBL0002', 'OUT', 'Port_0002', 'VESSEL', 'REL0002', 'Remark0002', '2022-10-03 17:30:00.000', 'VES_002', 'Ves_No_0002', 'Voy_0002'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0003', NULL, 'EMPTY', 'HBL0003', 'IN',  'Port_0003', 'VESSEL', 'REL0003', 'Remark0003', '2022-10-03 16:30:00.000', 'VES_003', 'Ves_No_0003', 'Voy_0003'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'USA',   'sample_cus', 'Depot_0004', NULL, 'FULL',  'HBL0004', 'OUT', 'Port_0004', 'VESSEL', 'REL0004', 'Remark0004', '2022-10-03 15:30:00.000', 'VES_004', 'Ves_No_0004', 'Voy_0004'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'USA',   'sample_cus', 'Depot_0005', NULL, 'EMPTY', 'HBL0005', 'IN',  'Port_0005', 'VESSEL', 'REL0005', 'Remark0005', '2022-10-03 14:30:00.000', 'VES_005', 'Ves_No_0005', 'Voy_0005'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0006', NULL, 'FULL',  'HBL0005', 'OUT', 'Port_0006', 'VESSEL', 'REL0006', 'Remark0006', '2022-10-03 12:30:00.000', 'VES_006', 'Ves_No_0006', 'Voy_0006'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0007', NULL, 'FULL',  'HBL0005', 'IN', 'Port_0007', 'VESSEL', 'REL0006', 'Remark0006', '2022-10-03 11:30:00.000', 'VES_006', 'Ves_No_0006', 'Voy_0006'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'Dubai', 'sample_cus', 'Depot_0008', NULL, 'FULL',  'HBL0005', 'IN', 'Port_0008', 'VESSEL', 'REL0006', 'Remark0006', '2022-10-03 10:30:00.000', 'VES_006', 'Ves_No_0006', 'Voy_0006'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0009', NULL, 'FULL',  'HBL0005', 'IN', 'Port_0009', 'VESSEL', 'REL0006', 'Remark0006', '2022-10-03 09:30:00.000', 'VES_006', 'Ves_No_0006', 'Voy_0006'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00100', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0009', NULL, 'FULL',  'HBL0005', 'IN', 'Port_00010', 'VESSEL', 'REL0006', 'Remark0006', '2022-10-03 08:30:00.000', 'VES_006', 'Ves_No_0006', 'Voy_0006'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00101', 'LIO', 'Ds_Contract_0099', 'Dubai', 'sample_cus', 'Depot_0001', '2022-10-04 17:30:00.000', 'EMPTY', 'HBL0001', 'IN',  'Port_0001', 'VESSEL', 'REL0001', 'Remark0001', '2022-10-03 18:30:00.000', 'VES_001', 'Ves_No_0001', 'Voy_0001'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00101', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0002', '2022-10-04 17:30:00.000', 'FULL',  'HBL0002', 'OUT', 'Port_0002', 'VESSEL', 'REL0002', 'Remark0002', '2022-10-03 17:30:00.000', 'VES_002', 'Ves_No_0002', 'Voy_0002'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00102', 'LIO', 'Ds_Contract_0099', 'Dubai', 'sample_cus', 'Depot_0001', NULL, 'EMPTY', 'HBL0001', 'IN',  'Port_0001', 'VESSEL', 'REL0001', 'Remark0001', '2022-10-03 18:30:00.000', 'VES_001', 'Ves_No_0001', 'Voy_0001'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00102', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0002', NULL, 'FULL',  'HBL0002', 'OUT', 'Port_0002', 'VESSEL', 'REL0002', 'Remark0002', '2022-10-03 17:30:00.000', 'VES_002', 'Ves_No_0002', 'Voy_0002'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '777', '777', 'Ds_Container_00102', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0003', NULL, 'EMPTY', 'HBL0003', 'IN',  'Port_0003', 'VESSEL', 'REL0003', 'Remark0003', '2022-10-03 16:30:00.000', 'VES_003', 'Ves_No_0003', 'Voy_0003'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '666', '666', 'Ds_Container_00110', 'LIO', 'Ds_Contract_0099', 'Dubai', 'sample_cus', 'Depot_0001', NULL, 'EMPTY', 'HBL0001', 'IN',  'Port_0001', 'VESSEL', 'REL0001', 'Remark0001', '2022-10-03 18:30:00.000', 'VES_001', 'Ves_No_0001', 'Voy_0001'),
	('2022-10-28 09:50:39.201', 'Admin', '{}'::jsonb, true, '2022-10-28 09:50:39.201', 'Admin', 'pune', '666', '666', 'Ds_Container_00110', 'LIO', 'Ds_Contract_0099', 'India', 'sample_cus', 'Depot_0002', NULL, 'FULL',  'HBL0002', 'OUT', 'Port_0002', 'VESSEL', 'REL0002', 'Remark0002', '2022-10-03 17:30:00.000', 'VES_002', 'Ves_No_0002', 'Voy_0002');
	

INSERT INTO app_config (created,created_by,extra_params,is_active,updated,updated_by,config_key,config_value,data_type,description,display_name) VALUES
	 ('2022-10-26 19:58:33.17','ADMIN',NULL,true,'2022-10-26 19:58:33.17','ADMIN','AUTH_ENABLED','false','BOOLEAN',NULL,NULL),
	 ('2022-11-04 15:08:10.150313','Admin','{}',true,'2022-11-04 15:08:10.150313','Admin','CONTRACT_INQUIRY_SORT_FIELD','contract_date','STRING','CONTRACT_INQUIRY_SORT_FIELD','CONTRACT_INQUIRY_SORT_FIELD');


-- IDLING INQUIRY SEEDS

INSERT INTO contract (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, alt_standards, build_down_scale, contract_date, contract_number, contract_type, depreciation_rate, di_fee, dispute_resolution, effective_date, empty_redelivery, equipment_quantity, est_term_date, estimated_resp_days, expire_date, free_days, general_minimum_limit, interest_rate, lease_date, lease_type, lessee_code, lessor_code, lift_off_charger, lift_on_charger, lump_sum, min_days, objection_notice, other_charges, payment_days, reference_number, repairs_notice, replacement_value, repos_notice, term_notice) VALUES
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_LI_0080', 'LI', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', 'ST', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_LI_0081', 'LI', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', 'LT', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_LI_0082', 'LI', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', 'MT', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_LI_0083', 'LI', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', 'OW', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_LI_0084', 'LI', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', 'RT', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_LI_0085', 'LI', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', 'SL', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_OV_0086', 'OV', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', '2H', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_OV_0087', 'OV', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', 'LP', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-09-01 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-01 18:30:00.000', 'Ds_Contract_OV_0088', 'OV', NULL, NULL, NULL, '2022-09-01 18:30:00.000', NULL, 2, NULL, NULL, '2025-12-31 18:30:00.000', NULL, NULL, NULL, '2025-12-31 18:30:00.000', 'NW', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL);

INSERT INTO contract_container_inventory (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_type, contract_container_count, contract_number)VALUES
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_LI_0080'),
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_LI_0081'),
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_LI_0082'),
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_LI_0083'),
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_LI_0084'),
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_LI_0085'),
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_OV_0086'),
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_OV_0087'),
	('2022-09-01 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-09-01 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_OV_0088');


INSERT INTO container (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_number, container_type, contract_number, max_cargo_weight, remark, tare_weight, vintage) VALUES
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_ST_00200', '20DV', 'Ds_Contract_LI_0080', NULL, NULL, NULL, NULL),
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_LT_00201', '20DV', 'Ds_Contract_LI_0081', NULL, NULL, NULL, NULL),
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_MT_00202', '20DV', 'Ds_Contract_LI_0082', NULL, NULL, NULL, NULL),
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_OW_00203', '20DV', 'Ds_Contract_LI_0083', NULL, NULL, NULL, NULL),
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_RT_00204', '20DV', 'Ds_Contract_LI_0084', NULL, NULL, NULL, NULL),
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_SL_00205', '20DV', 'Ds_Contract_LI_0085', NULL, NULL, NULL, NULL),
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_2H_00206', '20DV', 'Ds_Contract_OV_0086', NULL, NULL, NULL, NULL),
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_LP_00207', '20DV', 'Ds_Contract_OV_0087', NULL, NULL, NULL, NULL),
	('2022-09-01 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-09-01 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_NW_00208', '20DV', 'Ds_Contract_OV_0088', NULL, NULL, NULL, NULL);

INSERT INTO contract_container (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_number, container_status, container_type, contract_number, country, depot, full_empty_status, hbl_number, in_out_status, lessee_code, port, seller_amount, start_date, status_date, stop_date, vessel, vessel_number) values
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_ST_00200', 'GAT', '20DV', 'Ds_Contract_LI_0080', 'India', 'Depot001', 'FULL', NULL, 'IN', 'Lesse01', 'Port001', 110.00, '2022-09-03 00:00:00.000', '2022-09-03 00:00:00.000', NULL, NULL, NULL),
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port002", "depot": "Depot002", "price": 110, "country": "India", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_LT_00201', 'VSL', '20DV', 'Ds_Contract_LI_0081', 'India', 'Depot002', 'EMPTY', NULL, 'IN', 'Lesse01', 'Port002', 110.00, '2022-09-03 00:00:00.000', '2022-09-04 00:00:00.000', NULL, NULL, NULL),
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port003", "depot": "Depot003", "price": 110, "country": "India", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_MT_00202', 'ONH', '20DV', 'Ds_Contract_LI_0082', 'India', 'Depot003', 'FULL', NULL, 'IN', 'Lesse01', 'Port003', 110.00, '2022-09-03 00:00:00.000', '2022-09-05 00:00:00.000', NULL, NULL, NULL),
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port004", "depot": "Depot004", "price": 110, "country": "India", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_OW_00203', 'STN', '20DV', 'Ds_Contract_LI_0083', 'India', 'Depot004', 'EMPTY', NULL, 'IN', 'Lesse01', 'Port004', 110.00, '2022-09-03 00:00:00.000', '2022-09-06 00:00:00.000', NULL, NULL, NULL),
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port005", "depot": "Depot005", "price": 110, "country": "Dubai", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_RT_00204', 'ONH', '20DV', 'Ds_Contract_LI_0084', 'Dubai', 'Depot005', 'FULL', NULL, 'IN', 'Lesse01', 'Port005', 110.00, '2022-09-03 00:00:00.000', '2022-09-07 00:00:00.000', NULL, NULL, NULL),
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port006", "depot": "Depot006", "price": 110, "country": "Dubai", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_SL_00205', 'ONH', '20DV', 'Ds_Contract_LI_0085', 'Dubai', 'Depot006', 'EMPTY', NULL, 'IN', 'Lesse01', 'Port006', 110.00, '2022-09-03 00:00:00.000', '2022-09-08 00:00:00.000', NULL, NULL, NULL),
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port007", "depot": "Depot007", "price": 110, "country": "Dubai", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_2H_00206', 'ONH', '20DV', 'Ds_Contract_OV_0086', 'Dubai', 'Depot007', 'FULL', NULL, 'IN', 'Lesse01', 'Port007', 110.00, '2022-09-03 00:00:00.000', '2022-09-09 00:00:00.000', NULL, NULL, NULL),
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port008", "depot": "Depot008", "price": 110, "country": "Dubai", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_LP_00207', 'ONH', '20DV', 'Ds_Contract_OV_0087', 'Dubai', 'Depot008', 'EMPTY', NULL, 'IN', 'Lesse01', 'Port008', 110.00, '2022-09-03 00:00:00.000', '2022-09-10 00:00:00.000', NULL, NULL, NULL),
	('2022-09-01 00:00:00.000', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port009", "depot": "Depot009", "price": 110, "country": "Dubai", "paymentDate": 1662229800, "containerStatus": "ONH"}}'::jsonb, true, '2022-09-03 00:00:00.000', '123', NULL, '777', '777', 'Ds_Container_NW_00208', 'ONH', '20DV', 'Ds_Contract_OV_0088', 'Dubai', 'Depot009', 'FULL', NULL, 'IN', 'Lesse01', 'Port009', 110.00, '2022-09-03 00:00:00.000', '2022-09-11 00:00:00.000', NULL, NULL, NULL);


-- MOVEMENT UPLOAD SEEDS -- Multi tenant

INSERT INTO contract (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, alt_standards, build_down_scale, contract_date, contract_number, contract_type, depreciation_rate, di_fee, dispute_resolution, effective_date, empty_redelivery, equipment_quantity, est_term_date, estimated_resp_days, expire_date, free_days, general_minimum_limit, interest_rate, lease_date, lease_type, lessee_code, lessor_code, lift_off_charger, lift_on_charger, lump_sum, min_days, objection_notice, other_charges, payment_days, reference_number, repairs_notice, replacement_value, repos_notice, term_notice) VALUES
	('2022-10-31 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-10-31 07:27:11.778', 'Admin', NULL, '777', '777', NULL, NULL, '2022-09-30 18:30:00.000', 'Ds_Contract_0090', 'LO', NULL, NULL, NULL, '2022-10-01 18:30:00.000', NULL, 2, NULL, NULL, '2022-11-29 18:30:00.000', NULL, NULL, NULL, '2022-10-02 18:30:00.000', 'ST', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL),
	('2022-10-31 07:27:11.778', 'Admin', '{}'::jsonb, true, '2022-10-31 07:27:11.778', 'Admin', NULL, '778', '778', NULL, NULL, '2022-09-30 18:30:00.000', 'Ds_Contract_0091', 'LO', NULL, NULL, NULL, '2022-10-01 18:30:00.000', NULL, 2, NULL, NULL, '2022-11-29 18:30:00.000', NULL, NULL, NULL, '2022-10-02 18:30:00.000', 'ST', 'Lesse01', NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'Ref001', NULL, NULL, NULL, NULL);


INSERT INTO contract_container_inventory (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_type, contract_container_count, contract_number)VALUES
	('2022-10-31 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-10-31 07:27:11.782', 'Admin', NULL, '777', '777', '20DV', 2, 'Ds_Contract_0090'),
	('2022-10-31 07:27:11.782', 'Admin', '{}'::jsonb, true, '2022-10-31 07:27:11.782', 'Admin', NULL, '778', '778', '20DV', 2, 'Ds_Contract_0091');


INSERT INTO container (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_number, container_type, contract_number, max_cargo_weight, remark, tare_weight, vintage) VALUES
	('2022-10-31 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-10-31 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_00103', '20DV', 'Ds_Contract_0090', NULL, NULL, NULL, NULL),
	('2022-10-31 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-10-31 07:32:02.850', 'Admin', NULL, '777', '777', 'Ds_Container_00104', '20DV', 'Ds_Contract_0090', NULL, NULL, NULL, NULL),
	('2022-10-31 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-10-31 07:32:02.850', 'Admin', NULL, '778', '778', 'Ds_Container_00105', '20DV', 'Ds_Contract_0091', NULL, NULL, NULL, NULL),
	('2022-10-31 07:32:02.850', 'Admin', '{}'::jsonb, true, '2022-10-31 07:32:02.850', 'Admin', NULL, '778', '778', 'Ds_Container_00106', '20DV', 'Ds_Contract_0091', NULL, NULL, NULL, NULL);


INSERT INTO contract_container (created, created_by, extra_params, is_active, updated, updated_by, branch, sub_organization_code, tenant_code, container_number, container_status, container_type, contract_number, country, depot, full_empty_status, hbl_number, in_out_status, lessee_code, port, seller_amount, start_date, status_date, stop_date, vessel, vessel_number) VALUES
	('2022-10-31 07:32:02.820', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1664735400, "containerStatus": "ONH"}}'::jsonb, true, '2022-10-31 07:34:46.135', '123', NULL, '777', '777', 'Ds_Container_00103', 'ONH', '20DV', 'Ds_Contract_0090', 'India', 'Depot1', 'FULL', NULL, 'IN', 'Lesse01', 'Port1', 110.00, '2022-10-03 18:30:00.000', '2022-10-29 00:00:00.000', NULL, NULL, NULL),
	('2022-10-31 07:32:02.820', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1664735400, "containerStatus": "LIO"}}'::jsonb, true, '2022-10-31 07:34:46.135', '123', NULL, '777', '777', 'Ds_Container_00104', 'LIO', '20DV', 'Ds_Contract_0090', 'India', 'Depot1', 'FULL', NULL, 'IN', 'Lesse01', 'Port1', 110.00, '2022-10-03 18:30:00.000', '2022-10-29 00:00:00.000', NULL, NULL, NULL),
	('2022-10-31 07:32:02.820', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1664735400, "containerStatus": "ONH"}}'::jsonb, true, '2022-10-31 07:34:46.135', '123', NULL, '777', '777', 'Ds_Container_00105', 'ONH', '20DV', 'Ds_Contract_0091', 'India', 'Depot1', 'FULL', NULL, 'IN', 'Lesse01', 'Port1', 110.00, '2022-10-03 18:30:00.000', '2022-10-29 00:00:00.000', NULL, NULL, NULL),
	('2022-10-31 07:32:02.820', 'Admin', '{"START_CONTAINER_INFO": {"port": "Port001", "depot": "Depot001", "price": 110, "country": "India", "paymentDate": 1664735400, "containerStatus": "LIO"}}'::jsonb, true, '2022-10-31 07:34:46.135', '123', NULL, '777', '777', 'Ds_Container_00106', 'LIO', '20DV', 'Ds_Contract_0091', 'India', 'Depot1', 'FULL', NULL, 'IN', 'Lesse01', 'Port1', 110.00, '2022-10-03 18:30:00.000', '2022-10-29 00:00:00.000', NULL, NULL, NULL);


