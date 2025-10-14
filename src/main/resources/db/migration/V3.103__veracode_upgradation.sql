ALTER TABLE console_shipment_mapping ALTER COLUMN request_type TYPE SMALLINT;

ALTER TABLE date_time_change_logs ALTER COLUMN date_type TYPE SMALLINT;
ALTER TABLE hbl_terms_condition_template ALTER COLUMN type_of_hbl_print TYPE SMALLINT;
ALTER TABLE product_sequence_config ALTER COLUMN product_process_types TYPE SMALLINT;
ALTER TABLE product_sequence_config ALTER COLUMN generation_type TYPE SMALLINT;
ALTER TABLE routings ALTER COLUMN carriage TYPE SMALLINT;
ALTER TABLE shipment_setting ALTER COLUMN bol_number_generation TYPE SMALLINT;
ALTER TABLE shipment_setting ALTER COLUMN org_number_generation TYPE SMALLINT;
ALTER TABLE shipment_setting ALTER COLUMN shipment_id_generation_type TYPE SMALLINT;
ALTER TABLE tenant_products ALTER COLUMN product_type TYPE SMALLINT;