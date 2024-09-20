UPDATE carrier_details SET aircraft_type = SUBSTRING(aircraft_type, 1, 20) WHERE LENGTH(aircraft_type) > 20;
UPDATE carrier_details SET aircraft_registration = SUBSTRING(aircraft_registration, 1, 20) WHERE LENGTH(aircraft_registration) > 20;
UPDATE carrier_details SET journey_number = SUBSTRING(journey_number, 1, 10) WHERE LENGTH(journey_number) > 10;
UPDATE carrier_details SET journey_ref_number = SUBSTRING(journey_ref_number, 1, 10) WHERE LENGTH(journey_ref_number) > 10;
UPDATE carrier_details SET origin = SUBSTRING(origin, 1, 100) WHERE LENGTH(origin) > 100;
UPDATE carrier_details SET shipping_line = SUBSTRING(shipping_line, 1, 64) WHERE LENGTH(shipping_line) > 64;
UPDATE carrier_details SET destination = SUBSTRING(destination, 1, 100) WHERE LENGTH(destination) > 100;


UPDATE shipment_additional_details SET inspection = SUBSTRING(inspection, 1, 3) WHERE LENGTH(inspection) > 3;
UPDATE shipment_additional_details SET airway_bill_dims = SUBSTRING(airway_bill_dims, 1, 3) WHERE LENGTH(airway_bill_dims) > 3;
UPDATE shipment_additional_details SET phase = SUBSTRING(phase, 1, 3) WHERE LENGTH(phase) > 3;
UPDATE shipment_additional_details SET spot_rate_type = SUBSTRING(spot_rate_type, 1, 3) WHERE LENGTH(spot_rate_type) > 3;
UPDATE shipment_additional_details SET efreight_status = SUBSTRING(efreight_status, 1, 3) WHERE LENGTH(efreight_status) > 3;
UPDATE shipment_additional_details SET cha_job_number = SUBSTRING(cha_job_number, 1, 20) WHERE LENGTH(cha_job_number) > 20;
UPDATE shipment_additional_details SET ad_code = SUBSTRING(ad_code, 1, 10) WHERE LENGTH(ad_code) > 10;
UPDATE shipment_additional_details SET smtp_igm_number = SUBSTRING(smtp_igm_number, 1, 10) WHERE LENGTH(smtp_igm_number) > 10;
UPDATE shipment_additional_details SET external_notes = SUBSTRING(external_notes, 1, 256) WHERE LENGTH(external_notes) > 256;
UPDATE shipment_additional_details SET release_type = SUBSTRING(release_type, 1, 3) WHERE LENGTH(release_type) > 3;
UPDATE shipment_additional_details SET house_bill_type = SUBSTRING(house_bill_type, 1, 3) WHERE LENGTH(house_bill_type) > 3;
UPDATE shipment_additional_details SET on_board = SUBSTRING(on_board, 1, 3) WHERE LENGTH(on_board) > 3;
UPDATE shipment_additional_details SET screening_status = SUBSTRING(screening_status, 1, 3) WHERE LENGTH(screening_status) > 3;
UPDATE shipment_additional_details SET shipper_cod_pm = SUBSTRING(shipper_cod_pm, 1, 3) WHERE LENGTH(shipper_cod_pm) > 3;

UPDATE consolidation_details SET consolidation_number = SUBSTRING(consolidation_number, 1, 20) WHERE LENGTH(consolidation_number) > 20;
UPDATE consolidation_details SET transport_mode = SUBSTRING(transport_mode, 1, 3) WHERE LENGTH(transport_mode) > 3;
UPDATE consolidation_details SET consolidation_type = SUBSTRING(consolidation_type, 1, 100) WHERE LENGTH(consolidation_type) > 100;
UPDATE consolidation_details SET container_category = SUBSTRING(container_category, 1, 100) WHERE LENGTH(container_category) > 100;
UPDATE consolidation_details SET service_level = SUBSTRING(service_level, 1, 20) WHERE LENGTH(service_level) > 20;
UPDATE consolidation_details SET payment = SUBSTRING(payment, 1, 3) WHERE LENGTH(payment) > 3;
UPDATE consolidation_details SET agent_reference = SUBSTRING(agent_reference, 1, 64) WHERE LENGTH(agent_reference) > 64;
UPDATE consolidation_details SET co_load_mbl = SUBSTRING(co_load_mbl, 1, 64) WHERE LENGTH(co_load_mbl) > 64;
UPDATE consolidation_details SET co_load_booking_reference = SUBSTRING(co_load_booking_reference, 1, 64) WHERE LENGTH(co_load_booking_reference) > 64;
UPDATE consolidation_details SET mrn_number = SUBSTRING(mrn_number, 1, 50) WHERE LENGTH(mrn_number) > 50;
UPDATE consolidation_details SET igm_file_no = SUBSTRING(igm_file_no, 1, 10) WHERE LENGTH(igm_file_no) > 10;
UPDATE consolidation_details SET smtp_igm_number = SUBSTRING(smtp_igm_number, 1, 10) WHERE LENGTH(smtp_igm_number) > 10;
UPDATE consolidation_details SET edi_transaction_id = SUBSTRING(edi_transaction_id, 1, 50) WHERE LENGTH(edi_transaction_id) > 50;
UPDATE consolidation_details SET receiving_agent_freetext_address = SUBSTRING(receiving_agent_freetext_address, 1, 256) WHERE LENGTH(receiving_agent_freetext_address) > 256;



UPDATE containers SET hbl_delivery_mode = SUBSTRING(hbl_delivery_mode, 1, 9) WHERE LENGTH(hbl_delivery_mode) > 9;
UPDATE containers SET inner_package_number = SUBSTRING(inner_package_number, 1, 100) WHERE LENGTH(inner_package_number) > 100;
UPDATE containers SET inner_package_type = SUBSTRING(inner_package_type, 1, 50) WHERE LENGTH(inner_package_type) > 50;
UPDATE containers SET packs = SUBSTRING(packs, 1, 50) WHERE LENGTH(packs) > 50;
UPDATE containers SET packs_type = SUBSTRING(packs_type, 1, 50) WHERE LENGTH(packs_type) > 50;
UPDATE containers SET marks_n_nums = SUBSTRING(marks_n_nums, 1, 50) WHERE LENGTH(marks_n_nums) > 50;
UPDATE containers SET inner_package_measurement_unit = SUBSTRING(inner_package_measurement_unit, 1, 50) WHERE LENGTH(inner_package_measurement_unit) > 50;
UPDATE containers SET chargeable_unit = SUBSTRING(chargeable_unit, 1, 3) WHERE LENGTH(chargeable_unit) > 3;
UPDATE containers SET extra_params = SUBSTRING(extra_params, 1, 2000) WHERE LENGTH(extra_params) > 2000;
UPDATE containers SET remarks = SUBSTRING(remarks, 1, 1000) WHERE LENGTH(remarks) > 1000;
UPDATE containers SET allocated_weight_unit = SUBSTRING(allocated_weight_unit, 1, 4) WHERE LENGTH(allocated_weight_unit) > 4;
UPDATE containers SET achieved_weight_unit = SUBSTRING(achieved_weight_unit, 1, 4) WHERE LENGTH(achieved_weight_unit) > 4;
UPDATE containers SET achieved_volume_unit = SUBSTRING(achieved_volume_unit, 1, 4) WHERE LENGTH(achieved_volume_unit) > 4;
UPDATE containers SET serial_number = SUBSTRING(serial_number, 1, 100) WHERE LENGTH(serial_number) > 100;

UPDATE parties SET address_code = SUBSTRING(address_code, 1, 100) WHERE LENGTH(address_code) > 100;

UPDATE events SET entity_type = SUBSTRING(entity_type, 1, 100) WHERE LENGTH(entity_type) > 100;
UPDATE events SET place_name = SUBSTRING(place_name, 1, 100) WHERE LENGTH(place_name) > 100;
UPDATE events SET place_description = SUBSTRING(place_description, 1, 100) WHERE LENGTH(place_description) > 100;
UPDATE events SET latitude = SUBSTRING(latitude, 1, 100) WHERE LENGTH(latitude) > 100;
UPDATE events SET longitude = SUBSTRING(longitude, 1, 100) WHERE LENGTH(longitude) > 100;
UPDATE events SET source = SUBSTRING(source, 1, 100) WHERE LENGTH(source) > 100;
UPDATE events SET event_code = SUBSTRING(event_code, 1, 210) WHERE LENGTH(event_code) > 210;
UPDATE events SET description = SUBSTRING(description, 1, 512) WHERE LENGTH(description) > 512;

UPDATE file_repo SET entity_type = SUBSTRING(entity_type, 1, 100) WHERE LENGTH(entity_type) > 100;
UPDATE file_repo SET file_name = SUBSTRING(file_name, 1, 512) WHERE LENGTH(file_name) > 512;
UPDATE file_repo SET path = SUBSTRING(path, 1, 1024) WHERE LENGTH(path) > 1024;

UPDATE hbl_terms_condition_template SET template_code = SUBSTRING(template_code, 1, 50) WHERE LENGTH(template_code) > 50;
UPDATE hbl_terms_condition_template SET template_file_name = SUBSTRING(template_file_name, 1, 100) WHERE LENGTH(template_file_name) > 100;

UPDATE packing SET handling_info = SUBSTRING(handling_info, 1, 2500) WHERE LENGTH(handling_info) > 2500;

UPDATE routings SET mode = SUBSTRING(mode, 1, 4) WHERE LENGTH(mode) > 4;
UPDATE routings SET vessel_name = SUBSTRING(vessel_name, 1, 2048) WHERE LENGTH(vessel_name) > 2048;

UPDATE truck_driver_details SET driver_mobile_number = SUBSTRING(driver_mobile_number, 1, 25) WHERE LENGTH(driver_mobile_number) > 25;
UPDATE truck_driver_details SET truck_number_plate = SUBSTRING(truck_number_plate, 1, 20) WHERE LENGTH(truck_number_plate) > 20;
UPDATE truck_driver_details SET trailer_number_plate = SUBSTRING(trailer_number_plate, 1, 20) WHERE LENGTH(trailer_number_plate) > 20;
UPDATE truck_driver_details SET truck_or_trailer_type_id = SUBSTRING(truck_or_trailer_type_id, 1, 50) WHERE LENGTH(truck_or_trailer_type_id) > 50;
UPDATE truck_driver_details SET container_type_code = SUBSTRING(container_type_code, 1, 20) WHERE LENGTH(container_type_code) > 20;


UPDATE jobs SET order_number = SUBSTRING(order_number, 1, 50) WHERE LENGTH(order_number) > 50;
UPDATE jobs SET confirm_number = SUBSTRING(confirm_number, 1, 50) WHERE LENGTH(confirm_number) > 50;
UPDATE jobs SET invoice_number = SUBSTRING(invoice_number, 1, 50) WHERE LENGTH(invoice_number) > 50;
UPDATE jobs SET order_status = SUBSTRING(order_status, 1, 3) WHERE LENGTH(order_status) > 3;
UPDATE jobs SET description = SUBSTRING(description, 1, 1000) WHERE LENGTH(description) > 1000;
UPDATE jobs SET currency = SUBSTRING(currency, 1, 3) WHERE LENGTH(currency) > 3;
UPDATE jobs SET service_mode = SUBSTRING(service_mode, 1, 50) WHERE LENGTH(service_mode) > 50;
UPDATE jobs SET inco_term = SUBSTRING(inco_term, 1, 50) WHERE LENGTH(inco_term) > 50;
UPDATE jobs SET additional_terms = SUBSTRING(additional_terms, 1, 1000) WHERE LENGTH(additional_terms) > 1000;
UPDATE jobs SET transport_mode = SUBSTRING(transport_mode, 1, 3) WHERE LENGTH(transport_mode) > 3;
UPDATE jobs SET country_of_origin = SUBSTRING(country_of_origin, 1, 3) WHERE LENGTH(country_of_origin) > 3;

UPDATE shipment_details SET transport_mode = SUBSTRING(transport_mode, 1, 4) WHERE LENGTH(transport_mode) > 4;
UPDATE shipment_details SET shipment_type = SUBSTRING(shipment_type, 1, 3) WHERE LENGTH(shipment_type) > 3;
UPDATE shipment_details SET master_bill = SUBSTRING(master_bill, 1, 50) WHERE LENGTH(master_bill) > 50;
UPDATE shipment_details SET additional_terms = SUBSTRING(additional_terms, 1, 2048) WHERE LENGTH(additional_terms) > 2048;
UPDATE shipment_details SET shipment_id = SUBSTRING(shipment_id, 1, 50) WHERE LENGTH(shipment_id) > 50;
UPDATE shipment_details SET volume_unit = SUBSTRING(volume_unit, 1, 10) WHERE LENGTH(volume_unit) > 10;
UPDATE shipment_details SET entry_detail = SUBSTRING(entry_detail, 1, 3) WHERE LENGTH(entry_detail) > 3;
UPDATE shipment_details SET goods_value_currency = SUBSTRING(goods_value_currency, 1, 3) WHERE LENGTH(goods_value_currency) > 3;
UPDATE shipment_details SET insurance_value_currency = SUBSTRING(insurance_value_currency, 1, 3) WHERE LENGTH(insurance_value_currency) > 3;
UPDATE shipment_details SET job_status = SUBSTRING(job_status, 1, 3) WHERE LENGTH(job_status) > 3;
UPDATE shipment_details SET entry_ref_no = SUBSTRING(entry_ref_no, 1, 250) WHERE LENGTH(entry_ref_no) > 250;

UPDATE pickup_delivery_details SET ucr_reference = SUBSTRING(ucr_reference, 1, 50) WHERE LENGTH(ucr_reference) > 50;


ALTER TABLE carrier_details
    ALTER COLUMN aircraft_type TYPE VARCHAR(20),
    ALTER COLUMN aircraft_registration TYPE VARCHAR(20),
    ALTER COLUMN journey_number TYPE VARCHAR(10),
    ALTER COLUMN journey_ref_number TYPE VARCHAR(10),
    ALTER COLUMN origin TYPE VARCHAR(100),
    ALTER COLUMN shipping_line TYPE VARCHAR(64),
    ALTER COLUMN destination TYPE VARCHAR(100);

ALTER TABLE shipment_additional_details
    ALTER COLUMN inspection TYPE VARCHAR(3),
    ALTER COLUMN airway_bill_dims TYPE VARCHAR(3),
    ALTER COLUMN phase TYPE VARCHAR(3),
    ALTER COLUMN spot_rate_type TYPE VARCHAR(3),
    ALTER COLUMN efreight_status TYPE VARCHAR(3),
    ALTER COLUMN cha_job_number TYPE VARCHAR(20),
    ALTER COLUMN ad_code TYPE VARCHAR(10),
    ALTER COLUMN smtp_igm_number TYPE VARCHAR(10),
    ALTER COLUMN external_notes TYPE VARCHAR(256),
    ALTER COLUMN release_type TYPE VARCHAR(3),
    ALTER COLUMN house_bill_type TYPE VARCHAR(3),
    ALTER COLUMN on_board TYPE VARCHAR(3),
    ALTER COLUMN screening_status TYPE VARCHAR(3),
    ALTER COLUMN shipper_cod_pm TYPE VARCHAR(3);

ALTER TABLE consolidation_details
    ALTER COLUMN consolidation_number TYPE VARCHAR(20),
    ALTER COLUMN transport_mode TYPE VARCHAR(3),
    ALTER COLUMN consolidation_type TYPE VARCHAR(100),
    ALTER COLUMN container_category TYPE VARCHAR(100),
    ALTER COLUMN service_level TYPE VARCHAR(20),
    ALTER COLUMN payment TYPE VARCHAR(3),
    ALTER COLUMN agent_reference TYPE VARCHAR(64),
    ALTER COLUMN co_load_mbl TYPE VARCHAR(64),
    ALTER COLUMN co_load_booking_reference TYPE VARCHAR(64),
    ALTER COLUMN mrn_number TYPE VARCHAR(50),
    ALTER COLUMN igm_file_no TYPE VARCHAR(10),
    ALTER COLUMN smtp_igm_number TYPE VARCHAR(10),
    ALTER COLUMN edi_transaction_id TYPE VARCHAR(50),
    ALTER COLUMN receiving_agent_freetext_address TYPE VARCHAR(256);


ALTER TABLE containers
    ALTER COLUMN hbl_delivery_mode TYPE VARCHAR(9),
    ALTER COLUMN inner_package_number TYPE VARCHAR(100),
    ALTER COLUMN inner_package_type TYPE VARCHAR(50),
    ALTER COLUMN packs TYPE VARCHAR(50),
    ALTER COLUMN packs_type TYPE VARCHAR(50),
    ALTER COLUMN marks_n_nums TYPE VARCHAR(50),
    ALTER COLUMN inner_package_measurement_unit TYPE VARCHAR(50),
    ALTER COLUMN chargeable_unit TYPE VARCHAR(3),
    ALTER COLUMN extra_params TYPE VARCHAR(2000),
    ALTER COLUMN remarks TYPE VARCHAR(1000),
    ALTER COLUMN allocated_weight_unit TYPE VARCHAR(4),
    ALTER COLUMN achieved_weight_unit TYPE VARCHAR(4),
    ALTER COLUMN achieved_volume_unit TYPE VARCHAR(4),
    ALTER COLUMN serial_number TYPE VARCHAR(100);

ALTER TABLE parties
    ALTER COLUMN address_code TYPE VARCHAR(100);

ALTER TABLE events
    ALTER COLUMN entity_type TYPE VARCHAR(100),
    ALTER COLUMN place_name TYPE VARCHAR(100),
    ALTER COLUMN place_description TYPE VARCHAR(100),
    ALTER COLUMN latitude TYPE VARCHAR(100),
    ALTER COLUMN longitude TYPE VARCHAR(100),
    ALTER COLUMN source TYPE VARCHAR(100),
    ALTER COLUMN event_code TYPE VARCHAR(210),
    ALTER COLUMN description TYPE VARCHAR(512);

ALTER TABLE file_repo
    ALTER COLUMN entity_type TYPE VARCHAR(100),
    ALTER COLUMN file_name TYPE VARCHAR(512),
    ALTER COLUMN path TYPE VARCHAR(1024);

ALTER TABLE hbl_terms_condition_template
    ALTER COLUMN template_code TYPE VARCHAR(50),
    ALTER COLUMN template_file_name TYPE VARCHAR(100);

ALTER TABLE packing
    ALTER COLUMN handling_info TYPE VARCHAR(2500);

ALTER TABLE routings
    ALTER COLUMN mode TYPE VARCHAR(4),
    ALTER COLUMN vessel_name TYPE VARCHAR(2048);

ALTER TABLE truck_driver_details
    ALTER COLUMN driver_mobile_number TYPE VARCHAR(25),
    ALTER COLUMN truck_number_plate TYPE VARCHAR(20),
    ALTER COLUMN trailer_number_plate TYPE VARCHAR(20),
    ALTER COLUMN truck_or_trailer_type_id TYPE VARCHAR(50),
    ALTER COLUMN container_type_code TYPE VARCHAR(20);


ALTER TABLE jobs
    ALTER COLUMN order_number TYPE VARCHAR(50),
    ALTER COLUMN confirm_number TYPE VARCHAR(50),
    ALTER COLUMN invoice_number TYPE VARCHAR(50),
    ALTER COLUMN order_status TYPE VARCHAR(3),
    ALTER COLUMN description TYPE VARCHAR(1000),
    ALTER COLUMN currency TYPE VARCHAR(3),
    ALTER COLUMN service_mode TYPE VARCHAR(50),
    ALTER COLUMN inco_term TYPE VARCHAR(50),
    ALTER COLUMN additional_terms TYPE VARCHAR(1000),
    ALTER COLUMN transport_mode TYPE VARCHAR(3),
    ALTER COLUMN country_of_origin TYPE VARCHAR(3);

ALTER TABLE shipment_details
    ALTER COLUMN transport_mode TYPE VARCHAR(4),
    ALTER COLUMN shipment_type TYPE VARCHAR(3),
    ALTER COLUMN master_bill TYPE VARCHAR(50),
    ALTER COLUMN additional_terms TYPE VARCHAR(2048),
    ALTER COLUMN shipment_id TYPE VARCHAR(50),
    ALTER COLUMN volume_unit TYPE VARCHAR(10),
    ALTER COLUMN entry_detail TYPE VARCHAR(3),
    ALTER COLUMN goods_value_currency TYPE VARCHAR(3),
    ALTER COLUMN insurance_value_currency TYPE VARCHAR(3),
    ALTER COLUMN job_status TYPE VARCHAR(3),
    ALTER COLUMN entry_ref_no TYPE VARCHAR(250);

ALTER TABLE pickup_delivery_details
    ALTER COLUMN ucr_reference TYPE VARCHAR(50);

