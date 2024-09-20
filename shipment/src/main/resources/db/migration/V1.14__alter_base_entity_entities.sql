ALTER TABLE console_shipment_mapping
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE achieved_quantities
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE shipment_additional_details
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE allocations
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE arrival_departure_details
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE awb
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE booking_carriage
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE carrier_details
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE consolidation_details
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE containers
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE dg_mapping
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE el_details
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE email_templates
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE events
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE file_repo
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE hawb_lock_settings
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE hbl
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE hbl_lock_settings
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE hbl_terms_condition_template
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE jobs
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE logs
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE mawb_hawb_link
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE mawb_lock_settings
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE notes
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE packing
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE parties
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE pickup_delivery_details
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE product_sequence_config
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE reference_numbers
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE routings
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE services
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE shipment_details
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50),
   ALTER COLUMN assigned_to TYPE varchar (50);
ALTER TABLE shipments_containers_mapping
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE shipment_setting
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE tenant_products
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE truck_driver_details
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE validations
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE views
   ALTER COLUMN created_by TYPE varchar (50),
   ALTER COLUMN updated_by TYPE varchar (50);
ALTER TABLE events
  RENAME master_list TO event_code;
