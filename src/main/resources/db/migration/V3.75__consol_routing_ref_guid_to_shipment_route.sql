ALTER TABLE routings
ADD COLUMN consol_route_ref_guid uuid DEFAULT uuid_generate_v4() NULL;