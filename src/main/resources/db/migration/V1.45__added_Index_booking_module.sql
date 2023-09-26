CREATE INDEX IF NOT EXISTS container_booking_id ON containers( booking_id );
CREATE INDEX IF NOT EXISTS packing_booking_id ON packing( booking_id );
CREATE INDEX IF NOT EXISTS routings_booking_id ON routings( booking_id );
CREATE INDEX IF NOT EXISTS booking_charges_booking_id ON booking_charges( booking_id );

CREATE INDEX IF NOT EXISTS file_repo_entity_id ON file_repo( entity_id );
CREATE INDEX IF NOT EXISTS file_repo_entity_type ON file_repo( entity_type );

CREATE INDEX IF NOT EXISTS audit_log_entity_id ON audit_log( entity_id );
CREATE INDEX IF NOT EXISTS audit_log_entity ON audit_log( entity );
