ALTER TABLE customer_booking_backup
ADD CONSTRAINT customer_booking_backup_pkey PRIMARY KEY (id);

ALTER TABLE shipment_backup
ADD CONSTRAINT shipment_backup_pkey PRIMARY KEY (id);

ALTER TABLE consolidation_backup
ADD CONSTRAINT consolidation_backup_pkey PRIMARY KEY (id);

ALTER TABLE network_transfer_backup
ADD CONSTRAINT network_transfer_backup_pkey PRIMARY KEY (id);
