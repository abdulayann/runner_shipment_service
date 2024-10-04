ALTER TABLE shipment_setting
ADD CONSTRAINT unique_tenant_id UNIQUE (tenant_id);