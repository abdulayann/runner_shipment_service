UPDATE public.shipment_setting
SET is_network_transfer_entity_enabled=true,
is_entity_transfer_prerequisite_enabled=true,
is_entity_transfer_prerequisite_enabled_date=now(),
updated_at=now()
where tenant_id in ('735');