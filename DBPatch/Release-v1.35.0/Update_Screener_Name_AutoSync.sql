UPDATE awb
SET awb_cargo_info = jsonb_set(awb_cargo_info, '{isUserInitialsManuallyAdded}', 'false', true)
WHERE original_printed_at is null;

UPDATE awb
SET awb_cargo_info = jsonb_set(awb_cargo_info, '{isUserInitialsManuallyAdded}', 'true', true)
WHERE original_printed_at is not null;