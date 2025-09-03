ALTER TABLE IF EXISTS consolidation_details
  ADD COLUMN IF NOT EXISTS booking_agent_id bigint,
  ADD COLUMN IF NOT EXISTS booking_agent_number varchar(50),
  ADD COLUMN IF NOT EXISTS booking_agent_bl_number varchar(50);