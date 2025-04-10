CREATE INDEX IF NOT EXISTS idx_logs_history_entity_created
ON logs_history (entity_guid, created_at DESC);
