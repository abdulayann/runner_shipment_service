CREATE INDEX IF NOT EXISTS idx_dps_event_entity_id ON dps_event(entity_id);
CREATE INDEX IF NOT EXISTS idx_dps_event_entity_type ON dps_event(entity_type);
CREATE INDEX IF NOT EXISTS idx_dps_event_status ON dps_event(status);
CREATE INDEX IF NOT EXISTS idx_dps_event_implication_dps_event_id ON dps_event_implication(dps_event_id);
CREATE INDEX IF NOT EXISTS idx_dps_event_implication_implication ON dps_event_implication(implication);
