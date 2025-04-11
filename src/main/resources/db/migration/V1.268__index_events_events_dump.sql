CREATE INDEX IF NOT EXISTS idx_events_entity_id ON events(entity_id);
CREATE INDEX IF NOT EXISTS idx_events_entity_type ON events(entity_type);
CREATE INDEX IF NOT EXISTS idx_events_entity_id_type ON events(entity_id, entity_type);

CREATE INDEX IF NOT EXISTS idx_events_dump_entity_id ON events_dump(entity_id);
CREATE INDEX IF NOT EXISTS idx_events_dump_entity_type ON events_dump(entity_type);
CREATE INDEX IF NOT EXISTS idx_events_dump_entity_id_type ON events_dump(entity_id, entity_type);