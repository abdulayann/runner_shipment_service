CREATE TABLE IF NOT EXISTS dps_approval_detail (
    dps_event_id BIGINT not null,
    username varchar(255) not null,
    action_time TIMESTAMP,
    message TEXT,
    state varchar(255),
    approval_level varchar(255),
    role_name varchar(255),
    role_id varchar(255),
    CONSTRAINT fk_dps_approval_detail
      FOREIGN KEY (dps_event_id) REFERENCES dps_event(id)
      ON DELETE CASCADE
);