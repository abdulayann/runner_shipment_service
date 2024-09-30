Alter Table IF EXISTS achieved_quantities
    Add Column IF NOT EXISTS weight_utilization varchar,
    Add Column IF NOT EXISTS volume_utilization varchar;