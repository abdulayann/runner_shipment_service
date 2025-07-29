ALTER TABLE ti_packages
    ALTER COLUMN gross_weight TYPE numeric;
ALTER TABLE ti_packages
    ALTER COLUMN net_weight TYPE numeric;
ALTER TABLE ti_packages
    ALTER COLUMN volume TYPE numeric;
ALTER TABLE ti_containers
    ALTER COLUMN gross_weight TYPE numeric;
ALTER TABLE ti_containers
    ALTER COLUMN net_weight TYPE numeric;
ALTER TABLE ti_containers
    ALTER COLUMN volume TYPE numeric;