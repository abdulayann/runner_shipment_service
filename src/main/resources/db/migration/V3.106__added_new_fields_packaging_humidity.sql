ALTER TABLE packing
ADD COLUMN temp_set_point DECIMAL(10,2) NULL,
ADD COLUMN temp_set_point_unit VARCHAR(10) NULL,
ADD COLUMN min_humidity DECIMAL(10,2) NULL,
ADD COLUMN max_humidity DECIMAL(10,2) NULL,
ADD COLUMN humidity_set_point DECIMAL(10,2) NULL;