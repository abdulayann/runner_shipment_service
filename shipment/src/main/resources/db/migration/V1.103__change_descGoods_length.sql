UPDATE containers SET description_of_goods = SUBSTRING(description_of_goods, 1, 2048) WHERE LENGTH(description_of_goods) > 2048;
ALTER TABLE IF EXISTS containers
    ALTER COLUMN description_of_goods TYPE VARCHAR(2048);