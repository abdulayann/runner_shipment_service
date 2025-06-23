-- Update voyage field only if it is NULL
UPDATE routings
SET voyage = flight_number
WHERE voyage IS NULL;
