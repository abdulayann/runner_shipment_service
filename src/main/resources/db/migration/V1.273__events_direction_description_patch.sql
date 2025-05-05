UPDATE events
SET description = 'Arrived at Destination Port',
    direction = 'IMP'
WHERE event_code = 'ARDP' AND "source" = 'Cargoes Tracking' AND is_deleted = false;

UPDATE events
SET description = 'Empty Container Returned',
    direction = 'IMP'
WHERE event_code = 'EMCR' AND "source" = 'Cargoes Tracking' AND is_deleted = false;

UPDATE events
SET description = 'Full Container Gate in',
    direction = 'EXP'
WHERE event_code = 'FCGI' AND "source" = 'Cargoes Tracking' AND is_deleted = false;

UPDATE events
SET description = 'Flight Arrival',
    direction = 'IMP'
WHERE event_code = 'FLAR' AND "source" = 'Cargoes Tracking' AND is_deleted = false;

UPDATE events
SET description = 'Flight Departure',
    direction = 'EXP'
WHERE event_code = 'FLDR' AND "source" = 'Cargoes Tracking' AND is_deleted = false;

UPDATE events
SET description = 'Full Container Gate Out',
    direction = 'IMP'
WHERE event_code = 'FUGO' AND "source" = 'Cargoes Tracking' AND is_deleted = false;

UPDATE events
SET description = 'Consignee Notified',
    direction = 'IMP'
WHERE event_code = 'TNFD' AND "source" = 'Cargoes Tracking' AND is_deleted = false;

UPDATE events
SET description = 'Received from Flight',
    direction = 'IMP'
WHERE event_code = 'TRCF' AND "source" = 'Cargoes Tracking' AND is_deleted = false;

UPDATE events
SET description = 'Vessel Departed',
    direction = 'EXP'
WHERE event_code = 'VSDP' AND "source" = 'Cargoes Tracking' AND is_deleted = false;
