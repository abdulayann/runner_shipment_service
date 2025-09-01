UPDATE events set description = CASE
    WHEN event_code = 'INGE' THEN 'Generation of Invoice (Origin)'
    WHEN event_code = 'INGI' THEN 'Generation of Invoice (Destination)'
    WHEN event_code = 'INGO' THEN 'Generation of Invoice (Triangulation)'
    ELSE description
END
WHERE event_code IN ('INGE', 'INGI', 'INGO');