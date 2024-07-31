package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.entity.AirMessagingLogs;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public interface IAirMessagingLogsService extends ICommonService {

    AirMessagingLogs getRecentLogForEntityGuid(UUID guid);
}
