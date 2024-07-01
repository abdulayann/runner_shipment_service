package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ILogsHistoryDao;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.response.LogHistoryResponse;
import com.dpw.runner.shipment.services.entity.LogsHistory;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ILogsHistoryService;
import com.dpw.runner.shipment.services.utils.JsonCompression;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@Slf4j
public class LogsHistoryService implements ILogsHistoryService {

    @Autowired
    private ILogsHistoryDao logsHistoryDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Override
    public void createLogHistory(LogHistoryRequest request) {
        try {
            byte[] entityPayload = JsonCompression.compressJson(request.getEntityPayload());
            LogsHistory logsHistory = LogsHistory.builder()
                    .entityId(request.getEntityId())
                    .entityType(request.getEntityType())
                    .entityGuid(request.getEntityGuid())
                    .entityPayload(entityPayload)
                    .build();
            log.info("LogHistory Request: " + jsonHelper.convertToJson(logsHistory));
            logsHistoryDao.save(logsHistory);
            log.info("LogHistory created successfully.");
        } catch (Exception ex){
            log.error("Error occur during Log Entity Creation: " + ex.getMessage());
        }
    }

    @Override
    public LogHistoryResponse retrieveByEntityGuid(UUID entityGuid) throws RunnerException {
        var logsHistory = logsHistoryDao.findByEntityGuid(entityGuid);
        if(logsHistory.isEmpty()){
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        try {
            String entityPayload = JsonCompression.decompressJson(logsHistory.get().getEntityPayload());
            return LogHistoryResponse.builder()
                    .entityId(logsHistory.get().getEntityId())
                    .entityGuid(logsHistory.get().getEntityGuid())
                    .entityType(logsHistory.get().getEntityType())
                    .entityPayload(entityPayload)
                    .build();
        } catch (Exception ex) {
            log.error("Failed to decompress the entity json :" + ex.getMessage());
            throw new RunnerException("Failed to decompress the entity json :" + ex.getMessage());
        }
    }
}
