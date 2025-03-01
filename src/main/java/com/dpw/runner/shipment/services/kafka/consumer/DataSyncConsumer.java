package com.dpw.runner.shipment.services.kafka.consumer;

import com.dpw.runner.shipment.services.kafka.dto.SyncKafkaDto;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;


@Service
@Slf4j
@Generated
public class DataSyncConsumer {
    @Autowired
    private ISyncService syncService;
    @Autowired
    private V1AuthHelper v1AuthHelper;
    @Autowired
    private ObjectMapper objectMapper;

//    @KafkaListener(
//            topics = {"#{'${data.sync.kafka.queue}'}"},
//            autoStartup = "#{'${data.sync.kafka.autostart}'}",
//            groupId = "#{'${data.sync.kafka.subs}'}")
    public void consume(String message)
    {
        try {
            log.info("DataSyncConsumer message :" + message);
            SyncKafkaDto obj = objectMapper.readValue(message, SyncKafkaDto.class);
            syncService.callSync(obj.getData(), obj.getId(), obj.getGuid(), obj.getEntity(), v1AuthHelper.getHeadersForDataSyncFromKafka(obj.getUserName(), obj.getTenantId(), obj.getUpdateUsername()));
            log.info("Passed");
        } catch (Exception ex) {
            log.error("Exception occurred for event: {} for message: {} with exception: {}", LoggerEvent.KAFKA_PULL_FOR_V1_SYNC, message, ex.getLocalizedMessage());
        }
    }
}
