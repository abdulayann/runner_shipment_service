package com.dpw.runner.shipment.services.service.TO;

import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducerHelper;
import com.dpw.runner.shipment.services.commons.EAWBConstants;
import com.dpw.runner.shipment.services.entity.IntegrationEntity;
import com.dpw.runner.shipment.services.entity.ResponseEntity;
import com.dpw.runner.shipment.services.entity.ShipmentStatusPayload;
import com.dpw.runner.shipment.services.entity.enums.MessageType;
import com.dpw.runner.shipment.services.entity.enums.ResponseMessageType;
import com.dpw.runner.shipment.services.entity.enums.StatusType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.TO.impl.DBService;
import com.dpw.runner.shipment.services.service.TO.impl.ExternalIntegrationService;
import com.dpw.runner.shipment.services.service.TO.response.ExternalResponse;
import com.dpw.runner.shipment.services.utils.annotationimpl.StringModifierImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import javax.validation.ConstraintViolation;
import javax.validation.Validator;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public abstract class AbstractMessageService<X,Y> implements IDescartes<X, Y> {


    @Autowired
    DBService dbService;

    @Autowired
    private JsonHelper jsonHelperr;

    @Autowired
    ExternalIntegrationService externalIntegrationService;

    @Autowired
    public JsonHelper jsonHelper;

    @Autowired
    private Validator validator;

    @Value("${eawb.kafka.producer.queue}")
    private String shipmentKafkaTopic;

    @Autowired
    private KafkaProducerHelper kafkaProducerHelper;

    @Override
    public void process(X data) {
        ExternalResponse externalResponse = new ExternalResponse();
        Y payload = this.convertPayload(data);
        IntegrationEntity integrationEntity = this.createEntity(data, payload);
        //1) save payload in DB
        IntegrationEntity ent = dbService.saveIntegration(integrationEntity);

        try {
            StringModifierImpl.modifyString(payload);
        } catch (Exception ex) {
            log.error("CustomSpecialCharSerializer error : " + ex.getMessage());
        }
        //2) validations check
        this.validate(payload, externalResponse);
        //return if any Internal validation errors
        if (externalResponse.status != null && externalResponse.status.isError()) {
            this.updateMainTable(ent, externalResponse);
            kafkaProducerHelper.produceToKafka(createPayload(ent, externalResponse), UUID.randomUUID().toString() , getHeaders(), shipmentKafkaTopic, null);
            return;
        }
        //3) Convert json to XML
        externalIntegrationService.convertJsontoXML(this.getMessageType(), payload, externalResponse);
        if (externalResponse.status != null && externalResponse.status.isError()) {
            this.updateMainTable(ent, externalResponse);
            kafkaProducerHelper.produceToKafka(createPayload(ent, externalResponse), UUID.randomUUID().toString() , getHeaders(), shipmentKafkaTopic, null);
            return;
        }
        this.updateMainTable(ent, externalResponse);
        kafkaProducerHelper.produceToKafka(createPayload(ent, externalResponse), UUID.randomUUID().toString() , getHeaders(), shipmentKafkaTopic, null);
        ResponseEntity responseEntity = dbService.saveIntegrationResponse(ResponseEntity.builder().integrationEntityId(ent).build());
        //4) Descartes Filing
        externalIntegrationService.file(externalResponse.getXmlPayload(), externalResponse);

        if (externalResponse.getStatus()!= null && externalResponse.getStatus().isError()) {
            this.updateMainTable(ent, externalResponse);
        }
        ShipmentStatusPayload shipmentStatusPayload  = createPayload(ent, externalResponse);
        //5) Save in Ack table
        dbService.saveIntegrationResponse(createAck(responseEntity, externalResponse, shipmentStatusPayload ));
        //6) publish event if there is any error
        if (externalResponse.getStatus() != null && externalResponse.getStatus().isError()) {
            kafkaProducerHelper.produceToKafka(shipmentStatusPayload, UUID.randomUUID().toString() , getHeaders(), shipmentKafkaTopic, null);
        }
    }

    protected ResponseEntity createAck(ResponseEntity responseEntity, ExternalResponse externalResponse, ShipmentStatusPayload shipmentStatusPayload) {
        responseEntity.setMessageTypeKey(ResponseMessageType.XFNM);
        responseEntity.setMessageTypeValue(externalResponse.getStatus().name());
        responseEntity.setTransactionId(externalResponse.getTid());
        responseEntity.setInPayload(jsonHelper.convertValue(externalResponse.getDescartesResponse(), LinkedHashMap.class));
        responseEntity.setMetaData(externalResponse.getMessage());
        responseEntity.setOutPayload(jsonHelper.convertValue(shipmentStatusPayload, LinkedHashMap.class));
        return responseEntity;
    }

    protected  abstract MessageType getMessageType();

    protected abstract IntegrationEntity createEntity(X x, Y y);

    private void validate(Y data, ExternalResponse externalResponse) {
        Set<ConstraintViolation<Y>> validations =  validator.validate(data);
        if (!validations.isEmpty()) {
            String errors = validations.parallelStream()
                    .map(ConstraintViolation::getMessage)
                    .collect(Collectors.joining(","));
            externalResponse.setStatus(StatusType.INTERNAL_VALIDATION_ERROR);
            externalResponse.setMessage(errors);
        }
    }


    private ShipmentStatusPayload createPayload(IntegrationEntity ent, ExternalResponse externalResponse) {
        return ShipmentStatusPayload.builder()
                .guid(ent.getEntityId())
                .status(ent.getStatus())
                .errorMessage(ent.getMetaData())
                .xmlPayload(externalResponse.getXmlPayload())
                .build();
    }

    private void updateMainTable(IntegrationEntity ent, ExternalResponse externalResponse) {
        ent.setStatus(externalResponse.getStatus());
        ent.setMetaData(externalResponse.getMessage());
        ent.setXmlData(externalResponse.getXmlPayload());
        dbService.saveIntegration(ent);
    }

    private Map<String, String> getHeaders() {
        Map<String, String> headers = new HashMap<>();
        headers.put(EAWBConstants.HEADER_TYPE_TYPE, EAWBConstants.HEADER_TYPE_STATUS);
        return headers;
    }

}
