package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.AirMessagingLogsConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.customxmltojsonmapper.XMLToJSONConverter;
import com.dpw.runner.shipment.services.dao.interfaces.IAirMessagingLogsDao;
import com.dpw.runner.shipment.services.dto.request.AirMessagingLogsRequest;
import com.dpw.runner.shipment.services.dto.response.AirMessagingLogsResponse;
import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import com.dpw.runner.shipment.services.entity.enums.MessageType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.TO.impl.CommonAdapterService;
import com.dpw.runner.shipment.services.service.TO.impl.ExternalIntegrationService;
import com.dpw.runner.shipment.services.service.TO.response.ExternalResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingLogsService;
import com.dpw.runner.shipment.services.service.interfaces.IAirMessagingPayloadService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class AirMessagingPayloadService implements IAirMessagingPayloadService {
    private final IAirMessagingLogsDao airMessagingLogsDao;
    private final CommonAdapterService commonAdapterService;
    private final XMLToJSONConverter xmlToJSONConverter;
    @Autowired
    ExternalIntegrationService externalIntegrationService;

    @Autowired
    public AirMessagingPayloadService(IAirMessagingLogsDao airMessagingLogsDao, CommonAdapterService commonAdapterService, XMLToJSONConverter xmlToJSONConverter) {
        this.airMessagingLogsDao = airMessagingLogsDao;
        this.commonAdapterService = commonAdapterService;
        this.xmlToJSONConverter = xmlToJSONConverter;
    }

    @Override
    public ResponseEntity<JSONObject> getFailedPayloadById(Long id) throws RunnerException, IOException {
        String xmlPayload = airMessagingLogsDao.findById(id).get().getXmlPayload();
        JSONObject jsonObject = xmlToJSONConverter.xmlToJsonConverter(xmlPayload);
        return ResponseEntity.ok(jsonObject);
    }

    @Override
    @Retryable(
            value = RunnerException.class,
            maxAttempts = 3,
            backoff = @Backoff(delay = 2000))
    public ResponseEntity<IRunnerResponse> resubmitFailedPayload(Object payload, String messageType) throws RunnerException {
        ExternalResponse externalResponse = new ExternalResponse();
        externalIntegrationService.convertJsontoXML(getMessageType(messageType), payload, externalResponse);
        externalIntegrationService.file(externalResponse.getXmlPayload(), externalResponse);
        return ResponseHelper.buildSuccessResponse();
    }

    private MessageType getMessageType(String messageType) {
        if(Objects.equals(messageType, "FWB"))
            return MessageType.FWB;
        return MessageType.FZB;
    }

}
