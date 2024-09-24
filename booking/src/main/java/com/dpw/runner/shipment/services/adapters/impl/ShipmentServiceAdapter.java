package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.ShipmentServiceConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IShipmentServiceAdapter;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.ConsolidationDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.Objects;

@Service
@Slf4j
public class ShipmentServiceAdapter implements IShipmentServiceAdapter {

    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ShipmentServiceConfig shipmentServiceConfig;

    @Autowired
    private V1AuthHelper v1AuthHelper;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private CommonUtils commonUtils;

    public static class ShipmentResponse extends RunnerResponse<ShipmentDetailsResponse>{}
    public static class ConsolidationResponse extends RunnerResponse<ConsolidationDetailsResponse>{}

    @Override
    public ShipmentDetailsResponse createShipment(ShipmentDetailsResponse shipmentDetailsResponse) throws RunnerException {
        try {
            String url = shipmentServiceConfig.getBaseUrl() + shipmentServiceConfig.getCreateShipmentInV2Url();
            log.info("Calling shipment service for url: {}", url);
            String shipmentDetailsResponsejson = jsonHelper.convertToJson(shipmentDetailsResponse);
            HttpEntity<String> entity = new HttpEntity<>(shipmentDetailsResponsejson, V1AuthHelper.getHeaders());
            ResponseEntity<?> response = restTemplate.postForEntity(url, entity, RunnerResponse.class);
            return jsonHelper.readFromJson((((RunnerResponse<?>) Objects.requireNonNull(response.getBody())).getData()).toString(), ShipmentDetailsResponse.class);
        } catch (Exception e) {
            log.error("Error occurred while creating shipment: {}", e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ConsolidationDetailsResponse createConsolidation(ConsolidationDetailsRequest consolidationDetailsRequest) throws RunnerException {
        try {
            ConsolidationDetailsResponse consolidationDetailsResponse = jsonHelper.convertValue(consolidationDetailsRequest, ConsolidationDetailsResponse.class);
            String url = shipmentServiceConfig.getBaseUrl() + shipmentServiceConfig.getCreateConsolidationFromBookingUrl();
            String consolidationDetailsResponsejson = jsonHelper.convertToJson(consolidationDetailsResponse);
            HttpEntity<String> entity = new HttpEntity<>(consolidationDetailsResponsejson, V1AuthHelper.getHeaders());
            ResponseEntity<?> response = restTemplate.postForEntity(url, entity, RunnerResponse.class);
            return jsonHelper.readFromJson((((RunnerResponse<?>) Objects.requireNonNull(response.getBody())).getData()).toString(), ConsolidationDetailsResponse.class);
        } catch (Exception e) {
            log.error("Error occurred while creating consolidation: {}", e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getShipmentIdbyGuid(String guid) throws RunnerException {
        try {
            String url = shipmentServiceConfig.getBaseUrl() + shipmentServiceConfig.getGetByGuidUrl() + "?guid=" + guid;
            log.info("Calling shipment service for guid: {}", guid);
            HttpEntity<CustomerBookingRequest> entity = new HttpEntity<>(V1AuthHelper.getHeaders());
            ResponseEntity<ShipmentResponse> response = restTemplate.exchange(url, HttpMethod.GET, entity, ShipmentResponse.class);
            return ResponseEntity.status(response.getStatusCode()).body(response.getBody());
        } catch (Exception e) {
            log.error("Error occurred while getting shipment: {}", e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

}
