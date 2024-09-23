package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.ShipmentServiceConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IShipmentServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
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

import java.util.Optional;
import java.util.UUID;

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

    public static class MyResponseClass extends RunnerResponse<ShipmentDetailsResponse>{}

    @Override
    public ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException {
        try {
            String url = shipmentServiceConfig.getBaseUrl() + shipmentServiceConfig.getCreateShipmentInV2Url();

            log.info("Calling shipment service for booking number: {}", customerBookingRequest.getBookingNumber());

            HttpEntity<CustomerBookingRequest> entity = new HttpEntity<>(customerBookingRequest, V1AuthHelper.getHeaders());

            ResponseEntity<MyResponseClass> response = restTemplate.postForEntity(url, entity, MyResponseClass.class);

            return ResponseEntity.status(response.getStatusCode()).body(response.getBody());
        } catch (Exception e) {
            log.error("Error occurred while creating shipment: {}", e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getShipmentIdbyGuid(String guid) throws RunnerException {
        try {
            String url = shipmentServiceConfig.getBaseUrl() + shipmentServiceConfig.getGetByGuidUrl() + "?guid=" + guid;

            log.info("Calling shipment service for booking number: {}", guid);

            HttpEntity<CustomerBookingRequest> entity = new HttpEntity<>(V1AuthHelper.getHeaders());

            ResponseEntity<MyResponseClass> response = restTemplate.exchange(url, HttpMethod.GET, entity, MyResponseClass.class);

            return ResponseEntity.status(response.getStatusCode()).body(response.getBody());
        } catch (Exception e) {
            log.error("Error occurred while getting shipment: {}", e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

}
