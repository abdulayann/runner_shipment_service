package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.config.ShipmentServiceConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IShipmentServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.dpw.runner.shipment.services.utils.V2AuthHelper;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.time.LocalDateTime;

@Service
@Slf4j
public class ShipmentServiceAdapter implements IShipmentServiceAdapter {

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

    @Override
    public ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException {
        try {
            RestTemplate restTemplate = shipmentServiceConfig.restTemplateForShipment();
            String url = shipmentServiceConfig.getBaseUrl() + shipmentServiceConfig.getCreateShipmentInV2Url();

            log.info("Calling shipment service for booking number: {}", customerBookingRequest.getBookingNumber());

            HttpEntity<CustomerBookingRequest> entity = new HttpEntity<>(customerBookingRequest, V1AuthHelper.getHeaders());

            return restTemplate.postForEntity(url, entity, IRunnerResponse.class);
        } catch (Exception e) {
            log.error("Error occurred while creating shipment: {}", e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }
}
