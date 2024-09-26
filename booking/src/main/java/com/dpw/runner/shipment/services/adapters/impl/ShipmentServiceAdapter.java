package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.ShipmentServiceConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IShipmentServiceAdapter;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.Objects;

@Service
@Slf4j
public class ShipmentServiceAdapter implements IShipmentServiceAdapter {

    @Autowired
    private ShipmentServiceConfig shipmentServiceConfig;

    @Autowired
    private JsonHelper jsonHelper;

    public static class ShipmentResponse extends RunnerResponse<ShipmentDetailsResponse>{}


    private final RestTemplate restTemplate;

    public ShipmentServiceAdapter(@Qualifier("restTemplateForShipment") RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    public ShipmentDetailsResponse createShipment(ShipmentDetailsResponse shipmentDetailsResponse) throws RunnerException {
        try {
            String url = shipmentServiceConfig.getBaseUrl() + shipmentServiceConfig.getCreateShipmentInV2Url();
            log.info("Calling shipment service for url: {}", url);
            String shipmentDetailsResponsejson = jsonHelper.convertToJson(shipmentDetailsResponse);
            log.info("shipmentDetailsResponsejson: {}", shipmentDetailsResponsejson);
            ResponseEntity<?> response = restTemplate.exchange(RequestEntity.post(url).body(shipmentDetailsResponsejson), RunnerResponse.class);
            return jsonHelper.readFromJson((((RunnerResponse<?>) Objects.requireNonNull(response.getBody())).getData()).toString(), ShipmentDetailsResponse.class);
        } catch (Exception e) {
            log.error("Error occurred while creating shipment: {}", e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getShipmentIdbyGuid(String guid) throws RunnerException {
        try {
            String url = shipmentServiceConfig.getBaseUrl() + shipmentServiceConfig.getGetIdByGuidUrl() + "?guid=" + guid;
            log.info("Calling shipment service for guid: {}", guid);
            RequestEntity<Void> requestEntity = RequestEntity.get(url).build();
            ResponseEntity<ShipmentResponse> response = restTemplate.exchange(requestEntity, ShipmentResponse.class);
            return ResponseEntity.status(response.getStatusCode()).body(response.getBody());
        }catch (HttpClientErrorException.BadRequest e) {
            log.warn("BadRequest encountered for guid: {}", guid);
            return ResponseEntity.ok(null);
        } catch (Exception e) {
            log.error("Error occurred while getting shipment: {}", e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

}
