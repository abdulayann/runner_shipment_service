package com.dpw.runner.shipment.services.service.descarts.impl;

import com.dpw.runner.shipment.services.dto.DenialParty.request.SearchEntity;
import com.dpw.runner.shipment.services.dto.DenialParty.request.SearchEntityRequest;
import com.dpw.runner.shipment.services.dto.request.DenialPartySearchEntityRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.descarts.IDescartsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.List;


@Service
public class DescartsService implements IDescartsService {

    @Value("${descarts.service.searchentity.url}")
    private String EXTERNAL_API_URL;
    @Autowired
    private RestTemplate restTemplate;
    //private static final String EXTERNAL_API_URL = "https://rpstest.visualcompliance.com/RPS/RPSService.svc/SearchEntity";

    @Override
    public ResponseEntity<?> searchEntity(SearchEntityRequest commonRequestModel) {

        ResponseEntity<?> response;
        try {
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            HttpEntity<Object> requestEntity = new HttpEntity<>(commonRequestModel, headers);

            response = restTemplate.exchange(
                    EXTERNAL_API_URL,
                    HttpMethod.POST,
                    requestEntity,
                    Object.class
            );

            if (response.getStatusCode() == HttpStatus.OK) {
                return new ResponseEntity<>(response.getBody(), HttpStatus.OK);
            }
        } catch (Exception e) {
            return ResponseHelper.buildFailedResponse(e.getMessage());
        }
        return response;
    }
}
