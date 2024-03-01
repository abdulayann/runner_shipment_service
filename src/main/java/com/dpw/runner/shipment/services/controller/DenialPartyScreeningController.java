package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerListResponse;
import com.dpw.runner.shipment.services.dto.request.DenialPartySearchEntityRequest;
import com.dpw.runner.shipment.services.dto.response.DenialPartySearchResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.service.interfaces.IDenialPartyScreeningService;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;

@Slf4j
@SuppressWarnings("ALL")
@RestController
public class DenialPartyScreeningController {
    @Autowired
    private IDenialPartyScreeningService denialPartyScreeningService;
    @PostMapping(value = "/denial-party/search-entity")
    public ResponseEntity<IRunnerResponse> fetchbyQuery(@Valid @RequestBody @NonNull DenialPartySearchEntityRequest request) {
        return denialPartyScreeningService.createRequestAndSearchEntity(request);
    }

}


