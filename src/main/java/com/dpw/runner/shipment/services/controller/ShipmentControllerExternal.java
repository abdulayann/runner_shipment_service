package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping(ShipmentConstants.SHIPMENT_EXTERNAL_API_HANDLE)
@Slf4j
public class ShipmentControllerExternal {
    private IShipmentServiceV3 shipmentService;

    @Autowired
    public ShipmentControllerExternal(IShipmentServiceV3 shipmentService) {
        this.shipmentService = shipmentService;
    }

    @PostMapping(ApiConstants.API_LIST)
    public ResponseEntity<IRunnerResponse> list(@RequestBody @Valid ListCommonRequest listCommonRequest) {
        log.info("Received shipment list v3 external request with RequestId: {}", LoggerHelper.getRequestIdFromMDC());
        return shipmentService.listShipment(CommonRequestModel.buildRequest(listCommonRequest));
    }
}
