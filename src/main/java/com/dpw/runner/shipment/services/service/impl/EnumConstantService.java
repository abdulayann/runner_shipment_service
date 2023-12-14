package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.response.EnumConstantResponse;
import com.dpw.runner.shipment.services.dto.response.EnumResponse;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.service.interfaces.IEnumConstantService;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class EnumConstantService implements IEnumConstantService {
    @Override
    public ResponseEntity<?> list() {
        Map<String, List<EnumConstantResponse>> response = new HashMap<>();

        List<EnumConstantResponse> enumList = new ArrayList<>();
        for (ShipmentStatus shipmentStatus : ShipmentStatus.values()) {
            enumList.add(EnumConstantResponse.builder().id(shipmentStatus.getValue()).description(shipmentStatus.getDescription()).name(shipmentStatus.name()).build());
        }
        response.put(Constants.SHIPMENT_STATUS, enumList);

        enumList = new ArrayList<>();
        for (MasterDataType dataType : MasterDataType.values()) {
            enumList.add(EnumConstantResponse.builder().id(dataType.getId()).description(dataType.name()).name(dataType.name()).build());
        }
        response.put(Constants.MASTER_LIST, enumList);

        enumList = new ArrayList<>();
        for (Ownership dataType : Ownership.values()) {
            enumList.add(EnumConstantResponse.builder().id(dataType.getValue()).description(dataType.getDescription()).name(dataType.name()).build());
        }
        response.put(Constants.MASTER_LIST, enumList);

        enumList = new ArrayList<>();
        for (BookingStatus dataType : BookingStatus.values()) {
            enumList.add(EnumConstantResponse.builder().id(dataType.getValue()).description(dataType.getDescription()).name(dataType.name()).build());
        }
        response.put(Constants.BOOKING_STATUS, enumList);

        enumList = new ArrayList<>();
        for (TransportInstructionTemplateType dataType : TransportInstructionTemplateType.values()) {
            enumList.add(EnumConstantResponse.builder().id(dataType.getValue()).description(dataType.getDescription()).name(dataType.name()).build());
        }
        response.put(Constants.TI_TEMPLATE_TYPE, enumList);

        enumList = new ArrayList<>();
        for (CustomerCategoryRates dataType : CustomerCategoryRates.values()) {
            enumList.add(EnumConstantResponse.builder().id(dataType.getValue()).description(dataType.getDescription()).name(dataType.name()).build());
        }
        response.put(Constants.CUSTOMER_CATEGORY_RATES, enumList);

        return ResponseHelper.buildSuccessResponse(EnumResponse.builder().dataMap(response).build());

    }
}
