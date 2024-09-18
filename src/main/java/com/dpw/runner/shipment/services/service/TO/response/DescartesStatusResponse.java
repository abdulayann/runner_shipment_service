package com.dpw.runner.shipment.services.service.TO.response;

import com.dpw.runner.shipment.services.entity.ResponseEntity;
import lombok.Getter;
import lombok.Setter;

import java.util.List;


@Getter
@Setter
public class DescartesStatusResponse {

    private List<ResponseEntity> integrationStatusResponse;

    private String status;

    private String awbNumber;

    private String messageType;
}
