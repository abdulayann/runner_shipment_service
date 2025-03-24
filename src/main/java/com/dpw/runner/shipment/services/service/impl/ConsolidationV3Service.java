package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.ShipmentAttachDetachV3Request;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class ConsolidationV3Service implements IConsolidationV3Service {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IConsolidationService consolidationService;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;


    @Override
    public String attachShipments(ShipmentAttachDetachV3Request shipmentAttachDetachRequest) {
        return null;
    }
}
