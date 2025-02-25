package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.helpers.*;
import com.dpw.runner.shipment.services.service.interfaces.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class ShipmentServiceV3 implements IShipmentServiceV3 {

    @Autowired
    IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    INotificationDao notificationDao;


    @Override
    public ResponseEntity<IRunnerResponse> getPendingNotificationCount() {

        return ResponseHelper.buildSuccessResponse();
    }
    
}
