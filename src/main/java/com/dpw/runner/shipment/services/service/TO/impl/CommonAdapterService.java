package com.dpw.runner.shipment.services.service.TO.impl;

import com.dpw.runner.shipment.services.dto.response.AwbAirMessagingResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.TO.DescarteServiceFactory;
import com.dpw.runner.shipment.services.service.TO.request.AwbData;
import com.dpw.runner.shipment.services.service.TO.request.Root;
import org.apache.kafka.common.protocol.types.Field;
import org.apache.poi.ss.formula.functions.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
public class CommonAdapterService {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    DescarteServiceFactory descarteServiceFactory;

    public void process(String request) {
        //Root request = jsonHelper.readFromJson(message, Root.class);
        //if (request != null && Objects.equals(request.event, EAWBConstants.SHIPMENT_DESCARTES_MESSAGE_EVENT))
        AwbData awbData = jsonHelper.readFromJson(request, AwbData.class);
        descarteServiceFactory.getMessageService(awbData.getAwbShipmentInfo().getEntityType()).process(awbData);
    }

    public void test(String request) {
        //Root request = jsonHelper.readFromJson(message, Root.class);
       // if (request != null && Objects.equals(request.event, EAWBConstants.SHIPMENT_DESCARTES_MESSAGE_EVENT))
        AwbData awbData = jsonHelper.readFromJson(request, AwbData.class);
        descarteServiceFactory.getMessageService(awbData.getAwbShipmentInfo().getEntityType()).process(awbData);
    }

}
